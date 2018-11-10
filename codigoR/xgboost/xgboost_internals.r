
#limpio la memoria
rm( list=ls() )
gc()



switch ( Sys.info()[['sysname']],
         Windows = { directory.include  <-  "M:\\codigoR\\include\\"
                     directory.work     <-  "M:\\work\\"
                     directory.plan     <-  "M:\\plan\\"
                     directory.datasets <-  "M:\\datasets\\dias\\"
                   },
         Darwin  = { directory.include  <-  "~/dm/codigoR/include/"
                     directory.work     <-  "~/dm/work/"
                     directory.plan     <-  "~/dm/plan/"
                     directory.datasets <-  "~/dm/datasets/dias/"
                   },
         Linux   = { directory.include  <-  "~/cloud/cloud1/codigoR/include/"
                     directory.work     <-  "~/cloud/cloud1/work/"
                     directory.plan     <-  "~/cloud/cloud1/plan/"
                     directory.datasets <-  "~/cloud/cloud1/datasets/dias/"
                   }
        )


library( "data.table" )
library( xgboost )
library( Matrix )
library( DiagrammeR )





kcampos_separador     <-  "\t"
kcampo_id             <-  "numero_de_cliente"
kclase_nomcampo       <-  "clase_ternaria"
kclase_valor_positivo <-  "BAJA+2"
kcampos_a_borrar      <-  c( kcampo_id )


#La entrada
karchivos_generacion  <-  "201802_dias.txt"
karchivos_aplicacion  <-  "201804_dias.txt"



#parametros de salida
karchivo_salida        <- "xgboost_arboles.pdf"
karchivo_modelo        <- "xgboost_modelo.txt"


#------------------------------------------------------

#constantes de la funcion ganancia del problema
kprob_corte           <-      0.025
kganancia_acierto     <-  11700 
kganancia_noacierto   <-   -300


fganancia_logistic_xgboost   <- function(probs, clases) 
{

   vlabels <- getinfo(clases, "label")
  
   gan <-sum(   (probs > kprob_corte  ) * 
                 ifelse( vlabels== 1, kganancia_acierto, kganancia_noacierto )   
            )
        

   return(  list(metric = "ganancia", value =  ifelse(  is.na(gan) , 0, gan ) )  )
}
#------------------------------------------------------


datasets_xgboost = function( parchivos1, parchivos2 )
{
  #cargo 1
  setwd(  directory.datasets )
  varchivos1  <-  unlist(strsplit( parchivos1, split=","))
  dataset1 <- do.call(rbind, lapply( varchivos1, function(x) fread(x, header=TRUE, sep=kcampos_separador)))

  #cargo 2
  setwd(  directory.datasets )
  varchivos2  <-  unlist(strsplit( parchivos2, split=","))
  dataset2 <- do.call(rbind, lapply( varchivos2, function(x) fread(x, header=TRUE, sep=kcampos_separador)))
  
  dataset <-  rbind(  dataset1, dataset2 )
    
  #borro las variables que no me interesan
  dataset[ ,  (kcampos_a_borrar) := NULL    ] 

  #agrego al dataset las TENDENCIAS
  #campos_no_procesar  <- setdiff( names(dataset) ,  kcampos_buenos  )  

  #borro las variables que no me interesan
  #dataset[ ,  (campos_no_procesar) := NULL    ] 


  #dejo la clase en {0,1}  clase  binaria1
  dataset[, (kclase_nomcampo) := as.numeric( get(kclase_nomcampo) == kclase_valor_positivo  )] 

  dataset_sinclase   <- dataset[ , ! ( kclase_nomcampo), with=FALSE   ]


  #genero one-hot enconding
  options(na.action='na.pass')
  formula  <- formula(paste("~ .-1"))
  dataset_sinclase_sparse  <- sparse.model.matrix( formula, data = dataset_sinclase )

  desde1 <- 1
  hasta1 <- nrow( dataset1 )
  desde2 <- hasta1 + 1 
  hasta2 <- hasta1 + nrow( dataset2 )

  #genero el formato requerido por XGBoost
  ddataset1  <-   xgb.DMatrix( data  = data.matrix(dataset_sinclase_sparse[desde1:hasta1,]),
                               label = dataset[ desde1:hasta1 , get(kclase_nomcampo)], 
                               missing=NA 
                            )
							
  #genero el formato requerido por XGBoost
  ddataset2  <-   xgb.DMatrix( data  = data.matrix(dataset_sinclase_sparse[desde2:hasta2,]),
                               label = dataset[ desde2:hasta2 , get(kclase_nomcampo)], 
                               missing=NA 
                            )

  rm( dataset,  dataset_sinclase, dataset_sinclase_sparse )
  gc()

  return( list( "d1"=ddataset1,  "d2"=ddataset2)  )
}
#------------------------------------------------------

#cargo los datos
ldata <-  datasets_xgboost(  karchivos_generacion,  karchivos_aplicacion ) 
dgeneracion <-  ldata$d1
daplicacion <-  ldata$d2



#ATENCION   Decision Stumps,   arboles de altura 1
vmax_depth          <-    1

vnround             <-  528
veta                <-    0.04
vbase_score         <-    1131 / 233848
vgamma              <-    0
vmax_bin            <-  256
vtree_method        <- "hist"


vsubsample          <-    1
vcolsample_bytree   <-    1
vmin_child_weight   <-    0
valpha              <-    0
vlambda             <-    0



modelo  <- xgb.train( 
                      data = dgeneracion,
                      watchlist = list( train=dgeneracion, test=daplicacion ),
                      missing = NA,
                      objective='binary:logistic',
                      feval = fganancia_logistic_xgboost,
                      tree_method = vtree_method,
                      max_bin     = vmax_bin,
                      base_score  = vbase_score,
                      subsample   = vsubsample,
                      nround      = vnround,
                      eta         = veta,
                      max_depth   = vmax_depth,
                      alpha       = valpha,
                      lambda      = vlambda,
                      gamma       = vgamma,
                      min_child_weight =  vmin_child_weight
                     )    




#dibujo los primeros 10 arboles  0:9
setwd(  directory.work )
gr <- xgb.plot.tree(model = modelo, trees = 0:9, show_node_id = TRUE, render=FALSE)
export_graph(gr, karchivo_salida, width=1500, height=4000)


setwd(  directory.work )
xgb.dump(model=modelo, fname = "modelo.txt", with_stats = TRUE, dump_format = "text" )


#-----------------------------------------------------------
#A partir de aqui,  hago manualmente los calculos de XGBoost

#pongo los primeros cortes, que obtengo manualmente de la corrida de XGBoost
#atencion, NO estoy generando los arboles
#como son de altura 1, los arboles son apenas 1 regla

particiones      <- matrix(  ncol=vnround,  nrow=nrow(dgeneracion) )

setwd(  directory.datasets )
varchivos1  <-  unlist(strsplit( karchivos_generacion, split=","))
dataset  <- do.call(rbind, lapply( varchivos1, function(x) fread(x, header=TRUE, sep=kcampos_separador)))

#dejo la clase en {0,1}  clase  binaria1
dataset[, (kclase_nomcampo) := as.numeric( get(kclase_nomcampo) == kclase_valor_positivo  )] 

#hago esto a mano para no complicar con los nulos
dataset[is.na(Visa_Finiciomora),  Visa_Finiciomora := 0 ]
dataset[is.na(ttarjeta_visa),  ttarjeta_visa := 0 ]
dataset[is.na(mcaja_ahorro_Paquete), mcaja_ahorro_Paquete :=0 ]
dataset[is.na(mdescubierto_preacordado), mdescubierto_preacordado :=0 ]
dataset[is.na(mpasivos_margen), mpasivos_margen := 0 ]


particiones[, 1]  <- as.numeric( dataset[,"Visa_Finiciomora"] < 37 )
particiones[, 2]  <- as.numeric( dataset[,"ttarjeta_visa"] <  0.5 )
particiones[, 3]  <- as.numeric( dataset[,"mcaja_ahorro_Paquete"] < 2.26 )
particiones[, 4]  <- as.numeric( dataset[,"mcaja_ahorro_Paquete"] < 185.86 )
particiones[, 5]  <- as.numeric( dataset[,"tmovimientos_ultimos90dias"] < 30 )
particiones[, 6]  <- as.numeric( dataset[,"mdescubierto_preacordado"] < 0.585 )
particiones[, 7]  <- as.numeric( dataset[,"mcaja_ahorro_Paquete"] <= 218.25)
particiones[, 8]  <- as.numeric( dataset[,"mpasivos_margen"] <  48.15 )
particiones[, 9]  <- as.numeric( dataset[,"mcaja_ahorro_Paquete"] <= 218.25)
particiones[,10]  <- as.numeric( dataset[,"tmovimientos_ultimos90dias"] < 30 )



#calculo inicializacion
scores      <- matrix(  ncol=vnround,  nrow=nrow(dataset) )
scores_acum <- matrix(  ncol=vnround,  nrow=nrow(dataset) )
probs       <- matrix(  ncol=vnround,  nrow=nrow(dataset) )
gradient    <- matrix(  ncol=vnround,  nrow=nrow(dataset) )
hessian     <- matrix(  ncol=vnround,  nrow=nrow(dataset) )


#inicializacion
#tener en cuenta que vbase_score es la probabilidad inicial
scores[ ,1]       <- log( vbase_score/(1-vbase_score) )
scores_acum[ ,1]  <- scores[ ,1]

probs[ , 1]       <- exp(scores_acum[ ,1] )  / ( 1 + exp(scores_acum[ ,1] ))

gradient[ ,1]     <- probs[ ,1]  -  dataset[ , get(kclase_nomcampo)] 
hessian[  ,1]     <- probs[ ,1] * ( 1- probs[ ,1] )


step <-  data.frame(  tree_id=integer(),
                      gradient_left=double(), 
                      gradient_right=double(), 
                      gradient_root=double(),
                      hessian_left=double(),
                      hessian_right=double(), 
                      hessian_root=double(),
                      gain=double(),
                      leaf_left=double(), 
                      leaf_right=double() )


#------------------------------------
#Esta funcion supone que esta calculado todo  t-1
# gradiente  =  prob -  clase
# hessiano   =  prob * ( 1 - prob )
# leaf       =  - gradiente / hessiano
# prob_nueva =  inv.logit(  SUMA( leafs_anteriores ) )


favanzar_xgboost <- function( t ) 
{
  
  tree_id <-  t
  gradient_left   <-    sum( (1-particiones[,t]) * gradient[, t]  )
  gradient_right  <-    sum( particiones[,t]  * gradient[, t] )
  gradient_root   <-    sum( gradient[, t] )


  hessian_left    <-    sum( (1-particiones[ ,t]) * hessian[, t] )
  hessian_right   <-    sum( particiones[ ,t]  * hessian[, t] )
  hessian_root    <-    sum( hessian[, t] )

  gain         <-   (   gradient_left *gradient_left  / hessian_left 
                       + gradient_right*gradient_right / hessian_right
                       - gradient_root *gradient_root  / hessian_root
                      )

  #aqui entra en juego el  eta = learning_rate
  leaf_left       <-   ( - gradient_left  / hessian_left )  * veta
  leaf_right      <-   ( - gradient_right / hessian_right ) * veta

  scores[ particiones[,t]==0, t+1 ]  <<-  leaf_left  
  scores[ particiones[,t]==1, t+1 ]  <<-  leaf_right 

  scores_acum[ , t+1]  <<-  scores_acum[ , t]  + scores[ ,t+1]

  #esto es lo mismo que llamar a  inv.logit
  probs[ , t+1]        <<-  exp(scores_acum[ ,t+1]) / ( 1 + exp(scores_acum[ ,t+1]) )


  gradient[ ,t+1]  <<-  probs[ ,t+1]  -  dataset[ ,get(kclase_nomcampo)] 
  hessian[  ,t+1]  <<-  probs[ ,t+1] * ( 1- probs[ ,t+1] )

  step[ nrow(step)+1, ]  <<-  
                            c(  tree_id,
                                gradient_left, gradient_right, gradient_root,
                                hessian_left,  hessian_right,  hessian_root,
                                gain,
                                leaf_left, leaf_right
                              )
}

#------------------------------------

favanzar_xgboost(  1 )
favanzar_xgboost(  2 )
favanzar_xgboost(  3 )
favanzar_xgboost(  4 )
favanzar_xgboost(  5 )
favanzar_xgboost(  6 )
favanzar_xgboost(  7 )
favanzar_xgboost(  8 )


