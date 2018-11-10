#XGBoost  con arboles de altura=1 
#Decision Stumps



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




library( "xgboost" )
library( "Matrix" ) 

library( "data.table" )
library( "DiagrammeR" )




kcampos_separador     <-  "\t"
kcampo_id             <-  "numero_de_cliente"
kclase_nomcampo       <-  "clase_ternaria"
kclase_valor_positivo <-  "BAJA+2"
kcampos_a_borrar      <-  c( kcampo_id )


#La entrada
karchivos_generacion  <-  "201802_dias.txt"
karchivos_aplicacion  <-  "201804_dias.txt"



#La salida
karchivo_importancia  <-  "stumps_importancia_variables.txt"
karchivo_arboles      <-  "stumps_arbolitos.pdf"
karchivo_modelo       <-  "stumps_modelo_xgboost.txt"



karchivo_modelo_simple        <-  "stumps_modelo_simple.txt"
karchivo_modelo_simple_nulos  <-  "stumps_modelo_simple_nulos.txt"

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
veta               <-     0.04
vbase_score         <-    mean( getinfo(dgeneracion, "label") )  #la prob de positivo del universo
vgamma              <-    5
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




#grabo la importancia de las variables
setwd(  directory.work )
write.table(  xgb.importance(colnames(dgeneracion), model = modelo )
              , file= karchivo_importancia,
              , row.names=FALSE
              , col.names=TRUE
              , quote=FALSE
              , sep="\t"
              , eol = "\r\n"
           )



#grabo el dibujo los primeros 20 arboles  0:19
setwd(  directory.work )
gr <- xgb.plot.tree(model = modelo, trees = 0:19, show_node_id = TRUE, render=FALSE)
export_graph(gr, karchivo_arboles, width=1500, height=8000)


#grabo el modelo
setwd(  directory.work )
xgb.dump(model=modelo, fname = karchivo_modelo, with_stats = TRUE, dump_format = "text" )


#------------------


#Ahora empieza el manejo INFERNAL para ver como se genera la suma a partir de cada variable
#esto esta muuuy desprolijo
#esto es muuuy ineficiente
#es patetico hacer esto con loops !


#Te sugiero Rodrigo que veas que queda en esta variable  m,  
#y luego corras hasta el final y veas que queda en el archivo  modelo_simple.txt
m <- xgb.model.dt.tree( model= modelo, feature_names= colnames( dgeneracion ) )



garbol<-data.table( "arbol"    = numeric(), 
                    "atributo" = character(),
                    "corte"    = numeric(),
                    "score0"   = numeric(),
                    "score1"   = numeric()
                   )

garbol_na <-data.table( "arbol"    = numeric(), 
                        "atributo" = character(),
                        "score"    = numeric()
                      )




mlargo <- nrow( m )
for(  i in 1:mlargo )
{
  mm <-  m[ i,  ]

  if( !is.na( mm$Feature) &&  mm$Feature !=  'Leaf'  )
  { 
     indices0  <-  which(mm$Yes == m$ID)[[1]]  
     indices1  <-  which(mm$No  == m$ID)[[1]]  
     vscore0   <-  m[ indices0, "Quality" ]
     vscore1   <-  m[ indices1, "Quality" ]

     garbol_na <-  rbind( garbol_na, 
                          list( "arbol"    =  mm$Tree , 
                                "atributo" =  mm$Feature,  
                                "score"    =  ifelse(  mm$Yes==mm$Missing, vscore0, vscore1 )
                            ),
                          fill=TRUE
                        )



     garbol    <-  rbind( garbol, 
                          list(    "arbol"    =  mm$Tree , 
                                   "atributo" =  mm$Feature,  
                                   "corte"    =  mm$Split, 
                                   "score0"   =  vscore0,
                                   "score1"   =  vscore1
                               ),
                           fill=TRUE
                         )


  }
}


nulos <- garbol_na[ , .( "score" =sum(unlist(score)), cantidad =.N ) , by = atributo ]

buenos <- garbol[ , .( "score0" =sum(unlist(score0)), "score1" =sum(unlist(score1)), cantidad =.N ) , by = list(atributo, corte) ]

buenos <- buenos[ order(atributo, -corte) ]

buenos1 <-  buenos[  , list( corte, score0, score1, cantidad, "score11"=cumsum(unlist(score0)) ) , list(atributo) ]

buenos2 <-  buenos1[ order(atributo, corte) ]

buenos3 <-  buenos2[  , list( corte, score0, score1, cantidad, score11, "score00"=cumsum(unlist(score1)) -score1 ) , list(atributo) ]

buenos4 <- buenos3[ order(atributo, corte) ]

buenos5 <- buenos4[  ,  scorefinal := score00+score11 ]


deha  <-data.table(  
                    "atributo" = character(),
                    "desde"    = numeric(),
                    "hasta"    = numeric(),
                    "score"    = numeric()
                   )


mlargo <- nrow( buenos5 )
for(  i in 1:mlargo )
{

  if( (i==1)  |  
      (i>1 && buenos5[ i-1, "atributo" ]!=buenos5[ i, "atributo" ] ) 
    )
  {
    deha  <-  rbind(  deha,
                      list( "atributo" =  buenos5[ i, "atributo" ],
                            "desde"    =  as.numeric( NA ),
                            "hasta"    =  buenos5[ i, "corte" ],
                            "score"    =  buenos5[ i, "scorefinal"]
                          ),
                      fill=TRUE
                   )
  }


  if( (i>1) &&
      (buenos5[ i-1, "atributo" ]==buenos5[ i, "atributo" ] ) 
    )
  {
    deha  <-  rbind(  deha,
                      list( "atributo" =  buenos5[ i,   "atributo" ],
                            "desde"    =  buenos5[ i-1, "corte" ],
                            "hasta"    =  buenos5[ i,   "corte" ],
                            "score"    =  buenos5[ i,   "scorefinal"]
                          ),
                      fill=TRUE
                   )
  }


  final <- 0
  if( ( i==mlargo)  |  
      ( i<mlargo && buenos5[ i, "atributo" ]!=buenos5[ i+1, "atributo" ] )  
    )  final <- 1 


  if( final )
  {
    deha  <-  rbind(  deha,
                      list( "atributo" =  buenos5[ i, "atributo" ],
                            "desde"    =  buenos5[ i, "corte" ],
                            "hasta"    =  NA,
                            "score"    =  buenos5[ i, "scorefinal"] - buenos5[ i, "score0"] + buenos5[ i, "score1"]
                          ),
                      fill=TRUE
                   )
  }

}


setwd(  directory.work )
fwrite( deha,     file =karchivo_modelo_simple, row.names=FALSE, sep="\t" )
fwrite( nulos,    file =karchivo_modelo_simple_nulos, row.names=FALSE, sep="\t" )

