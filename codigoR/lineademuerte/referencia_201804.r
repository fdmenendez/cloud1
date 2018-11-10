#Este programa calcula que ganancia hubiera dado la prediccion de lineademuerte en 201804


#limpio la memoria
rm( list=ls() )
gc()




switch ( Sys.info()[['sysname']],
         Windows = { directory.work     <-  "M:\\work\\"
                     directory.datasets <-  "M:\\datasets\\hist\\"
                   },
         Darwin  = { directory.work     <-  "~/dm/work/"
                     directory.datasets <-  "~/dm/datasets/hist/"
                   },
         Linux   = { directory.work     <-  "~/cloud/cloud1/work/"
                     directory.datasets <-  "~/cloud/cloud1/datasets/hist/"
                   }
        )



library( "lightgbm" )
library( "Matrix" ) 

library( "data.table" )



kcampos_separador     <-  "\t"
kcampo_id             <-  "numero_de_cliente"
kclase_nomcampo       <-  "clase_ternaria"
kcampos_a_borrar      <-  c( kcampo_id )

kclase_valor_positivo_binaria2 <-  c( "BAJA+2", "BAJA+1" )
kclase_valor_positivo_binaria1 <-  c( "BAJA+2" )




karchivos_generacion  <-  "201802_hist.txt,201801_hist.txt,201712_hist.txt,201711_hist.txt,201710_hist.txt,201709_hist.txt,201708_hist.txt,201707_hist.txt"
karchivos_aplicacion  <-  "201804_hist.txt"  

#la decision de entrenar en OCHO meses se obtuvo de los testeos hechos sobre junio del 2017

#esta es la probabilidad de corte de {BAJA+2, BAJA+1}
kprob_corte_binaria2  <- 0.0365

#------------------------------------------------------------------------------
#constantes de la funcion ganancia del problema
kganancia_acierto     <-  11700 
kganancia_noacierto   <-   -300


fganancia_lightgbm_binaria2   <- function(probs, clases) 
{

   vlabels   <- getinfo(clases, "label")

  
   gan <-sum(   (probs > kprob_corte_binaria2  ) *
                 ifelse( vlabels== 1, kganancia_acierto, kganancia_noacierto ),
                na.rm = TRUE   
            )
        

   return(  list( name = "ganancia", 
                  value =  ifelse(  is.na(gan) , 0, gan ) ,
                  higher_better= TRUE 
                )
         )
}
#------------------------------------------------------------------------------
#para 201804 SI HAY clase, y SI puedo calcular la ganancia

#cargo los datasets

setwd(  directory.datasets )

varchivos_generacion  <-  unlist(strsplit( karchivos_generacion, split=","))
dataset_generacion    <-  do.call(rbind, lapply( varchivos_generacion, function(x) fread(x, header=TRUE, sep=kcampos_separador)))

#cre un vector con la clase en {0,1}  clase  binaria1
generacion_clase  <-  dataset_generacion[, get(kclase_nomcampo)] 
generacion_clase  <-  as.numeric(  generacion_clase %in% kclase_valor_positivo_binaria2  )


#ahora cargo el dataset de aplicacion
varchivos_aplicacion  <-  unlist(strsplit( karchivos_aplicacion, split=","))
dataset_aplicacion    <-  do.call(rbind, lapply( varchivos_aplicacion, function(x) fread(x, header=TRUE, sep=kcampos_separador)))

#cre un vector con la clase en {0,1}  clase  binaria1
aplicacion_clase  <-  dataset_aplicacion[, get(kclase_nomcampo)] 
aplicacion_clase  <-  as.numeric(  aplicacion_clase %in% kclase_valor_positivo_binaria1  )


#ahora hago la union los datasets, ya que para hacer el One Hot Encoding lo necesito
dataset_unido  <-  rbind(  dataset_generacion, dataset_aplicacion )

#le borro la clase a dataset_unido 
dataset_unido[ ,  (kclase_nomcampo) := NULL    ] 


#borro las variables que no me interesan
dataset_unido[ ,  (kcampos_a_borrar) := NULL    ] 



#genero one-hot enconding
options(na.action='na.pass')
formula  <- formula(paste("~ .-1"))
dataset_unido_sparse  <- sparse.model.matrix( formula, data = dataset_unido )



desde1 <- 1
hasta1 <- nrow( dataset_generacion )
desde2 <- hasta1 + 1 
hasta2 <- hasta1 + nrow( dataset_aplicacion )

#genero el formato requerido por XGBoost
dgeneracion  <-   lgb.Dataset( data  = data.matrix(dataset_unido_sparse[desde1:hasta1,]),
                               label = generacion_clase, 
                               free_raw_data=FALSE
                             )
				
#genero el formato requerido por lightgbm
daplicacion  <-   lgb.Dataset( data  = data.matrix(dataset_unido_sparse[desde2:hasta2,]),
                               label = aplicacion_clase, 
                               free_raw_data=FALSE
                             )

#como soy prolijo, borro lo que ya no voy a usar mas
rm( dataset_generacion, dataset_aplicacion, dataset_unido_sparse, dataset_unido, generacion_clase )
gc()


#---------------------

#Finalmente, llamo al LightGBM
#Uso parametros redondeados  que salieron de una Optimizacion Bayesiana


#LightGBM  con num_iterations=350, init_score=0.005, bagging_fraction=1, feature_fraction=0.14, 
#   learning_rate=0.025, min_data_in_leaf=85, num_leaves=75, lambda_l1=0.14, min_gain_to_split=0.96, max_bin=31


#mi querida random seed
set.seed( 102191  )


modelo = lgb.train( 
                   data = dgeneracion,
                   objective = "binary",
                   valids= list( valid=daplicacion),
                   eval = fganancia_lightgbm_binaria2 ,
                   num_iterations=350, 
                   init_score= 0.05,
                   bagging_fraction=1, 
                   feature_fraction=0.14, 
                   learning_rate=0.025, 
                   min_gain_to_split=0.96, 
                   lambda_l1=0.14,
                   max_bin=31, 
                   num_leaves=75
                 )


# la ganancia sale con la corrida de LightGBM
# [350]:  valid's binary_logloss:0.0280034        valid's ganancia:6.9762e+06
#  6.9762e+06