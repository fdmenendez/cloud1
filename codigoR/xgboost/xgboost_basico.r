#Objetivo: mostrar como se genera y aplica un modelo de XGBoost

#source( "/cloud/cloud1/codigoR/xgboost/xgboost_basico.r" )

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


setwd( directory.include )
source( "metrica.r" )


kcampos_separador     <-  "\t"
kcampo_id             <-  "numero_de_cliente"
kclase_nomcampo       <-  "clase_ternaria"
kclase_valor_positivo <-  "BAJA+2"
kcampos_a_borrar      <-  c( kcampo_id )

karchivo_generacion   <-   "201801_dias.txt,201712_dias.txt,201711_dias.txt"
karchivo_aplicacion   <-   "201804_dias.txt"


#------------------------------------------------------------------------------

setwd(  directory.datasets )
varchivos_generacion  <-  unlist(strsplit( karchivo_generacion, split=","))
dataset_generacion    <-  do.call(rbind, lapply( varchivos_generacion, function(x) fread(x, header=TRUE, stringsAsFactors=TRUE, sep=kcampos_separador)))


#borro las variables que no me interesan
dataset_generacion[ ,  (kcampos_a_borrar) := NULL    ] 

#dejo la clase en {0,1}  clase  binaria1
dataset_generacion[, (kclase_nomcampo) := as.numeric( get(kclase_nomcampo) == kclase_valor_positivo  )] 

dataset_generacion_sinclase   <- dataset_generacion[ , ! ( kclase_nomcampo), with=FALSE   ]


#genero one-hot enconding
options(na.action='na.pass')
formula  <- formula(paste("~ .-1"))
dataset_generacion_sinclase_sparse  <- sparse.model.matrix( formula, data = dataset_generacion_sinclase )


#genero el formato requerido por XGBoost
dgeneracion  <-   xgb.DMatrix( data  = data.matrix(dataset_generacion_sinclase_sparse),
                               label = dataset_generacion[ , get(kclase_nomcampo)], 
                               missing=NA 
                              )

#-------------------------
#genero el modelo

t0       <-  Sys.time()

modelo = xgb.train( 
               data = dgeneracion,
               missing = NA,
               objective="binary:logistic",
               nround=700, 
               base_score=mean(getinfo(dgeneracion, "label")),
               subsample=1, 
               colsample_bytree=0.50, 
               eta=0.020, 
               min_child_weight=8, 
               max_depth=10, 
               alpha=0.40, 
               lambda=0.50,
               gamma=10,
               max_bin=32, 
               tree_method='hist',
               num_leaves=255
              )

t1       <-  Sys.time()

tiempo <-  as.numeric(  t1 - t0, units = "secs")

#-------------------------
#aplico el modelo

setwd(  directory.datasets )
dataset_aplicacion <-  fread( karchivo_aplicacion, header=TRUE, sep=kcampos_separador) 

#borro las variables que no me interesan
dataset_aplicacion[ ,  (kcampos_a_borrar) := NULL    ] 


#dejo la clase en {0,1}  clase  binaria1
dataset_aplicacion[, (kclase_nomcampo) := as.numeric( get(kclase_nomcampo) == kclase_valor_positivo  )] 

dataset_aplicacion_sinclase   <- dataset_aplicacion[ , ! ( kclase_nomcampo), with=FALSE   ]


#genero one-hot enconding
options(na.action='na.pass')
formula  <- formula(paste("~ .-1"))
dataset_aplicacion_sinclase_sparse  <- sparse.model.matrix( formula, data = dataset_aplicacion_sinclase )


#genero el formato requerido por XGBoost
daplicacion  <-   xgb.DMatrix( data  = data.matrix(dataset_aplicacion_sinclase_sparse),
                               label = dataset_aplicacion[ , get(kclase_nomcampo)], 
                               missing=NA 
                              )

#aplico el modelo a datos nuevos
aplicacion_prediccion  <- predict(  modelo, daplicacion )



# calculo la ganancia
gan <-  fmetrica_ganancia_xgboost( 0.025, aplicacion_prediccion,  dataset_aplicacion[ , get(kclase_nomcampo)] ) 

# calculo el AUC
auc <-  fmetrica_auc_xgboost( aplicacion_prediccion,  dataset_aplicacion[ , get(kclase_nomcampo)] ) 


cat( "ganancia = ",  gan , "\n")
cat( "AUC = ",  auc, "\n"  )
cat( "tiempo = ",  tiempo, "\n"  )



