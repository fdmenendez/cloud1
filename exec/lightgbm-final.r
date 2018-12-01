#Objetivo: mostrar como se genera y aplica un modelo de LightGBM

#source( "/cloud/cloud1/codigoR/lightgbm/lightgbm_basico.r" )

#limpio la memoria
rm( list=ls() )
gc()

script.name<- "lightgbm-final"
kextension <- "exthist"


switch ( Sys.info()[['sysname']],
         Windows = { directory.include  <-  "M:\\codigoR\\include\\"
         directory.work     <-  paste0("M:\\work\\",script.name,"\\")
         directory.plan     <-  "M:\\plan\\"
         directory.datasets <-  paste0("M:\\datasets\\",kextension,"/")
         },
         Linux   = { directory.include  <-  "~/cloud/cloud1/codigoR/include/"
         directory.work     <-  paste0("~/cloud/cloud1/work/",script.name,"/")
         directory.plan     <-  "~/cloud/cloud1/plan/"
         directory.datasets <-  paste0("~/cloud/cloud1/datasets/",kextension,"/")
         }
)




library( "lightgbm" )
library( "Matrix" )



library( "data.table" )


setwd( directory.include )
source( "metrica.r" )
source( "utils.r" )


kcampos_separador     <-  "\t"
kcampo_id             <-  "numero_de_cliente"
kclase_nomcampo       <-  "clase_binaria"
kclase_valor_positivo <-  "BAJA+2"
kcampos_a_borrar      <-  c( kcampo_id )

#karchivo_generacion   <-   "201801_dias.txt,201712_dias.txt,201711_dias.txt"
karchivo_generacion   <-   "201802_,201712_,201711_,201710_,"
karchivo_aplicacion   <-   "201802_exthist.rds"
karchivo_validacion   <-   "201804_exthist.rds"

fganancia_logistic_lightgbm_binaria   <- function(probs, clases) 
{
  
  vlabels <- lightgbm::getinfo(clases, "label")
  
  gan <-sum(   (probs > vprob_corte  ) * 
                 ifelse( vlabels== 1, 11700, -300 ),
               na.rm = TRUE   
  )
  
  
  return(  list( name = "ganancia", 
                 value =  ifelse(  is.na(gan) , 0, gan ) ,
                 higher_better= TRUE 
  )
  )
}

#------------------------------------------------------------------------------

setwd(  directory.datasets )

dataset_generacion    <-  read.datasets(karchivo_generacion, directory.datasets, paste0(kextension,".rds"), "Y","lgb")
dataset_generacion_sinclase   <- dataset_generacion[ , -which(names(dataset_generacion) == kclase_nomcampo)   ]


#genero one-hot enconding
options(na.action='na.pass')
formula  <- formula(paste("~ .-1"))

dataset_unido_matrix  = model.matrix(formula,data = dataset_generacion[ , -which(names(dataset_generacion) == kclase_nomcampo)   ])
dataset_generacion_sinclase_sparse = as(dataset_unido_matrix, "dgCMatrix")
rm(dataset_unido_matrix)
#dataset_generacion_sinclase_sparse  <- sparse.model.matrix( formula, data = dataset_generacion_sinclase )


#genero el formato requerido por LightGBM
dgeneracion  <-   lgb.Dataset( data  = data.matrix(dataset_generacion_sinclase_sparse),
                               label = dataset_generacion[ , kclase_nomcampo], 
                               missing=NA,
                               free_raw_data=FALSE 
                              )

#-------------------------

dataset_aplicacion <-  as.data.table(readRDS( karchivo_aplicacion)) 
dataset_aplicacion <- clean.up.oot.lgb(dataset_aplicacion)

dataset_aplicacion_sinclase   <- dataset_aplicacion[ , -which(names(dataset_aplicacion) == kclase_nomcampo)   ]


#genero one-hot enconding
options(na.action='na.pass')
formula  <- formula(paste("~ .-1"))

dataset_unido_matrix  = model.matrix(formula, data = dataset_aplicacion_sinclase)
dataset_aplicacion_sinclase_sparse = as(dataset_unido_matrix, "dgCMatrix")
rm(dataset_unido_matrix)

#genero el formato requerido por LightGBM
daplicacion  <-   lgb.Dataset( data  = data.matrix(dataset_aplicacion_sinclase_sparse),
                               label = dataset_aplicacion[ , kclase_nomcampo], 
                               missing=NA,
                               free_raw_data=FALSE 
)


vprob_corte<-0.0454221695533055

#genero el modelo

t0       <-  Sys.time()

modelo = lightgbm::lgb.train( 
               data = dgeneracion,
               objective = "binary",
#               valids=list(validacion=daplicacion),
               seed=622513,
               init_score=0.005, 
               bagging_fraction=1, 
               feature_fraction=0.222006641372229, 
               learning_rate=0.00293473843439187, 
               min_data_in_leaf=98, 
               num_leaves=358, 
               lambda_l1=0.311053229656423, 
               min_gain_to_split=0.846700242868809, 
               max_bin=31,
                verbose=2,               
               num_iterations=5000,
#               early_stopping_round=500,
               eval=fganancia_logistic_lightgbm_binaria,
               metric="auc"
#               min_child_weight=8, 
#               max_depth=10, 
#               lambda_l2=10,
#               max_bin=32, 
#               is_unbalance = TRUE
              )

t1       <-  Sys.time()

tiempo <-  as.numeric(  t1 - t0, units = "secs")

#-------------------------
#aplico el modelo

setwd(  directory.datasets )

#dataset_aplicacion_sinclase_sparse  <- sparse.model.matrix( formula, data = dataset_aplicacion_sinclase )

dataset_validacion <-  as.data.table(readRDS( karchivo_validacion)) 
dataset_validacion <- clean.up.oot.lgb(dataset_validacion)

dataset_validacion_sinclase   <- dataset_validacion[ , -which(names(dataset_validacion) == kclase_nomcampo)   ]


#genero one-hot enconding
options(na.action='na.pass')
formula  <- formula(paste("~ .-1"))

dataset_unido_matrix  = model.matrix(formula, data = dataset_validacion_sinclase)
dataset_validacion_sinclase_sparse = as(dataset_unido_matrix, "dgCMatrix")
rm(dataset_unido_matrix)

#aplico el modelo a datos nuevos
aplicacion_prediccion  <- predict(  modelo, as.matrix( dataset_validacion_sinclase_sparse) )



# calculo la ganancia
gan <-  fmetrica_ganancia_lightgbm( vprob_corte, aplicacion_prediccion,  dataset_validacion[ , (kclase_nomcampo)] ) 

# calculo el AUC
auc <-  fmetrica_auc_lightgbm( aplicacion_prediccion,  dataset_validacion[ , (kclase_nomcampo)] ) 


cat( "ganancia = ",  gan , "\n")
cat( "AUC = ",  auc, "\n"  )
cat( "tiempo = ",  tiempo, "\n"  )

setwd(  directory.work)
save(modelo,modelo-lgbm-tune2)


