#Objetivo: mostrar como se genera y aplica un modelo de LightGBM

#source( "/cloud/cloud1/codigoR/lightgbm/lightgbm_basico.r" )

#limpio la memoria
rm( list=ls() )
gc()

script.name<- "lightgbm-basico"
kextension<-"exthist"

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
karchivo_generacion   <-   "nuevo_febrero.rds"
karchivo_aplicacion   <-   "nuevo_abril.rds"
karchivo_validacion   <-   "nuevo_abril.rds"

kmin_corte <- 0.005
kmax_corte <- 0.090

seed<-ksemilla_azar[5]

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

calcular_ganancia   <- function(probs, clases) 
{
  
  #  vlabels <- xgboost::getinfo(clases, "label")
  
  out<-data.frame(prob_corte=as.numeric(), ganancia=as.numeric())
  for (p in seq(kmin_corte,kmax_corte,.0001)) {
    sal<-data.frame(prob_corte=p,ganancia=sum((probs > p) * ifelse( clases == 1, 11700, -300 )))
    #  print(paste(sal,"\n"))
    out<-rbind(out,sal)
  }
  val<-out %>% filter(ganancia==max(out$ganancia)) %>% head(1)
  
  
  return(  val )
}

preparar_lgbm<- function(dataset, clase) {
  #genero one-hot enconding
  options(na.action='na.pass')
  formula  <- formula(paste("~ .-1"))
  
  dataset_unido_matrix  = model.matrix(formula,data = dataset[ , -which(names(dataset) == clase)   ])
  dataset_sinclase_sparse = as(dataset_unido_matrix, "dgCMatrix")
  
  #genero el formato requerido por LightGBM
  dlight  <-   lgb.Dataset( data  = data.matrix(dataset_sinclase_sparse),
                            label = dataset[ , clase], 
                            missing=NA,
                            free_raw_data=FALSE 
  )
  
  rm(dataset_unido_matrix,dataset_sinclase_sparse)
  gc()
  
  return(dlight)
  
}



preparar_xgb<- function(dataset,clase) {
  
  options(na.action='na.pass')
  formula  <- formula(paste("~ .-1"))
  
  dataset_unido_matrix  = model.matrix(formula,data = dataset[ , -which(names(dataset) == clase)   ])
  dataset_sinclase_sparse = as(dataset_unido_matrix, "dgCMatrix")
  
  #genero el formato requerido por XGBOOST
  dboost  <-   xgb.DMatrix( data  = data.matrix(dataset_sinclase_sparse),
                            label = dataset[ , clase], 
                            missing=NA
  )
  
  rm(dataset_unido_matrix,dataset_sinclase_sparse)
  gc()
  
  return(dboost)
}


preparar_matriz<-function(dataset,clase) {
  
  options(na.action='na.pass')
  formula  <- formula(paste("~ .-1"))
  dataset_unido_matrix  = model.matrix(formula, data = dataset[ , -which(names(dataset) == clase)   ])
  dataset_sinclase_sparse = as(dataset_unido_matrix, "dgCMatrix")
  rm(dataset_unido_matrix)
  gc()
  
  return(dataset_sinclase_sparse)
}

#------------------------------------------------------------------------------

setwd(  directory.datasets )

dataset_generacion    <-  as.data.table(readRDS( karchivo_aplicacion)) 

#genero el formato requerido por LightGBM
dgeneracion  <-   preparar_lgbm(dataset_generacion,kclase_nomcampo)

#-------------------------

dataset_aplicacion <-  as.data.table(readRDS( karchivo_aplicacion)) 
#genero el formato requerido por LightGBM
daplicacion  <-   preparar_lgbm(dataset_aplicacion,kclase_nomcampo)


t0       <-  Sys.time()

modelo = lightgbm::lgb.train( 
               data = dgeneracion,
               objective = "binary",
               valids=list(validacion=daplicacion),
               seed=seed,
               num_iterations=2000,
               early_stopping_round=300,
               init_score= .005,
#               eval=fganancia_logistic_lightgbm_binaria,
               metric="auc",
               verbose=2,
               bagging_fraction=1, 
               feature_fraction=0.300966366658785, 
               learning_rate=0.00480114553818008, 
               min_data_in_leaf=66,
#               min_child_weight=8, 
               max_depth=10, 
               lambda_l1=0.691594630645328,
#               lambda_l2=10,
               max_bin=31, 
               num_leaves=478,    
               min_gain_to_split = 0.678784428363361,
               subsample=1
#               is_unbalance = TRUE
              )

t1       <-  Sys.time()

tiempo <-  as.numeric(  t1 - t0, units = "secs")

#-------------------------
#aplico el modelo

setwd(  directory.datasets )

#dataset_aplicacion_sinclase_sparse  <- sparse.model.matrix( formula, data = dataset_aplicacion_sinclase )

dataset_validacion <-  as.data.table(readRDS( karchivo_validacion)) 

dataset_validacion_sinclase_sparse = preparar_matriz(dataset_validacion,kclase_nomcampo)

#aplico el modelo a datos nuevos
aplicacion_prediccion  <- predict(  modelo, as.matrix( dataset_validacion_sinclase_sparse) )



# calculo la ganancia
gan <-  calcular_ganancia(aplicacion_prediccion,  dataset_validacion[ , kclase_nomcampo] ) 

# calculo el AUC
auc <-  fmetrica_auc_lightgbm( aplicacion_prediccion,  dataset_validacion[ , kclase_nomcampo] ) 


cat( "ganancia = ",  gan$ganancia , "\n")
cat( "prob_corte = ",  gan$prob_corte , "\n")
cat( "AUC = ",  auc, "\n"  )
cat( "tiempo = ",  tiempo, "\n"  )

setwd(directory.work)

#save(modelo,file=paste0("lgb-",seed))

