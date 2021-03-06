#Objetivo: mostrar como se genera y aplica un modelo de LightGBM

#source( "/cloud/cloud1/codigoR/lightgbm/lightgbm_basico.r" )

#limpio la memoria
rm( list=ls() )
gc()

script.name<- "lightgbm-ensemble"
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
library("xgboost")



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
karchivo_generacion   <-    "201804_,201802_,201712_,201711_,201710_"
karchivo_generacion_ant <-  "201804_,201802_,201612_,201611_,201610_"
#karchivo_aplicacion   <-   paste0("201804_",kextension,".rds")
karchivo_validacion   <-   paste0("201806_",kextension,".rds")

kmin_corte <- 0.005
kmax_corte <- 0.1
kcortes<-seq(kmin_corte,kmax_corte,.0001)


lgb_iterations<-1000
lgb_stop<-200

xgb_iterations<-500
xgb_stop<-100

karchivo_salida<-"salida.txt"


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
  gan_bruta<-ifelse( clases == 1, 11700, -300 )
  for (p in kcortes) {
    sal<-data.frame(prob_corte=p,ganancia=sum((probs > p) * gan_bruta))
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

dataset_generacion    <-  as.data.frame(sample.datasets(karchivo_generacion, directory.datasets, 
                                                        paste0(kextension,".rds"), "Y","lgb"
                                                        ,1,123
))

dgeneracion<-preparar_lgbm(dataset_generacion,kclase_nomcampo)

#-------------------------

dataset_validacion <-  as.data.table(readRDS( karchivo_validacion)) 
#dataset_validacion <- clean.up.oot.lgb(dataset_validacion)
dataset_validacion$numero_de_cliente <- NULL 
dataset_validacion$clase_ternaria <- NULL 
dataset_validacion <- dataset_validacion %>% mutate_all(funs(replace(., is.nan(.), NA))) %>% 
  mutate_all(funs(replace(., is.infinite(.), NA))) %>% 
  mutate(canarito=runif(nrow(dataset_validacion)))




setwd(directory.work)

sal.lg<-NULL
i<-1

for (sem in ksemilla_azar[1:3]) {
  
  t0       <-  Sys.time()
  
  modelo = lightgbm::lgb.train( 
    data = dgeneracion,
    objective = "binary",
    seed=sem,
    num_iterations=lgb_iterations,
    #    early_stopping_round=lgb_stop,
    init_score= .005,
    #               eval=fganancia_logistic_lightgbm_binaria,
    metric="auc",
    verbose=2,
    bagging_fraction=1, 
    feature_fraction=0.50, 
    learning_rate=0.020, 
    min_child_weight=8, 
    max_depth=10, 
    lambda_l1=0.50,
    lambda_l2=10,
    max_bin=32, 
    num_leaves=255,    
    min_gain_to_split = 0
    #               is_unbalance = TRUE
  )
  
  t1       <-  Sys.time()
  
  tiempo <-  as.numeric(  t1 - t0, units = "secs")
  
  #-------------------------
  #aplico el modelo
  
  
  #aplico el modelo a datos nuevos
  aplicacion_prediccion  <- predict(  modelo, as.matrix( dataset_validacion) )
  # calculo la ganancia
  #gan <-  calcular_ganancia(aplicacion_prediccion,  dataset_validacion[ , kclase_nomcampo] ) 
  # calculo el AUC
  #auc <-  fmetrica_auc_lightgbm( aplicacion_prediccion,  dataset_validacion[ , kclase_nomcampo] ) 
  
  
  
  sal.lg[[i]]<-list(modelo=modelo,tiempo=tiempo,probs=aplicacion_prediccion)
  i<-i+1
}

save(sal.lg,file="modelos_lgb2017_final")


#--------------------------------------------------------------------------------------------
#----- MODELOS LIGHTGBM ANTERIORES
#--------------------------------------------------------------------------------------------

dataset_generacion_ant    <-  as.data.frame(sample.datasets(karchivo_generacion_ant, directory.datasets, 
                                                            paste0(kextension,".rds"), "Y","lgb"
                                                            ,1,123
))
dgeneracion_ant  <-   preparar_lgbm(dataset_generacion_ant,kclase_nomcampo)


sal.lg.ant<-NULL
i<-1

for (sem in ksemilla_azar[1:3]) {
  
  t0       <-  Sys.time()
  
  modelo = lightgbm::lgb.train( 
    data = dgeneracion_ant,
    objective = "binary",
    seed=sem,
    num_iterations=lgb_iterations,
    #    early_stopping_round=lgb_stop,
    init_score= .005,
    #               eval=fganancia_logistic_lightgbm_binaria,
    metric="auc",
    verbose=2,
    bagging_fraction=1, 
    feature_fraction=0.90, 
    learning_rate=0.020, 
    min_child_weight=8, 
    max_depth=10, 
    lambda_l1=0.60,
    lambda_l2=7.4,
    max_bin=32, 
    num_leaves=500,    
    min_gain_to_split = 0
    #               is_unbalance = TRUE
  )
  
  t1       <-  Sys.time()
  
  tiempo <-  as.numeric(  t1 - t0, units = "secs")
  
  #-------------------------
  #aplico el modelo
  
  
  #aplico el modelo a datos nuevos
  aplicacion_prediccion  <- predict(  modelo, as.matrix( dataset_validacion) )
  # calculo la ganancia
  #gan <-  calcular_ganancia(aplicacion_prediccion,  dataset_validacion[ , kclase_nomcampo] ) 
  # calculo el AUC
  #auc <-  fmetrica_auc_lightgbm( aplicacion_prediccion,  dataset_validacion[ , kclase_nomcampo] ) 
  
  
  sal.lg.ant[[i]]<-list(modelo=modelo,tiempo=tiempo,probs=aplicacion_prediccion)
  i<-i+1
}

save(sal.lg.ant,file="modelos_lgb2016_final")



#--------------------------------------------------------------------------------------------
#----- MODELOS XGBOOST
#--------------------------------------------------------------------------------------------


dgeneracion  <-   preparar_xgb(dataset_generacion,kclase_nomcampo)

#-------------------------


sal.xg<-NULL
i<-1

for (sem in ksemilla_azar[c(3,4)]) {
  
  t0       <-  Sys.time()
  
  modelo = xgb.train( 
    data = dgeneracion,
    missing = NA,
    objective="binary:logistic",
    #			   tree_method = "hist",
    grow_policy="lossguide",
    #               updater="grow_fast_histmaker",
    seed=sem,
    nround=xgb_iterations,
    base_score=0.005,
    #              base_score=mean(getinfo(dgeneracion, "label")),
    subsample=.5, 
    colsample_bytree=0.40, 
    eta=0.015, 
    min_child_weight=6, 
    max_depth=48, 
    alpha=0.63, 
    lambda=0.60,
    gamma=10,
    #               max_bin=32, 
    tree_method='hist',
    #               num_leaves=255
    maximize = T,
    eval_metric = "auc",
    #         FEVAL=fganancia_logistic_xgboost,
    missing = NA,
    stratified = TRUE
  )
  
  t1       <-  Sys.time()
  
  tiempo <-  as.numeric(  t1 - t0, units = "secs")
  
  aplicacion_prediccion  <- predict(  modelo, as.matrix( dataset_validacion) )
  #gan <-  calcular_ganancia(aplicacion_prediccion,  dataset_validacion[ , kclase_nomcampo] ) 
  #auc <-  fmetrica_auc_lightgbm( aplicacion_prediccion,  dataset_validacion[ , kclase_nomcampo] ) 
  
  
  
  sal.xg[[i]]<-list(modelo=modelo,tiempo=tiempo,probs=aplicacion_prediccion)
  i<-i+1
}


save(sal.xg,file="modelos_xg2017_final")




#--------------------------------------------------------------------------------------------
#----- MODELOS XGBOOST ANTERIORES
#--------------------------------------------------------------------------------------------


dgeneracion_ant  <-   preparar_xgb(dataset_generacion_ant,kclase_nomcampo)

sal.xg.ant<-NULL
i<-1

for (sem in ksemilla_azar[c(3,4)]) {
  
  t0       <-  Sys.time()
  
  modelo = xgb.train( 
    data = dgeneracion,
    missing = NA,
    objective="binary:logistic",
    #			   tree_method = "hist",
    grow_policy="lossguide",
    #               updater="grow_fast_histmaker",
    seed=sem,
    nround=xgb_iterations,
    base_score=0.005,
    #              base_score=mean(getinfo(dgeneracion, "label")),
    subsample=.8, 
    colsample_bytree=0.40, 
    eta=0.048, 
    min_child_weight=6, 
    max_depth=13, 
    alpha=0.58, 
    lambda=0.58,
    gamma=10,
    #               max_bin=32, 
    tree_method='hist',
    #               num_leaves=255
    maximize = T,
    eval_metric = "auc",
    #         FEVAL=fganancia_logistic_xgboost,
    missing = NA,
    stratified = TRUE
  )
  
  t1       <-  Sys.time()
  
  tiempo <-  as.numeric(  t1 - t0, units = "secs")
  
  aplicacion_prediccion  <- predict(  modelo, as.matrix( dataset_validacion) )
  #gan <-  calcular_ganancia(aplicacion_prediccion,  dataset_validacion[ , kclase_nomcampo] ) 
  #auc <-  fmetrica_auc_lightgbm( aplicacion_prediccion,  dataset_validacion[ , kclase_nomcampo] ) 
  
  
  sal.xg.ant[[i]]<-list(modelo=modelo,tiempo=tiempo,probs=aplicacion_prediccion)
  i<-i+1
}


save(sal.xg.ant,file="modelos_xg2016_final")




#---------------------------------------------------------------------------------
#----- ENSEMBLE
#---------------------------------------------------------------------------------

setwd(  directory.datasets )


dataset_validacion_full = as.data.table(readRDS( karchivo_validacion))



nuevo_junio<- dataset_validacion %>% select(
  mpasivos_margen,
  mcaja_ahorro_Paquete,
  tmovimientos_ultimos90dias,
  ttarjeta_visa__tend,
  ttarjeta_visa,
  mcuenta_corriente_Paquete,
  mv_Finiciomora,
  mprestamos_personales,
  Visa_Finiciomora,
  mv_mpagospesos__max,
  mvr_balance,
  ctarjeta_visa_transacciones,
  tmovimientos_ultimos90dias__min,
  mcuentas_saldo,
  mvr_balanceedad,
  tmovimientos_ultimos90dias__tend,
  mdescubierto_preacordado__min,
  mv_marca_atraso,
  mcuenta_corriente_Paquete__min,
  mvr_mpagospesos,
  mtarjeta_visa_consumo,
  Visa_fechaalta,
  mrentabilidad__min,
  mv_mpagospesos,
  tmovimientos_ultimos90dias__max
  #  clase_binaria
) %>% mutate(modelo.lg1=sal.lg[[1]]$probs,
             modelo.lg2=sal.lg[[2]]$probs,
             modelo.lg3=sal.lg[[3]]$probs,
             modelo.xg1=sal.xg[[1]]$probs,
             modelo.xg2=sal.xg[[2]]$probs,
             modelo.lg1.ant=sal.lg.ant[[1]]$probs,
             modelo.lg2.ant=sal.lg.ant[[2]]$probs,
             modelo.lg3.ant=sal.lg.ant[[3]]$probs,
             modelo.xg1.ant=sal.xg.ant[[1]]$probs,
             modelo.xg2.ant=sal.xg.ant[[2]]$probs,
             numero_de_cliente=dataset_validacion_full$numero_de_cliente
)



# logist <- glm(clase_binaria ~.,family=binomial(link='logit'),data=nuevo_febrero)
# 
# summary(logist)
# 
# fitted.results <- predict(model,newdata=nuevo_abril[,-which(names(nuevo_abril) == kclase_nomcampo)],type='response')
# 
# gan.glm<-calcular_ganancia(fitted.results,nuevo_abril[,kclase_nomcampo])
# 
# cat( "ganancia logistica = ",  gan.glm$ganancia , "\n", file=karchivo_salida, fill=FALSE, append=TRUE)
# cat( "corte logistica = ",  gan.glm$prob_corte , "\n", file=karchivo_salida, fill=FALSE, append=TRUE)
# 
# save(logist,file="logistica")


saveRDS(nuevo_junio,file=paste0(directory.datasets,"nuevo_junio.rds"))

quit(save="yes")



