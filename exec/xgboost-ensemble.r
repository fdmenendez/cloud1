#Objetivo: mostrar como se genera y aplica un modelo de XGBoost

#source( "/cloud/cloud1/codigoR/lightgbm/lightgbm_basico.r" )

#limpio la memoria
rm( list=ls() )
gc()

script.name<- "xgboost-ensemble"
kextension<-"exthist"

switch ( Sys.info()[['sysname']],
         Windows = { directory.include  <-  "M:\\codigoR\\include\\"
                     directory.work     <-  "M:\\work\\"
                     directory.plan     <-  "M:\\plan\\"
                     directory.datasets <-  "M:\\datasets\\"
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



library( "xgboost" )
library( "Matrix" )



library( "data.table" )
library( "dplyr")


setwd( directory.include )
source( "metrica.r" )
source( "utils.r" )
source( "xgboost-genetical-v3.r")


kcampos_separador     <-  "\t"
kcampo_id             <-  "numero_de_cliente"
kclase_nomcampo       <-  "clase_binaria"
kclase_valor_positivo <-  "BAJA+2"
kcampos_a_borrar      <-  c( kcampo_id )

#karchivo_generacion   <-   "201801_dias.txt,201712_dias.txt,201711_dias.txt"
karchivo_generacion   <-   "nuevo_febrero.rds"
karchivo_aplicacion   <-   paste0("nuevo_abril",".rds")
karchivo_validacion   <-   paste0("nuevo_abril",".rds")

kmin_corte <- 0.001
kmax_corte <- 0.090
kcortes<-seq(kmin_corte,kmax_corte,.0001)
karchivo_salida<-"salida.txt"

fganancia_logistic_xgboost   <- function(probs, clases) 
{
  
  vlabels <- xgboost::getinfo(clases, "label")
  
  gan <-sum(   (probs > kprob_corte  ) * 
                 ifelse( vlabels== 1, kganancia_acierto, kganancia_noacierto )   
  )
  
  
  return(  list(metric = "ganancia", value =  ifelse(  is.na(gan) , 0, round(gan,0) ) )  )
}

fganancia_logistic_lightgbm_binaria   <- function(probs, clases) 
{
  
#  vlabels <- xgboost::getinfo(clases, "label")
  
  gan <-sum(   (probs > vprob_corte  ) * 
                 ifelse( clases== 1, 11700, -300 ),
               na.rm = TRUE   
  )
  
  
  return(  list( name = "ganancia", 
                 value =  ifelse(  is.na(gan) , 0, gan ) 
#                 higher_better= TRUE 
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

#------------------------------------------------------------------------------

setwd(  directory.datasets )

dataset_generacion    <-  as.data.frame(readRDS( karchivo_generacion)) 
#dataset_generacion<- dataset_generacion %>% select(modelo.lg1,modelo.lg2,modelo.lg3,modelo.xg1,modelo.xg2,modelo.lg1.ant,
#                                            modelo.lg2.ant,modelo.lg3.ant,modelo.xg1.ant,modelo.xg2.ant,clase_binaria)

dgeneracion  <-   preparar_xgb(dataset_generacion,kclase_nomcampo)

dataset_aplicacion <-  as.data.frame(readRDS( karchivo_aplicacion)) 
#dataset_aplicacion<- dataset_aplicacion %>% select(modelo.lg1,modelo.lg2,modelo.lg3,modelo.xg1,modelo.xg2,modelo.lg1.ant,
#                                                modelo.lg2.ant,modelo.lg3.ant,modelo.xg1.ant,modelo.xg2.ant,clase_binaria)

daplicacion  <-   preparar_xgb(dataset_aplicacion,kclase_nomcampo)


dataset_validacion <-  as.data.frame(readRDS( karchivo_validacion))
#dataset_validacion<- dataset_validacion %>% select(modelo.lg1,modelo.lg2,modelo.lg3,modelo.xg1,modelo.xg2,modelo.lg1.ant,
#                                                modelo.lg2.ant,modelo.lg3.ant,modelo.xg1.ant,modelo.xg2.ant,clase_binaria)

dataset_validacion_sinclase_sparse = preparar_matriz(dataset_validacion,kclase_nomcampo)


setwd(  directory.work )

data.trn<-dataset_generacion
data.trn$clase_binaria<-as.factor(data.trn$clase_binaria)
data.oot<-dataset_aplicacion
data.oot$clase_binaria<-as.factor(data.oot$clase_binaria)
genet<-train_gen_xgboost(data.trn,data.oot,kclase_nomcampo,10,10,50,3,200,ksemilla_azar[2])

save(genet,file="xgboost-genetical-ensemble")

sal.xg<-NULL
i<-1
for (sem in ksemilla_azar) {
  
  #genero el modelo
  
  t0       <-  Sys.time()
  
  modelo = xgb.train( 
    data = dgeneracion,
    missing = NA,
    watchlist = list(train=dgeneracion,val=daplicacion),
    objective="binary:logistic",
    #			   tree_method = "hist",
    grow_policy="lossguide",
    #               updater="grow_fast_histmaker",
    nround=500,
    base_score=0.005,
    #              base_score=mean(getinfo(dgeneracion, "label")),
    subsample=.8, 
    colsample_bytree=0.30, 
    eta=0.052, 
    min_child_weight=10, 
    max_depth=32, 
    alpha=0.58, 
    lambda=0.54,
    gamma=10,
    #               max_bin=32, 
    tree_method='hist',
    #               num_leaves=255
    maximize = T,
    seed=ksemilla_azar[1],
    eval_metric = "auc",
    #         FEVAL=fganancia_logistic_xgboost,
    early_stopping_round = 200,
    missing = NA,
    stratified = TRUE
  )
  
  t1       <-  Sys.time()
  
  tiempo <-  as.numeric(  t1 - t0, units = "secs")
  
  testTask <- mlr::makeClassifTask(data = data.oot,target = kclase_nomcampo, positive = 1)
  
  #aplicacion_prediccion  <- predict(  genet$best_model, as.matrix( dataset_validacion_sinclase_sparse) )
  #gan <-  calcular_ganancia(aplicacion_prediccion,  dataset_validacion[ , kclase_nomcampo] ) 
  #auc <-  fmetrica_auc_lightgbm( aplicacion_prediccion,  dataset_validacion[ , kclase_nomcampo] ) 
  
  aplicacion_prediccion  <- predict(  genet$best_model, testTask )
  gan <-  calcular_ganancia(aplicacion_prediccion$data$prob.1,  dataset_validacion[ , kclase_nomcampo] ) 
  auc <-  fmetrica_auc_lightgbm( aplicacion_prediccion$data$prob.1,  dataset_validacion[ , kclase_nomcampo] ) 
  
  cat( "#-------------------------------------------------------- \n", file=karchivo_salida, fill=FALSE, append=TRUE)
  cat( "#-XGBOOST 2017 - ",sem , "\n", file=karchivo_salida, fill=FALSE, append=TRUE)
  cat( "#-------------------------------------------------------- \n", file=karchivo_salida, fill=FALSE, append=TRUE)
  cat( "ganancia = ",  gan$ganancia , "\n", file=karchivo_salida, fill=FALSE, append=TRUE)
  cat( "prob_corte = ",  gan$prob_corte , "\n", file=karchivo_salida, fill=FALSE, append=TRUE)
  cat( "AUC = ",  auc, "\n"  , file=karchivo_salida, fill=FALSE, append=TRUE)
  cat( "tiempo = ",  tiempo, "\n"  , file=karchivo_salida, fill=FALSE, append=TRUE)
  
  sal.xg[[i]]<-list(modelo=modelo,tiempo=tiempo,ganancia=gan$ganancia,pcorte=gan$prob_corte,probs=aplicacion_prediccion)
  i<-i+1
  
}

setwd(directory.work)
save(sal.xg,file="xgboost-ext-completo")
mat <- xgb.importance (feature_names = colnames(dataset_validacion_sinclase_sparse),model = modelo)
png(filename=paste0("xgboost-importance.png"))
xgb.plot.importance (importance_matrix = mat[1:25]) 
dev.off()

data_logist<- dataset_generacion %>% select(modelo.lg1,modelo.lg2,modelo.lg3,modelo.xg1,modelo.xg2,modelo.lg1.ant,
                                            modelo.lg2.ant,modelo.lg3.ant,modelo.xg1.ant,modelo.xg2.ant,clase_binaria)

data_logist_val<- dataset_validacion %>% select(modelo.lg1,modelo.lg2,modelo.lg3,modelo.xg1,modelo.xg2,modelo.lg1.ant,
                                            modelo.lg2.ant,modelo.lg3.ant,modelo.xg1.ant,modelo.xg2.ant,clase_binaria)

logist <- glm(clase_binaria ~.,family=binomial(link='logit'),data=data_logist)

summary(logist)

fitted.results <- predict(logist,newdata=data_logist_val[,-which(names(data_logist_val) == kclase_nomcampo)],type='response')

gan.glm<-calcular_ganancia(fitted.results,data_logist_val[,kclase_nomcampo])

cat( "ganancia logistica = ",  gan.glm$ganancia , "\n", file=karchivo_salida, fill=FALSE, append=TRUE)
cat( "corte logistica = ",  gan.glm$prob_corte , "\n", file=karchivo_salida, fill=FALSE, append=TRUE)

save(logist,file="logistica")
















#-----------------------------------------------------------------------------------
#PREDICCION FINAL
#------------------------------------------------------------------------------
setwd(directory.datasets)
dataset_final <-  as.data.frame(readRDS( "nuevo_junio.rds"))
#dataset_validacion<- dataset_validacion %>% select(modelo.lg1,modelo.lg2,modelo.lg3,modelo.xg1,modelo.xg2,modelo.lg1.ant,
#                                                modelo.lg2.ant,modelo.lg3.ant,modelo.xg1.ant,modelo.xg2.ant,clase_binaria)

clientes<-dataset_final$numero_de_cliente
dataset_final$numero_de_cliente<-NULL
dataset_final$clase_binaria<-NULL

#dataset_final_sinclase_sparse = preparar_matriz(dataset_final,kclase_nomcampo)

finalTask <- mlr::makeClassifTask(data = dataset_final)

#aplicacion_prediccion  <- predict(  genet$best_model, as.matrix( dataset_validacion_sinclase_sparse) )
#gan <-  calcular_ganancia(aplicacion_prediccion,  dataset_validacion[ , kclase_nomcampo] ) 
#auc <-  fmetrica_auc_lightgbm( aplicacion_prediccion,  dataset_validacion[ , kclase_nomcampo] ) 

aplicacion_prediccion  <- predict(  genet$best_model, newdata=dataset_final )

dataset_completo<-dataset_final %>% mutate(numero_de_cliente=clientes,
                                           probs=aplicacion_prediccion$data$prob.1,
                                           clase_final=ifelse(probs>genet$best_model_parms$prob_corte,1,0))

salida<-dataset_completo %>% filter(clase_final==1) %>% select(numero_de_cliente)
write.table(salida, file="etcheverrymason_menendez.txt", sep = "\t",
            row.names = FALSE,col.names = F)








#----------------------------
#PLAN B
#----------------------------

setwd(directory.datasets)
dataset_validacion <-  as.data.table(readRDS( "201806_exthist.rds")) 
clientes<-dataset_validacion$numero_de_cliente
dataset_validacion <- clean.up.oot.lgb(dataset_validacion)

dataset_validacion_sinclase_sparse<-preparar_matriz(dataset_validacion,kclase_nomcampo)

prediccion_planb<-predict(sal.xg[[1]]$modelo,dataset_validacion_sinclase_sparse)

dataset_planb<-data.frame(numero_de_cliente=clientes,probs=prediccion_planb,
                          clase_final=ifelse(prediccion_planb>sal.xg[[1]]$pcorte,1,0))

salida<-dataset_planb %>% filter(clase_final==1) %>% select(numero_de_cliente)
write.table(salida, file="etcheverrymason_menendez.txt", sep = "\t",
            row.names = FALSE,col.names = F)












