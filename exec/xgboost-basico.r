#Objetivo: mostrar como se genera y aplica un modelo de XGBoost

#source( "/cloud/cloud1/codigoR/lightgbm/lightgbm_basico.r" )

#limpio la memoria
rm( list=ls() )
gc()

script.name<- "xgboost-basico"
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



library( "xgboost" )
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
karchivo_generacion   <-   "201712_,201711_,201710_"
karchivo_aplicacion   <-   paste0("201802_",kextension,".rds")
karchivo_validacion   <-   paste0("201804_",kextension,".rds")

kmin_corte <- 0.005
kmax_corte <- 0.090
kcortes<-seq(kmin_corte,kmax_corte,.0001)

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

#------------------------------------------------------------------------------

setwd(  directory.datasets )

dataset_generacion    <-  read.datasets(karchivo_generacion, directory.datasets, 
										  paste0(kextension,".rds"), "Y","xgboost"
										  #,.3,ksemilla_azar[2]
										  )

#genero one-hot enconding
options(na.action='na.pass')
formula  <- formula(paste("~ .-1"))

dataset_unido_matrix  = model.matrix(formula,data = dataset_generacion[ , -which(names(dataset_generacion) == kclase_nomcampo)   ])
dataset_generacion_sinclase_sparse = as(dataset_unido_matrix, "dgCMatrix")

#dataset_generacion_sinclase_sparse  <- sparse.model.matrix( formula, data = dataset_generacion_sinclase )
dataset_generacion_clase <- as.matrix(dataset_generacion[ , kclase_nomcampo])

#genero el formato requerido por LightGBM
dgeneracion  <-   xgb.DMatrix( data  = data.matrix(dataset_generacion_sinclase_sparse),
                               label = dataset_generacion_clase, 
                               missing=NA
                              )

rm(dataset_unido_matrix,dataset_generacion_sinclase_sparse,dataset_generacion_clase)
gc()
#-------------------------

dataset_aplicacion <-  as.data.table(readRDS( karchivo_aplicacion)) 
dataset_aplicacion <- clean.up.oot(dataset_aplicacion)

#borro las variables que no me interesan
#dataset_aplicacion[ ,  (kcampos_a_borrar) := NULL    ] 


#dejo la clase en {0,1}  clase  binaria1
#dataset_aplicacion[, (kclase_nomcampo) := as.numeric( get(kclase_nomcampo) == kclase_valor_positivo  )] 

#genero one-hot enconding
options(na.action='na.pass')
formula  <- formula(paste("~ .-1"))

dataset_unido_matrix  = model.matrix(formula, data = dataset_aplicacion[ , -which(names(dataset_aplicacion) == kclase_nomcampo)   ])
dataset_aplicacion_sinclase_sparse = as(dataset_unido_matrix, "dgCMatrix")

dataset_aplicacion_clase <- as.matrix(dataset_aplicacion[ , kclase_nomcampo])
#genero el formato requerido por LightGBM
dvalidacion  <-   xgb.DMatrix( data  = data.matrix(dataset_aplicacion_sinclase_sparse),
                               label = dataset_aplicacion_clase, 
                               missing=NA
)

rm(dataset_unido_matrix,dataset_aplicacion_sinclase_sparse,dataset_aplicacion_clase)
gc()


dataset_validacion <-  as.data.table(readRDS( karchivo_validacion)) 
dataset_validacion <- clean.up.oot(dataset_validacion)
#genero one-hot enconding

options(na.action='na.pass')
formula  <- formula(paste("~ .-1"))
dataset_unido_matrix  = model.matrix(formula, data = dataset_validacion[ , -which(names(dataset_validacion) == kclase_nomcampo)   ])
dataset_validacion_sinclase_sparse = as(dataset_unido_matrix, "dgCMatrix")



#genero el modelo

t0       <-  Sys.time()

modelo = xgb.train( 
               data = dgeneracion,
               missing = NA,
			   watchlist = list(train=dgeneracion,val=dvalidacion),
               objective="binary:logistic",
#			   tree_method = "hist",
      			   grow_policy="lossguide",
#               updater="grow_fast_histmaker",
               nround=500,
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
			   early_stopping_round = 200,
			   missing = NA,
               stratified = TRUE
              )

t1       <-  Sys.time()

tiempo <-  as.numeric(  t1 - t0, units = "secs")

#-------------------------
#aplico el modelo

#dataset_aplicacion_sinclase_sparse  <- sparse.model.matrix( formula, data = dataset_aplicacion_sinclase )

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
save(modelo,file="xgboost-ext-completo")
mat <- xgb.importance (feature_names = colnames(dataset_validacion_sinclase_sparse),model = modelo)
png(filename=paste0("xgboost-importance.png"))
xgb.plot.importance (importance_matrix = mat[1:25]) 
dev.off()

