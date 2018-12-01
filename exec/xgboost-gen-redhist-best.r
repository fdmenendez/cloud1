rm( list=ls() )
gc()

script.name<-"xgboost-gen-redhist"
kextension <- "redhist"

switch ( Sys.info()[['sysname']],
         Windows = { directory.include  <-  "M:\\codigoR\\include\\"
         directory.work     <-  paste0("M:\\work\\",script.name,"\\")
         directory.plan     <-  "M:\\plan\\"
         directory.datasets <-  "M:\\datasets\\hist\\"
         directory.root     <-  "M:\\"
         },
         Darwin  = { directory.include  <-  "~/dm/codigoR/include/"
         directory.work     <-  "~/dm/work/"
         directory.plan     <-  "~/dm/plan/"
         directory.datasets <-  "~/dm/datasets/"
         },
         Linux   = { 
           directory.include  <-  "~/cloud/cloud1/codigoR/include/"
           directory.work     <-  paste0("~/cloud/cloud1/work/",script.name,"/")
           directory.plan     <-  "~/cloud/cloud1/plan/"
           directory.datasets <-  paste0("~/cloud/cloud1/datasets/",kextension,"/")
         }
)

kclase_nomcampo       <- "clase_binaria"
kchar_cols <- c("cliente_edad","cliente_antiguedad" )


source(paste0(directory.include,"utils.r"))
source(paste0(directory.include,"xgboost-genetical-v2.r"))
source(paste0(directory.include,"metrica.r"))

include.packages("data.table")
include.packages("dplyr")
include.packages("caret")
include.packages("mlr")



dataset<- readRDS(paste0(directory.datasets,"201802_",kextension,".rds"))
dataset<- rbind(dataset,readRDS(paste0(directory.datasets,"201801_",kextension,".rds")))
dataset<- rbind(dataset,readRDS(paste0(directory.datasets,"201712_",kextension,".rds")))
dataset<- rbind(dataset,readRDS(paste0(directory.datasets,"201711_",kextension,".rds")))
dataset<- rbind(dataset,readRDS(paste0(directory.datasets,"201710_",kextension,".rds")))
dataset<- rbind(dataset,readRDS(paste0(directory.datasets,"201709_",kextension,".rds")))


dataset <-clean.up(dataset)
dataset_training <- dummify(dataset,kchar_cols,kclase_nomcampo)


#head(dataset)

setwd(directory.work)

load("genetical-boost-6months-redhist")

set.seed( ksemilla_azar[2] )
trainTask <- makeClassifTask(data = dataset_training,target = kclase_nomcampo, positive = 1)
trainTask <- normalizeFeatures(trainTask,method = "standardize")
#
# #im_feat <- generateFilterValuesData(trainTask, method = c("information.gain","chi.squared"))
# #plotFilterValues(im_feat,n.show = 20)
#
#
# #load xgboost
set.seed(ksemilla_azar[2])
getParamSet("classif.xgboost")

#make learner with inital parameters
xg_set <- makeLearner("classif.xgboost", predict.type = "prob")
xg_set$par.vals <- list(
  objective = "binary:logistic",
  #   #  eval_metric = ganancia,
  #   eval_metric = "auc",
  maximize = T,
  verbose = 2,
  missing = NA,
  nfold=5,
  nrounds=20,
  FEVAL=fganancia_logistic_xgboost_hyp
  
)

xg_set$par.vals<-c(xg_set$par.vals,as.list(genet$best_model[1:nrow(limits)]))
vprob_corte<-genet$best_model$prob_corte

xgb.model<-train(xg_set, trainTask)

save(xgb.model,file=paste0("best-gen-model-",kextension))

dataset.oot<- readRDS(paste0(directory.datasets,"201804_",kextension,".rds"))
dataset.oot<-clean.up(dataset.oot)
ootTask <- makeClassifTask(data = dataset.oot, target = kclase_nomcampo, positive = 1)
ootTask <- normalizeFeatures(ootTask,method = "standardize")

predict.oot <- predict(xgb.model, ootTask)

cat(paste("Maxima Ganancia Posible:", sum(ifelse( dataset.oot[,kclase_nomcampo] == 1, 11700, 0 )),"\n"))

table(dataset.oot[,kclase_nomcampo])

cat(paste("Ganancia OOT:", sum((predict.oot$data$prob.1 >vprob_corte) * ifelse( dataset.oot[,kclase_nomcampo] == 1, 11700, -300 )),"\n"))



#limpio la memoria
rm( list=ls() )
gc()


#salgo del R sin grabar el gigante entorno
quit( save="no" )






