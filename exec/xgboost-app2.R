rm( list=ls() )
gc()

script.name<-"xgboost-app2"

switch ( Sys.info()[['sysname']],
         Windows = { directory.include  <-  "M:\\codigoR\\include\\"
         directory.work     <-  paste0("M:\\work\\",script.name,"\\")
         directory.plan     <-  "M:\\plan\\"
         directory.datasets <-  "M:\\datasets\\dias\\"
         directory.root     <-  "M:\\"
         },
         Darwin  = { directory.include  <-  "~/dm/codigoR/include/"
         directory.work     <-  "~/dm/work/"
         directory.plan     <-  "~/dm/plan/"
         directory.datasets <-  "~/dm/datasets/"
         },
         Linux   = { script.name<-Sys.info()["nodename"]
         directory.include  <-  "~/cloud/cloud1/codigoR/include/"
         directory.work     <-  paste0("~/cloud/cloud1/work/",script.name,"/")
         directory.plan     <-  "~/cloud/cloud1/plan/"
         directory.datasets <-  "~/cloud/cloud1/datasets/dias/"
         }
)

#Parametros  Repeated Random Sub Sampling Validation
ktraining_prob        <-  0.70
ksemilla_azar         <-  c( 102191, 200177, 410551, 552581, 892237 )
kclase_nomcampo       <- "clase_binaria"

source(paste0(directory.include,"utils.r"))
source(paste0(directory.include,"xgboost-genetical.r"))

include.packages("data.table")
include.packages("dplyr")
include.packages("caret")
include.packages("mlr")

ganancia   <- function(probs, clases) 
{
  
  ganancia_calculada  <- sum((probs >( 250/10000) ) * 
                                   ifelse( clases == 1, 11700, -300 )   
  ) 
  return(  list(metric = "ganancia", value = ganancia_calculada )  )
}


dataset<- fread(paste0(directory.datasets,"201802_dias.txt"), header=TRUE, sep="\t")
dataset<-clean.up(dataset)
head(dataset)
set.seed( ksemilla_azar[1] )
inTraining        <-  createDataPartition( dataset[ , kclase_nomcampo],   p = ktraining_prob, list = FALSE)
dataset_training  <-  dataset[  inTraining, ]
dataset_testing   <-  dataset[ -inTraining, ]

setwd(directory.work)

pop_inicial<-20
nro_hijos<-20
max_gen<-20
nfold<-50
nrounds<-50


genet<-train_gen_xgboost(dataset_training,kclase_nomcampo,pop_inicial,nro_hijos,
                         max_gen,nfold,nrounds,ksemilla_azar[1])

set.seed( ksemilla_azar[1] )
trainTask <- makeClassifTask(data = dataset_training,target = kclase_nomcampo, positive = 1)
testTask <- makeClassifTask(data = dataset_testing, target = kclase_nomcampo, positive = 1)

trainTask <- normalizeFeatures(trainTask,method = "standardize")
testTask <- normalizeFeatures(testTask,method = "standardize")

#im_feat <- generateFilterValuesData(trainTask, method = c("information.gain","chi.squared"))
#plotFilterValues(im_feat,n.show = 20)


#load xgboost
set.seed(ksemilla_azar[1])
getParamSet("classif.xgboost")

#make learner with inital parameters
xg_set <- makeLearner("classif.xgboost", predict.type = "prob")
xg_set$par.vals <- list(
  objective = "binary:logistic",
  #  eval_metric = ganancia,
  eval_metric = "auc",
  maximize = T,
  verbose = 2,
  missing = NA,
  nfold=10,
  nrounds=10

)

xg_set$par.vals<-c(xg_set$par.vals,as.list(genet$best_model[1:nrow(limits)]))

xgb.model<-train(xg_set, trainTask)

#test model
predict.xg <- predict(xgb.model, testTask)

sum((predict.xg$data$prob.1 >( 250/10000)) * ifelse( dataset_testing[,kclase_nomcampo] == 1, 11700, -300 ))

#write.csv(predict.xg$data,"salida-pred.csv")

dataset.oot<- fread(paste0(directory.datasets,"201804_dias.txt"), header=TRUE, sep="\t")
dataset.oot<-clean.up(dataset.oot)
ootTask <- makeClassifTask(data = dataset.oot, target = kclase_nomcampo, positive = 1)
ootTask <- normalizeFeatures(ootTask,method = "standardize")

predict.oot <- predict(xgb.model, ootTask)

sum(ifelse( dataset.oot[,kclase_nomcampo] == 1, 11700, 0 ))

table(dataset.oot[,kclase_nomcampo])

sum((predict.oot$data$prob.1 >( 250/10000)) * ifelse( dataset.oot[,kclase_nomcampo] == 1, 11700, -300 ))

#limpio la memoria
rm( list=ls() )
gc()


#salgo del R sin grabar el gigante entorno
quit( save="no" )






