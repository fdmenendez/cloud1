rm( list=ls() )
gc()

script.name<-"xgboost-app1"

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
include.packages("mlr")
include.packages("caret")
#include.packages("xgboost")
include.packages("mlrMBO")
include.packages("parallelMap")

ganancia   <- function(probs, clases) 
{
  
  vlabels <- xgboost::getinfo(clases, "label")
  
  ganancia_calculada  <- sum(    (probs >( 250/10000) ) * 
                                   ifelse( vlabels== 1, 11700, -300 )   
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
xg_set_mb <- makeLearner("classif.xgboost", predict.type = "prob")
xg_set_mb$par.vals <- list(
  objective = "binary:logistic",
#  eval_metric = ganancia,
  eval_metric = "auc",
  maximize = T,
  verbose = 0,
  nrounds = 50,
  tree_method = "hist",
  grow_policy="lossguide",
  updater="grow_fast_histmaker",
  missing = NA
)

#define parameters for tuning
xg_ps <- makeParamSet(
  makeIntegerParam("nrounds",lower=10,upper=10),
  makeIntegerParam("max_depth",lower=3,upper=40),
  makeNumericParam("lambda",lower=0.55,upper=0.60),
  makeNumericParam("eta", lower = 0.001, upper = 0.5),
  makeNumericParam("subsample", lower = 0.10, upper = 0.80),
  makeNumericParam("min_child_weight",lower=1,upper=5),
  makeNumericParam("colsample_bytree",lower = 0.2,upper = 0.8)
)

#define search function
rancontrol <- makeTuneControlRandom(maxit = 50L) #do 100 iterations

#3 fold cross validation
set_cv <- makeResampleDesc("CV",iters = 3L)

#tune parameters
xg_tune <- tuneParams(learner = xg_set_mb, task = trainTask, resampling = set_cv,measures = acc,par.set = xg_ps, control = rancontrol)

#set parameters
xg_new <- setHyperPars(learner = xg_set_mb, par.vals = xg_tune$x)

#train model
xgmodel <- train(xg_new, trainTask)

#test model
predict.xg_mb <- predict(xgmodel, testTask)

#write.csv(predict.xg_mb$data,"salida-pred-random.csv")

ganancia(predict.xg_mb$data$prob.1,kclase_nomcampo)

mbo.ctrl = makeMBOControl(save.on.disk.at = c(0, 5, 10, 20, 50, 75, 85, 95))
mbo.ctrl = setMBOControlTermination(mbo.ctrl, iters = 20)
surrogate.lrn = makeLearner("classif.xgboost", predict.type = "prob")
ctrl = mlr:::makeTuneControlMBO(learner = surrogate.lrn, mbo.control = mbo.ctrl)

parallelStartMulticore(cpus = 2L)
res.mbo = tuneParams(lrn, task, cv10, par.set = ps, control = ctrl, 
                     show.info = TRUE, measures = auc)
parallelStop()









