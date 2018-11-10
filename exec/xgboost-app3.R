rm( list=ls() )
gc()

script.name<-"xgboost-app3"

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
         Linux   = { 
         directory.include  <-  "~/cloud/cloud1/codigoR/include/"
         directory.work     <-  paste0("~/cloud/cloud1/work/",script.name,"/")
         directory.plan     <-  "~/cloud/cloud1/plan/"
         directory.datasets <-  "~/cloud/cloud1/datasets/dias/"
         }
)

#Parametros  Repeated Random Sub Sampling Validation
#ktraining_prob        <-  0.70
kclase_nomcampo       <- "clase_binaria"

source(paste0(directory.include,"utils.r"))
source(paste0(directory.include,"xgboost-genetical-v2.r"))
source(paste0(directory.include,"metrica.r"))

include.packages("data.table")
include.packages("dplyr")
include.packages("caret")
include.packages("mlr")
include.packages("pryr")



dataset_training<- fread(paste0(directory.datasets,"201802_dias.txt"), header=TRUE, sep="\t")
#dataset<- rbind(dataset,fread(paste0(directory.datasets,"201801_hist.txt"), header=TRUE, sep="\t"))
#dataset<- rbind(dataset,fread(paste0(directory.datasets,"201712_hist.txt"), header=TRUE, sep="\t"))

dataset_training<-clean.up(dataset_training)
head(dataset_training)
set.seed( ksemilla_azar[1] )

setwd(directory.work)

pop_inicial<-4
nro_hijos<-8
max_gen<-4
nfold<-3
nrounds<-5


genet<-train_gen_xgboost(dataset_training,kclase_nomcampo,pop_inicial,nro_hijos,
                         max_gen,nfold,nrounds,ksemilla_azar[1],fganancia_logistic_xgboost_hyp)

sum(ifelse( dataset_training[,kclase_nomcampo] == 1, 11700, 0 ))
save(genet, file="genetical-boost-3months")

set.seed( ksemilla_azar[1] )
trainTask <- makeClassifTask(data = dataset_training,target = kclase_nomcampo, positive = 1)
trainTask <- normalizeFeatures(trainTask,method = "standardize")

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
#  eval_metric = "auc",
  maximize = T,
  verbose = 2,
  missing = NA,
  nfold=3,
  nrounds=10,
  feval=fganancia_logistic_xgboost_hyp
  
)

vprob_corte<-genet$best_model$prob_corte
xg_set$par.vals<-c(xg_set$par.vals,as.list(genet$best_model[1:nrow(limits)]))

xgb.model<-train(xg_set, trainTask)

object.size(xgb.model)

#test model
#predict.xg <- predict(xgb.model, testTask)

#sum((predict.xg$data$prob.1 >( 250/10000)) * ifelse( dataset_testing[,kclase_nomcampo] == 1, 11700, -300 ))

#write.csv(predict.xg$data,"salida-pred.csv")

dataset.oot<- fread(paste0(directory.datasets,"201804_dias.txt"), header=TRUE, sep="\t")
dataset.oot<-clean.up(dataset.oot)
ootTask <- makeClassifTask(data = dataset.oot, target = kclase_nomcampo, positive = 1)
ootTask <- normalizeFeatures(ootTask,method = "standardize")

predict.oot <- predict(xgb.model, ootTask)

sum(ifelse( dataset.oot[,kclase_nomcampo] == 1, 11700, 0 ))

table(dataset.oot[,kclase_nomcampo])

#fganancia_logistic_xgboost(predict.oot$data$prob.1,dataset.oot[,kclase_nomcampo])
sum((predict.oot$data$prob.1 >vprob_corte) * ifelse( dataset.oot[,kclase_nomcampo] == 1, 11700, -300 ))

#limpio la memoria
rm( list=ls() )
gc()


#salgo del R sin grabar el gigante entorno
quit( save="no" )






