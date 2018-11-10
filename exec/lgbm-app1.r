rm( list=ls() )
gc()

script.name<-"lgbm-app1"

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
kclase_nomcampo       <- "clase_binaria"

source(paste0(directory.include,"utils.r"))
source(paste0(directory.include,"metrica.r"))
source(paste0(directory.include,"xgboost-genetical.r"))

include.packages("data.table")
include.packages("mlr")
include.packages("caret")
#include.packages("xgboost")
include.packages("lightgbm")
include.packages("Matrix")




dataset<- fread(paste0(directory.datasets,"201802_dias.txt"), header=TRUE, sep="\t") 
dataset<-clean.up(dataset)
head(dataset)

table(dataset$clase_binaria)

setwd(directory.work)

#genero one-hot enconding
options(na.action='na.pass')
formula  <- formula(paste("~ .-1"))

#dataset_sinclase_sparse  <- sparse.model.matrix( formula, data = dataset[,-which(names(dataset) == kclase_nomcampo) ] )

dataset_unido_matrix  = model.matrix(formula, data = dataset[,-which(names(dataset) == kclase_nomcampo) ])
dataset_unido_sparse2 = as(dataset_unido_matrix, "dgCMatrix")
rm(dataset_unido_matrix)

#genero el formato requerido por LightGBM
dgeneracion  <-   lgb.Dataset( data  = data.matrix(dataset_unido_sparse2),
                               label = dataset[,kclase_nomcampo], 
#                               missing=NA,
                               free_raw_data=FALSE 
)

object.size(dgeneracion)
#train_sparse = Matrix(as.matrix(dataset_training[,-which(names(dataset_training) == kclase_nomcampo) ]), sparse=TRUE)

#lgb.data <- lgb.Dataset(data=train_sparse, label=dataset_training[,kclase_nomcampo])
# lgb.grid <- list(objective = "binary",
#                 num_iterations=350, 
#                 init_score= 0.05,
#                 bagging_fraction=1, 
#                 feature_fraction=0.14, 
#                 learning_rate=0.025, 
#                 min_gain_to_split=0.96, 
#                 lambda_l1=0.14,
#                 max_bin=31, 
#                 num_leaves=75,
#                 is_unbalance = TRUE)

set.seed( ksemilla_azar[1] )
lgb.model = lgb.train(data = dgeneracion,                    
                      objective = "binary",
                      num_iterations=50, 
#                      init_score= mean(getinfo(dgeneracion, "label")),
                      bagging_fraction=1, 
                      feature_fraction=0.50, 
                      learning_rate=0.020, 
                      min_child_weight=8, 
                      max_depth=10, 
                      lambda_l1=0.50,
                      lambda_l2=10,
                      max_bin=32, 
                      num_leaves=255)


lgb.model$best_iter
lgb.model$record_evals

dataset.oot<- fread(paste0(directory.datasets,"201804_dias.txt"), header=TRUE, sep="\t") 
dataset.oot<- clean.up(dataset.oot)


options(na.action='na.pass')
formula  <- formula(paste("~ .-1"))

dataset_unido_matrix  = model.matrix(formula, data = dataset.oot[,-which(names(dataset.oot) == kclase_nomcampo) ])
dataset.oot_unido_sparse2 = as(dataset_unido_matrix, "dgCMatrix")
rm(dataset_unido_matrix)


#aplico el modelo a datos nuevos
aplicacion_prediccion  <- predict(  lgb.model, as.matrix( dataset.oot_unido_sparse2) )











