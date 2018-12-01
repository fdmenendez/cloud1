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
karchivo_generacion   <-   "201712_,201711_,201710_,201709_,201708_,201707_"
karchivo_aplicacion   <-   paste0("201802_",kextension,".rds")
karchivo_validacion   <-   paste0("201804_",kextension,".rds")

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

#------------------------------------------------------------------------------

setwd(  directory.datasets )

dataset_generacion    <-  as.data.frame(sample.datasets(karchivo_generacion, directory.datasets, 
                                          paste0(kextension,".rds"), "Y","lgb"
                                          ,.3,seed
                                        ))

#genero one-hot enconding
options(na.action='na.pass')
formula  <- formula(paste("~ .-1"))

dataset_unido_matrix  = model.matrix(formula,data = dataset_generacion[ , -which(names(dataset_generacion) == kclase_nomcampo)   ])
dataset_generacion_sinclase_sparse = as(dataset_unido_matrix, "dgCMatrix")

#dataset_generacion_sinclase_sparse  <- sparse.model.matrix( formula, data = dataset_generacion_sinclase )


#genero el formato requerido por LightGBM
dgeneracion  <-   lgb.Dataset( data  = data.matrix(dataset_generacion_sinclase_sparse),
                               label = dataset_generacion[ , kclase_nomcampo], 
                               missing=NA,
                               free_raw_data=FALSE 
                              )

rm(dataset_unido_matrix,dataset_generacion_sinclase_sparse)
gc()
#-------------------------

dataset_aplicacion <-  as.data.table(readRDS( karchivo_aplicacion)) 
dataset_aplicacion <- clean.up.oot.lgb(dataset_aplicacion)

#borro las variables que no me interesan
#dataset_aplicacion[ ,  (kcampos_a_borrar) := NULL    ] 


#dejo la clase en {0,1}  clase  binaria1
#dataset_aplicacion[, (kclase_nomcampo) := as.numeric( get(kclase_nomcampo) == kclase_valor_positivo  )] 

#genero one-hot enconding
options(na.action='na.pass')
formula  <- formula(paste("~ .-1"))

dataset_unido_matrix  = model.matrix(formula, data = dataset_aplicacion[ , -which(names(dataset_aplicacion) == kclase_nomcampo)   ])
dataset_aplicacion_sinclase_sparse = as(dataset_unido_matrix, "dgCMatrix")


#genero el formato requerido por LightGBM
dvalidacion  <-   lgb.Dataset( data  = data.matrix(dataset_aplicacion_sinclase_sparse),
                               label = dataset_aplicacion[ , kclase_nomcampo], 
                               missing=NA,
                               free_raw_data=FALSE 
)

rm(dataset_unido_matrix,dataset_aplicacion_sinclase_sparse)
gc()
#genero el modelo

t0       <-  Sys.time()

modelo = lightgbm::lgb.train( 
               data = dgeneracion,
               objective = "binary",
               valids=list(validacion=dvalidacion),
               seed=seed,
               num_iterations=2000,
               early_stopping_round=300,
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

setwd(  directory.datasets )

#dataset_aplicacion_sinclase_sparse  <- sparse.model.matrix( formula, data = dataset_aplicacion_sinclase )

dataset_validacion <-  as.data.table(readRDS( karchivo_validacion)) 
dataset_validacion <- clean.up.oot.lgb(dataset_validacion)
#genero one-hot enconding

options(na.action='na.pass')
formula  <- formula(paste("~ .-1"))
dataset_unido_matrix  = model.matrix(formula, data = dataset_validacion[ , -which(names(dataset_validacion) == kclase_nomcampo)   ])
dataset_validacion_sinclase_sparse = as(dataset_unido_matrix, "dgCMatrix")

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

save(modelo,file=paste0("sampled-lgb-",seed))

