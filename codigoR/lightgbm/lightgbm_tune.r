#Objetivo buscar los parametros optimos de LightGBM para un archivos de <train, test>
#Modelo con libreria  lightgbm con optimizacion bayesiana

#toma los parametros del entorno
#la salida queda en hyperparameter_GLOBAL.txt
#si train == test  entonces hace cross validation


#source( "/cloud/cloud1/codigoR/lightgbm/lightgbm_tune.r" )

#limpio la memoria
rm( list=ls() )
gc()


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
                     directory.work     <-  "~/cloud/cloud1/work/"
                     directory.plan     <-  "~/cloud/cloud1/plan/"
                     directory.datasets <-  "~/cloud/cloud1/datasets/dias/"
                   }
        )



library( "lightgbm" )
library( "Matrix" )

library( "data.table" )


library( "DiceKriging" )
library( "mlrMBO")


setwd( directory.include )
source( "metrica.r" )


#parametros de entrada del script R
args <- commandArgs(trailingOnly = TRUE)

#args <-  c( "7102",  "201712_dias.txt",   "201802_dias.txt" )

if(length(args)<3) 
{
  stop("se debe proveer como parametro  experimento, archivos_train, archivos_test ", call.=FALSE)
} else
{ 
  kexperimento      <-  args[1] 
  karchivo_train    <-  args[2]
  karchivo_test     <-  args[3]

  kcross <-  0
  if( karchivo_train == karchivo_test ) kcross <-  1

}


#Parametros entrada de nuestro dataset
kcampos_separador     <-  "\t"
kcampo_id             <-  "numero_de_cliente"
kclase_nomcampo       <-  "clase_ternaria"
kclase_valor_positivo <-  "BAJA+2"
kcampos_a_borrar      <-  c( kcampo_id )



#sobre el funcionamiento de programa
#en el LightGBM
kcrossvalidation_folds <-      5
ksemilla               <- 102191
kmax_bin               <-     31
kbagging_fraction      <-      1.0
kinit_score            <-      0.005
knum_iterations        <-   5000
kearly_stopping_round  <-    200
kdesplazamiento        <-     30


#Parametros  mlrMBO
kiteraciones          <-  200
ksaveondisk_time      <-  600   # cada 600 segundos guarda a disco cuanto avanzo
karchivo_trabajo      <-  paste( "lightgbm_tune_", kexperimento, ".RDATA", sep="" )


#Parametros salida
karchivo_salida       <-  "hyperparameter_GLOBAL.txt"

karchivo_imagen       <-   paste( "lightgbm_tune_evolucion_", kexperimento, ".jpg", sep="" )



#estos valores se graban en el archivo de salida
kclase                <-  "ternaria"
kprograma             <-  "lightgbm_tune.r"
kalgoritmo            <-  "lightgbm"
kbusqueda             <-  "MBO"
kestimacion           <-  "5crossvalidation"
kobservaciones        <-  ""




#------------------------------------------------------
#dibujo la evolucion de la metrica

EvolucionRefresh  = function( parchivo_imagen, parchivo_salida, pexperimento )
{  
 
  #leo el archivo de salida, que tiene la info para graficar la evolucion
  setwd( directory.work )
  salida <-  read.table( parchivo_salida, header=TRUE, sep=kcampos_separador )
  salida <-  subset( salida, experimento == pexperimento) 

  if( nrow( salida ) >= 1 )
  {

    #una hora tiene 3600 segundos, un dia tiene 24 horas
    tiempoacum  <- cumsum( salida[ , "tiempo" ] ) /(3600*24)
    metricamax  <- cummax( salida[ , "metrica" ] )

  
    #dibujo la curva 
    jpeg(file = parchivo_imagen,  width = 6, height = 4, units = 'in', res = 300)

    tituvar  <-  paste( "(iter=", nrow(salida), " max=", max(salida[ , "metrica" ], na.rm=TRUE),  ")" )
    plot( tiempoacum, 
          metricamax, 
          type="n",
          main=paste( "Evolucion Metrica", tituvar ), 
          xlab="tiempo en DIAS ", 
          ylab="Metrica ", 
          pch=19 )

    lines( tiempoacum, metricamax, type="l" , col="red", lwd=2 )
    lines( tiempoacum, salida[ , "metrica" ], type="l" , col="green3" )

    dev.off()

  }

}
#------------------------------------------------------
#corre  lightgbm  usando  crossvalidation

modelo_lightgbm_ganancia_MBO_crossvalidation = function( x )
{

  t0       <-  Sys.time()
  modelo.cv = lgb.cv( 
                          seed = ksemilla,
                          data = dgeneracion,  
                          stratified = TRUE,       nfold = kcrossvalidation_folds ,  # cross-validation
                          objective="binary",
						  eval = fganancia_logistic_lightgbm,
						  metric = "auc",
                          num_iterations= knum_iterations,
                          early_stopping_round = kearly_stopping_round,
                          init_score = kinit_score ,
                          bagging_fraction = kbagging_fraction, 
                          feature_fraction = x$pfeature_fraction, 
                          learning_rate = x$plearning_rate,
                          min_data_in_leaf = x$pmin_data_in_leaf, 
                          num_leaves = x$pnum_leaves,
                          lambda_l1 = x$plambda_l1, min_gain_to_split = x$pmin_gain_to_split,
                          max_bin = kmax_bin
                    )
 
  t1       <-  Sys.time()
  tiempo   <-  as.numeric(  t1 - t0, units = "secs")


  largo <-  length( unlist( modelo.train$record_evals$valid$ganancia$eval ) )
  iteracion_max <- which.max(  unlist( modelo.cv$record_evals$valid$ganancia$eval ) )  + kdesplazamiento

  iteracion_max  <- ifelse(  iteracion_max>largo, largo, iteracion_max )

  #esta es la forma NATURAL  de calcular la ganancia
  
  metrica_ultima     <- unlist( modelo.cv$record_evals$valid$ganancia$eval )[ iteracion_max ] 
  auc_ultima         <- unlist( modelo.cv$record_evals$valid$auc$eval )[ iteracion_max ] 


  st_parametros = paste( "num_iterations=",     iteracion_max,       ", ",
                         "init_score=",         kinit_score,         ", ",
                         "bagging_fraction=",   kbagging_fraction,   ", ",
                         "feature_fraction=",   x$pfeature_fraction, ", ",
                         "learning_rate=",      x$plearning_rate,    ", ",
                         "min_data_in_leaf=",   x$pmin_data_in_leaf, ", ",
                         "num_leaves=",         x$pnum_leaves,       ", ",
                         "lambda_l1=",          x$plambda_l1,        ", ",
                         "min_gain_to_split=",  x$pmin_gain_to_split,", ",
                         "max_bin=",            kmax_bin,        
                         sep = ""
                       )



  #escribo al archivo de salida los resultados de esta corrida
  #kcrossvalidation_folds * metrica_ultima, 
  setwd( directory.work )
  cat( kexperimento,
       kcrossvalidation_folds * metrica_ultima, 
       auc_ultima,
       tiempo,
       st_parametros,
       format(Sys.time(), "%Y%m%d %H%M%S"), 
       kclase, 
       kprograma, 
       kalgoritmo, 
       kbusqueda, 
       kestimacion,
       karchivo_train, karchivo_test,
       kobservaciones,
       "\n", sep="\t", file=karchivo_salida, fill=FALSE, append=TRUE 
     )

  #escribo el grafico con la evolucion de la optimizacion
  EvolucionRefresh(  karchivo_imagen,  karchivo_salida, kexperimento )


  return( - kcrossvalidation_folds * metrica_ultima )    #devuelvo el negativo porque la libreria  mlrMBO   solo minimiza funciones
}
#------------------------------------------------------


modelo_lightgbm_ganancia_MBO_simple = function( x )
{
	
  t0       <-  Sys.time()
  
  modelo.train = lgb.train(
                          seed = ksemilla, 
                          data = dgeneracion,  
                          valids= list( valid=daplicacion),
                          objective="binary",
						  eval = fganancia_logistic_lightgbm ,
						  metric = "auc",
                          num_iterations= knum_iterations,
                          early_stopping_round = kearly_stopping_round,
                          init_score = kinit_score ,
                          bagging_fraction = kbagging_fraction, 
                          feature_fraction = x$pfeature_fraction, 
                          learning_rate = x$plearning_rate,
                          min_data_in_leaf = x$pmin_data_in_leaf, 
                          num_leaves = x$pnum_leaves,
                          lambda_l1 = x$plambda_l1, min_gain_to_split = x$pmin_gain_to_split,
                          max_bin = kmax_bin
                         )
  
 
 
  t1       <-  Sys.time()
  tiempo   <-  as.numeric(  t1 - t0, units = "secs")


  largo <-  length( unlist( modelo.train$record_evals$valid$ganancia$eval ) )
  iteracion_max  <- which.max(  unlist( modelo.train$record_evals$valid$ganancia$eval ) ) + kdesplazamiento

 
  iteracion_max  <- ifelse(  iteracion_max>largo, largo, iteracion_max )
  
  #esta es la forma NATURAL  de calcular la ganancia
  metrica_ultima     <- unlist( modelo.train$record_evals$valid$ganancia$eval )[ iteracion_max ] 
  auc_ultima         <- unlist( modelo.train$record_evals$valid$auc$eval )[ iteracion_max ] 
  

  st_parametros = paste( "num_iterations=",     iteracion_max,       ", ",
                         "init_score=",         kinit_score,         ", ",
                         "bagging_fraction=",   kbagging_fraction,   ", ",
                         "feature_fraction=",   x$pfeature_fraction, ", ",
                         "learning_rate=",      x$plearning_rate,    ", ",
                         "min_data_in_leaf=",   x$pmin_data_in_leaf, ", ",
                         "num_leaves=",         x$pnum_leaves,       ", ",
                         "lambda_l1=",          x$plambda_l1,        ", ",
                         "min_gain_to_split=",  x$pmin_gain_to_split,", ",
                         "max_bin=",            kmax_bin,             
                         sep = ""
                       )



  #escribo al archivo de salida los resultados de esta corrida
  setwd( directory.work )
  cat( kexperimento,
       metrica_ultima, 
       auc_ultima,
       tiempo,
       st_parametros,
       format(Sys.time(), "%Y%m%d %H%M%S"), 
       kclase, 
       kprograma, 
       kalgoritmo, 
       kbusqueda, 
       kestimacion,
       karchivo_train, karchivo_test,
       kobservaciones,
       "\n", sep="\t", file=karchivo_salida, fill=FALSE, append=TRUE 
     )

  #escribo el grafico con la evolucion de la optimizacion
  EvolucionRefresh(  karchivo_imagen,  karchivo_salida, kexperimento )

  return( -metrica_ultima )    #devuelvo el negativo porque la libreria  mlrMBO   solo minimiza funciones
}
#------------------------------------------------------

datasets_lightgbm = function( parchivos1, parchivos2 )
{
  #cargo 1
  varchivos1  <-  unlist(strsplit( parchivos1, split=","))
  setwd(  directory.datasets )
  dataset1 <- do.call(rbind, lapply( varchivos1, function(x) fread(x, header=TRUE, sep=kcampos_separador)))

  #cargo 2
  varchivos2  <-  unlist(strsplit( parchivos2, split=","))
  setwd(  directory.datasets )
  dataset2 <- do.call(rbind, lapply( varchivos2, function(x) fread(x, header=TRUE, sep=kcampos_separador)))
  
  dataset <-  rbind(  dataset1, dataset2 )
    
  #borro las variables que no me interesan
  dataset[ ,  (kcampos_a_borrar) := NULL    ] 

  #dejo la clase en {0,1}  clase  binaria1
  dataset[, (kclase_nomcampo) := as.numeric( get(kclase_nomcampo) == kclase_valor_positivo  )] 

  dataset_sinclase   <- dataset[ , ! ( kclase_nomcampo), with=FALSE   ]


  #genero one-hot enconding
  options(na.action='na.pass')
  formula  <- formula(paste("~ .-1"))
  dataset_sinclase_sparse  <- sparse.model.matrix( formula, data = dataset_sinclase )

  desde1 <- 1
  hasta1 <- nrow( dataset1 )
  desde2 <- hasta1 + 1 
  hasta2 <- hasta1 + nrow( dataset2 )

  #genero el formato requerido por XGBoost
  ddataset1  <-   lgb.Dataset( data  = data.matrix(dataset_sinclase_sparse[desde1:hasta1,]),
                               label = dataset[ desde1:hasta1 , get(kclase_nomcampo)], 
                               free_raw_data=FALSE 
                            )
							
  #genero el formato requerido por XGBoost
  ddataset2  <-   lgb.Dataset( data  = data.matrix(dataset_sinclase_sparse[desde2:hasta2,]),
                               label = dataset[ desde2:hasta2 , get(kclase_nomcampo)], 
                               free_raw_data=FALSE 
                            )

  rm( dataset1, dataset2, dataset,  dataset_sinclase, dataset_sinclase_sparse )
  gc()

  return( list( "d1"=ddataset1,  "d2"=ddataset2)  )
}
#------------------------------------------------------

dataset_lightgbm = function( parchivos )
{
  #cargo generacion
  varchivos  <-  unlist(strsplit( parchivos, split=","))
  setwd(  directory.datasets )
  dataset <- do.call(rbind, lapply( varchivos, function(x) fread(x, header=TRUE, sep=kcampos_separador)))

  #borro las variables que no me interesan
  dataset[ ,  (kcampos_a_borrar) := NULL    ] 

  #dejo la clase en {0,1}  clase  binaria1
  dataset[, (kclase_nomcampo) := as.numeric( get(kclase_nomcampo) == kclase_valor_positivo  )] 

  dataset_sinclase   <- dataset[ , ! ( kclase_nomcampo), with=FALSE   ]


  #genero one-hot enconding
  options(na.action='na.pass')
  formula  <- formula(paste("~ .-1"))
  dataset_sinclase_sparse  <- sparse.model.matrix( formula, data = dataset_sinclase )


  #genero el formato requerido por LightGBM
  ddataset  <-   lgb.Dataset( data  = data.matrix(dataset_sinclase_sparse),
                              label = dataset[ , get(kclase_nomcampo)], 
                              free_raw_data=FALSE  
                            )

  rm( dataset,  dataset_sinclase, dataset_sinclase_sparse )
  gc()

  return( ddataset )
}
#------------------------------------------------------


#escribo los  titulos  del archivo salida
setwd( directory.work )
if( !file.exists( karchivo_salida) )
{
  cat("experimento",
      "metrica",
      "metrica2",
      "tiempo",
      "parametros",
      "fecha", 
      "clase", "programa", "algoritmo", "busqueda" , "estimacion",
      "dataset_train", "dataset_test", "observaciones",
      "\n", sep="\t", file=karchivo_salida, fill=FALSE, append=FALSE )

}




if( kcross==0 ){   
  modelo_lightgbm_ganancia_MBO   <- modelo_lightgbm_ganancia_MBO_simple

  #cargo evaluacion
  ldata        <- datasets_lightgbm( karchivo_train, karchivo_test )
  dgeneracion  <- ldata$d1
  daplicacion  <- ldata$d2

  #cargo generacion
  varchivos  <-  unlist(strsplit( karchivo_test, split=","))
  setwd(  directory.datasets )
  matrix_aplicacion <- as.matrix(  do.call(rbind, lapply( varchivos, function(x) fread(x, header=TRUE, sep=kcampos_separador))) )
 
} else { 
  #cargo solamente 
  dgeneracion <- dataset_lightgbm( karchivo_train )
  modelo_lightgbm_ganancia_MBO   <- modelo_lightgbm_ganancia_MBO_crossvalidation

}



configureMlr(show.learner.output = FALSE)


#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
obj.fun <- makeSingleObjectiveFunction(
		name = "prueba",
		fn   = modelo_lightgbm_ganancia_MBO,
		par.set = makeParamSet(
                                       makeNumericParam("pfeature_fraction" ,  lower=0.05    , upper=   1.0),
                                       makeNumericParam("plearning_rate"    ,  lower=0.0     , upper=   0.1),
                                       makeNumericParam("plambda_l1"        ,  lower=0.0     , upper=   1.0),
                                       makeNumericParam("pmin_gain_to_split",  lower=0.0     , upper=   1.0),
                                       makeIntegerParam("pmin_data_in_leaf" ,  lower=1L      , upper=  100L),
                                       makeIntegerParam("pnum_leaves"       ,  lower=10L     , upper=  512L)
		),
		has.simple.signature = FALSE,
		global.opt.value = -1
		)



ctrl <-  makeMBOControl(propose.points = 1L, save.on.disk.at.time = ksaveondisk_time,  save.file.path = karchivo_trabajo )
ctrl <-  setMBOControlTermination( ctrl, iters = kiteraciones )
ctrl <-  setMBOControlInfill( ctrl, crit = makeMBOInfillCritEI(), opt  = "focussearch", opt.focussearch.points = kiteraciones )

if( kalgoritmo=="lightgbm" )   Nuggets <- 1e-8*var( lightgbm::getinfo(dgeneracion, "label") )

surr.km <-  makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", nugget=Nuggets, control = list(trace = FALSE))

EvolucionRefresh(  karchivo_imagen,  karchivo_salida, kexperimento )

setwd( directory.work )
if( !file.exists( karchivo_trabajo) )
{
  #lanzo la busqueda bayesiana
  res  <-  mbo(obj.fun, learner = surr.km, control = ctrl)
} else {

  #retoma el procesamiento en donde lo dejo
  mboContinue( karchivo_trabajo )
}




#limpio la memoria
rm( list=ls() )
gc()


#salgo del R sin grabar el gigante entorno
quit( save="no" )

