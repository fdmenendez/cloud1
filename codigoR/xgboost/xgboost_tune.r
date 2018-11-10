#Objetivo buscar los parametros optimos de XGBoost para un archivos de <train, test>
#Modelo con libreria  xgboost con optimizacion bayesiana

#toma los parametros del entorno
#la salida queda en hyperparameter_GLOBAL.txt
#si train == test  entonces hace cross validation


#source( "/cloud/cloud1/codigoR/xgboost/xgboost_tune.r" )

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



library( "xgboost" )
library( "Matrix" )

library( "data.table" )


library( "DiceKriging" )
library( "mlrMBO")


setwd( directory.include )
source( "metrica.r" )


#parametros de entrada del script R
args <- commandArgs(trailingOnly = TRUE)


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
#en el XGboost
kcrossvalidation_folds <-      5
ksemilla               <- 102191
kmax_bin               <-     32
ksubsample             <-      1.0
kbase_score            <-      0.005
knthread               <-     16


#Parametros  mlrMBO
kiteraciones          <-  200
ksaveondisk_time      <-  600   # cada 600 segundos guarda a disco cuanto avanzo
karchivo_trabajo      <-  paste( "xgboost_tune_", kexperimento, ".RDATA", sep="" )


#Parametros salida
karchivo_salida       <-  "hyperparameter_GLOBAL.txt"

karchivo_imagen       <-   paste( "xgboost_tune_evolucion_", kexperimento, ".jpg", sep="" )



#estos valores se graban en el archivo de salida
kclase                <-  "ternaria"
kprograma             <-  "xgboost_tune.r"
kalgoritmo            <-  "xgboost"
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

    tituvar  <-  paste( "(iter=", nrow(salida), " max=", max(salida[ , "metrica" ]),  ")" )
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
#corre  xgboost  usando  crossvalidation

modelo_xgboost_ganancia_MBO_crossvalidation = function( x    )
{
  set.seed( ksemilla )
  t0       <-  Sys.time()
  modelo.cv = xgb.cv( 
                           data = dgeneracion,  
                           missing = NA,
                           stratified = TRUE,       nfold = kcrossvalidation_folds ,  # cross-validation
                           objective="binary:logistic",
                           nround= x$pnround,
                           base_score = kbase_score ,
                           feval = fganancia_logistic_xgboost,
                           subsample = ksubsample, 
                           colsample_bytree = x$pcolsample_bytree, 
                           eta = x$peta,
                           min_child_weight = x$pmin_child_weight, 
                           max_depth = x$pmax_depth,
                           alpha = x$palpha, lambda = x$plambda, gamma = x$pgamma,
                           tree_method = "hist",
                           max_bin = kmax_bin,
                           nthread = knthread 
                     )
 
  t1       <-  Sys.time()
  tiempo   <-  as.numeric(  t1 - t0, units = "secs")


  #esta es la forma NATURAL  de calcular la ganancia
  metrica_ultima     <- unlist( modelo.cv$evaluation_log[ , test_ganancia_mean] )[ x$pnround ] 


  st_parametros = paste( "nround=",             x$pnround,           ", ",
                         "base_score=",         kbase_score,         ", ",
                         "subsample=",          ksubsample,          ", ",
                         "colsample_bytree=",   x$pcolsample_bytree,  ", ",
                         "eta=",                x$peta,              ", ",
                         "min_child_weight=",   x$pmin_child_weight, ", ",
                         "max_depth=",          x$pmax_depth,        ", ",
                         "alpha=",              x$palpha,            ", ",
                         "lambda=",             x$plambda,           ", ",
                         "gamma=",              x$pgamma,            ", ",
                         "max_bin=",            kmax_bin,            ", ",
                         "tree_method=",        "'hist'",            
                         sep = ""
                       )



  #escribo al archivo de salida los resultados de esta corrida
  setwd( directory.work )
  cat( kexperimento,
       kcrossvalidation_folds * metrica_ultima, 
       0, #dejo AUC en 0,  o sea sin calcular
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



  return( - kcrossvalidation_folds * metrica_ultima )   # devuelvo el negativo porque la libreria  mlrMBO   solo minimiza funciones
}
#------------------------------------------------------


modelo_xgboost_ganancia_MBO_simple = function( x )
{

 	
  set.seed( ksemilla )
  t0       <-  Sys.time()
  modelo.train = xgb.train( 
                           data = dgeneracion,  
                           missing = NA,
                           watchlist = list( train=dgeneracion, test=daplicacion ),
                           objective="binary:logistic",
                           nround= x$pnround,
                           base_score = kbase_score ,
                           feval = fganancia_logistic_xgboost,
                           subsample = ksubsample, 
                           colsample_bytree = x$pcolsample_bytree, 
                           eta = x$peta,
                           min_child_weight = x$pmin_child_weight, 
                           max_depth = x$pmax_depth,
                           alpha = x$palpha, lambda = x$plambda, gamma = x$pgamma,
                           tree_method = "hist",
                           max_bin = kmax_bin,
                           nthread = knthread
                          )
 
  t1       <-  Sys.time()
  tiempo   <-  as.numeric(  t1 - t0, units = "secs")


  #esta es la forma NATURAL  de calcular la ganancia
  metrica_ultima     <- unlist( modelo.train$evaluation_log[ , test_ganancia] )[ x$pnround ] 


  #aplico el modelo a datos nuevos
  aplicacion_prediccion  <- predict(  modelo.train, daplicacion )


  # calculo la ganancia
  gan <-  fmetrica_ganancia_xgboost( 0.025, aplicacion_prediccion,  getinfo(daplicacion, "label") ) 

  # calculo el AUC
  auc <-  fmetrica_auc_xgboost( aplicacion_prediccion,  getinfo(daplicacion, "label") ) 


  #metrica_ultima y  gan  tienen que ser exactamente iguales

  setwd( directory.work )
  st_parametros = paste( "nround=",             x$pnround,           ", ",
                         "base_score=",         kbase_score,         ", ",
                         "subsample=",          ksubsample,          ", ",
                         "colsample_bytree=",   x$pcolsample_bytree,  ", ",
                         "eta=",                x$peta,              ", ",
                         "min_child_weight=",   x$pmin_child_weight, ", ",
                         "max_depth=",          x$pmax_depth,        ", ",
                         "alpha=",              x$palpha,            ", ",
                         "lambda=",             x$plambda,           ", ",
                         "gamma=",              x$pgamma,            ", ",
                         "max_bin=",            kmax_bin,            ", ",
                         "tree_method=",        "'hist'",            
                         sep = ""
                       )



  #escribo al archivo de salida los resultados de esta corrida
  cat( kexperimento,
       gan, 
       auc,
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

  return( -gan )   # devuelvo el negativo porque la libreria  mlrMBO   solo minimiza funciones
}
#------------------------------------------------------

dataset_xgboost = function( parchivos )
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


  #genero el formato requerido por XGBoost
  ddataset  <-   xgb.DMatrix( data  = data.matrix(dataset_sinclase_sparse),
                              label = dataset[ , get(kclase_nomcampo)], 
                              missing=NA 
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


#cargo generacion
dgeneracion <- dataset_xgboost( karchivo_train )


if( kcross==0 ){   
  modelo_xgboost_ganancia_MBO   <- modelo_xgboost_ganancia_MBO_simple 

  #cargo evaluacion
  daplicacion <- dataset_xgboost( karchivo_test )

 
} else { 
  modelo_xgboost_ganancia_MBO   <- modelo_xgboost_ganancia_MBO_crossvalidation 
}



configureMlr(show.learner.output = FALSE)


#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
obj.fun <- makeSingleObjectiveFunction(
		name = "prueba",
		fn   = modelo_xgboost_ganancia_MBO,
		par.set = makeParamSet(
                                       makeNumericParam("pcolsample_bytree" ,  lower=0.05    , upper=   1.0),
                                       makeNumericParam("peta"              ,  lower=0.0     , upper=   0.1),
                                       makeNumericParam("palpha"            ,  lower=0.0     , upper=   1.0),
                                       makeNumericParam("plambda"           ,  lower=0.0     , upper=   1.0),
                                       makeNumericParam("pgamma"            ,  lower=0.0     , upper=  20.0),
                                       makeNumericParam("pmin_child_weight" ,  lower=0.0     , upper=  10.0),
                                       makeIntegerParam("pmax_depth"        ,  lower=2L      , upper=  20L),
                                       makeIntegerParam("pnround"           ,  lower=100L    , upper=1000L)
		),
		has.simple.signature = FALSE,
		global.opt.value = -1
		)



ctrl <-  makeMBOControl(propose.points = 1L, save.on.disk.at.time = ksaveondisk_time,  save.file.path = karchivo_trabajo )
ctrl <-  setMBOControlTermination( ctrl, iters = kiteraciones )
ctrl <-  setMBOControlInfill( ctrl, crit = makeMBOInfillCritEI(), opt  = "focussearch", opt.focussearch.points = kiteraciones )

if( kalgoritmo=="xgboost" )   Nuggets <- 1e-8*var( xgboost::getinfo(dgeneracion, "label") )

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

