#Modelo con libreria  ranger con optimizacion bayesiana

#Objetivo buscar los parametros optimos de ranger para un archivos de <train, test>

#toma los parametros del entorno
#la salida queda en hyperparameter_GLOBAL.txt
#si train == test  entonces hace cross validation



#source( "/cloud/cloud1/codigoR/ranger/ranger_tune.r" )

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




library( "ranger" )
library( "randomForest" )  #solo se usa para imputar nulos

library( "data.table" )
library( "caret" )

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



#Parametros  Repeated Random Sub Sampling Validation
ktraining_prob        <-  0.70
ksemilla_azar         <-  c( 102191, 200177, 410551, 552581, 892237 )


#Parametros  mlrMBO
kiteraciones          <-  200
ksaveondisk_time      <-  600   # cada 600 segundos guarda a disco cuanto avanzo
karchivo_trabajo      <-  paste( "ranger_tune_", kexperimento, ".RDATA", sep="" )


#Parametros salida
karchivo_salida       <-  "hyperparameter_GLOBAL.txt"

karchivo_imagen       <-   paste( "ranger_tune_evolucion_", kexperimento, ".jpg", sep="" )



#estos valores se graban en el archivo de salida
kclase                <-  "ternaria"
kprograma             <-  "ranger_tune.r"
kalgoritmo            <-  "ranger"
kbusqueda             <-  "MBO"
kestimacion           <-  "Montecarlo"
kobservaciones        <-  "na.roughfix"




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
#------------------------------------------------------------------------------
#Genera el modelo usando una semilla

modelo_ranger_gral = function( pdataset_training, pdataset_testing,  pnum.trees, pmin.node.size, pmtry, psplitrule )
{

  
  # generacion del modelo
  formula  <-  formula( paste(kclase_nomcampo, "~ .") )

  t0       <-  Sys.time()
  modelo   <-  ranger( formula, data = pdataset_training, probability=TRUE, num.trees=pnum.trees,  min.node.size= pmin.node.size, mtry= pmtry, splitrule= psplitrule )	
  t1       <-  Sys.time()

  tiempo <-  as.numeric(  t1 - t0, units = "secs")

 
  #aplico el modelo a datos nuevos
  testing_prediccion  <- predict(  modelo, pdataset_testing )


  # calculo la ganancia
  gan <-  fmetrica_ganancia_rpart( testing_prediccion$predictions[ ,kclase_valor_positivo ],  pdataset_testing[ , get(kclase_nomcampo)] ) 

  # calculo el AUC
  auc <-  fmetrica_auc_rpart( testing_prediccion$predictions[ ,kclase_valor_positivo ],  pdataset_testing[ , get(kclase_nomcampo)] ) 

  st_parametros = paste( "num.trees=",      pnum.trees,     ", ",
                         "min.node.size=",  pmin.node.size, ", " ,
                         "mtry=",           pmtry,          ", " ,
                         "splitrule=",      "'", psplitrule, "'",
                         sep = ""
                       )

  return(  list( "ganancia"=gan,  "tiempo"= tiempo,  "auc"=auc,  "parametros"=st_parametros )  )
}

#------------------------------------------------------------------------------
#Genera el modelo usando una semilla

modelo_ranger_uno = function( psemilla, pnum.trees, pmin.node.size, pmtry, psplitrule )
{

  set.seed( psemilla )
  inTraining        <-  createDataPartition( dataset[ , get(kclase_nomcampo)],   p = ktraining_prob, list = FALSE)
  dataset_training  <-  dataset[  inTraining, ]
  dataset_testing   <-  dataset[ -inTraining, ]


  res <-  modelo_ranger_gral(  dataset_training, dataset_testing,  pnum.trees, pmin.node.size, pmtry, psplitrule )

  return(  res  )
}

#------------------------------------------------------
#corre  ranger  usando  las semillas, y promedia el resultado

modelo_ranger_ganancia_MBO_Montecarlo = function( x = list( pnum.trees, pmin.node.size, pmtry, psplitrule)    )
{

  translator_vector <- c( "gini",  "extratrees"  )
  vsplitrule        <-  translator_vector[ x$psplitrule ] 

  res  <-   lapply( ksemilla_azar, modelo_ranger_uno, pnum.trees=x$pnum.trees,  pmin.node.size=x$pmin.node.size, pmtry=x$pmtry, psplitrule=vsplitrule )


  gan        <-  unlist( lapply( res, '[[', "ganancia" ) )
  tiempo     <-  unlist( lapply( res, '[[', "tiempo" ) )
  auc        <-  unlist( lapply( res, '[[', "auc" ) )
  parametros <-  unlist( lapply( res, '[[', "parametros" ) )

  #escribo en el archivo de salida aqui adentro de la funcion, porque es la unica oportunidad que tengo


  #escribo al archivo de salida los resultados de esta corrida
  cat( kexperimento,
       mean(gan)/ ( 1 - ktraining_prob ), 
       mean(auc),
       sum(tiempo),
       parametros[1],
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

  return( - mean(gan)/ ( 1 - ktraining_prob ) )   # devuelvo el negativo porque la libreria  mlrMBO   solo minimiza funciones
}
#------------------------------------------------------

#corre  ranger  SIN semillas, ya que test es distinto a train

modelo_ranger_ganancia_MBO_simple = function( x = list( pnum.trees, pmin.node.size, pmtry, psplitrule)  )
{

  translator_vector <- c( "gini",  "extratrees"  )
  vsplitrule        <-  translator_vector[ x$psplitrule ] 

  res <- modelo_ranger_gral(  dataset, dataset_test,  x$pnum.trees, x$pmin.node.size, x$pmtry, vsplitrule )

  #escribo al archivo de salida los resultados de esta corrida
  cat( kexperimento,
       res$gan, 
       res$auc,
       res$tiempo,
       res$parametros,
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

  return( -res$gan )   # devuelvo el negativo porque la libreria  mlrMBO   solo minimiza funciones
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



#cargo los datos de TODOS los archivos de la lista  karchivos_train
#notar que se agrego a fread el parametro stringsAsFactors=TRUE
setwd( directory.datasets )
varchivos_train  <-  unlist(strsplit( karchivo_train, split=","))
dataset <- do.call(rbind, lapply( varchivos_train, function(x) fread(x, header=TRUE, stringsAsFactors=TRUE, sep=kcampos_separador)))
 

#borro las variables que no me interesan
dataset[ ,  (kcampos_a_borrar) := NULL    ] 


#imputo los nulos, ya que ranger no acepta nulos
dataset <-  na.roughfix( dataset )


if( kcross==0 ){   
  modelo_ranger_ganancia_MBO   <- modelo_ranger_ganancia_MBO_simple 

  #cargo los datos de TODOS los archivos de la lista  karchivos_test
  #notar que se agrego a fread el parametro stringsAsFactors=TRUE
  setwd( directory.datasets )
  varchivos_test  <-  unlist(strsplit( karchivo_test, split=","))
  dataset_test    <- do.call(rbind, lapply( varchivos_test, function(x) fread(x, stringsAsFactors=TRUE, header=TRUE, sep=kcampos_separador)))
  
  #borro las variables que no me interesan
  dataset_test[ ,  (kcampos_a_borrar) := NULL    ] 

  #imputo los nulos, ya que ranger no acepta nulos
  dataset_test <-  na.roughfix( dataset_test )


} else { 
  modelo_ranger_ganancia_MBO   <- modelo_ranger_ganancia_MBO_Montecarlo 
}



configureMlr(show.learner.output = FALSE)


#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
obj.fun <- makeSingleObjectiveFunction(
		name = "prueba",
		fn   = modelo_ranger_ganancia_MBO,
		par.set = makeParamSet(
                        makeIntegerParam("pnum.trees"        ,  lower=50L   , upper=1000L ),
			makeIntegerParam("pmin.node.size"    ,  lower=1L    , upper=500L ),
			makeIntegerParam("pmtry"             ,  lower=2L    , upper= trunc( 2*sqrt( ncol(dataset) )) ),
			makeIntegerParam("psplitrule"        ,  lower=1L    , upper= 2L )
		),
		has.simple.signature = FALSE,
		global.opt.value = -1
		)



ctrl <-  makeMBOControl(propose.points = 1L, save.on.disk.at.time = ksaveondisk_time,  save.file.path = karchivo_trabajo )
ctrl <-  setMBOControlTermination( ctrl, iters = kiteraciones )
ctrl <-  setMBOControlInfill( ctrl, crit = makeMBOInfillCritEI(), opt  = "focussearch", opt.focussearch.points = kiteraciones )

surr.km <-  makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", control = list(trace = FALSE))

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

