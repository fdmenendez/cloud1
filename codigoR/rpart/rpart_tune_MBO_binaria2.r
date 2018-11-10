#Modelo con libreria  rpart con optimizacion bayesiana
#Estimo la ganancia con  Repeated Random Sub Sampling Validation   ( Monte Carlo Cross Validation )
#comparar con Grid Search, en cuanto a tiempo y optimalidad de los resultados


#Estadisticos y actuarios no entren en panico porque estamos entrenando y evaluando en el mismo mes, ya vamos a mejorar .

#exp-1320
#source( "~/cloud/cloud1/codigoR/rpart/rpart_tune_binaria2.r" )

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



library( "rpart" )
library( "data.table" )
library( "caret" )

library( "DiceKriging" )
library( "mlrMBO")



setwd( directory.include )
source( "metrica.r" )




#Parametros entrada de nuestro dataset
karchivo_entrada      <-  "201802_dias.txt"
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
karchivo_trabajo      <-  "rpart_tune_MBO_binaria2.RDATA"


#Parametros salida
karchivo_salida       <-  "hyperparameter_GLOBAL.txt"
karchivo_imagen       <-  "rpart_tune_MBO_binaria2_evolucion.jpg"

#estos valores se graban en el archivo de salida
kexperimento          <-  1320
kclase                <-  "ternaria"
kprograma             <-  "rpart_tune_MBO_binaria2.r"
kalgoritmo            <-  "rpart"
kbusqueda             <-  "MBO"
kestimacion           <-  "Montecarlo"
kobservaciones        <-  "5 semillas"



#escribe un jpg con la evolucion de la ganancia
#tomo como entrada el archivo de salida que se genera en cada iteracion

#------------------------------------------------------
#NUEVA funcion que calcula la ganancia con pprob_corte_binaria2

fmetrica_ganancia_rpart_corte = function( pprob_corte_binaria2, probs, clases )
{
    return( sum( (probs > pprob_corte_binaria2 ) *
                 ifelse( clases== kclase_valor_positivo,
                         kganancia_acierto,
                         kganancia_noacierto
                       )
                )
          )
}
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
    setwd( directory.work )
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

modelo_rpart_uno = function( psemilla, pmaxdepth, pminbucket, pminsplit, pcp, pprob_corte_binaria2 )
{

  set.seed( psemilla )
  inTraining        <-  createDataPartition( dataset[ , get(kclase_nomcampo)],   p = ktraining_prob, list = FALSE)
  dataset_training  <-  dataset[  inTraining, ]
  dataset_testing   <-  dataset[ -inTraining, ]

  #creo la clase  clase_binaria2
  dataset_training[ , clase_binaria2:=  ifelse( get(kclase_nomcampo) == 'CONTINUA', 'CONTINUA', 'BAJA+2+1' ) ]
  #borro la variable  kclase_nomcampo "clase_ternaria" 
  dataset_training[ , (kclase_nomcampo) := NULL ]


  # generacion del modelo
  formula  <-  formula( paste('clase_binaria2', "~ .") )

  t0       <-  Sys.time()
  modelo   <-  rpart( formula,   data = dataset_training,  xval=0, maxdepth=pmaxdepth, minbucket=pminbucket, minsplit=pminsplit, cp=pcp )
  t1       <-  Sys.time()

  tiempo <-  as.numeric(  t1 - t0, units = "secs")


  #aplico el modelo a datos nuevos
  testing_prediccion  <- predict(  modelo, dataset_testing , type = "prob")


  # calculo la ganancia normalizada  en testing
  # usando la NUEVA  pprob_corte_binaria2
  gan <-  fmetrica_ganancia_rpart_corte( pprob_corte_binaria2, testing_prediccion[, 'BAJA+2+1' ],  dataset_testing[ , get(kclase_nomcampo)] ) / ( 1- ktraining_prob )

  # calculo el AUC en testing
  auc  <-  fmetrica_auc_rpart( testing_prediccion[, 'BAJA+2+1' ],  dataset_testing[ , get(kclase_nomcampo)] )


  return(  list( "ganancia"=gan,  "tiempo"= tiempo,  "auc"=auc )  )

}

#------------------------------------------------------
#corre  rpart  usando  las semillas, y promedia el resultado

modelo_rpart_ganancia_MBO = function( x = list( pmaxdepth, pminbucket, pminsplit, pcp, pprob_corte_binaria2 )  )
{

  vminbucket  <-   round( x$pminbucket * x$pminsplit )

  res  <-   lapply( ksemilla_azar, modelo_rpart_uno, pmaxdepth=x$pmaxdepth,  pminbucket=vminbucket, pminsplit=x$pminsplit, pcp=x$pcp, pprob_corte_binaria2 = x$pprob_corte_binaria2 )


  gan     <-  unlist( lapply( res, '[[', "ganancia" ) )
  tiempo  <-  unlist( lapply( res, '[[', "tiempo" ) )
  auc     <-  unlist( lapply( res, '[[', "auc" ) )


  #escribo en el archivo de salida aqui adentro de la funcion, porque es la unica oportunidad que tengo

  #genero el string con los parametros
  st_parametros = paste( "xval=",      0,           ", " ,
                         "maxdepth=",  x$pmaxdepth, ", " ,
                         "minbucket=", vminbucket,  ", " ,
                         "minsplit=",  x$pminsplit, ", " ,
                         "cp=",        x$pcp,
                         sep = ""
                       )

  #escribo al archivo de salida los resultados de esta corrida
  cat( kexperimento,
       mean(gan), 
       mean(auc),
       sum(tiempo),
       st_parametros,
       format(Sys.time(), "%Y%m%d %H%M%S"), 
       kclase, 
       kprograma, 
       kalgoritmo, 
       kbusqueda, 
       kestimacion,
       karchivo_entrada, karchivo_entrada,
       paste( "prob_corte_binaria2=", x$prob_corte_binaria2, sep=""),   #aqui pongo el nuevo parametro
       "\n", sep="\t", file=karchivo_salida, fill=FALSE, append=TRUE 
     )

  #escribo el grafico con la evolucion de la optimizacion
  EvolucionRefresh(  karchivo_imagen,  karchivo_salida, kexperimento )

  return( - mean(gan) )   # devuelvo el negativo porque la libreria  mlrMBO   solo minimiza funciones
}
#------------------------------------------------------



#cargo los datos
setwd( directory.datasets )
dataset <- fread( karchivo_entrada, header=TRUE, sep=kcampos_separador )


#borro las variables que no me interesan
dataset[ ,  (kcampos_a_borrar) := NULL    ] 



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





configureMlr(show.learner.output = FALSE)


#configuro la busqueda bayesiana,  los hiperparametros que se van a optimizar
obj.fun <- makeSingleObjectiveFunction(
		name = "prueba",
		fn   = modelo_rpart_ganancia_MBO,
		par.set = makeParamSet(
			makeIntegerParam("pmaxdepth"           ,  lower=3L    , upper=30L ),
			makeNumericParam("pminbucket"          ,  lower=0.1   , upper=0.5 ),
			makeIntegerParam("pminsplit"           ,  lower=1L    , upper=200L),
			makeNumericParam("pcp"                 ,  lower=0.0   , upper= 0.0001 ),
			makeNumericParam("pprob_corte_binaria2",  lower=0.025 , upper= 0.200 )
		),
		has.simple.signature = FALSE,
		global.opt.value = -1
		)



ctrl <-  makeMBOControl(propose.points = 1L, save.on.disk.at.time = ksaveondisk_time,  save.file.path = karchivo_trabajo )
ctrl <-  setMBOControlTermination( ctrl, iters = kiteraciones )
ctrl <-  setMBOControlInfill( ctrl, crit = makeMBOInfillCritEI(), opt  = "focussearch", opt.focussearch.points = kiteraciones )

surr.km <-  makeLearner("regr.km", predict.type = "se", covtype = "matern3_2", control = list(trace = FALSE))

lrn  <- makeMBOLearner(ctrl, obj.fun)

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

