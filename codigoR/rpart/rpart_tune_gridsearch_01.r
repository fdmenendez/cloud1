#Modelo con libreria  rpart con busqueda  Grid Search
#Estimo la ganancia con  Repeated Random Sub Sampling Validation   ( Monte Carlo Cross Validation )
#Por favor, no desesperarse por la ESPANTOSA granularidad de la Grid Search
#notar el uso de CPU y memoria RAM

#Si este programa se corta,  se lo debe volver a correr y automaticamente retoma desde donde llego la vez anterior

#Estadisticos y actuarios no entren en panico porque estamos entrenando y evaluando en el mismo mes, ya vamos a mejorar

#exp-1100
#source( "~/cloud/cloud1/codigoR/rpart/rpart_tune_gridsearch_01.r" )


#limpio la memoria
rm( list=ls() )
gc()


library( "rpart" )
library( "data.table" )
library( "caret" )


switch ( Sys.info()[['sysname']],
         Windows = { directory.include  <-  "M:\\codigoR\\include\\"
                     directory.work     <-  "M:\\work\\"
                     directory.plan     <-  "M:\\plan\\"
                     directory.datasets <-  "M:\\datasets\\"
                   },
         Darwin  = { directory.include  <-  "~/dm/codigoR/include/"
                     directory.work     <-  "~/dm/work/"
                     directory.plan     <-  "~/dm/plan/"
                     directory.datasets <-  "~/dm/datasets/"
                   },
         Linux   = { directory.include  <-  "~/cloud/cloud1/codigoR/include/"
                     directory.work     <-  "~/cloud/cloud1/work/"
                     directory.plan     <-  "~/cloud/cloud1/plan/"
                     directory.datasets <-  "~/cloud/cloud1/datasets/"
                   }
        )



setwd( directory.include )
source( "metrica.r" )


#Parametros entrada de nuestro dataset
karchivo_entrada      <-  "201802.txt"

kcampos_separador     <-  "\t"
kcampo_id             <-  "numero_de_cliente"
kclase_nomcampo       <-  "clase_ternaria"
kclase_valor_positivo <-  "BAJA+2"
kcampos_a_borrar      <-  c( kcampo_id )


#Parametros  Repeated Random Sub Sampling Validation
ktraining_prob        <-  0.70
ksemilla_azar         <-  c( 102191, 200177, 410551, 552581, 892237 )


karchivo_salida       <-  "hyperparameter_GLOBAL.txt"

#estos valores se graban en el archivo de salida
kexperimento          <-  1100
kclase                <-  "ternaria"
kprograma             <-  "rpart_tune_gridsearch_01.r"
kalgoritmo            <-  "rpart"
kbusqueda             <-  "gridsearch"
kestimacion           <-  "Montecarlo"
kobservaciones        <-  "5 semillas"




#------------------------------------------------------
#Genera el modelo usando una semilla

modelo_rpart_uno = function( psemilla, pmaxdepth, pminbucket, pminsplit, pcp )
{

  set.seed( psemilla )
  inTraining        <-  createDataPartition( dataset[ , get(kclase_nomcampo)],   p = ktraining_prob, list = FALSE)
  dataset_training  <-  dataset[  inTraining, ]
  dataset_testing   <-  dataset[ -inTraining, ]



  # generacion del modelo
  formula  <-  formula( paste(kclase_nomcampo, "~ .") )

  t0       <-  Sys.time()
  modelo   <-  rpart( formula,   data = dataset_training,  xval=0, maxdepth=pmaxdepth, minbucket=pminbucket, minsplit=pminsplit, cp=pcp )
  t1       <-  Sys.time()

  tiempo <-  as.numeric(  t1 - t0, units = "secs")


  #aplico el modelo a datos nuevos
  testing_prediccion  <- predict(  modelo, dataset_testing , type = "prob")


  # calculo la ganancia normalizada  en testing
  gan <-  fmetrica_ganancia_rpart( testing_prediccion[, kclase_valor_positivo ],  dataset_testing[ , get(kclase_nomcampo)] ) / ( 1- ktraining_prob )


  # calculo el AUC en testing
  auc <- fmetrica_auc_rpart( testing_prediccion[ ,kclase_valor_positivo],  dataset_testing[ , get(kclase_nomcampo)] )


  return(  list( "ganancia"=gan,  "tiempo"= tiempo,  "auc"=auc )  )

}

#------------------------------------------------------
#corre  rpart  usando  las semillas, y promedia el resultado

modelo_rpart_ganancia = function( dataset, pmaxdepth, pminbucket, pminsplit, pcp  )
{

  res  <-   lapply( ksemilla_azar, modelo_rpart_uno, pmaxdepth=pmaxdepth,  pminbucket=pminbucket, pminsplit=pminsplit, pcp=pcp )


  return(  list( "ganancia" = unlist( lapply( res, '[[', "ganancia" ) ), 
                 "tiempo"   = unlist( lapply( res, '[[', "tiempo" ) ),
                 "auc"      = unlist( lapply( res, '[[', "auc" ) )
               )
        )

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
  
   
  lineas_salida <- 0
} else
{
  salida <-  read.table( karchivo_salida, header=TRUE, sep=kcampos_separador )
  lineas_salida <- nrow( salida )
}



linea <- 1


for( vcp  in  c( 0, 0.0005,  0.001, 0.005 ) )
{
for( vminsplit  in  c(  2, 5, 10, 20, 50, 100, 200, 400, 500, 800, 1000 )  )
{
for( vminbucket  in  c( trunc(vminsplit/4), trunc(vminsplit/3) )  )
{
for(  vmaxdepth  in  c(  4, 6, 8, 10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30 ) )
{ 

  if( linea > lineas_salida )  #no volver a procesar si en la corrida anterior se llego a esa lina
  {

    res <- modelo_rpart_ganancia( dataset, pmaxdepth=vmaxdepth, pminbucket=vminbucket, pminsplit=vminsplit, pcp=vcp  )

    #genero el string con los parametros
    st_parametros = paste( "xval=",      0,          ", " ,
                           "maxdepth=",  vmaxdepth,  ", " ,
                           "minbucket=", vminbucket, ", " ,
                           "minsplit=",  vminsplit,  ", " ,
                           "cp=",        vcp,
                           sep = ""
                         )

    cat( kexperimento, 
         mean(res$ganancia), 
         mean(res$auc),
         sum(res$tiempo),
         st_parametros,
         format(Sys.time(), "%Y%m%d %H%M%S"), 
         kclase, 
         kprograma, 
         kalgoritmo, 
         kbusqueda, 
         kestimacion,
         karchivo_entrada, karchivo_entrada,
         kobservaciones,
         "\n", sep="\t", file=karchivo_salida, fill=FALSE, append=TRUE 
       )
  }

  linea <- linea+1

}
}
}
}



#limpio la memoria
rm( list=ls() )
gc()


#salgo del R sin grabar el gigante entorno
quit( save="no" )

