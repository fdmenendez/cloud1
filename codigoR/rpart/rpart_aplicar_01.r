#Aplicacion del modelo de rpart

#Aqui la ganancia NO se estima, sino que se calcula directamente. 
#No hay training/testing


#exp-1200
#source( "~/cloud/cloud1/codigoR/rpart/rpart_aplicar_01.r" )

#limpio la memoria
rm( list=ls() )
gc()



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


library( "rpart" )
library( "data.table" )


setwd( directory.include )
source( "metrica.r" )



Parametros entrada de nuestro dataset
karchivo_generacion   <-  "201802.txt"

karchivo_aplicacion   <-  "201804.txt"

kcampos_separador     <-  "\t"
kcampo_id             <-  "numero_de_cliente"
kclase_nomcampo       <-  "clase_ternaria"
kclase_valor_positivo <-  "BAJA+2"
kcampos_a_borrar      <-  c( kcampo_id )




karchivo_salida       <-  "apply_GLOBAL.txt"

#estos valores se graban en el archivo de salida
kclase                <-  "ternaria"
kprograma             <-  "rpart_aplicar_01.r"
kalgoritmo            <-  "rpart"
kobservaciones        <-  "5 semillas"

karchivo_intermedio   <-  "rpart_aplicar_intermedio.txt"

#------------------------------------------------------


#cargo los datos de generacion
setwd( directory.datasets )
dataset_generacion <- fread( karchivo_generacion, header=TRUE, sep=kcampos_separador )


#borro las variables que no me interesan
dataset_generacion[ ,  (kcampos_a_borrar) := NULL    ] 


#estos son los valores que en la corrida optimizacion de hiperparametros dan la mayor ganancia
vxval       <-   0
vmaxdepth   <-  16
vminbucket  <-   6
vminsplit   <-  20 
vcp         <-   0


#notar que el modelo se genera utilizando TODO el dataset
formula  <-  formula( paste(kclase_nomcampo, "~ .") )
modelo   <-  rpart( formula,   data = dataset_generacion,  
                    xval=vxval, maxdepth=vmaxdepth, minbucket=vminbucket, minsplit=vminsplit, cp=vcp )


#------------------------
#ahora paso a aplicar el modelo que recien genere

#cargo los datos de aplicacion
setwd( directory.datasets )
dataset_aplicacion <- fread( karchivo_aplicacion, header=TRUE, sep=kcampos_separador )


#aplico el modelo a datos nuevos
aplicacion_prediccion  <- predict(  modelo, dataset_aplicacion , type = "prob")



#-----------------------
#Las siguientes lineas son para mostrar el detalle de la prediccion y el calculo de la ganancia
#agrego la columno  numero_de_cliente y la clase
data_original <-   as.data.table( cbind( dataset_aplicacion[ , get(kcampo_id)],  dataset_aplicacion[ , get(kclase_nomcampo)  ] ) )
colnames( data_original ) <-  c( kcampo_id, kclase_nomcampo ) 

#para facilitarme la vida, agrego el campo positivo
data_original[  , positivo:= as.numeric( get(kclase_nomcampo)== kclase_valor_positivo ) ]


#pego el ID y la clase con  la matrix de las predicciones
data_pred      <-  as.data.table( cbind(   data_original,  aplicacion_prediccion ) )


#calculo si envio o no a la campana de retencion
data_pred[  , enviar := as.numeric( get( kclase_valor_positivo ) > kprob_corte ) ]   #kprob_corte=0.025 0 si no envio, 1 si envio


#hago unos calculos a ver cuantos envio a la campana de retencion
nrow( data_pred )
sum(  data_pred$enviar )
sum(  data_pred$enviar ) / nrow( data_pred )


data_pred[  , ganancia :=  ifelse(  enviar, ifelse( positivo, kganancia_acierto, kganancia_noacierto), NA ) ]    #kganancia_acierto=11700

ganancia_manual <- sum( data_pred$ganancia, na.rm=TRUE )

envios_marketing      <-  sum(  data_pred$enviar )
aciertos_marketing    <-  sum(  data_pred[ , enviar*positivo]  )
noaciertos_marketing  <-  sum(  data_pred[ , (1-enviar)*positivo] ) 

ganancia_manual2      <-  aciertos_marketing*kganancia_acierto +  noaciertos_marketing*kganancia_noacierto

#Que llamativo, en mi campana de marketing le estoy acertando a menos del 10% 
indice_aciertos       <-   aciertos_marketing/envios_marketing


true_positive         <-  sum( data_pred[ , enviar*positivo], na.rm=TRUE )
false_positive        <-  sum( data_pred[ , enviar*(1-positivo)], na.rm=TRUE ) 
false_negative        <-  sum( data_pred[ , (1-enviar)*positivo], na.rm=TRUE )
true_negative         <-  sum( data_pred[ , (1-enviar)*(1-positivo)], na.rm=TRUE )

densidad_universo     <-  sum( data_pred$positivo) / nrow(  data_pred )
densidad_envio        <-  sum( data_pred[ , enviar*positivo] ) / sum(   data_pred$enviar )
lift_envio            <-  densidad_envio / densidad_universo

#grabo la prediccion a un archivo 
setwd( directory.work )
fwrite( data_pred,  file=karchivo_intermedio, row.names=FALSE, sep="\t" )

#-----------------------



#continuo con el codigo normal

# calculo la ganancia en los datos de prediccion
gan <-  fmetrica_ganancia_rpart( aplicacion_prediccion[, kclase_valor_positivo ],  dataset_aplicacion[ , get(kclase_nomcampo)] )


# calculo el AUC en esting
auc <-  fmetrica_auc_rpart( aplicacion_prediccion[, kclase_valor_positivo ],  dataset_aplicacion[ , get(kclase_nomcampo)] )



#escribo los  titulos  del archivo salida, en caso que no los tenga
setwd( directory.work )
if( !file.exists( karchivo_salida) )
{
  cat("metrica",
      "metrica2",
      "parametros",
      "fecha", 
      "clase", "programa", "algoritmo", 
      "dataset_generacion", "dataset_aplicacion", "observaciones",
      "\n", sep="\t", file=karchivo_salida, fill=FALSE, append=FALSE )
}



#genero el string con los parametros
st_parametros = paste( "xval=",      vxval,      ", " ,
                       "maxdepth=",  vmaxdepth,  ", " ,
                       "minbucket=", vminbucket, ", " ,
                       "minsplit=",  vminsplit,  ", " ,
                       "cp=",        vcp,
                       sep = ""
                     )

#escribo el resultado de esta aplicacion
cat(
     gan, 
     auc,
     st_parametros,
     format(Sys.time(), "%Y%m%d %H%M%S"), 
     kclase, 
     kprograma, 
     kalgoritmo, 
     karchivo_generacion, karchivo_aplicacion,
     kobservaciones,
     "\n", sep="\t", file=karchivo_salida, fill=FALSE, append=TRUE 
  )



#limpio la memoria
rm( list=ls() )
gc()


#salgo del R sin grabar el gigante entorno
quit( save="no" )

