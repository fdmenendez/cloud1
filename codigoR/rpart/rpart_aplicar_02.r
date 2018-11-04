#Aplicacion del modelo de rpart

#Aqui la ganancia NO se estima, sino que se calcula directamente. 
#No hay training/testing

#Se trabaja con estos hiperparametros fijos de rpart, los que obviamente en algun momento se optimizaran
#  vxval       <-   0
#  vmaxdepth   <-  16
#  vminbucket  <-   6
#  vminsplit   <-  20 
#  vcp         <-   0

#Se aplica  rpart con los anteriores parametros a varios meses de generacion y aplicacion
#se trabaja con los datasets de fechas relativas en dias


#source( "~/cloud/cloud1/codigoR/rpart/rpart_aplicar_02.r" )

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



setwd( directory.include )
source( "metrica.r" )


kapply_plan   <-  "apply_plan.txt"


kcampos_separador     <-  "\t"
kcampo_id             <-  "numero_de_cliente"
kclase_nomcampo       <-  "clase_ternaria"
kclase_valor_positivo <-  "BAJA+2"
kcampos_a_borrar      <-  c( kcampo_id )



karchivo_salida      <-  "apply_GLOBAL.txt"

#estos valores se graban en el archivo de salida
kclase                <-  "ternaria"
kprograma             <-  "rpart_aplicar_02.r"
kalgoritmo            <-  "rpart"
kobservaciones        <-  ""



#------------------------------------------------------------------------------

faplicar_modelo  = function( pst_mesgeneracion, pst_mesevaluacion )
{

  varchivos_train  <-  unlist(strsplit( pst_mesgeneracion, split=", "))
  #cargo los datos de TODOS los archivos de la lista  karchivos_train
  setwd( directory.datasets )
  dataset_generacion <- do.call(rbind, lapply( varchivos_train, function(x) fread(x, header=TRUE, sep=kcampos_separador)))
 


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

  #cargo los datos de TODOS los archivos de la lista  pst_mesevaluacion
  setwd( directory.datasets )
  varchivos_aplicar  <-  unlist(strsplit( pst_mesevaluacion, split=", "))
  dataset_aplicacion <- do.call(rbind, lapply( varchivos_aplicar, function(x) fread(x, header=TRUE, sep=kcampos_separador)))
 


  #aplico el modelo a datos nuevos
  aplicacion_prediccion  <- predict(  modelo, dataset_aplicacion , type = "prob")


  # calculo la ganancia en los datos de prediccion
  gan <-  fmetrica_ganancia_rpart( aplicacion_prediccion[, kclase_valor_positivo ],  dataset_aplicacion[ , get(kclase_nomcampo)] )
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
  setwd( directory.work )
  cat(  
       gan, 
       auc,
       st_parametros,
       format(Sys.time(), "%Y%m%d %H%M%S"), 
       kclase, 
       kprograma, 
       kalgoritmo, 
       pst_mesgeneracion, pst_mesevaluacion,
       kobservaciones,
       "\n", sep="\t", file=karchivo_salida, fill=FALSE, append=TRUE 
    )

}
#------------------------------------------------------------------------------

#leo el plan de donde voy a generar el modelo y a que meses lo voy a aplicar para medir la ganancia
setwd( directory.plan )
dataset_mesesgeneracion <- fread( kapply_plan, header=TRUE, sep="\t" )

mapply(  faplicar_modelo,  dataset_mesesgeneracion$mes_generacion,  dataset_mesesgeneracion$mes_evaluacion )


#limpio la memoria
rm( list=ls() )
gc()


#salgo del R sin grabar el gigante entorno
quit( save="no" )

