#La salida queda en  /work/apply_GLOBAL.txt

#source( "/cloud/cloud1/codigoR/ranger/ranger_apply.r" )




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


setwd( directory.include )
source( "metrica.r" )



kcampos_separador     <-  "\t"
kcampo_id             <-  "numero_de_cliente"
kclase_nomcampo       <-  "clase_ternaria"
kclase_valor_positivo <-  "BAJA+2"
kcampos_a_borrar      <-  c( kcampo_id )



karchivo_salida      <-  "apply_GLOBAL.txt"

#estos valores se graban en el archivo de salida
kclase                <-  "ternaria"
kprograma             <-  "ranger_apply.r"
kalgoritmo            <-  "ranger"
kobservaciones        <-  ""


#------------------------------------------------------------------------------

faplicar_modelo_parametros  = function( pst_parametros, pdataset_generacion, pdataset_aplicacion, pst_mesgeneracion, pst_mesevaluacion )
{

  #notar que el modelo se genera utilizando TODO el dataset
  formula  <-  formula( paste(kclase_nomcampo, "~ .") )


  #atencion con el lenguaje R, la funcion eval y la funcion parse  que pertenecen a El Lado Oscuro del R
  t0       <-  Sys.time()

  eval( parse( text =  paste("modelo  <- ranger( 
                                                formula,
                                                probability=TRUE,
                                                data = pdataset_generacion, "
                              , pst_parametros
                              ,")"
                              ,sep=""
                             )                    
                      
               )
         )
  

  t1       <-  Sys.time()
  tiempo   <-  as.numeric(  t1 - t0, units = "secs")




  #aplico el modelo a datos nuevos
  aplicacion_prediccion  <- predict(  modelo, pdataset_aplicacion )


  # calculo la ganancia en los datos de prediccion
  gan <-  fmetrica_ganancia_rpart( aplicacion_prediccion$predictions[, kclase_valor_positivo ],  pdataset_aplicacion[ , get(kclase_nomcampo)] )


  # calculo el AUC en testing
  auc <-  fmetrica_auc_rpart( aplicacion_prediccion$predictions[, kclase_valor_positivo ],  pdataset_aplicacion[ , get(kclase_nomcampo)] )


  #escribo el resultado de esta aplicacion
  setwd(  directory.work )
  cat(  
       gan, 
       auc,
       pst_parametros,
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
#Esta funcion recibe
#Un string con la lista de archivos para generacion
#Un string con la lista de archivos para evaluacion
#Un VECTOR  donde cada elemento es un string de parametros

aplicar_ranger  = function( pst_mesgeneracion, pst_mesevaluacion,  plista_parametros )
{

  #cargo generacion
  varchivos_train  <-  unlist(strsplit( pst_mesgeneracion, split=","))
  setwd(  directory.datasets )
  dataset_generacion <- do.call(rbind, lapply( varchivos_train, function(x) fread(x, header=TRUE, stringsAsFactors=TRUE, sep=kcampos_separador)))

  #borro las variables que no me interesan
  dataset_generacion[ ,  (kcampos_a_borrar) := NULL    ] 

  #imputo los nulos, ya que ranger no acepta nulos
  dataset_generacion <-  na.roughfix( dataset_generacion )


  #cargo evaluacion
  varchivos_aplicar  <-  unlist(strsplit( pst_mesevaluacion, split=","))
  setwd(  directory.datasets )
  dataset_aplicacion <- do.call(rbind, lapply( varchivos_aplicar, function(x) fread(x, header=TRUE, stringsAsFactors=TRUE, sep=kcampos_separador)))

  #imputo los nulos, ya que ranger no acepta nulos
  dataset_aplicacion <-  na.roughfix( dataset_aplicacion )


  lapply( plista_parametros$parametros, 
          faplicar_modelo_parametros,
          pdataset_generacion=dataset_generacion, 
          pdataset_aplicacion=dataset_aplicacion, 
          pst_mesgeneracion=pst_mesgeneracion, 
          pst_mesevaluacion=pst_mesevaluacion
        )

}
#------------------------------------------------------------------------------
