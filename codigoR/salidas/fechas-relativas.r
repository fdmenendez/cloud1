#Objetivo : pasar los campos de fechas absolutas a fechas relativas

#paso los campos fecha a la cantidad de dias de la foto


#source( "~/cloud/cloud1/codigoR/FeatureEngineering/fechas_relativas.r" )


#limpio la memoria
rm( list=ls() )
gc()



library( "data.table" )
library( "dplyr")


kcarpeta_datasets        <-  "~/cloud/cloud1/datasets/"

kcampos_separador        <-  "\t"
karchivo_entrada         <-  "paquete_premium.txt"

kcampo_foto              <- "foto_mes"

karchivo_salida_prefijo  <-  "./dias/"
karchivo_salida_sufijo   <-  "_dias.txt"


#------------------------------------------------------
#Esta funcion calcula la cantidad de dias entre la foto_mes y la fecha
#la foto_mes 201804 se interpreta como la fehca "20180501 00:00:00"

fdias_entre  = function( pfoto_mes, pfecha )
{
  
  foto_mes       <- as.POSIXlt( as.Date(  paste(pfoto_mes, "01", sep=""), format='%Y%m%d'  ) )
  foto_mes$mon   <- foto_mes$mon +1

  fecha         <-  as.Date(  as.character(pfecha), format='%Y%m%d'  )

  return( as.numeric( difftime(foto_mes, fecha, units = c("days")) ) )
}
#------------------------------------------------------
#guarda el archivo de un mes

fguardar_foto  = function( pfoto_mes, pdataset )
{
  
  dataset_mes   <-   pdataset %>% filter( get(kcampo_foto) == pfoto_mes )

  archivo_salida_mes <- paste( karchivo_salida_prefijo,  pfoto_mes,  karchivo_salida_sufijo, sep="" )

  fwrite(  dataset_mes, file=archivo_salida_mes , sep=kcampos_separador, na="", row.names=FALSE ) 
}
#------------------------------------------------------



setwd( kcarpeta_datasets )

#lectura rapida del dataset  usando fread  de la libreria  data.table
dataset <- fread(  karchivo_entrada, header=TRUE, sep=kcampos_separador ) 


nrow( dataset )
ncol( dataset )


#paso los campos fecha a dias relativos

dataset  <- mutate( dataset,
                    Master_Fvencimiento    = fdias_entre( get(kcampo_foto), Master_Fvencimiento ),
                    Master_Finiciomora     = fdias_entre( get(kcampo_foto), Master_Finiciomora ),
                    Master_fultimo_cierre  = fdias_entre( get(kcampo_foto), Master_fultimo_cierre ),
                    Master_fechaalta       = fdias_entre( get(kcampo_foto), Master_fechaalta ),
                    Visa_Fvencimiento      = fdias_entre( get(kcampo_foto), Visa_Fvencimiento ),
                    Visa_Finiciomora       = fdias_entre( get(kcampo_foto), Visa_Finiciomora ),
                    Visa_fultimo_cierre    = fdias_entre( get(kcampo_foto), Visa_fultimo_cierre ),
                    Visa_fechaalta         = fdias_entre( get(kcampo_foto), Visa_fechaalta )
                  )



#obtengo todas las foto_mes distintas que hay en el dataset grande
fotos_distintas <-   dataset %>% distinct( get(kcampo_foto) ) %>% pull()


lapply(  fotos_distintas,  fguardar_foto,  pdataset=dataset ) 


#limpio la memoria

rm( list=ls() )
gc()



quit( save="no" )


