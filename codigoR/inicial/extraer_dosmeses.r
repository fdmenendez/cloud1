#extraigo los meses de 201802 y 201804


#source( "~/cloud/cloud1/codigoR/inicial/extraer_dosmeses.r" )


#limpio la memoria
rm( list=ls() )
gc()



library( "data.table" )
library( "dplyr")


kcarpeta_datasets        <-  "~/cloud/cloud1/datasets/"

kcampos_separador        <-  "\t"
karchivo_entrada         <-  "paquete_premium.txt"

kcampo_foto              <- "foto_mes"

karchivo_salida_prefijo  <-  ""
karchivo_salida_sufijo   <-  ".txt"


#------------------------------------------------------

fguardar_foto  = function( pfoto_mes, pdataset )
{
  
  archivo_salida_mes <- paste( karchivo_salida_prefijo,  pfoto_mes,  karchivo_salida_sufijo, sep="" )

  write.table(  pdataset %>% filter( get(kcampo_foto) == pfoto_mes ), 
                file=archivo_salida_mes , 
                sep=kcampos_separador, na="", row.names=FALSE ) 
}
#------------------------------------------------------



setwd( kcarpeta_datasets )

#lectura rapida del dataset  usando fread  de la libreria  data.table
dataset <- fread(  karchivo_entrada, header=TRUE, sep=kcampos_separador ) 


nrow( dataset )
ncol( dataset )


fguardar_foto(  "201802", dataset )
fguardar_foto(  "201804", dataset )



#limpio la memoria

rm( list=ls() )
gc()



quit( save="no" )


