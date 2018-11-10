#Objetivo: crear variables derivadas al dataset, basadas en el mismo registro

#Feature Engineering
#creo nuevas variables dentro del mismo mes
#NO utilizo la historia
#Agrego suma de MasterCard y Visa

#Condimentar a gusto con nuevas variables


#source( "~/cloud/cloud1/codigoR/FeatureEngineering/fe_presente.r" )



#limpio la memoria
rm( list=ls() )
gc()



library( "data.table" )
library( "dplyr")


kcarpeta_datasets        <-  "~/cloud/cloud1/datasets/"

kcampos_separador        <-  "\t"
karchivo_entrada         <-  "paquete_premium.zip"

kcampo_foto              <- "foto_mes"

kextension               <-  "ext"
karchivo_salida_prefijo  <-  paste( "./", kextension, "/",    sep="" )
karchivo_salida_sufijo   <-  paste( "_", kextension, ".txt",    sep="" )


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
dataset <- fread(  paste("unzip -cq", karchivo_entrada), header=TRUE, sep=kcampos_separador ) 


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




#se crean los nuevos campos para Master y Visa, teniendo en cuenta los NA's

dataset  <- mutate( dataset,
              mv_cuenta_estado2       = pmax( Master_cuenta_estado, Visa_cuenta_estado, na.rm = TRUE),
              mv_marca_atraso         = pmax( Master_marca_atraso, Visa_marca_atraso, na.rm = TRUE) ,
              mv_mfinanciacion_limite = rowSums( cbind( Master_mfinanciacion_limite,  Visa_mfinanciacion_limite) , na.rm=TRUE ),
              mv_Fvencimiento         = pmin( Master_Fvencimiento, Visa_Fvencimiento, na.rm = TRUE) ,
              mv_Finiciomora          = pmin( Master_Finiciomora, Visa_Finiciomora, na.rm = TRUE) ,
              mv_msaldototal          = rowSums( cbind( Master_msaldototal,  Visa_msaldototal) , na.rm=TRUE ),
              mv_msaldopesos          = rowSums( cbind( Master_msaldopesos,  Visa_msaldopesos) , na.rm=TRUE ),
              mv_msaldodolares        = rowSums( cbind( Master_msaldodolares,  Visa_msaldodolares) , na.rm=TRUE ),
              mv_mconsumospesos       = rowSums( cbind( Master_mconsumospesos,  Visa_mconsumospesos) , na.rm=TRUE ),
              mv_mconsumosdolares     = rowSums( cbind( Master_mconsumosdolares,  Visa_mconsumosdolares) , na.rm=TRUE ),
              mv_mlimitecompra        = rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ),
              mv_madelantopesos       = rowSums( cbind( Master_madelantopesos,  Visa_madelantopesos) , na.rm=TRUE ),
              mv_madelantodolares     = rowSums( cbind( Master_madelantodolares,  Visa_madelantodolares) , na.rm=TRUE ),
              mv_fultimo_cierre       = pmax( Master_fultimo_cierre, Visa_fultimo_cierre, na.rm = TRUE),
              mv_mpagado              = rowSums( cbind( Master_mpagado,  Visa_mpagado) , na.rm=TRUE ),
              mv_mpagospesos          = rowSums( cbind( Master_mpagospesos,  Visa_mpagospesos) , na.rm=TRUE ),
              mv_mpagosdolares        = rowSums( cbind( Master_mpagosdolares,  Visa_mpagosdolares) , na.rm=TRUE ),
              mv_fechaalta            = pmax( Master_fechaalta, Visa_fechaalta, na.rm = TRUE),
              mv_mconsumototal        = rowSums( cbind( Master_mconsumototal,  Visa_mconsumototal) , na.rm=TRUE ),
              mv_tconsumos            = rowSums( cbind( Master_tconsumos,  Visa_tconsumos) , na.rm=TRUE ),
              mv_tadelantosefectivo   = rowSums( cbind( Master_tadelantosefectivo,  Visa_tadelantosefectivo) , na.rm=TRUE ),
              mv_mpagominimo          = rowSums( cbind( Master_mpagominimo,  Visa_mpagominimo) , na.rm=TRUE ) ,
              mvr_Master_mlimitecompra= Master_mlimitecompra / mv_mlimitecompra ,
              mvr_Visa_mlimitecompra  = Visa_mlimitecompra / mv_mlimitecompra ,
              mvr_msaldototal         = mv_msaldototal / mv_mlimitecompra ,
              mvr_msaldopesos         = mv_msaldopesos / mv_mlimitecompra ,
              mvr_msaldopesos2        = mv_msaldopesos / mv_msaldototal ,
              mvr_msaldodolares       = mv_msaldodolares / mv_mlimitecompra ,
              mvr_msaldodolares2      = mv_msaldodolares / mv_msaldototal ,
              mvr_mconsumospesos      = mv_mconsumospesos / mv_mlimitecompra ,
              mvr_mconsumosdolares    = mv_mconsumosdolares / mv_mlimitecompra ,
              mvr_madelantopesos      = mv_madelantopesos / mv_mlimitecompra ,
              mvr_madelantodolares    = mv_madelantodolares / mv_mlimitecompra ,
              mvr_mpagado             = mv_mpagado / mv_mlimitecompra ,
              mvr_mpagospesos         = mv_mpagospesos / mv_mlimitecompra ,
              mvr_mpagosdolares       = mv_mpagosdolares / mv_mlimitecompra ,
              mvr_mconsumototal       = mv_mconsumototal  / mv_mlimitecompra ,
              mvr_mpagominimo         = mv_mpagominimo  / mv_mlimitecompra 
              )





#obtengo todas las foto_mes distintas que hay en el dataset grande
fotos_distintas <-   dataset %>% distinct( get(kcampo_foto) ) %>% pull()

#creo la carpeta donde van los resultados
dir.create(file.path(kcarpeta_datasets, kextension), showWarnings = FALSE)

#grabo todas las fotos
lapply(  fotos_distintas,  fguardar_foto,  pdataset=dataset ) 



#limpio la memoria

rm( list=ls() )
gc()



quit( save="no" )


