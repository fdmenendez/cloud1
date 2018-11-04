#' ---
#' @title:  Generacion de variables calculadas
#' @author: Ale
#' @date:   2018/10/07
#' @description: Generar nuevas variables a partir de las variables originales 
#' con la correccion de fecha
#' @file: calculadas.r
#' ---

#limpio la memoria
rm( list=ls() )
gc()

directory.include  <-  "C:\\Users\\saralb\\Desktop\\UBA\\cloud\\codigoR\\include"
directory.work     <-  "C:\\Users\\saralb\\Desktop\\UBA\\cloud\\work\\"
directory.plan     <-  "C:\\Users\\saralb\\Desktop\\UBA\\cloud\\plan\\"
directory.exec     <-  "C:\\Users\\saralb\\Desktop\\UBA\\cloud\\exec\\"
directory.datasets <-  "C:\\Users\\saralb\\Desktop\\UBA\\DropBox\\UBA2018_20180803\\datasets\\dias\\"

# Agregamos un nuevo directorio
directory.output <-  "C:\\Users\\saralb\\Desktop\\UBA\\DropBox\\UBA2018_20180803\\datasets\\calculadas\\"

# switch ( Sys.info()[['sysname']],
#          Windows = { directory.include  <-  "M:\\codigoR\\include\\"
#          directory.work     <-  "M:\\work\\"
#          directory.plan     <-  "M:\\plan\\"
#          directory.datasets <-  "M:\\datasets\\dias\\"
#          directory.datasets <-  "M:\\datasets\\calculadas\\"
#          },
#          Darwin  = { directory.include  <-  "~/dm/codigoR/include/"
#          directory.work     <-  "~/dm/work/"
#          directory.plan     <-  "~/dm/plan/"
#          directory.datasets <-  "~/dm/datasets/dias/"
#          directory.datasets <-  "~/dm/datasets/calculadas/"
#          },
#          Linux   = { directory.include  <-  "~/cloud/cloud1/codigoR/include/"
#          directory.work     <-  "~/cloud/cloud1/work/"
#          directory.plan     <-  "~/cloud/cloud1/plan/"
#          directory.datasets <-  "~/cloud/cloud1/datasets/dias/"
#          directory.datasets <-  "~/cloud/cloud1/datasets/calculadas/"
#          }
# )

library( "data.table" )
library( "dplyr")

# Primer periodo
k.pp.primero <- "201601"

# Transforma de periodo a fecha para utilizar las funciones de R
pp_2_date <- function (pp) as.Date(paste0(pp, "01"), format="%Y%m%d")
# Vuelve a trabajar con periodos
date_2_pp <- function(date) format(date, format="%Y%m")


# Calcula todos los periodos a procesar
periodos <- sapply(
            seq(pp_2_date(k.pp.primero), by = paste (1, "months"), length = 30),
            date_2_pp)

kcampos_separador     <-  "\t"

for (pp in periodos) {
  ds <- fread(paste0(directory.datasets,pp,"_dias.txt"), 
              header=TRUE, sep=kcampos_separador)
  
  ds  <- ds %>% mutate(
    max_marca_atraso  = pmax( Master_marca_atraso, Visa_marca_atraso, na.rm = TRUE) ,
    total_limite_compra  = rowSums( cbind( Master_mlimitecompra,  Visa_mlimitecompra) , na.rm=TRUE ),
    r_master_limite_compra = Master_mlimitecompra / total_limite_compra ,
    r_visa_limite_compra   = Visa_mlimitecompra / total_limite_compra
    
    ##
    ## Agregar nuevas variables aquí.  
    ##
  )
  fwrite(ds, file=paste0(directory.output, pp, "_calc.txt") , sep=kcampos_separador, na="", row.names=FALSE )   
  # Lo borro
  rm(ds)
  
}

quit( save="no" )

