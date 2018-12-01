#Objetivo: crear la clase baja a abril




#limpio la memoria
rm( list=ls() )
gc()

script.name<- "cambio-baja-mas"

switch ( Sys.info()[['sysname']],
         Windows = { directory.include  <-  "M:\\codigoR\\include\\"
         directory.work     <-  paste0("M:\\work\\",script.name,"\\")
         directory.plan     <-  "M:\\plan\\"
         directory.datasets <-  "M:\\datasets\\"
         directory.root     <-  "M:\\"
         },
         Darwin  = { directory.include  <-  "~/dm/codigoR/include/"
         directory.work     <-  "~/dm/work/"
         directory.plan     <-  "~/dm/plan/"
         directory.datasets <-  "~/dm/datasets/"
         },
         Linux   = { 
           directory.include  <-  "~/cloud/cloud1/codigoR/include/"
           directory.work     <-  paste0("~/cloud/cloud1/work/",script.name,"/")
           directory.plan     <-  "~/cloud/cloud1/plan/"
           directory.datasets <-  paste0("~/cloud/cloud1/datasets/")
         }
)

source(paste0(directory.include,"utils.r"))

include.packages( "dplyr")


kextension_in               <-  "exthist"
kextension_out              <-  "clasehist"

switch ( Sys.info()[['sysname']],
         Windows = {
           karchivo_entrada_prefijo <-  paste( "", kextension_in, "\\",    sep="" )
           karchivo_salida_prefijo  <-  paste( "", kextension_out, "\\",    sep="" )
         },
         Linux = {
           karchivo_entrada_prefijo <-  paste( "./", kextension_in, "/",    sep="" )
           karchivo_salida_prefijo  <-  paste( "./", kextension_out, "/",    sep="" )
         }
)

karchivo_salida_sufijo   <-  paste( kextension_out, ".rds",    sep="" )
karchivos_generacion  <-  "201801_,201712_,201711_,201710_,201709_,201708_"
karchivo_base <- "201802_"

dir.create(file.path(directory.datasets, kextension_out), showWarnings = FALSE)

agregar_clase_baja <- function(data) {
  ent<-readRDS(paste0(karchivo_entrada_prefijo,data,kextension_in,".rds"))
  jnt<-ent %>% inner_join(fija, by = "numero_de_cliente")
  saveRDS(jnt, paste0(karchivo_salida_prefijo,data,karchivo_salida_sufijo))
  
}


setwd(directory.datasets)

fija<-readRDS(paste0(karchivo_entrada_prefijo,karchivo_base,kextension_in,".rds")) %>% select(numero_de_cliente,clase_ternaria)
colnames(fija)<-c("numero_de_cliente","clase_final")

varchivos_generacion  <-  unlist(strsplit( karchivos_generacion, split=","))
lapply(varchivos_generacion,agregar_clase_baja)


#limpio la memoria

rm( list=ls() )
gc()



quit( save="no" )


