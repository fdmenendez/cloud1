rm( list=ls() )
gc()

script.name<-"general-dias-describe"

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
         Linux   = { directory.include  <-  "~/cloud/cloud1/codigoR/include/"
         directory.work     <-  paste0("~/cloud/cloud1/work/",script.name,"/")
         directory.plan     <-  "~/cloud/cloud1/plan/"
         directory.datasets <-  "~/cloud/cloud1/datasets/"
         }
)

source(paste0(directory.include,"utils.r"))

library(data.table)

if (!require(tidyr)) install.packages('tidyr')
library(tidyr)

if (!require(dplyr)) install.packages('dplyr')
library(dplyr)

if (!require(parallel)) install.packages('parallel')

# if (!require(pbapply)) install.packages('pbapply')
# library(pbapply)

include.packages('Hmisc')
library("Hmisc")



#Parametros entrada
kcampos_separador     <-  "\t"
kcampo_id             <-  "numero_de_cliente"
kclase_nomcampo       <-  "clase_ternaria"
kclase_valor_positivo <-  "BAJA+2"
kcampos_a_borrar      <-  c( kcampo_id )
kprefijo              <- "desc_"


getDescribe<-function(x) {
  #  x<-"201601_dias_ext.txt"
  if (!file.exists(paste0(directory.work,kprefijo,x))) {
    dataset <- fread(paste0(directory.datasets,'dias/',x),header=TRUE, sep=kcampos_separador) # load file
    dataset[ ,  (kcampos_a_borrar) := NULL    ] 
    dataset<- dataset %>% mutate(clase_binaria = as.factor(ifelse(clase_ternaria == 'CONTINUA',0,1)))
    
    d<-describe(dataset)
    capture.output(d, file = paste0(directory.work,kprefijo,x))
  }
  else paste0(directory.work,kprefijo,x," exists!")
    
}

files <- dir(path=paste0(directory.datasets,"dias"), pattern="*.txt", full.names=F, recursive=FALSE)
#lapply(files, getDescribe)
mclapply(files, getDescribe, mc.cores = detectCores()-2, mc.preschedule =T)
#pblapply(files, getDescribe, cl = detectCores()-2)


#limpio la memoria
rm( list=ls() )
gc()


#salgo del R sin grabar el gigante entorno
quit( save="no" ,status = 0)









