rm( list=ls() )
gc()

script.name<-"general-fetuares1"

switch ( Sys.info()[['sysname']],
         Windows = { directory.include  <-  "M:\\codigoR\\include\\"
         directory.work     <-  paste0("M:\\work\\",script.name,"\\")
         directory.plan     <-  "M:\\plan\\"
         directory.datasets <-  "M:\\datasets\\dias\\"
         directory.root     <-  "M:\\"
         },
         Darwin  = { directory.include  <-  "~/dm/codigoR/include/"
         directory.work     <-  "~/dm/work/"
         directory.plan     <-  "~/dm/plan/"
         directory.datasets <-  "~/dm/datasets/"
         },
         Linux   = { script.name<-Sys.info()["nodename"]
         directory.include  <-  "~/cloud/cloud1/codigoR/include/"
         directory.work     <-  paste0("~/cloud/cloud1/work/",script.name,"/")
         directory.plan     <-  "~/cloud/cloud1/plan/"
         directory.datasets <-  "~/cloud/cloud1/datasets/dias/"
         }
)

source(paste0(directory.include,"utils.r"))

include.packages('data.table')
include.packages("dplyr")
include.packages("skimr")
include.packages("mlr")


#Parametros entrada de nuestro dataset
karchivo_entrada      <-  "201802_dias.txt"
kcampos_separador     <-  "\t"
kcampo_id             <-  "numero_de_cliente"
kclase_nomcampo       <-  "clase_binaria"
kclase_valor_positivo <-  1
kcampos_a_borrar      <-  c( kcampo_id )

clean.up<- function(df) {
  df<- df %>%
    mutate(
      
      
    )
}

df<- fread(paste0(directory.datasets,"201802_dias.txt"), header=TRUE, sep="\t")
head(df)


desc <- skim(df) %>% as.data.frame()

desc_df <- desc %>% group_by(variable, type) %>% select(variable, type,stat,value) %>% spread(key=stat, value=value)

desc_df

summ<-summarizeColumns(df)

#CALCULAR EL MONTO TOTAL DE PRESTAMOS Y PASAR LOS TIPOS DE PRESTAMOS A DUMMY


