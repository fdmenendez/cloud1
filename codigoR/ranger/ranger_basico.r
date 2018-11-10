
#source( "/cloud/cloud1/codigoR/ranger/ranger_basico.r" )

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

karchivo_generacion   <-   "201801_dias.txt,201712_dias.txt,201711_dias.txt"
karchivo_aplicacion   <-   "201804_dias.txt"


#------------------------------------------------------------------------------

setwd(  directory.datasets )
varchivos_generacion  <-  unlist(strsplit( karchivo_generacion, split=","))
dataset_generacion    <-  do.call(rbind, lapply( varchivos_generacion, function(x) fread(x, header=TRUE, stringsAsFactors=TRUE, sep=kcampos_separador)))


#borro las variables que no me interesan
dataset_generacion[ ,  (kcampos_a_borrar) := NULL    ] 

#imputo los nulos, ya que ranger no acepta nulos
dataset_generacion <-  na.roughfix( dataset_generacion )



#-------------------------
#genero el modelo
formula  <-  formula( paste(kclase_nomcampo, "~ .") )

t0       <-  Sys.time()

modelo  <- ranger( data = dataset_generacion,  
                   formula,  
                   probability=TRUE,
                   num.trees= 900, 
                   min.node.size= 360, 
                   mtry= 4, 
                   splitrule='gini'
                 )       

t1       <-  Sys.time()

tiempo <-  as.numeric(  t1 - t0, units = "secs")


#-------------------------
#aplico el modelo

setwd(  directory.datasets )
dataset_aplicacion <-  fread( karchivo_aplicacion, header=TRUE, stringsAsFactors=TRUE, sep=kcampos_separador) 

#borro las variables que no me interesan
dataset_aplicacion[ ,  (kcampos_a_borrar) := NULL    ] 

#imputo los nulos, ya que ranger no acepta nulos
dataset_aplicacion <-  na.roughfix( dataset_aplicacion )


#aplico el modelo a datos nuevos
aplicacion_prediccion  <- predict(  modelo, dataset_aplicacion )



# calculo la ganancia
# notar que el resultado queda en   aplicacion_prediccion$predictions 

gan <-  fmetrica_ganancia_rpart( aplicacion_prediccion$predictions[ ,kclase_valor_positivo ],  dataset_aplicacion[ , get(kclase_nomcampo)] ) 

# calculo el AUC
auc <-  fmetrica_auc_rpart( aplicacion_prediccion$predictions[ ,kclase_valor_positivo ],  dataset_aplicacion[ , get(kclase_nomcampo)] ) 


cat( "ganancia = ",  gan , "\n")
cat( "AUC = ",  auc, "\n"  )
cat( "tiempo = ",  tiempo, "\n"  )




