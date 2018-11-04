#Aplica los modelos  que faltan apliar
#de hyperparameter_GLOBAL.txt  salen los mejores < algoritmo, parametros >
#de apply_plan.txt  salen  < mes_generacion, mes_aplicacion >
#en apply_GLOBAL.txt  ya esta lo que se aplico
#finalmente, se aplica a todo lo que aun no fue aplicado

#source( "/cloud/cloud1/codigoR/buscarperiodo/apply_masivo.r" )

#limpio la memoria
rm( list=ls() )
gc()


switch ( Sys.info()[['sysname']],
         Windows = { directory.include  <-  "M:\\codigoR\\include\\"
                     directory.work     <-  "M:\\work\\"
                     directory.plan     <-  "M:\\plan\\"
                     directory.exec     <-  "M:\\exec\\"
                     directory.datasets <-  "M:\\datasets\\dias\\"
                   },
         Darwin  = { directory.include  <-  "~/dm/codigoR/include/"
                     directory.work     <-  "~/dm/work/"
                     directory.plan     <-  "~/dm/plan/"
                     directory.exec     <-  "~/dm/exec/"
                     directory.datasets <-  "~/dm/datasets/dias/"
                   },
         Linux   = { directory.include  <-  "~/cloud/cloud1/codigoR/include/"
                     directory.work     <-  "~/cloud/cloud1/work/"
                     directory.plan     <-  "~/cloud/cloud1/plan/"
                     directory.exec     <-  "~/cloud/cloud1/exec/"
                     directory.datasets <-  "~/cloud/cloud1/datasets/dias/"
                   }
        )



library( "data.table" )



kapply_plan               <-  "apply_plan.txt"
khyperparameter_GOBAL     <-  "hyperparameter_GLOBAL.txt"
kapply_GLOBAL             <-  "apply_GLOBAL.txt"


kmin_MBO_aceptable    <-   100

#------------------------------------------------------

#todo esto en SQL seria taaan elegante ...

setwd( directory.work )
if( !file.exists( kapply_GLOBAL ) )
{
  cat("metrica",
      "metrica2",
      "parametros",
      "fecha", 
      "clase", "programa", "algoritmo", 
      "dataset_generacion", "dataset_aplicacion", "observaciones",
      "\n", sep="\t", file=kapply_GLOBAL, fill=FALSE, append=FALSE )

}


setwd( directory.work )
dataset_salidas  <- fread( khyperparameter_GOBAL, header=TRUE, sep="\t" )
nrow( dataset_salidas )

cantidades  <-  dataset_salidas[, .(count = .N ), by = experimento]

mejores <-  dataset_salidas[, .SD[which.max(metrica),], by = experimento]
mejores[cantidades, on = 'experimento', cantidad := i.count]

#me quedo solamente con las corridas de MBO con mas de kmin_MBO_aceptable iteraciones
mejores  <- mejores[ cantidad > kmin_MBO_aceptable ]


tabla_parametros_mejores  <-  unique( mejores[ , c( "algoritmo", "parametros")]  )


#cargo el plan
setwd( directory.plan )
dataset_plan  <- fread( kapply_plan, header=TRUE, sep="\t" )

#creo el producto cartesiano  de   tabla_parametros_mejores, dataset_plan
tabla_parametros_mejores[ , k:=1 ]
dataset_plan[ , k:=1 ]

posible_procesar  <-  merge( tabla_parametros_mejores, dataset_plan, by = "k" , allow.cartesian=TRUE)
posible_procesar[ , k:=NULL ]


#cambio los nombres de columnas
setnames( posible_procesar, "mes_generacion", "dataset_generacion" )
setnames( posible_procesar, "mes_evaluacion", "dataset_aplicacion" )

#dejo sin espacios 
posible_procesar[ , parametros:= gsub(" ", "", parametros) ]
posible_procesar[ , dataset_generacion:= gsub(" ", "", dataset_generacion) ]
posible_procesar[ , dataset_aplicacion:= gsub(" ", "", dataset_aplicacion) ]



#ahora cargo lo que ya se proceso
setwd( directory.work )
dataset_apply  <- fread( kapply_GLOBAL, header=TRUE, sep="\t" )
dataset_apply  <- dataset_apply[ , c("algoritmo", "parametros", "dataset_aplicacion", "dataset_generacion" ) ]


#finalmente hago el MINUS de las dos tablas
if(  nrow( dataset_apply ) > 0 )
{
  real_procesar  <-  fsetdiff( unique(posible_procesar),  unique(dataset_apply) )
} else {
  real_procesar  <- unique(posible_procesar)
}


nrow( posible_procesar )
nrow( dataset_apply )
nrow( real_procesar )


real_procesar2   <-  real_procesar[ , list(list(.SD)), by=list(algoritmo, dataset_generacion, dataset_aplicacion) ]
setnames( real_procesar2,  "V1", "lparametros" )
nrow( real_procesar2 )




#finalmente, luego de tanto mover datos en R, llamo a la aplicacion de los modelos


#Aplico el modelo de rpart
lista <- real_procesar2[ algoritmo=="rpart", ]
if( nrow( lista )>0 )
{
  setwd( directory.include )
  source( "../rpart/rpart_apply.r" )

  mapply( aplicar_rpart,  
          lista$dataset_generacion, 
          lista$dataset_aplicacion, 
          lista$lparametros
        )
}


#Aplico el modelo de ranger
lista <- real_procesar2[ algoritmo=="ranger", ]
if( nrow( lista )>0 )
{
  setwd( directory.include )
  source( "../ranger/ranger_apply.r" )

  mapply( aplicar_ranger,  
          lista$dataset_generacion, 
          lista$dataset_aplicacion, 
          lista$lparametros
        )
}


lista <- real_procesar2[ algoritmo=="xgboost", ]
if( nrow( lista )>0 )
{
  setwd( directory.include )
  source( "../xgboost/xgboost_apply.r" )

  mapply( aplicar_xgboost,  
          lista$dataset_generacion, 
          lista$dataset_aplicacion, 
          lista$lparametros
        )
}


lista <- real_procesar2[ algoritmo=="lightgbm", ]
if( nrow( lista )>0 )
{
  setwd( directory.include )
  source( "./lightgbm/lightgbm_apply.r" )

  mapply( aplicar_lightgbm,  
          lista$dataset_generacion, 
          lista$dataset_aplicacion, 
          lista$lparametros
        )
}



#limpio la memoria
rm( list=ls() )
gc()


#salgo del R sin grabar el gigante entorno
quit( save="no" )

