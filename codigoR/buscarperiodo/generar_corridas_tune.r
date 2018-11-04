#Genero los scripts de corrida
#para hacer muchas  Optimizaciones Bayesianas con MBO
#para TODOS los algoritmos


#source( "/cloud/cloud1/codigoR/buscarperiodo/generar_corridas_tune.r" )

#limpio la memoria
rm( list=ls() )
gc()

switch ( Sys.info()[['sysname']],
         Windows = { directory.plan     <-  "M:\\plan\\"
                     directory.exec     <-  "M:\\exec\\"
                   },
         Darwin  = { directory.plan     <-  "~/dm/plan/"
                     directory.exec     <-  "~/dm/exec/"
                   },
         Linux   = { directory.plan     <-  "~/cloud/cloud1/plan/"
                     directory.exec     <-  "~/cloud/cloud1/exec/"
                   }
        )


library( "data.table" )

kprefijo              <-  "exp-"
ksufijo_exp           <-  ".r"
ksufijo_sh            <-  ".sh"


#------------------------------------------------------
#Esta funcion genera cada uno de los .r de las corrida Exp

GenerarCorridaExp  = function( mes_test, mes_train, experimento, palgoritmo )
{
 
  varchivo_salida <- paste( kprefijo, palgoritmo, "-", experimento, ksufijo_exp, sep="" )
  vtrain  <-  gsub( " ", "",  mes_train )

  vtest  <-  mes_test
  if(  nchar(mes_test)==0  )  vtest <-  mes_train
  vtest  <-  gsub( " ", "",  vtest )

  vprograma  <-  paste( "~/cloud/cloud1/codigoR/", palgoritmo, "/", palgoritmo, "_tune.r", sep="" )

  setwd(  directory.exec )   
  cat( 
       vprograma,
       experimento,
       vtrain,
       vtest,
       "\n", sep=" ", file=varchivo_salida, fill=FALSE, append=FALSE 
     )

}
#------------------------------------------------------

GenerarCorridaSh  = function( mes_test, mes_train, experimento, palgoritmo )
{
 
  varchivo_salida <- paste( kprefijo, palgoritmo, "-", experimento, ksufijo_sh, sep="" )
  vtrain  <-  gsub( " ", "",  mes_train )

  vtest  <-  mes_test
  if(  nchar(mes_test)==0  )  vtest <-  mes_train
  vtest  <-  gsub( " ", "",  vtest )


  vprograma  <-  paste( "~/cloud/cloud1/codigoR/", palgoritmo, "/", palgoritmo, "_tune.r", sep="" )

  setwd(  directory.exec )     
  cat( "Rscript --vanilla",
       vprograma,
       experimento,
       vtrain,
       vtest,
       "\n", sep=" ", file=varchivo_salida, fill=FALSE, append=FALSE 
     )


  varchivo_run <- paste( "run-", palgoritmo, ".sh" , sep="" )

  cat( "./",
       varchivo_salida,
       "\n", sep="",  file=varchivo_run, fill=FALSE, append=TRUE
     )
}
#------------------------------------------------------

GenerarCorridasTune = function( palgoritmo, pexperimento_inicial, pplan )
{

  #cargo el plan de la corrida
  #para una tabla tan pequeña usar  read.table::fread es matar un mosquito con un cañon
  setwd(directory.plan)
  dataset <- fread( pplan, header=TRUE, sep="\t" )



  #ordeno el plan de corrida
  dataset <-  dataset[ order( -rank( nchar(mes_test)), -rank(mes_test), -rank(nchar(mes_train)), mes_train ) ]

  #agrego una nueva columna a  dataset
  #asignando el numero de experimento en forma secuencial, comensando de  kexperimento_inicial
  dataset[, "experimento" := seq.int(nrow(dataset)) + pexperimento_inicial ] 


  #genero los pseudo scripts exp
  mapply(  GenerarCorridaExp,  dataset$mes_test,  dataset$mes_train,  dataset$experimento, MoreArgs=list( palgoritmo=palgoritmo)  )


  #genero los scripts  sh
  mapply(  GenerarCorridaSh,  dataset$mes_test,  dataset$mes_train,  dataset$experimento, MoreArgs=list( palgoritmo=palgoritmo) )

}
#------------------------------------------------------



GenerarCorridasTune( "rpart",     4100, "generate_plan.txt" )
GenerarCorridasTune( "ranger",    5100, "generate_plan_ranger.txt" )
GenerarCorridasTune( "xgboost",   6100, "generate_plan.txt" )
GenerarCorridasTune( "lightgbm",  7100, "generate_plan.txt" )



#limpio la memoria
rm( list=ls() )
gc()


#salgo del R sin grabar el gigante entorno
quit( save="no" )

