
#WARNING, WOMEN AT WORK  =  todavia no funciona  (en lenguaje inclusivo)
#La salida queda en  /work/apply_GLOBAL.txt


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



library( "lightgbm" )
library( "Matrix" ) 

library( "data.table" )


setwd( directory.include )
source( "metrica.r" )


kcampos_separador     <-  "\t"
kcampo_id             <-  "numero_de_cliente"
kclase_nomcampo       <-  "clase_ternaria"
kclase_valor_positivo <-  "BAJA+2"
kcampos_a_borrar      <-  c( kcampo_id )



karchivo_salida      <-  "apply_GLOBAL.txt"

#estos valores se graban en el archivo de salida
kclase                <-  "binaria1"
kprograma             <-  "lightgbm_apply.r"
kalgoritmo            <-  "lightgbm"
kobservaciones        <-  ""


#------------------------------------------------------------------------------

faplicar_lightgbm_parametros  = function( pst_parametros, pdataset_generacion, pdataset_aplicacion, pdataset_aplicacion_clase, pst_mesgeneracion, pst_mesevaluacion )
{

  #atencion con el lenguaje R, la funcion eval y la funcion parse  que pertenecen a El Lado Oscuro del R
  t0       <-  Sys.time()

  eval( parse( text =  paste("modelo  <- lgb.train( 
                                                    data = pdataset_generacion,
                                                    objective='binary', "
                                                   , pst_parametros
                                                   ,")"
                                                   ,sep=""
                            )                    
                      
             )
      )
  

  t1       <-  Sys.time()
  tiempo   <-  as.numeric(  t1 - t0, units = "secs")




  #aplico el modelo a datos nuevos
  aplicacion_prediccion  <- predict(  modelo, pdataset_aplicacion )


  # calculo la ganancia
  gan <-  fmetrica_ganancia_lightgbm( 0.025, aplicacion_prediccion,  pdataset_aplicacion_clase ) 

  # calculo el AUC
  auc <-  fmetrica_auc_lightgbm( aplicacion_prediccion,  pdataset_aplicacion_clase ) 


  #escribo el resultado de esta aplicacion
  setwd(  directory.work )
  cat(  
       gan, 
       auc,
       pst_parametros,
       format(Sys.time(), "%Y%m%d %H%M%S"), 
       kclase, 
       kprograma, 
       kalgoritmo, 
       pst_mesgeneracion, pst_mesevaluacion,
       kobservaciones,
       "\n", sep="\t", file=karchivo_salida, fill=FALSE, append=TRUE 
    )


}
#------------------------------------------------------

datasets_lightgbm = function( parchivos1, parchivos2 )
{
  #cargo 1
  varchivos1  <-  unlist(strsplit( parchivos1, split=","))
  setwd(  directory.datasets )
  dataset1 <- do.call(rbind, lapply( varchivos1, function(x) fread(x, header=TRUE, sep=kcampos_separador)))

  #cargo 2
  varchivos2  <-  unlist(strsplit( parchivos2, split=","))
  setwd(  directory.datasets )
  dataset2 <- do.call(rbind, lapply( varchivos2, function(x) fread(x, header=TRUE, sep=kcampos_separador)))
  
  dataset <-  rbind(  dataset1, dataset2 )
    
  #borro las variables que no me interesan
  dataset[ ,  (kcampos_a_borrar) := NULL    ] 

  #dejo la clase en {0,1}  clase  binaria1
  dataset[, (kclase_nomcampo) := as.numeric( get(kclase_nomcampo) == kclase_valor_positivo  )] 

  dataset_sinclase   <- dataset[ , ! ( kclase_nomcampo), with=FALSE   ]


  #genero one-hot enconding
  options(na.action='na.pass')
  formula  <- formula(paste("~ .-1"))
  dataset_sinclase_sparse  <- sparse.model.matrix( formula, data = dataset_sinclase )

  desde1 <- 1
  hasta1 <- nrow( dataset1 )
  desde2 <- hasta1 + 1 
  hasta2 <- hasta1 + nrow( dataset2 )

  #genero el formato requerido por lightgbm
  ddataset1  <-   lgb.Dataset( data  = data.matrix(dataset_sinclase_sparse[desde1:hasta1,]),
                               label = dataset[ desde1:hasta1 , get(kclase_nomcampo)], 
                               free_raw_data=FALSE
                            )
							
  #genero el formato requerido por lightgbm
  ddataset2       <-   as.matrix( dataset_sinclase_sparse[desde2:hasta2, ] ) 
  ddataset2_clase <- dataset[desde2:hasta2,get(kclase_nomcampo)]

  rm( dataset1, dataset2, dataset,  dataset_sinclase, dataset_sinclase_sparse )
  gc()

  return( list( "d1"=ddataset1,  "d2"=ddataset2, "d2clase"=ddataset2_clase )  )
}
#------------------------------------------------------
#Esta funcion recibe
#Un string con la lista de archivos para generacion
#Un string con la lista de archivos para evaluacion
#Un VECTOR  donde cada elemento es un string de parametros

aplicar_lightgbm  = function( pst_mesgeneracion, pst_mesevaluacion,  plista_parametros )
{

  #cargo los datasets
  ldata <-  datasets_lightgbm(  pst_mesgeneracion, pst_mesevaluacion ) 
  dgeneracion       <-  ldata$d1
  daplicacion       <-  ldata$d2
  daplicacion_clase <-  ldata$d2clase



  lapply( plista_parametros$parametros, 
          faplicar_lightgbm_parametros,
          pdataset_generacion=dgeneracion, 
          pdataset_aplicacion=daplicacion, 
          pdataset_aplicacion_clase=daplicacion_clase, 
          pst_mesgeneracion=pst_mesgeneracion, 
          pst_mesevaluacion=pst_mesevaluacion
        )

}
#------------------------------------------------------------------------------
