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



library( "xgboost" )
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
kprograma             <-  "xgboost_apply.r"
kalgoritmo            <-  "xgboost"
kobservaciones        <-  ""


#------------------------------------------------------------------------------

faplicar_xgboost_parametros  = function( pst_parametros, pdataset_generacion, pdataset_aplicacion, pst_mesgeneracion, pst_mesevaluacion )
{

  #atencion con el lenguaje R, la funcion eval y la funcion parse  que pertenecen a El Lado Oscuro del R
  t0       <-  Sys.time()

  eval( parse( text =  paste("modelo  <- xgb.train( 
                                                    data = pdataset_generacion,
                                                    missing = NA,
                                                    objective='binary:logistic', "
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
  gan <-  fmetrica_ganancia_xgboost( 0.025, aplicacion_prediccion,  getinfo(pdataset_aplicacion, "label") ) 

  # calculo el AUC
  auc <-  fmetrica_auc_xgboost( aplicacion_prediccion,  getinfo(pdataset_aplicacion, "label") ) 


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

datasets_xgboost = function( parchivos1, parchivos2 )
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

  #genero el formato requerido por XGBoost
  ddataset1  <-   xgb.DMatrix( data  = data.matrix(dataset_sinclase_sparse[desde1:hasta1,]),
                               label = dataset[ desde1:hasta1 , get(kclase_nomcampo)], 
                               missing=NA 
                            )
							
  #genero el formato requerido por XGBoost
  ddataset2  <-   xgb.DMatrix( data  = data.matrix(dataset_sinclase_sparse[desde2:hasta2,]),
                               label = dataset[ desde2:hasta2 , get(kclase_nomcampo)], 
                               missing=NA 
                            )

  rm( dataset1, dataset2, dataset,  dataset_sinclase, dataset_sinclase_sparse )
  gc()

  return( list( "d1"=ddataset1,  "d2"=ddataset2)  )
}
#------------------------------------------------------
#Esta funcion recibe
#Un string con la lista de archivos para generacion
#Un string con la lista de archivos para evaluacion
#Un VECTOR  donde cada elemento es un string de parametros

aplicar_xgboost  = function( pst_mesgeneracion, pst_mesevaluacion,  plista_parametros )
{

  #cargo los datasets
  ldata <-  datasets_xgboost(  pst_mesgeneracion, pst_mesevaluacion ) 
  dgeneracion <-  ldata$d1
  daplicacion <-  ldata$d2



  lapply( plista_parametros$parametros, 
          faplicar_xgboost_parametros,
          pdataset_generacion=dgeneracion, 
          pdataset_aplicacion=daplicacion, 
          pst_mesgeneracion=pst_mesgeneracion, 
          pst_mesevaluacion=pst_mesevaluacion
        )

}
#------------------------------------------------------------------------------



