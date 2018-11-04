#Objetivo:  mostrar  Repeated Random Sub Sampling Validation 

#Modelo con libreria  rpart
#Estimo la ganancia con  Repeated Random Sub Sampling Validation   ( Monte Carlo Cross Validation )

#source( "M:\\codigoR\\inicial\\MiPrimerModelo_02.r" )

#limpio la memoria
rm( list=ls() )
gc()


library( "rpart" )
library( "data.table" )
library( "rpart.plot" )
library( "caret" )
library( "ROCR" )


#Parametros entrada
karchivo_entrada      <-  "datasets\\201802.txt"
kcampos_separador     <-  "\t"
kcampo_id             <-  "numero_de_cliente"
kclase_nomcampo       <-  "clase_ternaria"
kclase_valor_positivo <-  "BAJA+2"
kcampos_a_borrar      <-  c( kcampo_id )


ktraining_prob        <-  0.70
ksemilla_azar         <-  c( 102191, 200177, 410551, 552581, 892237 )


#constantes de la funcion ganancia del problema
kprob_corte           <-      0.025
kganancia_acierto     <-  11700 
kganancia_noacierto   <-    -300

#------------------------------------------------------
#Esta funcion calcula la ganancia de una prediccion
#Quedarse solo con las predicciones con probabilidad mayor a  kprob_corte
#Si es un acierto  sumar  kganancia_acierto    ( +11700 ) 
#Si NO es acierto  sumar  kganancia_noacierto  (   -300 )

fmetrica_ganancia_rpart  = function( probs, clases )
{
 
  return(  sum(    (probs > kprob_corte  ) * 
                   ifelse( clases== kclase_valor_positivo, kganancia_acierto, kganancia_noacierto )   
              )
         )

}
#------------------------------------------------------
#Esta funcion calcula AUC  Area Under Curve  de la Curva ROC

fmetrica_auc_rpart  = function( probs, clases )
{
  testing_binaria  <-  as.numeric( clases == kclase_valor_positivo  )
  pred             <-  ROCR::prediction(  probs, testing_binaria, label.ordering=c( 0, 1))
  auc_testing      <-  ROCR::performance( pred,"auc"); 
 
  return( unlist(auc_testing@y.values) )

}
#------------------------------------------------------

#corre  rpart  usando  las semillas, y promedia el resultado

modelo_rpart_ganancia = function( dataset  )
{
  gan      <- c()
  auc      <- c() 
  tiempo   <- c()

  cantidad_semillas <- length( ksemilla_azar )
	
  for( s in  1:cantidad_semillas )
  {

    set.seed( ksemilla_azar[s] )
    inTraining        <-  createDataPartition( dataset[ , get(kclase_nomcampo)],   p = ktraining_prob, list = FALSE)
    dataset_training  <-  dataset[  inTraining, ]
    dataset_testing   <-  dataset[ -inTraining, ]



    # generacion del modelo
    formula  <-  formula( paste(kclase_nomcampo, "~ .") )

    t0       <-  Sys.time()
    modelo   <-  rpart( formula,   data = dataset_training,   cp=0.0,  xval=0 )
    t1       <-  Sys.time()

    tiempo[s] <-  as.numeric(  t1 - t0, units = "secs")


    #aplico el modelo a datos nuevos
    testing_prediccion  <- predict(  modelo, dataset_testing , type = "prob")


    # calculo la ganancia normalizada  en testing
    gan[s] <-  fmetrica_ganancia_rpart( testing_prediccion[, kclase_valor_positivo ],  dataset_testing[ , get(kclase_nomcampo)] ) / ( 1- ktraining_prob )

    # calculo el AUC en testing
    auc[s]     <- fmetrica_auc_rpart( testing_prediccion[ ,kclase_valor_positivo],  dataset_testing[ , get(kclase_nomcampo)] )

  }


   return(  list( "ganancia"=mean(gan),  "vganancias"=gan ,  "tiempo"= tiempo,  "AUC"=mean(auc), "vAUCs"=auc )  )

}
#------------------------------------------------------



#cargo los datos
dataset <- fread( karchivo_entrada, header=TRUE, sep=kcampos_separador )


#borro las variables que no me interesan
dataset[ ,  (kcampos_a_borrar) := NULL    ] 


res  <-  modelo_rpart_ganancia( dataset )

print( res )


