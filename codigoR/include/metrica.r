library( "ROCR" )


#constantes de la funcion ganancia del problema
kprob_corte           <-      0.025
kganancia_acierto     <-  11700 
kganancia_noacierto   <-   -300

#variable - se inicializa para que no falle
vprob_corte           <-      0.025 
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

#Esta funcion calcula la ganancia de una prediccion
#Quedarse solo con las predicciones con probabilidad mayor a  kprob_corte
#Si es un acierto  sumar  kganancia_acierto    ( +11700 ) 
#Si NO es acierto  sumar  kganancia_noacierto  (   -300 )

fmetrica_ganancia_xgboost  = function( pprob_corte, probs, clases )
{
 
  res <-  sum(    (probs > pprob_corte  ) * 
                   ifelse( clases== 1, kganancia_acierto, kganancia_noacierto ) 
                   , na.rm = TRUE  
              )

  return(  ifelse(  is.na(res) , 0, res )  
         )

}
#------------------------------------------------------
#Esta funcion calcula AUC  Area Under Curve  de la Curva ROC

fmetrica_auc_xgboost  = function( probs, clases )
{
  pred             <-  ROCR::prediction(  probs, clases, label.ordering=c( 0, 1))
  auc_testing      <-  ROCR::performance( pred,"auc"); 
 
  return( unlist(auc_testing@y.values) )

}
#------------------------------------------------------

fganancia_logistic_xgboost   <- function(probs, clases) 
{
  
  vlabels <- xgboost::getinfo(clases, "label")
  
  gan <-sum(   (probs > kprob_corte  ) * 
                 ifelse( vlabels== 1, kganancia_acierto, kganancia_noacierto )   
  )
  
  
  return(  list(metric = "ganancia", value =  ifelse(  is.na(gan) , 0, round(gan,0) ) )  )
}

fganancia_logistic_xgboost_hyp   <- function(probs, clases) 
{
  
  vlabels <- xgboost::getinfo(clases, "label")
  
  gan <-sum(   (probs > vprob_corte  ) * 
                 ifelse( vlabels== 1, kganancia_acierto, kganancia_noacierto )   
  )
  
  
  return(  list(metric = "ganancia", value =  ifelse(  is.na(gan) , 0, round(gan,0) ) )  )
}
#------------------------------------------------------
#Esta funcion calcula la ganancia de una prediccion
#Quedarse solo con las predicciones con probabilidad mayor a  kprob_corte
#Si es un acierto  sumar  kganancia_acierto    ( +11700 ) 
#Si NO es acierto  sumar  kganancia_noacierto  (   -300 )

fmetrica_ganancia_lightgbm  = function( pprob_corte, probs, clases )
{
 
  res <-  sum(    (probs > pprob_corte  ) * 
                   ifelse( clases== 1, kganancia_acierto, kganancia_noacierto ) 
                   , na.rm = TRUE  
              )

  return(  ifelse(  is.na(res) , 0, res )  
         )

}
#------------------------------------------------------
#Esta funcion calcula AUC  Area Under Curve  de la Curva ROC

fmetrica_auc_lightgbm  = function( probs, clases )
{
  pred             <-  ROCR::prediction(  probs, clases, label.ordering=c( 0, 1))
  auc_testing      <-  ROCR::performance( pred,"auc"); 
 
  return( unlist(auc_testing@y.values) )

}
#------------------------------------------------------

fganancia_logistic_lightgbm   <- function(probs, clases) 
{

   vlabels <- getinfo(clases, "label")
  
   gan <-sum(   (probs > kprob_corte  ) * 
                 ifelse( vlabels== 1, kganancia_acierto, kganancia_noacierto ),
                na.rm = TRUE   
            )
        

   return(  list( name = "ganancia", 
                  value =  ifelse(  is.na(gan) , 0, gan ) ,
                  higher_better= TRUE 
                )
         )
}
#------------------------------------------------------