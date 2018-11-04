
# fmetricas <- function(probs, clases, cutoff=0.025, proporcion=1, label="", type="", semilla=NA) {
#   
#   # AUC
#   binaria  <-  as.numeric(clases == "BAJA+2")
#   roc_pred <-  ROCR::prediction(probs, binaria, label.ordering=c( 0, 1))
#   auc_t <-  ROCR::performance( roc_pred,"auc"); 
#   auc <- unlist(auc_t@y.values)
#   
#   # Ganancia
#   ganancia <- sum((probs > cutoff  ) * ifelse( clases== "BAJA+2", 11700, -300 )) 
#   
#   # Ganancia normalizada, proyectamos la ganancia según el porcentaje de la muestra.
#   ganancia_normalizada <- ganancia / proporcion
#   
#   return(data.frame(label, semilla, type, ganancia, ganancia_normalizada, auc))
# }




