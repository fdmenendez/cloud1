#Este programa genera la salida final
#La salida queda en  /work/lineademuerte_entregar_UBA.txt  , lineademuerte_probabilidades_UBA.txt  y lineademuerte_importancia_UBA.txt
#Este programa necesita para correr 8vCPU y 80GB de RAM
#para correr se merece una digna maquina Inmortal
#Correrlo desde RStudio



#No se crea ninguna variable en el mismo mes
#No se imputan nulos, no se quitan outliers
#No se crean variables que sean predicciones de modelos
#No se ajusta por inflacion
#No se traen variables externas
#No se hace stacking de modelos al final



#Lo unico importante de este programa es
#se usa el algoritmo  LightGBM
#se usan los archivos    _hist.txt  (son los que estan disponibles en el Dropbox)
#Entrenar en los ultimos OCHO meses que tienen clase  201709 a 201804
#clase_binaria2  positivos={BAJA+2, BAJA+1} para entregar,  prob_corte_binaria2 = 0.0365
#LightGBM  con num_iterations=350, init_score=0.005, bagging_fraction=1, feature_fraction=0.14, 
#   learning_rate=0.025, min_data_in_leaf=85, num_leaves=75, lambda_l1=0.14, min_gain_to_split=0.96, max_bin=31


#Se deja EXPRESA CONSTANCIA que no se crea ninguna variable que dependa del negocio, 
#los archivos _hist.txt solo tienen el maximo, minimo y tendencia de los ultimos 6 meses


switch ( Sys.info()[['sysname']],
         Windows = { directory.work     <-  "M:\\work\\"
                     directory.datasets <-  "M:\\datasets\\hist\\"
                   },
         Darwin  = { directory.work     <-  "~/dm/work/"
                     directory.datasets <-  "~/dm/datasets/hist/"
                   },
         Linux   = { directory.work     <-  "~/cloud/cloud1/work/"
                     directory.datasets <-  "~/cloud/cloud1/datasets/hist/"
                   }
        )



library( "lightgbm" )
library( "Matrix" ) 

library( "data.table" )



kcampos_separador     <-  "\t"
kcampo_id             <-  "numero_de_cliente"
kclase_nomcampo       <-  "clase_ternaria"
kclase_valor_positivo <-  c( "BAJA+2", "BAJA+1" )
kcampos_a_borrar      <-  c( kcampo_id )



karchivo_salida      <-  "lineademuerte_entregar_UBA.txt"
karchivo_probs       <-  "lineademuerte_probabilidades_UBA.txt"
karchivo_importancia <-  "lineademuerte_importancia_UBA.txt"


karchivos_generacion  <-  "201804_hist.txt,201803_hist.txt,201802_hist.txt,201801_hist.txt,201712_hist.txt,201711_hist.txt,201710_hist.txt,201709_hist.txt"
karchivos_aplicacion  <-  "201806_hist.txt"  

#aplico el modelo a junio, que NO tiene clase
#la decision de entrenar en OCHO meses se obtuvo de los testeos hechos sobre junio del 2017

#esta es la probabilidad de corte de {BAJA+2, BAJA+1}
kprob_corte_binaria2  <- 0.0365

#------------------------------------------------------------------------------
#debe tenerse en cuenta que para 201806 la clase esta VACIA
#por lo que NO se puede calcular la ganancia
#esta vez, realmente estamos prediciendo el futuro
#es un salto al vacio

#cargo los datasets

setwd(  directory.datasets )

varchivos_generacion  <-  unlist(strsplit( karchivos_generacion, split=","))
dataset_generacion    <-  do.call(rbind, lapply( varchivos_generacion, function(x) fread(x, header=TRUE, sep=kcampos_separador)))

#cre un vector con la clase en {0,1}  clase  binaria1
generacion_clase  <-  dataset_generacion[, get(kclase_nomcampo)] 
generacion_clase  <-  as.numeric(  generacion_clase %in% kclase_valor_positivo  )


#ahora cargo el dataset de aplicacion
varchivos_aplicacion  <-  unlist(strsplit( karchivos_aplicacion, split=","))
dataset_aplicacion    <-  do.call(rbind, lapply( varchivos_aplicacion, function(x) fread(x, header=TRUE, sep=kcampos_separador)))

#obtengo el campo numero_de_cliente
aplicacion_numerodecliente  <- dataset_aplicacion[, get(kcampo_id)] 

#ahora hago la union los datasets, ya que para hacer el One Hot Encoding lo necesito
dataset_unido  <-  rbind(  dataset_generacion, dataset_aplicacion )

#le borro la clase a dataset_unido 
dataset_unido[ ,  (kclase_nomcampo) := NULL    ] 


#borro las variables que no me interesan
dataset_unido[ ,  (kcampos_a_borrar) := NULL    ] 



#genero one-hot enconding
options(na.action='na.pass')
formula  <- formula(paste("~ .-1"))
dataset_unido_sparse  <- sparse.model.matrix( formula, data = dataset_unido )



desde1 <- 1
hasta1 <- nrow( dataset_generacion )
desde2 <- hasta1 + 1 
hasta2 <- hasta1 + nrow( dataset_aplicacion )

#genero el formato requerido por XGBoost
dgeneracion  <-   lgb.Dataset( data  = data.matrix(dataset_unido_sparse[desde1:hasta1,]),
                               label = generacion_clase, 
                               free_raw_data=FALSE
                             )
				
#genero el formato requerido por lightgbm
daplicacion  <-   as.matrix( dataset_unido_sparse[desde2:hasta2, ] )	


#como soy prolijo, borro lo que ya no voy a usar mas
rm( dataset_generacion, dataset_aplicacion, dataset_unido_sparse, dataset_unido, generacion_clase )
gc()


#---------------------

#Finalmente, llamo al LightGBM
#Uso parametros redondeados  que salieron de una Optimizacion Bayesiana


#LightGBM  con num_iterations=350, init_score=0.005, bagging_fraction=1, feature_fraction=0.14, 
#   learning_rate=0.025, min_data_in_leaf=85, num_leaves=75, lambda_l1=0.14, min_gain_to_split=0.96, max_bin=31


#mi querida random seed
set.seed( 102191  )


modelo = lgb.train( 
                   data = dgeneracion,
                   objective = "binary",
                   num_iterations=350, 
                   init_score= 0.05,
                   bagging_fraction=1, 
                   feature_fraction=0.14, 
                   learning_rate=0.025, 
                   min_gain_to_split=0.96, 
                   lambda_l1=0.14,
                   max_bin=31, 
                   num_leaves=75
                 )


#aplico el modelo a datos nuevos
aplicacion_prediccion  <- predict(  modelo, daplicacion )

#uno las columnas de numero_de_cliente y la probabilidad recien calculada
prediccion_final  <-  cbind(  aplicacion_numerodecliente, aplicacion_prediccion )

#le doy nombre a las columnas
colnames( prediccion_final )  <-  c( kcampo_id, "prob_positivo" )

#lo paso a formato  data.table  que es donde estoy mas comodo 
prediccion_final <- as.data.table( prediccion_final )

#grabo todas las probabilidad, simplemente para tenerlo
setwd(  directory.work )
fwrite( prediccion_final[ order(-prob_positivo) ], 
        file=karchivo_probs, row.names=FALSE, col.names=TRUE, quote=FALSE, sep="\t", eol = "\r\n")



#Ahora grabo la salida que debo entregar en la materia, que son solamente los ids
#me quedo solamente con los numero_de_cliente donde probabilidad > kprob_corte_binaria2   
setwd(  directory.work )
fwrite( as.data.table( prediccion_final[ prob_positivo > kprob_corte_binaria2, get(kcampo_id) ] ), 
        file=karchivo_salida, row.names=FALSE, col.names=FALSE, quote=FALSE, sep="\t", eol = "\r\n")



#por el mismo precio, grabo la importancia de las variables
write.table(  lgb.importance( model = modelo )
              , file= karchivo_importancia
              , row.names=FALSE
              , col.names=TRUE
              , quote=FALSE
              , sep="\t"
              , eol = "\r\n"
           )





