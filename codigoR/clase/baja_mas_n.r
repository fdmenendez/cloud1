
library( "data.table" )
library( "dplyr" )
library( "ggplot2" )
library( "xgboost" )
library( "ROCR" )
library( "tidyr" )


ds  <-  fread("unzip -c /home/sas/UBA2018/datasets/paquete_premium.zip", header=TRUE, sep="\t")

sub_ds <- ds %>% select(numero_de_cliente,foto_mes,clase_ternaria)

rm(ds)
gc()

sub_ds <- sub_ds %>% arrange(numero_de_cliente,foto_mes)

# Recibe el vector con clase_ternaria para cada cliente.
my_target <- function (x) {
  baja1 <- which(x == "BAJA+1") 
  if (length(baja1) == 0) {
    out <- rep(0, length(x))
  } else {
    
    # Un cliente puede tener más de un baja+1 en toda la historia, pe: 7626542
    diff(c(0,baja1))
    out <- unlist(lapply(diff(c(0,baja1)), function (x) { seq(x,1)}))
    
    # Puede pasar que el cliente vuelva y no se vaya 
    if (length(out) != length(x)) {
      out <- c(out, rep(0,length(x)-length(out))) 
    }
  }
  out
}

nuevo_target <- sub_ds %>% 
  group_by(numero_de_cliente) %>% 
  mutate(tr = my_target(clase_ternaria)) %>%
  mutate(tr = ifelse(clase_ternaria == "",NA,tr))

# Estudiamos los targets en el pasado:

ds201706  <-  fread("/home/sas/UBA2018/datasets/dias/201706_dias.txt", header=TRUE, sep="\t")
ds201706_w_tr <- ds201706 %>% inner_join(nuevo_target) 

# Inspeccionamos los nuevos targets

ds201706_w_tr %>% group_by(tr) %>% summarise(n = n())

# Examinada en el EDA
ggplot(ds201706_w_tr, aes(x=mcomisiones_mantenimiento)) +
  facet_grid(tr ~ .) +
  geom_density()

# Variables importantes del XGBoost
ggplot(ds201706_w_tr, aes(x=mcaja_ahorro_Paquete)) +
  facet_grid(tr ~ .) +
  geom_density()

ggplot(ds201706_w_tr, aes(x=tmovimientos_ultimos90dias)) +
  facet_grid(tr ~ .) +
  geom_density()


ggplot(ds201706_w_tr, aes(x=mpasivos_margen)) +
  facet_grid(tr ~ .) +
  geom_density()

# Armamos modelos para diferentes tipos de targets y medimos en agosto:

# Limpiamos la memoria salvo los nuevos target
rm(list=ls()[ls() != "nuevo_target"])
gc()

# Lista de meses que vamos a trabajar:
directorio <- "/home/sas/UBA2018/datasets/dias/"
meses <- c("201703","201704","201705","201706","201707","201708")
dataset <- do.call(rbind, lapply( meses, function(x) fread(paste0(directorio,x,"_dias.txt"), header=TRUE, sep="\t")))

# agregamos el nuevo target:
dataset <- dataset %>% inner_join(nuevo_target) 

# Métrica para comparar resultados
response <- function (prob, clase, n=10000) {
  juntos <- data.frame(prob, clase) %>% arrange(-prob) %>% head(n)
  sum(juntos$clase)/n
}

# Funcion de entrenamiento

modelar <- function(train, train_clase, test, clase_test, param="nround=20", s = 12345) {
  set.seed(s)
  
  train$clase_ternaria <- NULL
  train$tr <- NULL
  train$foto_mes <- NULL
  train$numero_de_cliente <- NULL
  
  test$clase_ternaria <- NULL
  test$tr <- NULL
  test$foto_mes <- NULL
  test$numero_de_cliente <- NULL
  
  dtrain <- xgb.DMatrix(data = data.matrix(train),label = train_clase, missing=NA )
  
  eval(parse(
    text =  paste(
      "modelo  <- xgb.train(
      data = dtrain,
      missing = NA,
      objective='binary:logistic', "
      ,
      param
      ,
      ")"
      ,
      sep = ""
    )
  ))
  
  pred <- predict(modelo, data.matrix(test),  type = "prob")
  pred_test <- prediction(pred,  test_clase )
  auc_test <- ROCR::performance( pred_test, "auc")
  data.frame(auc=unlist(auc_test@y.values), response= response(pred,test_clase))
}

# El de toda la vida:

train <- dataset[dataset$foto_mes == 201706, ]
train_clase <- ifelse(train$tr == 2, 1, 0)
test <- dataset[dataset$foto_mes == 201708, ]
test_clase <- ifelse(test$clase_ternaria == "BAJA+2",1,0)
print(modelar(train, train_clase, test, test_clase))
gc()


# Las clases juntas:
train_clase <- ifelse(train$tr == 2 | train$tr == 1 , 1, 0)
print(modelar(train, train_clase, test, test_clase))
gc()

# Mas meses:

train <- dataset[dataset$foto_mes == 201706 | dataset$foto_mes == 201705, ]
train_clase <- ifelse(train$tr == 2 | train$tr == 1 , 1, 0)
test <- dataset[dataset$foto_mes == 201708, ]
test_clase <- ifelse(test$clase_ternaria == "BAJA+2",1,0)
print(modelar(train, train_clase, test, test_clase))
gc(verbose = FALSE)


## Probamos multiples escenarios:
parametros <- fread("/home/sas/UBA2018/work/parametros.txt", header=TRUE, sep="\t")

resultados <- data.frame()

test <- dataset[dataset$foto_mes == 201708, ]
test_clase <- ifelse(test$clase_ternaria == "BAJA+2",1,0)

for (p in 1:nrow(parametros)) {
  
  caso <- data.frame(caso="1mes_simple")
  train <- dataset[dataset$foto_mes == 201706, ]
  train_clase <- ifelse(train$tr == 2, 1, 0)
  r <- modelar(train, train_clase, test, test_clase,param = parametros[p,"parametros"])
  resultados <- rbind(resultados, cbind(r,caso,p=parametros[p,"nombre"]))
  
  caso <- data.frame(caso=paste("1mes_comb",p))
  train_clase <- ifelse(train$tr == 2 | train$tr == 1 , 1, 0)
  r <- modelar(train, train_clase, test, test_clase,param = parametros[p,"parametros"])
  resultados <- rbind(resultados, cbind(r,caso,p=parametros[p,"nombre"]))
  
  caso <- data.frame(caso=paste("2mes_comb",p))
  train <- dataset[dataset$foto_mes == 201706 | dataset$foto_mes == 201705, ]
  train_clase <- ifelse(train$tr == 2 | train$tr == 1 , 1, 0)
  r <- modelar(train, train_clase, test, test_clase,param = parametros[p,"parametros"])
  resultados <- rbind(resultados, cbind(r,caso,p=parametros[p,"nombre"]))
  
  gc(verbose = FALSE)
}

fwrite(resultados, "resultados.txt", sep="\t")
