#Instalar librerías de dependencia testthat, roxygen2, devtools, R6
install.packages("testthat")
install.packages("roxygen2")
install.packages("devtools")
install.packages("R6")

#Es necesario instalar la librería lgbdl
devtools::install_github("Laurae2/lgbdl")

#Instalamos la librería LightGBM usando lgbdl
library(lgbdl)
lgb.dl(commit = "master",
       compiler = "gcc",
       repo = "https://github.com/Microsoft/LightGBM",
       cores = 2)

#Cargamos las librerías necesarias para verificar que están Ok
library(testthat)
library(roxygen2)
library(R6)
library(lightgbm)

#Prueba de funcionamiento (Extraído de la página de GitHub)
#https://github.com/Microsoft/LightGBM/tree/master/R-package

library(lightgbm)
data(agaricus.train, package='lightgbm')
train <- agaricus.train
dtrain <- lgb.Dataset(train$data, label=train$label)
params <- list(objective="regression", metric="l2")
model <- lgb.cv(params, dtrain, 10, nfold=5, min_data=1, learning_rate=1, early_stopping_rounds=10)

