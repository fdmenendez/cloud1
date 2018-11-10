#Objetivo :  mostar que los archivos se cargan mas rapido con data.table::fread

#comparo dos formas de leer el dataset


#cargo los datos, forma estandar
t0       <-  Sys.time()
dataset1 <-  read.table("M:\\datasets\\201802.txt", header=TRUE, sep="\t", row.names="numero_de_cliente" )
t1       <-  Sys.time()

tcorrida <-  as.numeric(  t1 - t0, units = "secs")
cat( "tiempo carga dataset con read.table ",  tcorrida, "\n" )

cat( "cantidad de registros del dataset: ", nrow(dataset1), "\n" ) 
cat( "cantidad de columnas  del dataset: ", ncol(dataset1), "\n" ) 



library( "data.table" )
t0        <-  Sys.time()
dataset2  <-  fread("M:\\datasets\\201802.txt", header=TRUE, sep="\t")
t1        <-  Sys.time()

tcorrida <-  as.numeric(  t1 - t0, units = "secs")
cat( "tiempo carga dataset con data.table::fread ",  tcorrida, "\n" )

cat( "cantidad de registros del dataset: ", nrow(dataset2), "\n" ) 
cat( "cantidad de columnas  del dataset: ", ncol(dataset2), "\n" ) 


#conclusion   usar  library( "data.table" )  y leer los datos con fread
