#Objetivo:  mostrar un arbol de probabilidad (decision)

#Arbol con libreria  rpart
#No se hace training ni testing, simplemente es el primer contacto con el dataset
#esto decididamente NO es un modelo, actuarios y estadisticos por favor no ponerse nerviosos
#ejecutar linea a linea


library( "rpart" )

#cargo los datos
dataset <- read.table("M:\\datasets\\201802.txt", header=TRUE, sep="\t", row.names="numero_de_cliente" )

cat( "cantidad de registros del dataset: ", nrow(dataset), "\n" ) 
cat( "cantidad de columnas  del dataset: ", ncol(dataset), "\n" ) 

#veo la distribucion de la clase
ftable( dataset$clase_ternaria )



#generacion del modelo del arbol
modelo  <- rpart( clase_ternaria ~ .   ,   data = dataset, cp=0.005 )


#impresion basica del arbol
plot( modelo, uniform=TRUE, main="Mi primer arbol")
text( modelo, use.n=TRUE, all=TRUE, cex=.8, digits=10)


#el summary del modelo
summary( modelo )


#impresion de los caminos
path.rpart( modelo , 1)
path.rpart( modelo , 2)
path.rpart( modelo , 3)
path.rpart( modelo , 3)
