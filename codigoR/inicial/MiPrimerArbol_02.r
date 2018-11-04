#Objetivo:  mostar un arbol con una impresion un poco mas elaborada

#Arbol con libreria  rpart
#se trabaja con constantes para ordenar el codigo fuente

setwd("C:\\Users\\fernando.d.menendez\\Google Drive\\Maestria\\Finanzas\\cloud1")

#source( "M:\\codigoR\\inicial\\MiPrimerArbol_02.r" )

#limpio la memoria
rm( list=ls() )
gc()


library( "rpart" )
library( "data.table" )
library( "rpart.plot" )

#Parametros entrada
karchivo_entrada      <-  "datasets\\201804.txt"
kcampos_separador     <-  "\t"
kcampo_id             <-  "numero_de_cliente"
kclase_nomcampo       <-  "clase_ternaria"
kclase_valor_positivo <-  "BAJA+2"
kcampos_a_borrar      <-  c( kcampo_id )


#Parametros salida
karchivo_imagen       <-  "work\\arbol_02.jpg"



#cargo los datos
dataset <- fread( karchivo_entrada, header=TRUE, sep=kcampos_separador )


#borro las variables que no me interesan
dataset[ ,  (kcampos_a_borrar) := NULL    ] 

# generacion del modelo
formula  <-  formula( paste( kclase_nomcampo, "~ .") )

t0       <-  Sys.time()
modelo   <-  rpart( formula,   data = dataset,   cp=0.005 )
t1       <-  Sys.time()

tcorrida <-  as.numeric(  t1 - t0, units = "secs")
print(  tcorrida )



# impresion basica del arbol
plot( modelo, uniform=TRUE, main="Mi primer arbol")
text( modelo, use.n=TRUE, all=TRUE, cex=.8, digits=10)


#impresion un poco mas elaborada del arbol
jpeg(file = karchivo_imagen,  width = 12, height = 4, units = 'in', res = 300)
prp( modelo, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0 )
dev.off()


