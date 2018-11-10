#Objetivo:  mostrar que con el parametro xval=0   rpart corre 5 veces mas rapido

#Arbol con libreria  rpart
#se trabaja con constantes para ordenar el codigo fuente


#source( "M:\\codigoR\\inicial\\MiPrimerArbol_02.r" )

#limpio la memoria
rm( list=ls() )
gc()


library( "rpart" )
library( "data.table" )
library( "rpart.plot" )

#Parametros entrada
karchivo_entrada      <-  "M:\\datasets\\201802.txt"
kcampos_separador     <-  "\t"
kcampo_id             <-  "numero_de_cliente"
kclase_nomcampo       <-  "clase_ternaria"
kclase_valor_positivo <-  "BAJA+2"
kcampos_a_borrar      <-  c( kcampo_id )


#Parametros salida
karchivo_imagen1      <-  "M:\\work\\arbol_02.jpg"
karchivo_imagen2      <-  "M:\\work\\arbol_02_optim.jpg"



#cargo los datos
dataset <- fread( karchivo_entrada, header=TRUE, sep=kcampos_separador )


#borro las variables que no me interesan
dataset[ ,  (kcampos_a_borrar) := NULL    ] 

# generacion del modelo
formula  <-  formula( paste( kclase_nomcampo, "~ .") )

t0       <-  Sys.time()
modelo1  <-  rpart( formula,   data = dataset,   cp=0.005 )
t1       <-  Sys.time()

tcorrida <-  as.numeric(  t1 - t0, units = "secs")
print(  tcorrida )


#impresion un poco mas elaborada del arbol
jpeg(file = karchivo_imagen1,  width = 12, height = 4, units = 'in', res = 300)
prp( modelo1, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0 )
dev.off()




#ahora genero modelo2  PERO con el nuevo parametro  xval=0

t0       <-  Sys.time()
modelo2  <-  rpart( formula,   data = dataset,   cp=0.005,   xval=0 )
t1       <-  Sys.time()

tcorrida <-  as.numeric(  t1 - t0, units = "secs")
print(  tcorrida )


#impresion un poco mas elaborada del arbol
jpeg(file = karchivo_imagen2,  width = 12, height = 4, units = 'in', res = 300)
prp( modelo2, extra=101, digits=5, branch=1, type=4, varlen=0, faclen=0 )
dev.off()



#Conclusion, empezar a correr con el parametro xval=0 (al menos por ahora ),
#ya que los arboles en este experimento salieron iguales
#y el tiempo de corrida es mucho menor
#el alumno aplicado investigara si esto se cumple para todos los casos



