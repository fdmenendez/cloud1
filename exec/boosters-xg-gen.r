rm( list=ls() )
gc()

script.name<- "boosters-xg-gen"
kextension <- "redhist"

switch ( Sys.info()[['sysname']],
         Windows = { directory.include  <-  "M:\\codigoR\\include\\"
         directory.work     <-  paste0("M:\\work\\",script.name,"\\")
         directory.plan     <-  "M:\\plan\\"
         directory.datasets <-  paste0("M:\\datasets\\",kextension,"\\")
         directory.root     <-  "M:\\"
         },
         Linux   = { 
         directory.include  <-  "~/cloud/cloud1/codigoR/include/"
         directory.work     <-  paste0("~/cloud/cloud1/work/",script.name,"/")
         directory.plan     <-  "~/cloud/cloud1/plan/"
         directory.datasets <-  paste0("~/cloud/cloud1/datasets/",kextension,"/")
         }
)

kclase_nomcampo       <- "clase_binaria"

source(paste0(directory.include,"utils.r"))
source(paste0(directory.include,"xgboost-genetical-v2.r"))
source(paste0(directory.include,"metrica.r"))

include.packages("data.table")
include.packages("dplyr")
include.packages("caret")
include.packages("mlr")

setwd(directory.work)

# pop_inicial<-10
# nro_hijos<-6
# max_gen<-20
# nfold<-3
# nrounds<-10

dataset.oot<- as.data.table(readRDS(paste0(directory.datasets,"201802_",kextension,".rds")))
dataset.oot<- clean.up.oot(dataset.oot)

exp<-read.csv(paste0(directory.plan,script.name,"-",kextension,"-exp.csv"), stringsAsFactors=F)
colnames(exp)<-c("files", "mas_clases",	"pop_inicial",	"nro_hijos",	"max_gen",	"nfold",	"nrounds",	"exp",	"ganacia")


for (dt in 1:nrow(exp)) {
  ifelse(!is.na(exp[dt,"exp"]),
         cat(paste("Experimento",dt,"completo...\n")),
         {
           # varchivos_generacion  <-  unlist(strsplit( exp[dt,"files"], split=","))
           # dataset    <-  do.call(rbind, lapply( varchivos_generacion, function(x) readRDS(paste0(directory.datasets,x,kextension,".rds"))))
           # dataset_training<-clean.up(dataset)
           
           dataset_training<-read.datasets(exp[dt,"files"],directory.datasets,paste0(kextension,".rds"),exp[dt,"mas_clases"],"xgboost")
           
           genet<-train_gen_xgboost(dataset_training, 
                                    dataset.oot, 
                                    kclase_nomcampo,
                                    exp[dt,"pop_inicial"],
                                    exp[dt,"nro_hijos"],
                                    exp[dt,"max_gen"],
                                    exp[dt,"nfold"],
                                    exp[dt,"nrounds"],
                                    ksemilla_azar[2],
                                    fganancia_logistic_xgboost_hyp)
           
           save(genet, file=paste0("genetical-boost-",kextension,"-",dt))
           exp[dt,"exp"]<-dt
           exp[dt,"ganancia"]<-genet$best_model_parms$fitness
           write.csv(exp, paste0(directory.plan,script.name,"-",kextension,"-exp.csv"), row.names = F)
           cat(paste("Experimento",dt,"completo...\n"))
           rm(genet)
           gc()
         })
}


#limpio la memoria
rm( list=ls() )
gc()


#salgo del R sin grabar el gigante entorno
quit( save="no" )






