rm( list=ls() )
gc()

script.name<- "boosters-lgb-gen-sample"

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
         directory.datasets <-  "~/cloud/cloud1/datasets/"
         }
)

kclase_nomcampo       <- "clase_binaria"

source(paste0(directory.include,"utils.r"))
source(paste0(directory.include,"lgbm-genetical.r"))
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

exp<-read.csv(paste0(directory.plan,script.name,"-exp.csv"), stringsAsFactors=F)
colnames(exp)<-c("job","files","test","valid","ext", "mas_clases",	"subsample","pop_inicial",	"nro_hijos",	"max_gen",	"nrounds","early_stopping_round",	"exp",	"ganancia_test", "pcorte_max", "ganancia_valid")


for (dt in 1:nrow(exp)) {
  ifelse(!is.na(exp[dt,"exp"]),
         cat(paste("Experimento",dt,"completo...\n")),
         {
           # varchivos_generacion  <-  unlist(strsplit( exp[dt,"files"], split=","))
           # dataset    <-  do.call(rbind, lapply( varchivos_generacion, function(x) readRDS(paste0(directory.datasets,x,kextension,".rds"))))
           # dataset_training<-clean.up(dataset)
           
           dataset_training<-as.data.frame(sample.datasets(exp[dt,"files"],
                                             paste0(directory.datasets,exp[dt,"ext"],"/"),
                                             paste0(exp[dt,"ext"],".rds"),
                                             exp[dt,"mas_clases"],
                                             "lgb",
                                             exp[dt,"subsample"],
                                             ksemilla_azar[2]))
           
           dataset.oot<- as.data.table(readRDS(paste0(paste0(directory.datasets,exp[dt,"ext"],"/"),exp[dt,"test"],exp[dt,"ext"],".rds")))
           dataset.oot<- clean.up.oot.lgb(dataset.oot)
           
           
           genet<-train_gen_lightgbm(data.trn=dataset_training, 
                                    data.oot=dataset.oot, 
                                    campo_clase=kclase_nomcampo,
                                    pop_inicial=exp[dt,"pop_inicial"],
                                    nro_hijos=exp[dt,"nro_hijos"],
                                    max_gen=exp[dt,"max_gen"],
                                    nrounds=exp[dt,"nrounds"],
                                    early_stopping_round=exp[dt,"early_stopping_round"],
                                    seed=ksemilla_azar[2],
                                    FEVAL=fganancia_logistic_lightgbm_binaria)
           
#           save(genet, file=paste0("genetical-boost-",exp[dt,"ext"],"-",dt))
           write.csv(genet$full_family, paste0("genetical-boost-",exp[dt,"ext"],"-",exp[dt,"job"],".csv"),row.names = F)
           exp[dt,"exp"]<-dt
           
           
           dataset.val<- as.data.table(readRDS(paste0(paste0(directory.datasets,exp[dt,"ext"],"/"),exp[dt,"valid"],exp[dt,"ext"],".rds")))
           dataset.val<- clean.up.oot.lgb(dataset.val)
           
           valid<-aplicar_modelo(genet$best_model,dataset.val,kclase_nomcampo)
           
           exp[dt,"ganancia_test"]<-genet$best_model_parms$fitness
           exp[dt,"ganancia_valid"]<-valid$ganancia
           exp[dt,"pcorte_max"]<-valid$prob_corte
           
           write.csv(exp, paste0(directory.plan,script.name,"-exp.csv"), row.names = F)
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






