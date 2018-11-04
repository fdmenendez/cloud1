rm( list=ls() )
gc()

script.name<-"general-eda"

switch ( Sys.info()[['sysname']],
         Windows = { directory.include  <-  "M:\\codigoR\\include\\"
         directory.work     <-  paste0("M:\\work\\",script.name)
         directory.plan     <-  "M:\\plan\\"
         directory.datasets <-  "M:\\datasets\\"
         directory.root     <-  "M:\\"
         },
         Darwin  = { directory.include  <-  "~/dm/codigoR/include/"
         directory.work     <-  "~/dm/work/"
         directory.plan     <-  "~/dm/plan/"
         directory.datasets <-  "~/dm/datasets/"
         },
         Linux   = { directory.include  <-  "~/cloud/cloud1/codigoR/include/"
         directory.work     <-  paste0("~/cloud/cloud1/work/",script.name)
         directory.plan     <-  "~/cloud/cloud1/plan/"
         directory.datasets <-  "~/cloud/cloud1/datasets/"
         }
)

library(data.table)

if (!require(tidyr)) install.packages('tidyr')
library(tidyr)

if (!require(dplyr)) install.packages('dplyr')
library(dplyr)

setwd(directory.datasets)
ds  <-  fread("paquete_premium.txt", header=TRUE, sep="\t", verbose = T)
#ds  <-  fread("201802.txt", header=TRUE, sep="\t")

setwd(directory.work)
sum1<-ds %>% 
  group_by(foto_mes, clase_ternaria) %>% 
  summarise(cantidad = n()) %>% 
  filter(foto_mes < 201804) %>%
  spread(key=clase_ternaria, value=cantidad)
write.csv(ds, "mes-a-mes.csv")


data <- ds %>%
  group_by(foto_mes) %>% 
  summarise(cantidad = n())

data$x <- seq.int(nrow(data))

if (!require(ggplot2)) install.packages('ggplot2')
library(ggplot2)

grp<-ggplot(data, aes(x = x, y = cantidad))+
  geom_line(color="green", size = 2)+
  scale_x_continuous(labels = data$foto_mes, breaks = 1:max(data$x), name="Foto Mes") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Cantidad de personas premium a lo largo de los últimos 2 años") 
ggsave("cantidad-premium.jpg",grp)

ratios <- ds %>% 
  group_by(foto_mes, clase_ternaria) %>% 
  summarise(cantidad = n()) %>% 
  filter(foto_mes <= 201804) %>%
  spread(key=clase_ternaria, value=cantidad) %>%
  mutate (r_baja1 = `BAJA+1` / ( `BAJA+2` + `BAJA+1` + CONTINUA )) %>%
  mutate (r_baja2 = `BAJA+2` / ( `BAJA+2` + `BAJA+1` + CONTINUA ))

write.csv(ratios, "ratios.csv")

ratios$x <- seq.int(nrow(ratios))

ratios <- ratios %>%
  select(x, foto_mes,  r_baja1, r_baja2) %>%
  gather(clase, y, r_baja1, r_baja2, -foto_mes, -x)

grp<-ggplot(ratios, aes(x = x, y = y))+
  geom_line(aes(colour = clase), size = 1.5) +
  scale_x_continuous(labels = unique(ratios$foto_mes), breaks = seq(1,28,1), name="Foto Mes") +
  scale_y_continuous(name="% Churn") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Variación del Churn a lo largo de los últimos 2 años") 
ggsave("ratios.jpg",grp)


#limpio la memoria
rm( list=ls() )
gc()


#salgo del R sin grabar el gigante entorno
quit( save="no" ,status = 0)
