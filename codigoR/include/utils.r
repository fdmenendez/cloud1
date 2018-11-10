
ksemilla_azar         <-  c(333491,443501,622513,922781,982183)

include.packages <- function (pkg) {
  repos <- "http://cran.r-project.org" 
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) {
    print("Installing required packages, please wait...")
    install.packages(new.pkg,repos = repos, dependencies = TRUE)
  }
  sapply(pkg, require, character.only = TRUE)
}


clean.up <- function(dataset) {
  include.packages("dplyr")
  dataset$numero_de_cliente <- NULL 
  dataset<- dataset %>% dplyr::mutate(clase_binaria = as.factor(ifelse(clase_ternaria == 'CONTINUA',0,1))
#                                      ,foto_mes = as.character(foto_mes)
                                      )
  dataset$clase_ternaria<-NULL
  return(dataset)
}