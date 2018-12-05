
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
  dataset<- dataset %>% dplyr::mutate(clase_binaria = as.factor(ifelse(clase_ternaria == 'CONTINUA',0,1)),
                                      canarito=runif(nrow(dataset))
                                      #                                      ,foto_mes = as.character(foto_mes)
  )
  dataset$clase_ternaria<-NULL
  dataset <- dataset %>% mutate_all(funs(replace(., is.nan(.), NA))) %>% 
    mutate_all(funs(replace(., is.infinite(.), NA)))
  
  return(dataset)
}

clean.up.lgb <- function(dataset) {
  include.packages("dplyr")
  dataset$numero_de_cliente <- NULL 
  dataset<- dataset %>% dplyr::mutate(clase_binaria = as.numeric(ifelse(clase_ternaria == 'CONTINUA',0,1)),
                                      canarito=runif(nrow(dataset))
                                      #                                      ,foto_mes = as.character(foto_mes)
  )
  dataset$clase_ternaria<-NULL
  dataset <- dataset %>% mutate_all(funs(replace(., is.nan(.), NA))) %>% 
    mutate_all(funs(replace(., is.infinite(.), NA)))
  
  return(dataset)
}

clean.up.oot <- function(dataset) {
  include.packages("dplyr")
  dataset$numero_de_cliente <- NULL 
  dataset<- dataset %>% dplyr::mutate(clase_binaria = as.factor(ifelse(clase_ternaria == 'BAJA+2',1,0)),
                                      canarito=runif(nrow(dataset))
                                      #                                      ,foto_mes = as.character(foto_mes)
  )
  dataset$clase_ternaria<-NULL
  dataset <- dataset %>% mutate_all(funs(replace(., is.nan(.), NA))) %>% 
    mutate_all(funs(replace(., is.infinite(.), NA)))
  
  return(dataset)
}

clean.up.oot.lgb <- function(dataset) {
  include.packages("dplyr")
  dataset$numero_de_cliente <- NULL 
  dataset<- dataset %>% dplyr::mutate(clase_binaria = as.numeric(ifelse(clase_ternaria == 'BAJA+2',1,0)),
                                      canarito=runif(nrow(dataset))
                                      #                                      ,foto_mes = as.character(foto_mes)
  )
  dataset$clase_ternaria<-NULL
  dataset <- dataset %>% mutate_all(funs(replace(., is.nan(.), NA))) %>% 
    mutate_all(funs(replace(., is.infinite(.), NA)))
  
  return(dataset)
}


# dummify <- function (dataset, char_cols, clase) {
#   dataset <- mlr::createDummyFeatures(
#     dataset, target = clase,
#     cols = char_cols
#   )
#   
#   return(dataset)
# }




read.datasets <- function(files, prefijo, sufijo, mas_clases = "N", alg) {
  
  # files<-"201712_,201802_"
  # prefijo <- "M:\\datasets\\dias\\"
  # sufijo <- "dias.rds"
  # mas_clases <- "Y"
  # alg <- "xgboost"
  
  include.packages("dplyr")
  include.packages("purrr")
  
  varchivos_generacion  <-  sort(unlist(strsplit( files, split=",")), decreasing = T)
  dataset    <-  lapply( varchivos_generacion, 
                         function(x) data.table::as.data.table(readRDS(paste0(prefijo,x,sufijo))))
  
  ifelse(alg=="xgboost",{
    dataset <- dataset %>% purrr::map(function(x) {
      x %>% mutate(clase_binaria = as.factor(ifelse(clase_ternaria == 'CONTINUA',0,1)),
                   canarito=runif(nrow(x))) %>% 
        select(-clase_ternaria) %>% mutate_all(funs(replace(., is.nan(.), NA))) %>% 
        mutate_all(funs(replace(., is.infinite(.), NA)))
    })
  },{
    dataset <- dataset %>% purrr::map(function(x) {
      x %>% mutate(clase_binaria = as.numeric(ifelse(clase_ternaria == 'CONTINUA',0,1)),
                   canarito=runif(nrow(x))) %>% 
        select(-clase_ternaria) %>% mutate_all(funs(replace(., is.nan(.), NA))) %>% 
        mutate_all(funs(replace(., is.infinite(.), NA)))
    })
  })
  
  
  #  max_class <- dataset[[1]] %>% filter(clase_binaria == 1) %>% select(numero_de_cliente)
  max_class <- dataset[[1]] %>% filter(clase_binaria == 1) %>% pull(numero_de_cliente)
  
  full_data <- dataset[[1]]
  
  if (length(dataset)>1) {
    for (i in 2:length(dataset)) {
      if (mas_clases=="Y" & i<=3) {
        #      dataset[[i]]["numero_de_cliente" %in% max_class, "clase_binaria"] <- 1
        dataset[[i]] <- dataset[[i]] %>% mutate(clase_binaria=replace(clase_binaria,
                                                                      numero_de_cliente %in% max_class,1))
      }
      full_data <- rbind(full_data, dataset[[i]])
    }
  }
  
  full_data$numero_de_cliente<-NULL
  rm(dataset)
  gc()
  
  return(full_data)
  
}


sample.datasets <- function(files, prefijo, sufijo, mas_clases = "N", alg, subsample_pct = 0.3, seed = 123456) {
  
  # files<-"201712_,201802_"
  # prefijo <- "M:\\datasets\\dias\\"
  # sufijo <- "dias.rds"
  # mas_clases <- "Y"
  # alg <- "xgboost"
  # subsample_pct <-.2
  # seed<-1234
  # i<-2
  
  include.packages("dplyr")
  include.packages("purrr")
  
  if (subsample_pct == 1) {
	return(read.datasets(files, prefijo, sufijo, mas_clases, alg))
  }
  
  varchivos_generacion  <-  sort(unlist(strsplit( files, split=",")), decreasing = T)
  dataset    <-  lapply( varchivos_generacion, 
                         function(x) data.table::as.data.table(readRDS(paste0(prefijo,x,sufijo))))
  
  ifelse(alg=="xgboost",{
    dataset <- dataset %>% purrr::map(function(x) {
      x %>% mutate(clase_binaria = as.factor(ifelse(clase_ternaria == 'CONTINUA',0,1)),
                   canarito=runif(nrow(x))) %>% 
        select(-clase_ternaria) %>% mutate_all(funs(replace(., is.nan(.), NA))) %>% 
        mutate_all(funs(replace(., is.infinite(.), NA)))
    })
  },{
    dataset <- dataset %>% purrr::map(function(x) {
      x %>% mutate(clase_binaria = as.numeric(ifelse(clase_ternaria == 'CONTINUA',0,1)),
                   canarito=runif(nrow(x))) %>% 
        select(-clase_ternaria) %>% mutate_all(funs(replace(., is.nan(.), NA))) %>% 
        mutate_all(funs(replace(., is.infinite(.), NA)))
    })
  })
  
  
  #  max_class <- dataset[[1]] %>% filter(clase_binaria == 1) %>% select(numero_de_cliente)
  max_class <- dataset[[1]] %>% filter(clase_binaria == 1) %>% pull(numero_de_cliente)
  
  set.seed(seed)
  parcial<-dataset[[1]] %>% group_by(clase_binaria) %>% sample_frac(subsample_pct)
  
  full_data <- parcial
  

  if (length(dataset)>1) {
    for (i in 2:length(dataset)) {
      if (mas_clases=="Y" & i<=3) {
        #      dataset[[i]]["numero_de_cliente" %in% max_class, "clase_binaria"] <- 1
        dataset[[i]] <- dataset[[i]] %>% mutate(clase_binaria=replace(clase_binaria,
                                                                      numero_de_cliente %in% max_class,1))
      }
      
      aux<-dataset[[i]] %>% filter(numero_de_cliente %in% full_data$numero_de_cliente) %>% data.table::as.data.table()
      set.seed(seed)
      aux<-dataset[[i]] %>% filter(!(numero_de_cliente %in% full_data$numero_de_cliente)) %>% 
        group_by(clase_binaria) %>% sample_frac(subsample_pct/2) %>% data.table::as.data.table() %>% bind_rows(aux)
      
      full_data <- full_data %>% bind_rows(aux)
    }
  }
  
  full_data$numero_de_cliente<-NULL
  rm(dataset)
  gc()
  
  return(full_data)
  
}


aplicar_modelo <- function(modelo, dataset, clase,xgb.genet=F, kmin_corte=.005, kmax_corte=.1) {
  
  ifelse(xgb.genet,
         {
           testTask <- mlr::makeClassifTask(data = dataset,target = clase, positive = 1)
           aplicacion_prediccion  <- predict(  modelo, testTask )
           aplicacion_prediccion <- aplicacion_prediccion$data$prob.1
         },
         {
           options(na.action='na.pass')
           formula  <- formula(paste("~ .-1"))
           dataset_unido_matrix  = model.matrix(formula, data = dataset[ , -which(names(dataset) == clase)   ])
           dataset_validacion_sinclase_sparse = as(dataset_unido_matrix, "dgCMatrix")
           rm(dataset_unido_matrix)
           gc()
           
           aplicacion_prediccion  <- predict(  modelo, as.matrix( dataset_validacion_sinclase_sparse) )
         })
  
  out<-data.frame(prob_corte=as.numeric(), ganancia=as.numeric())
  kcortes<-seq(kmin_corte,kmax_corte,.0001)
  gan_bruta<-ifelse( dataset[,clase] == 1, 11700, -300 )
  
  
  for (p in kcortes) {
    sal<-data.frame(prob_corte=p,ganancia=sum((aplicacion_prediccion > p) * gan_bruta))
    #  print(paste(sal,"\n"))
    out<-rbind(out,sal)
  }
  val<-out %>% filter(ganancia==max(out$ganancia)) %>% head(1)
  
  
  return(  val )
}





