
require(dplyr)

kbest_of <- 3

limits=data.frame(
  params=c("pfeature_fraction","plearning_rate","plambda_l1","pmin_gain_to_split","pmin_data_in_leaf","pnum_leaves","prob_corte"),
  low=c(.05,0,0,0,1,10,.015),
  uppr=c(1,.1,1,1,100,512,.1),
  dec=c(2,1,0,0,0,0,3),
  stringsAsFactors = FALSE
)


fganancia_logistic_lightgbm_binaria   <- function(probs, clases) 
{
  
  vlabels <- getinfo(clases, "label")
  
  gan <-sum(   (probs > vprob_corte_binaria2  ) * 
                 ifelse( vlabels== 1, kganancia_acierto, kganancia_noacierto ),
               na.rm = TRUE   
  )
  
  
  return(  list( name = "ganancia", 
                 value =  ifelse(  is.na(gan) , 0, gan ) ,
                 higher_better= TRUE 
                 )
  )
}

initilialize_population <- function(nbr_individuals, sem) {

#  set.seed(sem)
  out<-as.data.frame(apply(limits,1, function(x) round(runif(nbr_individuals, as.numeric(x["low"]),as.numeric(x["uppr"])),as.numeric(x["dec"]))))
  out<-cbind(out,rep(1,nbr_individuals))
  colnames(out)<-c(as.character(limits$params),c("gen"))
  rownames<-NULL
  return(out)
}


makeGeneticLearner2 <- function (parms, dgeneracion, sem) {

#    set.seed(sem)
    vals<-c(list(
      objective = "binary:logistic"),
      as.list(parms), list(
        #  eval_metric = ganancia,
        silent = 1,
        tree_method = "hist",
        grow_policy="lossguide",
        updater="grow_fast_histmaker",
        #    early_stopping_rounds = round(nrounds*.4,0),
        maximize = T,
        verbose = 0,
        missing = NA,
        stratified = TRUE))
  
    
    xg_set <- mlr::makeLearner("classif.xgboost", 
                             predict.type = "prob",
                             par.vals = vals)
  

  xg_model <- mlr::train(xg_set, task = trainTask)
  
  modelo = lightGBM::lgb.train( 
    data = dgeneracion,
    objective = "binary",
    num_iterations=700, 
    init_score= mean(getinfo(dgeneracion, "label")),
    bagging_fraction=1, 
    feature_fraction=0.50, 
    learning_rate=0.020, 
    min_child_weight=8, 
    max_depth=10, 
    lambda_l1=0.50,
    lambda_l2=10,
    max_bin=32, 
    num_leaves=255
  )
  
  sal<-xg_model$learner.model$evaluation_log %>% .[order(.[,2],decreasing = T),2] %>% head(1)
  rm(xg_model)
  return(sal)
  
}

train_population<-function(pop, dgeneracion, best_iter = 5, num_iterations = 10, sem = 1234, FEVAL=NULL, eval_metric = "auc") {
  

  ifelse(is.null(FEVAL),
        {parms.fijo<-list(nrounds=nrounds, best_iter = best_iter, eval_metric=eval_metric)},
        {parms.fijo<-list(nrounds=nrounds, best_iter = best_iter, feval=FEVAL)})
  
  
  xg.sal<-NULL
  
  test<-pop %>% filter(gen==max(gen)) %>% select(-gen) %>% apply(.,1,as.list)
  for(x in test) {
    vprob_corte<-x$prob_corte
    parms<-x  %>% append(.,parms.fijo)
    xg.sal<-rbind(xg.sal,makeGeneticLearner2(parms, dgeneracion, sem))
    gc()
  }
  return(mutate(pop,fitness=unlist(xg.sal)))
}


crossover<-function(pop, nro_hijos, sem =1234) {
  out<-NULL

#  set.seed(sem)
  for (i in 1:(nro_hijos/2)) {
  
    order <- sample(1:nrow(limits),nrow(limits),replace = F)
    nro_param <- sample(1:(nrow(limits)-1),1)
    c1 <- c(rep(1,nro_param),rep(2,nrow(limits)-nro_param)) %>% .[order]
    c2 <- c(rep(2,nro_param),rep(1,nrow(limits)-nro_param)) %>% .[order]
    
    padres_mat<-pop %>% filter(gen==max(pop$gen)) %>% arrange(desc(fitness)) %>% head(kbest_of) %>% sample_n(2)
    out<-rbind(out,mapply(function (x,y) {padres_mat[x,y]},c1,1:nrow(limits)))
    out<-rbind(out,mapply(function (x,y) {padres_mat[x,y]},c2,1:nrow(limits)))
  }
  out.mtd<-mutacion(out,sem)
  colnames(out.mtd)<-c(as.character(limits$params))
  out.mtd<-rbind(as.data.frame(out.mtd),as.data.frame(padres_mat[,1:nrow(limits)]))
  out.full<- unique(out.mtd) %>% mutate(gen=max(pop$gen)+1)
  
  
  return(out.full)
}

mutacion<- function(pop, sem = 1234) {
  
#  set.seed(sem)
  rdm=as.matrix(apply(limits,1, function(x) round(runif(nrow(pop), as.numeric(x["low"]),as.numeric(x["uppr"])),as.numeric(x["dec"]))))
  for (i in 1:nrow(pop)) {
    ind<-sample(1:nrow(limits),1)
    pop[i,ind]<-rdm[i,ind]
  }
  return(pop)
}




train_gen_xgboost<-function(data, campo_clase, pop_inicial, nro_hijos, max_gen, best_iter = 3, 
                            nrounds = 5, sem = 1234, FEVAL=NULL,eval_metric = "auc") {
  
  # data <- dataset_training
  # campo_clase<-"clase_binaria"
  # pop_inicial<-4
  # nro_hijos<-2
  # max_gen<-3
  # nfold<-3
  # nrounds<-10
  # sem=1234
  # eval_metric<-"auc"
  # FEVAL<-fganancia_logistic_xgboost_hyp
  
  
  
  
  cat("---- Genetical LightGBM------------\n")
  cat("---- PARMS: -----------------------\n")
  cat(paste("---- campo_clase =",campo_clase,"\n"))
  cat(paste("---- pop_inicial =",pop_inicial,"\n"))
  cat(paste("---- nro_hijos =",nro_hijos,"\n"))
  cat(paste("---- max_gen =",max_gen,"\n"))
  cat(paste("---- nfold =",nfold,"\n"))
  cat(paste("---- nrounds =",nrounds,"\n"))
  cat(paste("---- sem =",sem,"\n"))
  cat(paste("---- FEVAL =",as.character(substitute(FEVAL)),"\n"))
  cat(paste("---- eval_metric =",eval_metric,"\n"))
  
  #genero one-hot enconding
  options(na.action='na.pass')
  formula  <- formula(paste("~ .-1"))
  
  dataset_unido_matrix  = model.matrix(formula, data = data[,-which(names(data) == campo_clase)])
  dataset_generacion_sinclase_sparse = as(dataset_unido_matrix, "dgCMatrix")
  rm(dataset_unido_matrix)
  
  dgeneracion  <-   lgb.Dataset( data  = data.matrix(dataset_generacion_sinclase_sparse),
                                 label = data[ , campo_clase], 
                                 missing=NA,
                                 free_raw_data=FALSE 
  )
  
  t0 <- Sys.time()
  
  set.seed(sem)
  ifelse(file.exists("genetical-exec.csv"),{
    pop<-read.csv("genetical-exec.csv")
    
    ifelse(max(pop$gen)>=max_gen, {
      cat("-- Genetical: Starting from scratch\n\n")
      pop<-initilialize_population(pop_inicial,sem)
      pop<-train_population(pop,dgeneracion,best_iter,nrounds, sem, FEVAL, eval_metric)
      pop.family<-pop
    },{
      cat(paste("-- Genetical: Re-starting from generation\n\n",max(pop$gen)+1,"\n"))
      pop.family<-pop
    })
  },{
    cat("-- Genetical: Starting from scratch\n\n")
    pop<-initilialize_population(pop_inicial,sem)
    pop<-train_population(pop,dgeneracion,best_iter,nrounds, sem, FEVAL, eval_metric)
    pop.family<-pop
  })
  
  while (max(pop$gen)+1<=max_gen) {
    pop<-crossover(pop,nro_hijos,sem)
    pop<-train_population(pop,dgeneracion,best_iter,nrounds, sem, FEVAL, eval_metric)
    pop.family<-rbind(pop.family,pop)
    cat(paste("--- Saving state - current generation =",max(pop$gen),"\n"))
    write.csv(pop.family, "genetical-exec.csv",row.names = F)
  }
  t1 <- Sys.time()
  
  result_lst<-list(
    "best_model" = pop.family %>% mutate(fitness = round(fitness,0)) %>% arrange(desc(fitness)) %>% head(1),
    "full_family" = pop.family %>% mutate(fitness = round(fitness,0)),
    "Start" = t0,
    "End" = t1
  )
  return(result_lst)
}










