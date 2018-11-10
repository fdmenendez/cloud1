
require(dplyr)

kbest_of <- 3

limits=data.frame(
  params=c("max_depth","lambda","eta","subsample","min_child_weight","colsample_bytree","alpha","prob_corte"),
  low=c(3,.50,.001,.1,1,.2,.50,.015),
  uppr=c(50,.60,.5,.8,7,1,.60,.1),
  dec=c(0,2,3,1,0,1,2,3),
  stringsAsFactors = FALSE
)


initilialize_population <- function(nbr_individuals, sem) {

  set.seed(sem)
  out<-as.data.frame(apply(limits,1, function(x) round(runif(nbr_individuals, as.numeric(x["low"]),as.numeric(x["uppr"])),as.numeric(x["dec"]))))
  out<-cbind(out,rep(1,nbr_individuals))
  colnames(out)<-c(as.character(limits$params),c("gen"))
  rownames<-NULL
  return(out)
}


makeGeneticLearner2 <- function (parms, trainTask, sem) {

    set.seed(sem)
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
  
  sal<-xg_model$learner.model$evaluation_log %>% .[order(.[,2],decreasing = T),2] %>% head(1)
  rm(xg_model)
  return(sal)
  
}

train_population<-function(pop, trainTask, nfold = 5, nrounds = 10, sem = 1234, FEVAL=NULL, eval_metric = "auc") {
  

  ifelse(is.null(FEVAL),
        {parms.fijo<-list(nrounds=nrounds, nfold = nfold, eval_metric=eval_metric)},
        {parms.fijo<-list(nrounds=nrounds, nfold = nfold, feval=FEVAL)})
  
  
  xg.sal<-NULL
  
  test<-pop %>% filter(gen==max(gen)) %>% select(-gen) %>% apply(.,1,as.list)
  for(x in test) {
    vprob_corte<-x$prob_corte
    parms<-x  %>% append(.,parms.fijo)
    xg.sal<-rbind(xg.sal,makeGeneticLearner2(parms, trainTask, sem))
    gc()
  }
  return(mutate(pop,fitness=unlist(xg.sal)))
}


crossover<-function(pop, nro_hijos, sem =1234) {
  out<-NULL

  set.seed(sem)
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
  
  set.seed(sem)
  rdm=as.matrix(apply(limits,1, function(x) round(runif(nrow(pop), as.numeric(x["low"]),as.numeric(x["uppr"])),as.numeric(x["dec"]))))
  for (i in 1:nrow(pop)) {
    ind<-sample(1:nrow(limits),1)
    pop[i,ind]<-rdm[i,ind]
  }
  return(pop)
}




train_gen_xgboost<-function(data, campo_clase, pop_inicial, nro_hijos, max_gen, nfold = 3, 
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
  
  
  
  
  cat("---- Genetical XGBoost ------------\n")
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
  
  trainTask <- mlr::makeClassifTask(data = data,target = campo_clase, positive = 1)
  trainTask <- mlr::normalizeFeatures(trainTask,method = "standardize")
  
  getParamSet("classif.xgboost")
  configureMlr(on.par.without.desc = "quiet",show.learner.output = FALSE)
  
  t0 <- Sys.time()
  

  ifelse(file.exists("genetical-exec.csv"),{
    pop<-read.csv("genetical-exec.csv")
    
    ifelse(max(pop$gen)>=max_gen, {
      cat("-- Genetical: Starting from scratch\n\n")
      pop<-initilialize_population(pop_inicial,sem)
      pop<-train_population(pop,trainTask,nfold,nrounds, sem, FEVAL, eval_metric)
      pop.family<-pop
    },{
      cat(paste("-- Genetical: Re-starting from generation\n\n",max(pop$gen)+1,"\n"))
      pop.family<-pop
    })
  },{
    cat("-- Genetical: Starting from scratch\n\n")
    pop<-initilialize_population(pop_inicial,sem)
    pop<-train_population(pop,trainTask,nfold,nrounds, sem, FEVAL, eval_metric)
    pop.family<-pop
  })
  
  while (max(pop$gen)+1<=max_gen) {
    pop<-crossover(pop,nro_hijos,sem)
    pop<-train_population(pop,trainTask,nfold,nrounds, sem, FEVAL, eval_metric)
    pop.family<-rbind(pop.family,pop)
    cat(paste("--- Saving state - current generation =",max(pop$gen),"\n"))
    write.csv(pop.family, "genetical-exec.csv",row.names = F)
  }
  t1 <- Sys.time()
  
  result_lst<-list(
    "best_model" = pop %>% mutate(fitness = round(fitness,0)) %>% arrange(desc(fitness)) %>% head(1),
    "full_family" = pop.family %>% mutate(fitness = round(fitness,0)),
    "Start" = t0,
    "End" = t1
  )
  return(result_lst)
}










