
require(dplyr)

limits=data.frame(
  params=c("max_depth","lambda","eta","subsample","min_child_weight","colsample_bytree","alpha"),
  low=c(3,.50,.001,.1,1,.2,.50),
  uppr=c(50,.60,.5,.8,7,1,.60),
  dec=c(0,2,3,1,0,1,2),
  stringsAsFactors = FALSE
)


initilialize_population <- function(nbr_individuals, sem) {

  set.seed(sem)
  out=as.data.frame(apply(limits,1, function(x) round(runif(nbr_individuals, as.numeric(x["low"]),as.numeric(x["uppr"])),as.numeric(x["dec"]))))
  out<-cbind(out,rep(1,nbr_individuals))
  colnames(out)<-c(as.character(limits$params),c("gen"))
  rownames<-NULL
  return(out)
}


makeGeneticLearner <- function (parms, train, FEVAL=NULL, eval_metric="auc", nrounds=5, nfold = 5, sem = 1234) {
  set.seed(sem)
  xg_set <- mlr::makeLearner("classif.xgboost", predict.type = "prob")
  
  xg_set$par.vals <- c(list(
    objective = "binary:logistic",
    #  eval_metric = ganancia,
    eval_metric = eval_metric,
    maximize = T,
    verbose = 0,
    nrounds = nrounds,
    missing = NA,
    silent = 1,
    tree_method = "hist",
    grow_policy="lossguide",
    updater="grow_fast_histmaker",
#    early_stopping_rounds = round(nrounds*.4,0),
#    nthread=parallel::detectCores()-1,
    stratified = TRUE
  )
  ,as.list(parms[1:nrow(limits)]))
  
  ifelse(is.null(FEVAL),
         {xg_set$par.vals[["eval_metric"]]<-eval_metric},
         {xg_set$par.vals[["feval"]]<-FEVAL})
  
  xg_set$par.vals[["nfold"]]<-nfold
  
#  print(xg_set$par.vals)
  xg_model <- mlr::train(xg_set, task = train)
  
#  print(xg_model$learner$par.vals)
  return(xg_model$learner.model$evaluation_log %>% .[order(.[,2],decreasing = T),2] %>% head(1))
  
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
  
  
  # ifelse(is.null(FEVAL),
  #        {xg_set$par.vals[["eval_metric"]]<-eval_metric},
  #        {xg_set$par.vals[["feval"]]<-FEVAL})
  

  #  print(xg_set$par.vals)
  xg_model <- mlr::train(xg_set, task = trainTask)
  #xg_model <- mlr::train(xg_set, task = trainTask)
  
  #  print(xg_model$learner$par.vals)
  return(xg_model$learner.model$evaluation_log %>% .[order(.[,2],decreasing = T),2] %>% head(1))
  
}

train_population<-function(pop, trainTask, nfold = 5, nrounds = 10, sem = 1234, FEVAL=NULL, eval_metric = "auc") {
  

#  xg.sal<-apply(pop[pop$gen==max(pop$gen),],1,makeGeneticLearner,train = train, FEVAL=FEVAL, 
#     eval_metric=eval_metric, nrounds=nrounds, nfold = nfold, sem = sem)
    
#  parms.fijo<-list(nrounds=nrounds, nfold = nfold, sem = sem,FEVAL=FEVAL, eval_metric=eval_metric)
  ifelse(is.null(FEVAL),
        {parms.fijo<-list(nrounds=nrounds, nfold = nfold, eval_metric=eval_metric)},
        {parms.fijo<-list(nrounds=nrounds, nfold = nfold, feval=FEVAL)})
  
  
  xg.sal<-NULL
  
  test<-pop %>% filter(gen==max(gen)) %>% select(-gen) %>% apply(.,1,as.list)
  for(x in test) {
    parms<-x  %>% append(.,parms.fijo)
    xg.sal<-rbind(xg.sal,makeGeneticLearner2(parms, trainTask, sem))
  }
  gc()
  return(mutate(pop,fitness=unlist(xg.sal)))
}


crossover<-function(pop, nro_hijos, sem =1234) {
  out<-NULL
  padres_mat<-pop %>% filter(gen==max(pop$gen)) %>% arrange(desc(fitness)) %>% head(2)
  
  set.seed(sem)
  for (i in 1:(nro_hijos/2)) {
  
    order=sample(1:nrow(limits),nrow(limits),replace = F)
    nro_param=sample(1:(nrow(limits)-1),1)
    c1=c(rep(1,nro_param),rep(2,nrow(limits)-nro_param)) %>% .[order]
    c2=c(rep(2,nro_param),rep(1,nrow(limits)-nro_param)) %>% .[order]
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
#    set.seed(sem)
    ind<-sample(1:6,1)
    pop[i,ind]<-rdm[i,ind]
  }
  return(pop)
}


# pop<-initilialize_population(4)
# 
# library(data.table)
# febrero  <-  fread("M:\\datasets\\dias\\201802_dias.txt", header=TRUE, sep="\t")
# abril  <-  fread("M:\\datasets\\dias\\201804_dias.txt", header=TRUE, sep="\t")
# 
# 
# febrero$clase_binaria <- as.factor(ifelse(febrero$clase_ternaria == "BAJA+2", 1, 0))
# abril$clase_binaria <- as.factor(ifelse(abril$clase_ternaria == "BAJA+2", 1, 0))
# 
# febrero$clase_ternaria <- NULL
# abril$clase_ternaria<-NULL
# 
# str(febrero)
# train   <- xgboost::xgb.DMatrix( data = data.matrix(febrero),  label = clases_febrero, missing=NA )
# mdl<-train_population(pop,train,2,2)
# nuevos<-crossover(mdl,6)

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
  # FEVAL<-fganancia_logistic_xgboost
  
  
#  clases <- ifelse(data$clase_ternaria == "BAJA+2", 1, 0)
#  train <- xgboost::xgb.DMatrix( data = data.matrix(data),  label = labels, missing=NA )
  
  trainTask <- mlr::makeClassifTask(data = data,target = campo_clase, positive = 1)
  trainTask <- mlr::normalizeFeatures(trainTask,method = "standardize")
  
  getParamSet("classif.xgboost")
  configureMlr(on.par.without.desc = "quiet")
  
  t0 <- Sys.time()
  

  ifelse(file.exists("genetical-exec.csv"),{
    pop<-read.csv("genetical-exec.csv")
    
    ifelse(max(pop$gen)>=max_gen, {
      
      pop<-initilialize_population(pop_inicial,sem)
      pop<-train_population(pop,trainTask,nfold,nrounds, sem, FEVAL, eval_metric)
      pop.family<-pop
    },{
      pop.family<-pop
    })
  },{
    pop<-initilialize_population(pop_inicial,sem)
    pop<-train_population(pop,trainTask,nfold,nrounds, sem, FEVAL, eval_metric)
    pop.family<-pop
  })
  
  while (max(pop$gen)+1<=max_gen) {
    pop<-crossover(pop,nro_hijos,sem)
    pop<-train_population(pop,trainTask,nfold,nrounds, sem, FEVAL, eval_metric)
    pop.family<-rbind(pop.family,pop)
    cat(paste("---- Saving state - current generation =",max(pop$gen)))
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










