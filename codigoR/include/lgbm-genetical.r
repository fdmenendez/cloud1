
require(dplyr)
require(Matrix)

kbest_of <- 3
kmin_corte <- 0.005
kmax_corte <- 0.1
kcortes<-seq(kmin_corte,kmax_corte,.0001)

vbest_model <- NULL
vbest_win <- 0
vprob_corte <- .025

kmax_bin               <-     32
kbagging_fraction      <-      1.0
kmin_child_weight      <-      8
kinit_score            <-      0.005
kmax_depth             <-     10

limits=data.frame(
  params=c("feature_fraction",
           "learning_rate",
           "lambda_l1",
           "lambda_l2",
           "min_gain_to_split",
           "min_data_in_leaf",
           "num_leaves",
           #           "scale_pos_weight",
           "parm_prob_corte"),
  
  low=c(.04,
        0.01,
        0.1,
        0.1,
        0.1,
        1,
        10,
        #        1,
        .015),
  
  uppr=c(1,
         .10,
         1,
         10,
         1,
         100,
         512,
         #         5000,
         .030),
  
  dec=c(2,
        2,
        1,
        1,
        1,
        0,
        0,
        #        0,
        3),
  stringsAsFactors = FALSE
)

testigo<-c(.5,.02,.5,10,0,20,255,.025)

fganancia_logistic_lightgbm_binaria   <- function(probs, clases) 
{
  
  vlabels <- lightgbm::getinfo(clases, "label")
  
  gan <-sum(   (probs > vprob_corte  ) * 
                 ifelse( vlabels== 1, 11700, -300 ),
               na.rm = TRUE   
  )
  
  
  return(  list( name = "ganancia", 
                 value =  ifelse(  is.na(gan) , 0, gan ) ,
                 higher_better= TRUE 
  )
  )
}

calcular_ganancia   <- function(probs, clases) 
{
  
  #  vlabels <- xgboost::getinfo(clases, "label")
  
  out<-data.frame(prob_corte=as.numeric(), fitness=as.numeric())
  gan_bruta<-ifelse( clases == 1, 11700, -300 )
  for (p in kcortes) {
    sal<-data.frame(max_prob_corte=p,fitness=sum((probs > p) * gan_bruta))
    #  print(paste(sal,"\n"))
    out<-rbind(out,sal)
  }
  val<-out %>% filter(fitness==max(out$fitness)) %>% head(1) %>% 
    mutate(ganancia_parm=sum(   (probs > vprob_corte  ) * 
                                  gan_bruta,na.rm = TRUE)) %>%
    select(3,1,2)
  
  
  return(  val )
}

initilialize_population <- function(nbr_individuals) {
  
  #  set.seed(sem)
  out<-as.data.frame(apply(limits,1, function(x) round(runif(nbr_individuals, as.numeric(x["low"]),as.numeric(x["uppr"])),as.numeric(x["dec"]))))
  out<-rbind(out,testigo)
  out<-cbind(out,rep(1,nbr_individuals+1))
  colnames(out)<-c(as.character(limits$params),c("gen"))
  rownames<-NULL
  return(out)
}


makeGeneticLearner2 <- function (parms, dgeneracion, dvalidacion, data_predict) {
  
  #    set.seed(sem)
  # vals<-c(
  #   list(objective = "binary"
  #        ,data = dgeneracion
  #        ,valids = list(validacion=dvalidacion)
  #        ,init_score= mean(lightgbm::getinfo(dgeneracion, "label"))
  #        ,bagging_fraction = kbagging_fraction
  #        ,max_bin = kmax_bin
  #        ,min_child_weight=kmin_child_weight 
  #        ,verbose = 2
  #        ,max_depth=10
  #        ,is_unbalance = TRUE
  #   ),
  #   as.list(parms))
  
  #  print(vals)
  
  lgb_model <- lightgbm::lgb.train(
    data = dgeneracion,  
    objective="binary",
    valids= list(validacion=dvalidacion),
    seed = parms$seed, 
    num_iterations= parms$nrounds,
    early_stopping_round = parms$early_stopping_round,
    init_score = kinit_score ,
    eval = parms$eval ,
    metric = "auc",
#    verbose = -1,
    bagging_fraction = kbagging_fraction, 
    feature_fraction = parms$feature_fraction, 
    learning_rate = parms$learning_rate,
    min_child_weight=kmin_child_weight,
    max_depth=kmax_depth,
    lambda_l1 = parms$lambda_l1, 
    min_data_in_leaf = parms$min_data_in_leaf, 
    max_bin = kmax_bin,
    num_leaves = parms$num_leaves,
    lambda_l2 = parms$lambda_l2, 
    min_gain_to_split = parms$min_gain_to_split,
    is_unbalance = TRUE
  )
  
  pred<-predict(lgb_model,data_predict)
  
#  sal<-lgb_model$best_score
  #  sal<-fmetrica_ganancia_lightgbm( vprob_corte, pred, lightgbm::getinfo(dvalidacion, "label") )
  
  sal<-calcular_ganancia(pred,lightgbm::getinfo(dvalidacion, "label"))
  
  if (sal$fitness >= vbest_win) {
    vbest_model<<-lgb_model
    vbest_win<<-sal$fitness
  }
  
  rm(lgb_model)
  return(sal)
  
}

train_population<-function(pop, dgeneracion, dvalidacion, nrounds = 50, 
                           early_stopping_round = 20, seed = 1234, FEVAL=NULL, 
                           eval_metric = "auc",data_predict) {
  
  
  ifelse(is.null(FEVAL),
         {parms.fijo<-list(nrounds=nrounds, 
                           early_stopping_round = early_stopping_round,
                           metric=eval_metric, seed=seed)},
         {parms.fijo<-list(nrounds=nrounds, 
                           early_stopping_round = early_stopping_round,
                           metric=eval_metric,
                           eval=FEVAL, seed=seed)})
  
  
  lgb.sal<-NULL
  
  test<-pop %>% filter(gen==max(gen)) %>% select(-gen) %>% apply(.,1,as.list)
  for(x in test) {
    vprob_corte<-x$parm_prob_corte
    parms<-x  %>% append(.,parms.fijo)
    parms$prob_corte<-NULL
    lgb.sal<-rbind(lgb.sal,makeGeneticLearner2(parms, dgeneracion, dvalidacion, data_predict))
    gc()
  }
  #  return(mutate(pop,fitness=(xg.sal)))
  return(cbind(pop,lgb.sal))
  
}


crossover<-function(pop, nro_hijos) {
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
  out.mtd<-mutacion(out)
  colnames(out.mtd)<-c(as.character(limits$params))
  out.mtd<-rbind(as.data.frame(out.mtd),as.data.frame(padres_mat[,1:nrow(limits)]))
  out.full<- unique(out.mtd) %>% mutate(gen=max(pop$gen)+1)
  
  
  return(out.full)
}

mutacion<- function(pop) {
  
  #  set.seed(sem)
  rdm=as.matrix(apply(limits,1, function(x) round(runif(nrow(pop), as.numeric(x["low"]),as.numeric(x["uppr"])),as.numeric(x["dec"]))))
  for (i in 1:nrow(pop)) {
    ind<-sample(1:nrow(limits),1)
    pop[i,ind]<-rdm[i,ind]
  }
  return(pop)
}




train_gen_lightgbm<-function(data.trn, data.oot, campo_clase, pop_inicial, nro_hijos, max_gen, 
                            nrounds = 500,early_stopping_round=50, seed = 1234, 
                            FEVAL=NULL,eval_metric = "auc") {
  
  
  # source("~/cloud/cloud1/codigoR/include/utils.r")
  # source("~/cloud/cloud1/codigoR/include/metrica.r")
  # data.trn<-as.data.frame(sample.datasets("201712_","~/cloud/cloud1/datasets/exthist/","exthist.rds","Y","lgb",.3,443501))
  # data.oot<- data.table::as.data.table(readRDS(paste0("~/cloud/cloud1/datasets/exthist/","201802_exthist.rds")))
  # data.oot<-clean.up.oot.lgb(data.oot)
  # 
  # 
  # #data.trn <- dataset_training
  # #data.oot <- dataset.oot
  # campo_clase<-"clase_binaria"
  # pop_inicial<-4
  # nro_hijos<-2
  # max_gen<-3
  # #  nfold<-3
  # early_stopping_round <-300
  # nrounds<-1000
  # seed=443501
  # eval_metric<-"auc"
  # FEVAL<-fganancia_logistic_lightgbm_binaria
  
  
  
  
  cat("---- Genetical Lightgbm -----------\n")
  cat("---- PARMS: -----------------------\n")
  cat(paste("---- campo_clase =",campo_clase,"\n"))
  cat(paste("---- pop_inicial =",pop_inicial,"\n"))
  cat(paste("---- nro_hijos =",nro_hijos,"\n"))
  cat(paste("---- max_gen =",max_gen,"\n"))
  cat(paste("---- nrounds =",nrounds,"\n"))
  cat(paste("---- early_stopping_round =",early_stopping_round,"\n"))
  cat(paste("---- seed =",seed,"\n"))
  cat(paste("---- FEVAL =",as.character(substitute(FEVAL)),"\n"))
  cat(paste("---- eval_metric =",eval_metric,"\n"))
  
  #genero one-hot enconding
  options(na.action='na.pass')
  formula  <- formula(paste("~ .-1"))
  
  dataset_unido_matrix  = model.matrix(formula, data = data.trn[,-which(names(data.trn) == campo_clase)])
  dataset_generacion_sinclase_sparse = as(dataset_unido_matrix, "dgCMatrix")
  rm(dataset_unido_matrix)
  
  dgeneracion  <-   lightgbm::lgb.Dataset( data  = data.matrix(dataset_generacion_sinclase_sparse),
                                           label = data.trn[ , campo_clase], 
                                           missing=NA,
                                           free_raw_data=FALSE 
  )
  
  
  dataset_unido_matrix  = model.matrix(formula, data = data.oot[,-which(names(data.oot) == campo_clase)])
  dataset_validacion_sinclase_sparse = as(dataset_unido_matrix, "dgCMatrix")
  rm(dataset_unido_matrix)
  
  dvalidacion  <-   lightgbm::lgb.Dataset( data  = data.matrix(dataset_validacion_sinclase_sparse),
                                           label = data.oot[ , campo_clase],
                                           missing=NA,
                                           free_raw_data=FALSE
  )
  
  t0 <- Sys.time()
  
  set.seed(seed)
  ifelse(file.exists("lgb-genetical-exec.csv"),{
    pop<-read.csv("lgb-genetical-exec.csv")
    
    ifelse(max(pop$gen)>=max_gen, {
      cat("-- Genetical: Starting from scratch\n\n")
      pop<-initilialize_population(pop_inicial)
      pop<-train_population(pop,dgeneracion,dvalidacion,nrounds,early_stopping_round, seed, FEVAL, eval_metric,dataset_validacion_sinclase_sparse)
      pop.family<-pop
    },{
      cat(paste("-- Genetical: Re-starting from generation\n\n",max(pop$gen)+1,"\n"))
      pop.family<-pop
    })
  },{
    cat("-- Genetical: Starting from scratch\n\n")
    pop<-initilialize_population(pop_inicial)
    pop<-train_population(pop,dgeneracion,dvalidacion,nrounds,early_stopping_round, seed, FEVAL, eval_metric,dataset_validacion_sinclase_sparse)
    pop.family<-pop
  })
  
  while (max(pop$gen)+1<=max_gen) {
    pop<-crossover(pop,nro_hijos)
    pop<-train_population(pop,dgeneracion,dvalidacion,nrounds,early_stopping_round, seed, FEVAL, eval_metric,dataset_validacion_sinclase_sparse)
    pop.family<-rbind(pop.family,pop)
    cat(paste("--- Saving state - current generation =",max(pop$gen),"\n"))
    write.csv(pop.family, "lgb-genetical-exec.csv",row.names = F)
  }
  t1 <- Sys.time()
  
  result_lst<-list(
    "best_model" = vbest_model,
    "best_model_parms" = pop.family %>% mutate(fitness = round(fitness,0)) %>% arrange(desc(fitness)) %>% head(1),
    "full_family" = pop.family %>% mutate(fitness = round(fitness,0)),
    "tiempo"   =  as.numeric(  t1 - t0, units = "secs")
  )
  return(result_lst)
}

