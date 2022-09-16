# MCMC simulation

# data load
data <- read.csv('../data_aligned.csv')

# compliance
CS <- c('compliance_1','compliance_2',
        'compliance_4','compliance_5','compliance_6',
        'compliance_8')

# pss
pss.vars <- c('perceived_stress_sca_1','perceived_stress_sca_2','perceived_stress_sca_3',
              'perceived_stress_sca_4','perceived_stress_sca_5','perceived_stress_sca_6',
              'perceived_stress_sca_7','perceived_stress_sca_8','perceived_stress_sca_9',
              'perceived_stress_sca_10')


# simulation function

# simulation function
simulation_CLE <- function(times,n,data,n.include,seed=1){
  # get data
  data.mi <- data
  items.PSUP <- c('compliance_1','compliance_2',
                  'compliance_4','compliance_5','compliance_6',
                  'compliance_8')
  
  compliance <- items.PSUP
  
  # list for return
  cor.mean <- rep(0,times)
  cor.var <- rep(0,times)
  R2.loading <- rep(0,times)
  R2.intercept <- rep(0,times)
  
  # simulation replication
  set.seed(seed)
  # repeat
  j <- 1
  while(1){
    
    G <- 28 # groups
    I <- 6 # items
    
    
    # lambda, nu, and error_var
    err_var.cle <- matrix(1, nrow=G,ncol=I)
    # simulate data
    # enter group mu and sigma
    data.mi$COMP <- rowMeans(data.mi[,compliance])
    mu<-scale(aggregate(x=data.mi$COMP,
                        by = list(data.mi$UserLanguage),
                        FUN=mean, na.rm=T)[,2])[,1]
    sigma <- (aggregate(x=data.mi$COMP,
                        by = list(data.mi$UserLanguage),
                        FUN=sd, na.rm=T)[,2])
    N <- rep(n,G)
    dat <- invariance_alignment_simulate(
      par.cle$nu,par.cle$lambda,err_var.cle,mu,sigma,N
    )

    # directly extract from cfa
    
    cfa.model.PSUP <-'PSUP =~ compliance_1 + compliance_2+compliance_4+
  compliance_5 + compliance_6 + compliance_8'
    
    cfa.test <- cfa(cfa.model.PSUP,dat,group='group',estimator='WLSMV')
    params <- parameterEstimates(cfa.test)
    lambda <-  matrix( params[ params$op=="=~", "est"], nrow=28,  byrow=T)
    nu <- matrix( params[ params$op=="~1"  & params$se !=0, "est" ], nrow=28,  byrow=T)
    err_var <- matrix( params[ params$op=="~~"  & params$rhs=='PSUP', "est" ], nrow=28,  byrow=T)
    
    #flag <- sum(par.simul$err_var < 0)
    flag <- sum(err_var < 0)
    if (flag > 0){
      next
    }
    
    
    tryCatch(
      expr={
        mod1.simul <- invariance.alignment(lambda = lambda, 
                                           nu =  nu, align.scale = c(0.2, 0.4), 
                                           align.pow = c(0.25, 0.25))}
      ,
      error = function(e){
        flag <- 1
      },
      warning = function(wan){
        flag <- 1
      })
    
    if (exists('mod1.simul')==FALSE){
      flag <- 1
    }
    if (flag > 0){
      next
    }
    
    # true vs aligned scores
    
    
    
    cfa.model.simul <- cfa.model.PSUP
    cfa.simul <- cfa(cfa.model.simul,dat,estimator='WLSMV',group='group',
                     group.equal=c('loadings','intercepts'),meanstructure=T)
    
    # get group mean
    params.simul <- parameterEstimates(cfa.simul)
    alpha.simul <- params.simul[(params.simul$op=='~1')&(params.simul$lhs=='PSUP'),'est']
    
    # group mean correlation (Muthen 2018)
    correlation <- corr.test(alpha.simul,mod1.simul$pars$alpha0,method='spearman')$r
    
    
    
    psi.simul <- params.simul[(params.simul$op=='~~')&(params.simul$lhs=='PSUP'),'est']
    correlation.psi <- corr.test(psi.simul,mod1.simul$pars$psi0,method='spearman')$r
    
    cor.mean[j] <- correlation
    cor.var[j] <- correlation.psi
    
    R2.loading[j] <- mod1.simul$es.invariance['R2',1]
    R2.intercept[j] <- mod1.simul$es.invariance['R2',2]
    
    # minus error detection
    if (correlation < 0 ){
      flag <- 1
      
    }
    if (correlation.psi < 0){
      flag <- 1
    }
    if (mod1.simul$es.invariance['R2',1] < 0){
      flag <- 1
    }
    if (mod1.simul$es.invariance['R2',2] < 0){
      flag <- 1
    }
    
    
    if (flag ==0){
      j <- j + 1
    }
    
    # stop
    if (j >= times+1){
      break
    }
  }
  # make matrix
  # with error flag
  
  to.return <-cbind(cor.mean,cor.var,R2.loading,R2.intercept)
  to.return <- data.matrix(to.return)
  
  message  (sprintf('%d/%d Done',j,times))
  
  return(to.return)
  
}

# use five cores

library(foreach)
library(parallel)
library(doParallel)
library(sirt)

cores <- 5
times <- 500

# start with compliance
data.mi<-data

# extract parameters
par.cle <- invariance_alignment_cfa_config(dat = data.mi[,CS], 
                                           group = data.mi$UserLanguage)

cl <- parallel::makeCluster(cores,type='FORK')
doParallel::registerDoParallel(cl)

# n = 100

start_100 <-Sys.time()
now <- foreach (i = seq(1,times)) %dopar%{
  # get result
  simulation_CLE(1,100,data.mi,28,i)
  #  message(sprintf('%d',i))
}
end_100<-Sys.time()
elapsed_100 <- end_100 - start_100
# merge result
for (i in 1:times){
  if (i == 1){
    simulate_100 <- now[[1]]
  }else{
    simulate_100 <- rbind(simulate_100,now[[i]])
  }
}
# save n = 100
write.csv(data.frame(simulate_100),file='simulate_100_cs.csv',row.names = F)


# n=200
# multicore processing for n=200
start_200 <-Sys.time()
now <- foreach (i = seq(1,times)) %dopar%{
  # get result
  simulation_CLE(1,200,data.mi,28,i)
  #  message(sprintf('%d',i))
}
end_200<-Sys.time()
elapsed_200 <- end_200 - start_200
# merge result
for (i in 1:times){
  if (i == 1){
    simulate_200 <- now[[1]]
  }else{
    simulate_200 <- rbind(simulate_200,now[[i]])
  }
}
# save n = 200
write.csv(data.frame(simulate_200),file='simulate_200_cs.csv',row.names = F)

# n= 500
start_500 <-Sys.time()
now <- foreach (i = seq(1,times)) %dopar%{
  # get result
  simulation_CLE(1,500,data.mi,28,i)
  #  message(sprintf('%d',i))
}
end_500<-Sys.time()
elapsed_500 <- end_500 - start_500
# merge result
for (i in 1:times){
  if (i == 1){
    simulate_500 <- now[[1]]
  }else{
    simulate_500 <- rbind(simulate_500,now[[i]])
  }
}
# save n = 200
write.csv(data.frame(simulate_500),file='simulate_500_cs.csv',row.names = F)
# stop cluster
parallel::stopCluster(cl)

