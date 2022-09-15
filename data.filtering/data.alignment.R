# alignment
# PSS
# Compliance

library(lavaan)
library(sirt)
library(MASS)
library(psych)

# compliance
CS <- c('compliance_1','compliance_2',
        'compliance_4','compliance_5','compliance_6',
        'compliance_8')

# pss
pss.vars <- c('perceived_stress_sca_1','perceived_stress_sca_2','perceived_stress_sca_3',
              'perceived_stress_sca_4','perceived_stress_sca_5','perceived_stress_sca_6',
              'perceived_stress_sca_7','perceived_stress_sca_8','perceived_stress_sca_9',
              'perceived_stress_sca_10')



#function for factor score adjustment
aligned.factor.scores <- function(lambda,nu,y){
  #calculate inverse matrix
  lambda1 <- ginv((lambda))
  #create matrix for nu
  ns <- nrow(y)
  nus <- matrix(nu,nrow=ns, ncol=length(nu), byrow=T)
  # y - nu
  y_nu <- y - nu
  # f = inv(lambda)*(y-nu)
  F <- lambda1 %*% t(as.matrix(y_nu))
}

# load data
data.mi <- read.csv('../data_filtered.csv')

# set and examine fitmeasures
fits <- c('rmsea.scaled','srmr','cfi.scaled','tli.scaled')

#####
# 1. compliance alignment

# cfa
cfa.model.cs <- 'CS =~ compliance_1 + compliance_2+compliance_4+
  compliance_5 + compliance_6 + compliance_8'
cfa.whole.cs<- cfa(model=cfa.model.cs,data=data.mi,estimator='WLSMV', group = 
                       'UserLanguage')
fitMeasures(cfa.whole.cs)[fits]
# rmsea.scaled         srmr   cfi.scaled   tli.scaled 
# 0.07202464   0.04819525   0.94399486   0.90665810 

# configural acceptable -> metric
cfa.metric.cs <- cfa(model=cfa.model.cs,data=data.mi,estimator='WLSMV', group = 
                       'UserLanguage', group.equal='loadings')
fitMeasures(cfa.metric.cs)[fits]
# rmsea.scaled         srmr   cfi.scaled   tli.scaled 
# 0.08266381   0.08209318   0.88670599   0.87704526
# unacceptable -> alingment

# do alignment
#extract parameters
par.CS <- invariance_alignment_cfa_config(dat = data.mi[,CS],
                                          group = data.mi$UserLanguage)
#do alignment
mod1.CS <- invariance.alignment(lambda = par.CS$lambda, nu =
                                  par.CS$nu, align.scale = c(0.2, 0.4), align.pow = c(0.25, 0.25))
mod1.CS$es.invariance['R2',]
#loadings intercepts 
#0.9680318  0.9957155  well done


#####
# 2. PSS

# general CFA: PSS
cfa.model.pss <- 'PSS =~ perceived_stress_sca_1 + perceived_stress_sca_2+
  perceived_stress_sca_3 + perceived_stress_sca_4 + perceived_stress_sca_5+
  perceived_stress_sca_6 + perceived_stress_sca_7 + perceived_stress_sca_8+
  perceived_stress_sca_9 + perceived_stress_sca_10'
cfa.whole.pss <- cfa(model=cfa.model.pss,data=data.mi,estimator='WLSMV', group = 
                       'UserLanguage')
fitMeasures(cfa.whole.pss)[fits]
# msea.scaled         srmr   cfi.scaled   tli.scaled 
#  0.09364678   0.06235554   0.87683158   0.84164061 
# not good -> alignment

# measurement alignment test
# extract parameters
par.pss <- invariance_alignment_cfa_config(dat = data.mi[,pss.vars], 
                                           group = data.mi$UserLanguage,
                                           estimator='WLSMV')
# do alignment
mod1.pss <- invariance.alignment(lambda = par.pss$lambda, nu =
                                   par.pss$nu, align.scale = c(0.2, 0.4), align.pow = c(0.25, 0.25))
# test performance
mod1.pss$es.invariance['R2',]
#loadings intercepts 
# 0.9861811  0.9930769   Very good

# factor score calculation

for (i in 1:length(langs.include)){
  if (i == 1){
    # create new matrix
    data.aligned <- data.mi[data.mi$UserLanguage==langs.include[i],] 
    # aligned factor score
    F.CS <- aligned.factor.scores(mod1.CS$lambda.aligned[i,],
                                  mod1.CS$nu.aligned[i,],
                                  data.mi[data.mi$UserLanguage==langs.include[i],CS])
    F.pss <- aligned.factor.scores(mod1.pss$lambda.aligned[i,],
                                    mod1.pss$nu.aligned[i,],
                                    data.mi[data.mi$UserLanguage==langs.include[i],pss.vars])
    
    data.aligned$CS <-t(F.CS)
    data.aligned$PSS <-t(F.pss)
  }else
  {
    #bind
    current <- data.mi[data.mi$UserLanguage==langs.include[i],]
    F.CS <- aligned.factor.scores(mod1.CS$lambda.aligned[i,],
                                  mod1.CS$nu.aligned[i,],
                                  current[,CS])
    F.pss <- aligned.factor.scores(mod1.pss$lambda.aligned[i,],
                                   mod1.pss$nu.aligned[i,],
                                   current[,pss.vars])
    
    current$CS <-t(F.CS)
    current$PSS <-t(F.pss)
    
    data.aligned <- rbind(data.aligned,current)
  }
}

# save csv
write.csv(data.filtered,file='../data_aligned.csv',row.names = F)


