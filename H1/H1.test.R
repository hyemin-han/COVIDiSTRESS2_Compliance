# H1 test
# compliance ~ social norms
# with multiple imputations

library(brms)

load('H1.data.RData')

# standardize
for (i in 1:20){
  data.H1[[i]]$CS <- scale(data.H1[[i]]$CS)
  data.H1[[i]]$S1 <- scale(data.H1[[i]]$S1)
  data.H1[[i]]$S2 <- scale(data.H1[[i]]$S2)
}

# null model
prior.coef <- brms::prior(cauchy(0.,1),class='b')
b.test.0 <- brms::brm_multiple(CS ~ (1|residing_country),
                               data=data.H1, family = gaussian(),
                               cores=4,chains=4, save_pars = save_pars(all = T),
                               sample_prior ='yes', seed=1660415)
# random intercepts
b.test.1 <- brms::brm_multiple(CS ~ S1+S2+(1|residing_country),
                               data=data.H1, family = gaussian(),
                               cores=4,chains=4, save_pars = save_pars(all = T),
                               sample_prior ='yes', seed=1660415,prior=prior.coef)

# random slopes
b.test.2 <- brms::brm_multiple(CS ~ S1+S2+(1+S1+S2|residing_country),
                               data=data.H1, family = gaussian(),
                               cores=4,chains=4, save_pars = save_pars(all = T),
                               sample_prior ='yes', seed=1660415,prior=prior.coef)


## BFs
bf.10 <- bayes_factor(b.test.1,b.test.0,log=T)
bf.20 <- bayes_factor(b.test.2,b.test.0,log=T)
bf.21 <- bayes_factor(b.test.2,b.test.1,log=T)

# hypothesis testing
h.s1<- hypothesis(b.test.2,'S1=0')
h.s2<- hypothesis(b.test.2,'S2=0')

log(h.s1$hypothesis$Evid.Ratio)
log(h.s2$hypothesis$Evid.Ratio)

hats<-round(b.test.2$rhats,3)[,1:9]


##### 
# frequentist
library(lmerTest)
library(merTools) # cite merTools

# do fitting
mod.2.list <- lmerModList(CS ~ S1+S2+(1+S1+S2|residing_country),
                          data= data.H1)

'
Fixed Effects:
            estimate std.error statistic            df
(Intercept)    0.058     0.053     1.093 551228263.603
S1             0.282     0.018    15.318   4317505.332
S2             0.194     0.015    13.326   2462306.428'

# t-test for S1 and S2
dt(15.318, df = 4317505.332) #<.001
dt(13.326, df = 2462306.428) #<.001

# save
save.image('H1.result.RData')

# random slopes
# ROPE
library(bayestestR)
# get tables
btable.h1 <- describe_posterior(b.test.2, effect='random')
# save result
write.csv(as.data.frame(btable.h1),'H1_TABLE.csv')
