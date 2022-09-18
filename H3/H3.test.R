# H3
# CS ~ trust

library(psych)
library(BayesFactor)
library(lmerTest)
library(brms)
library(EMAtools)

# load data
data <- read.csv ('../data_aligned.csv')

# start with trust exploration

# data extract
CS <- c('compliance_1','compliance_2',
        'compliance_4','compliance_5','compliance_6',
        'compliance_8')
# pick required variables
data.BF <- data[,c('CS','age','gender',
                   'relationship_status','work_location','SSS_faml',
                   'cohabiting_adults_no','cohabiting_kids_no_1',
                   'cohabiting_kids_no_2','cohabiting_kids_no_3',
                   'residing_country',
                   'trust_1','trust_2','trust_3','trust_4',
                   'trust_5','trust_6','trust_7')]
data.BF <- na.omit(data.BF)
data.BF$residing_country <- as.factor(data.BF$residing_country)

# do Bayesian model selection
bs.H3 <- generalTestBF(CS ~ trust_1 +
                           trust_2 + trust_3 + trust_4 + trust_5 + trust_6 +
                           trust_7 + SSS_faml + age + gender + relationship_status+
                           work_location+cohabiting_adults_no+cohabiting_kids_no_1+
                           cohabiting_kids_no_2 + cohabiting_kids_no_3 + residing_country,
                         whichRandom = 'residing_country', neverExclude = c(
                                                                            'age','gender',
                                                                            'relationship_status','work_location','SSS_faml',
                                                                            'cohabiting_adults_no','cohabiting_kids_no_1',
                                                                            'cohabiting_kids_no_2','cohabiting_kids_no_3',
                                                                            'residing_country'   
                         ), data=data.BF,
                         multicore = F)

# highest BFs
head(bs.H3)
'
Bayes factor analysis
--------------
[1] trust_1 + trust_5 + trust_7 + SSS_faml + age + gender + relationship_status + work_location + cohabiting_adults_no + cohabiting_kids_no_1 +     cohabiting_kids_no_2 + cohabiting_kids_no_3 + residing_country               : 4.688544e+322 ±1.55%
[2] trust_2 + trust_5 + trust_7 + SSS_faml + age + gender + relationship_status + work_location + cohabiting_adults_no + cohabiting_kids_no_1 +     cohabiting_kids_no_2 + cohabiting_kids_no_3 + residing_country               : 3.884032e+322 ±1.76%
[3] trust_5 + trust_6 + trust_7 + SSS_faml + age + gender + relationship_status + work_location + cohabiting_adults_no + cohabiting_kids_no_1 +     cohabiting_kids_no_2 + cohabiting_kids_no_3 + residing_country               : 3.491201e+322 ±3.8%
[4] trust_2 + trust_5 + trust_6 + trust_7 + SSS_faml + age + gender + relationship_status + work_location + cohabiting_adults_no +     cohabiting_kids_no_1 + cohabiting_kids_no_2 + cohabiting_kids_no_3 +     residing_country : 3.368818e+322 ±2.33%
[5] trust_1 + trust_2 + trust_5 + trust_7 + SSS_faml + age + gender + relationship_status + work_location + cohabiting_adults_no +     cohabiting_kids_no_1 + cohabiting_kids_no_2 + cohabiting_kids_no_3 +     residing_country : 2.467574e+322 ±4.11%
[6] trust_1 + trust_5 + trust_6 + trust_7 + SSS_faml + age + gender + relationship_status + work_location + cohabiting_adults_no +     cohabiting_kids_no_1 + cohabiting_kids_no_2 + cohabiting_kids_no_3 +     residing_country : 1.142051e+322 ±2.21%

Against denominator:
  Intercept only 
---
Bayes factor type: BFlinearModel, JZS'
# trust 1 5 7

# do brms
data.test <- data

# standardize variables
data.test <- data
data.test$CS <- scale(data.test$CS)
data.test$SSS_faml <- scale(data.test$SSS_faml)
data.test$age <- scale(data.test$age)
data.test$trust_1 <- scale(data.test$trust_1)
data.test$trust_5 <- scale(data.test$trust_5)
data.test$trust_7 <- scale(data.test$trust_7)
data.test$cohabiting_adults_no <- scale(data.test$cohabiting_adults_no)
data.test$cohabiting_kids_no_1 <- scale(data.test$cohabiting_kids_no_1)
data.test$cohabiting_kids_no_2 <- scale(data.test$cohabiting_kids_no_2)
data.test$cohabiting_kids_no_3 <- scale(data.test$cohabiting_kids_no_3)

# group means to be added: pss, sss_faml, hc, trust_1, trust_5, trust_7, age
# sort by country
data.test <- data.test[order(data.test$residing_country),]

# relevel
data.test$work_location<-as.factor(data.test$work_location)
data.test$work_location <- relevel(data.test$work_location,'At my workplace (other than home)')
data.test$relationship_status <- as.factor(data.test$relationship_status)
data.test$relationship_status <- relevel(data.test$relationship_status,'Married')

# calculate group means
library("dplyr")
gm.SSS_faml <-  data.test %>%
  group_by(residing_country) %>%
  summarise(Mean = mean(c(SSS_faml), na.rm = T))
gm.age <-  data.test %>%
  group_by(residing_country) %>%
  summarise(Mean = mean(c(age), na.rm = T))
gm.trust_1 <-  data.test %>%
  group_by(residing_country) %>%
  summarise(Mean = mean(c(trust_1), na.rm = T))
gm.trust_5 <-  data.test %>%
  group_by(residing_country) %>%
  summarise(Mean = mean(c(trust_1), na.rm = T))
gm.trust_7 <-  data.test %>%
  group_by(residing_country) %>%
  summarise(Mean = mean(c(trust_1), na.rm = T))

# put!
for (i in 1:nrow(gm.SSS_faml)){
  data.test[data.test$residing_country == as.character(gm.SSS_faml[i,'residing_country']),
            'm.SSS_faml'] <- as.numeric(gm.SSS_faml[i,2])
  data.test[data.test$residing_country == as.character(gm.SSS_faml[i,'residing_country']),
            'm.age'] <- as.numeric(gm.age[i,2])
  data.test[data.test$residing_country == as.character(gm.SSS_faml[i,'residing_country']),
            'm.trust_1'] <- as.numeric(gm.trust_1[i,2])
  data.test[data.test$residing_country == as.character(gm.SSS_faml[i,'residing_country']),
            'm.trust_5'] <- as.numeric(gm.trust_5[i,2])
  data.test[data.test$residing_country == as.character(gm.SSS_faml[i,'residing_country']),
            'm.trust_7'] <- as.numeric(gm.trust_7[i,2])
}



# null model/intercept/full(slopes)
prior.coef <- brms::prior(cauchy(0.,1),class='b')
b.test.0 <- brms::brm(CS ~ SSS_faml + age + gender + relationship_status + work_location + 
                        cohabiting_adults_no + cohabiting_kids_no_1 + cohabiting_kids_no_2 + 
                        cohabiting_kids_no_3 +(1 | residing_country) ,
                      data=data.test, family = gaussian(),
                      cores=4,chains=4, save_pars = save_pars(all = T),
                      sample_prior ='yes', seed=1660415, iter=10000,
                      prior=prior.coef)
b.test.1<- brms::brm(CS ~  SSS_faml + age + gender + relationship_status + work_location + 
                       cohabiting_adults_no + cohabiting_kids_no_1 + cohabiting_kids_no_2 + 
                       cohabiting_kids_no_3 + (1 | residing_country) +trust_1 + 
                       trust_5 + trust_7 + 
                        m.age + m.SSS_faml + m.trust_1 + m.trust_5 + m.trust_7,
                     data=data.test, family = gaussian(),
                     cores=4,chains=4, save_pars = save_pars(all = T),
                     sample_prior ='yes', seed=1660415,prior=prior.coef,
                     iter=4000)
b.test.2<- brms::brm(CS ~ SSS_faml + age + gender + relationship_status + work_location + 
                       cohabiting_adults_no + cohabiting_kids_no_1 + cohabiting_kids_no_2 + 
                       cohabiting_kids_no_3 + (1+trust_1+
                                                 trust_5+trust_7| residing_country) +trust_1 + 
                       trust_5 + trust_7 + 
                        m.age + m.SSS_faml +  m.trust_1 + m.trust_5 + m.trust_7,
                     data=data.test, family = gaussian(),
                     cores=4,chains=4, save_pars = save_pars(all = T),
                     sample_prior ='yes', seed=1660415,prior=prior.coef,
                     iter=10000)

# bfs
bf.10 <- bayes_factor(b.test.1,b.test.0,log=T) # 429.46078
bf.20 <- bayes_factor(b.test.2,b.test.0,log=T) # 495.74695
bf.21 <- bayes_factor(b.test.2,b.test.1,log=T) # 66.27347

# random slope model
# hypothesis
h.trust_1<-hypothesis(b.test.2,'trust_1=0')
h.trust_5<-hypothesis(b.test.2,'trust_5=0')
h.trust_7<-hypothesis(b.test.2,'trust_7=0')

log(1/h.trust_1$hypothesis$Evid.Ratio) # -1.138503 # not well supported
log(1/h.trust_5$hypothesis$Evid.Ratio) # 35.55586
log(1/h.trust_7$hypothesis$Evid.Ratio) # 34.91366

# random slopes
# ROPE
library(bayestestR)

# get tables
btable.h2 <- describe_posterior(b.test.2, effect='random')
# save result
write.csv(as.data.frame(btable.h2),'H3_TABLE.csv')

# save
save.image('H3.test.RData')
