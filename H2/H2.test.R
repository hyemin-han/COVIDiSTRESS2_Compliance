# H2 test
library(psych)
library(BayesFactor)
library(doMC)
library(lmerTest)

# compliance
CS <- c('compliance_1','compliance_2',
        'compliance_4','compliance_5','compliance_6',
        'compliance_8')

# pss
pss.vars <- c('perceived_stress_sca_1','perceived_stress_sca_2','perceived_stress_sca_3',
              'perceived_stress_sca_4','perceived_stress_sca_5','perceived_stress_sca_6',
              'perceived_stress_sca_7','perceived_stress_sca_8','perceived_stress_sca_9',
              'perceived_stress_sca_10')


# first, load the aligned data
data <- read.csv ('../data_aligned.csv')
#cs
psych::alpha(data[,CS]) #.79
#pss
psych::alpha(data[,pss.vars],check.keys = T) #.87

# cronbach alpha for latent variable values
psych::alpha()

# calculate moral values
data$hc <- rowSums(data[,c('moral.values_1_midneutral','moral.values_7_midneutral')])/2
data$fr <- rowSums(data[,c('moral.values_2_midneutral','moral.values_8_midneutral')])/2
data$il <- rowSums(data[,c('moral.values_3_midneutral','moral.values_9_midneutral')])/2
data$ar <- rowSums(data[,c('moral.values_4_midneutral','moral.values_10_midneutral')])/2
data$ps <- rowSums(data[,c('moral.values_5_midneutral','moral.values_11_midneutral')])/2

# deal with work_location na -> no job?
data[is.na(data$work_location),'work_location']<-'NA'

# do Bayesian model selection
# pick required variables
data.BF <- data[,c('CS','PSS','hc','fr','il','ar','ps','age','gender',
                   'relationship_status','work_location','SSS_faml',
                   'cohabiting_adults_no','cohabiting_kids_no_1',
                   'cohabiting_kids_no_2','cohabiting_kids_no_3',
                   'residing_country',
                   'trust_1','trust_2','trust_3','trust_4',
                   'trust_5','trust_6','trust_7')]
data.BF <- na.omit(data.BF)
data.BF$residing_country <- as.factor(data.BF$residing_country)

# do Bayesian model selection
# CS ~ trust + moral_value + PSS + SSS_fam
# take too long -> another method.
while(0){
bs.H2 <- generalTestBF(CS ~ PSS + hc + fr + il + ar + ps + trust_1 +
                         trust_2 + trust_3 + trust_4 + trust_5 + trust_6 +
                         trust_7 + SSS_faml + age + gender + relationship_status+
                         work_location+cohabiting_adults_no+cohabiting_kids_no_1+
                         cohabiting_kids_no_2 + cohabiting_kids_no_3 + residing_country,
                       whichRandom = 'residing_country', neverExclude = c('PSS',
                         'age','gender',
                         'relationship_status','work_location','SSS_faml',
                         'cohabiting_adults_no','cohabiting_kids_no_1',
                         'cohabiting_kids_no_2','cohabiting_kids_no_3',
                         'residing_country'   
                       ), data=data.BF,
                       multicore = F)
}

# test with lmer (brutal force)
lmer.null <- lmer(CS ~ PSS + SSS_faml + age + gender + relationship_status+
                    work_location+cohabiting_adults_no+cohabiting_kids_no_1+
                    cohabiting_kids_no_2 + cohabiting_kids_no_3 + (1|residing_country),
                  data=data.BF)
# create formula and test
# create all possible formular
formulas <- NULL
formula.base <- 'CS ~ PSS + SSS_faml + age + gender + relationship_status+
                    work_location+cohabiting_adults_no+cohabiting_kids_no_1+
                    cohabiting_kids_no_2 + cohabiting_kids_no_3 + (1|residing_country)'
vars <- c('trust_1','trust_2','trust_3','trust_4','trust_5','trust_6','trust_7',
          'hc','fr','il','ar','ps')

count <- 1
# combination from 1 to 12
for (i in 1:12){
  # the current combination
  current <- combn(vars,i)
  current.num <- ncol(current)
  for (j in 1:current.num){
    # create formula
    current.formula <- ' '
    current.row <- length(current[,j])
    # one element
    #if (is.null(current.row)){
    #  current.row <- 1
    #}
    for (k in 1:current.row){
      current.formula <- sprintf ('%s + %s', current.formula ,current[k,j])
    }
    # add to base
    formulas[[count]] <- formula(sprintf('%s + %s',formula.base,current.formula))
    count <- count + 1
  }
  
}

# do all test
results<-NULL
BFs <- rep(0,(count-1))
result.null <- lmer(formula.base,data.BF)

for (i in 1:(count-1))
{
  # lmer
  now <- lmer(formulas[[i]],data.BF)
  # calc bayes factor
  BFs[i] <- exp (( BIC(now) - BIC(result.null) )/2)
  message(i)
}



# compare with full
result.full <- lmer(CS ~ PSS + SSS_faml + age + gender + relationship_status+
                      work_location+cohabiting_adults_no+cohabiting_kids_no_1+
                      cohabiting_kids_no_2 + cohabiting_kids_no_3 + (1|residing_country)+
                      trust_1+trust_2+trust_3+trust_4+trust_5+trust_6+trust_7+
                      hc+fr+il+ar+ps,data=data.BF)

# logarithm
logBFs <- -1 * log(BFs)

# max logBF = logBFs[414] = 269.322
# CS ~ PSS + SSS_faml + age + gender + relationship_status + work_location + 
#cohabiting_adults_no + cohabiting_kids_no_1 + cohabiting_kids_no_2 + 
#  cohabiting_kids_no_3 + (1 | residing_country) + +trust_1 + 
#  trust_5 + trust_7 + hc


# do brms
library(brms)

# standardize variables
data.test <- data
data.test$PSS <- scale(data.test$PSS)
data.test$CS <- scale(data.test$CS)
data.test$SSS_faml <- scale(data.test$SSS_faml)
data.test$age <- scale(data.test$age)
data.test$hc <- scale(data.test$hc)
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
gm.PSS <-  data.test %>%
  group_by(residing_country) %>%
  summarise(Mean = mean(c(PSS), na.rm = T))
gm.SSS_faml <-  data.test %>%
  group_by(residing_country) %>%
  summarise(Mean = mean(c(SSS_faml), na.rm = T))
gm.age <-  data.test %>%
  group_by(residing_country) %>%
  summarise(Mean = mean(c(age), na.rm = T))
gm.hc <-  data.test %>%
  group_by(residing_country) %>%
  summarise(Mean = mean(c(hc), na.rm = T))
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
for (i in 1:nrow(gm.PSS)){
  data.test[data.test$residing_country == as.character(gm.PSS[i,'residing_country']),
            'm.PSS'] <- as.numeric(gm.PSS[i,2])
  data.test[data.test$residing_country == as.character(gm.PSS[i,'residing_country']),
            'm.SSS_faml'] <- as.numeric(gm.SSS_faml[i,2])
  data.test[data.test$residing_country == as.character(gm.PSS[i,'residing_country']),
            'm.age'] <- as.numeric(gm.age[i,2])
  data.test[data.test$residing_country == as.character(gm.PSS[i,'residing_country']),
            'm.hc'] <- as.numeric(gm.hc[i,2])
  data.test[data.test$residing_country == as.character(gm.PSS[i,'residing_country']),
            'm.trust_1'] <- as.numeric(gm.trust_1[i,2])
  data.test[data.test$residing_country == as.character(gm.PSS[i,'residing_country']),
           'm.trust_5'] <- as.numeric(gm.trust_5[i,2])
  data.test[data.test$residing_country == as.character(gm.PSS[i,'residing_country']),
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
b.test.1<- brms::brm(CS ~ PSS + SSS_faml + age + gender + relationship_status + work_location + 
    cohabiting_adults_no + cohabiting_kids_no_1 + cohabiting_kids_no_2 + 
    cohabiting_kids_no_3 + (1 | residing_country) +trust_1 + 
    trust_5 + trust_7 + hc+
      m.PSS + m.age + m.SSS_faml + m.hc + m.trust_1 + m.trust_5 + m.trust_7,
                               data=data.test, family = gaussian(),
                               cores=4,chains=4, save_pars = save_pars(all = T),
                               sample_prior ='yes', seed=1660415,prior=prior.coef,
    iter=4000)
b.test.2<- brms::brm(CS ~ PSS + SSS_faml + age + gender + relationship_status + work_location + 
                       cohabiting_adults_no + cohabiting_kids_no_1 + cohabiting_kids_no_2 + 
                       cohabiting_kids_no_3 + (1+PSS+SSS_faml+trust_1+
                                                 trust_5+trust_7+hc| residing_country) +trust_1 + 
                       trust_5 + trust_7 + hc+
                       m.PSS + m.age + m.SSS_faml + m.hc + m.trust_1 + m.trust_5 + m.trust_7,
                     data=data.test, family = gaussian(),
                     cores=4,chains=4, save_pars = save_pars(all = T),
                     sample_prior ='yes', seed=1660415,prior=prior.coef,
                     iter=10000)

# bayes factor comparison
bf.10 <- bayes_factor(b.test.1,b.test.0,log=T) #2378.51192
bf.20 <- bayes_factor(b.test.2,b.test.0,log=T,maxiter=10000) #2444.58583
bf.21 <- bayes_factor(b.test.2,b.test.1,log=T,maxiter=10000) # 66.46738

# best <- random slopes
# hypothesis testing
h.PSS<-hypothesis(b.test.2,'PSS=0')
h.SES<-hypothesis(b.test.2,'SSS_faml=0')
h.trust_1<-hypothesis(b.test.2,'trust_1=0')
h.trust_5<-hypothesis(b.test.2,'trust_5=0')
h.trust_7<-hypothesis(b.test.2,'trust_7=0')
h.hc<-hypothesis(b.test.2,'hc=0')

h.age <- hypothesis(b.test.2,'age=0')
h.male<- hypothesis(b.test.2,'genderMale=0')
h.dating<- hypothesis(b.test.2,'relationship_statusDating=0')
h.home <- hypothesis(b.test.2,'work_locationAthome=0')
h.request <- hypothesis(b.test.2,'work_locationAtlocationsIwassenttobymyemployerorrequestedtogotobyaclient
                        =0')
h.NA <- hypothesis(b.test.2,'work_locationNA=0')
h.kid1 <- hypothesis(b.test.2,'cohabiting_kids_no_1=0')

log(1/h.PSS$hypothesis$Evid.Ratio) # 4.168339
log(1/h.SES$hypothesis$Evid.Ratio) # --1.828466 -> x
log(1/h.trust_1$hypothesis$Evid.Ratio) # 2.918206
log(1/h.trust_5$hypothesis$Evid.Ratio) # 34.85931
log(1/h.trust_7$hypothesis$Evid.Ratio) # 38.52488
log(1/h.hc$hypothesis$Evid.Ratio) # 38.99873
log(1/h.age$hypothesis$Evid.Ratio) # Inf +
log(1/h.male$hypothesis$Evid.Ratio) # Inf -
log(1/h.dating$hypothesis$Evid.Ratio) # 0.2887181 -> x
log(1/h.home$hypothesis$Evid.Ratio) # 41.1946 +
log(1/h.request$hypothesis$Evid.Ratio) # 2.300696 -
log(1/h.NA$hypothesis$Evid.Ratio) # 2.46492
log(1/h.kid1$hypothesis$Evid.Ratio) # 0.1506536


# additional numbers
2*log(1/hypothesis(b.test.2,'relationship_statusSingle=0')$hypothesis$Evid.Ratio) #-4.191761
2*log(1/hypothesis(b.test.2,'relationship_statusCohabitating=0')$hypothesis$Evid.Ratio)#-4.513815
2*log(1/hypothesis(b.test.2,'relationship_statusSeparatedDdivorced=0')$hypothesis$Evid.Ratio)#--3.28407
2*log(1/hypothesis(b.test.2,'relationship_statusWidowed=0')$hypothesis$Evid.Ratio) #1.956581
2*log(1/hypothesis(b.test.2,'relationship_statusOtherorwouldnotsay=0')$hypothesis$Evid.Ratio) #-1.55951

2*log(1/hypothesis(b.test.2,'genderOtherDWouldrathernotsay=0')$hypothesis$Evid.Ratio) #-0.5107212
2*log(1/hypothesis(b.test.2,'work_locationDontknowDprefernottosay=0')$hypothesis$Evid.Ratio) #-0.7037723
2*log(1/hypothesis(b.test.2,'work_locationOtherlocations=0')$hypothesis$Evid.Ratio) #1.156162

2*log(1/hypothesis(b.test.2,'cohabiting_adults_no=0')$hypothesis$Evid.Ratio) #-6.520622
2*log(1/hypothesis(b.test.2,'cohabiting_kids_no_2=0')$hypothesis$Evid.Ratio) #-6.034203
2*log(1/hypothesis(b.test.2,'cohabiting_kids_no_3=0')$hypothesis$Evid.Ratio) #-6.503352

# frequentist analysis with the best model
test.2 <- lmer(CS ~ PSS + SSS_faml + age + gender + relationship_status + work_location + 
             cohabiting_adults_no + cohabiting_kids_no_1 + cohabiting_kids_no_2 + 
             cohabiting_kids_no_3 + (1+PSS+SSS_faml+trust_1+
                                       trust_5+trust_7+hc| residing_country) +trust_1 + 
             trust_5 + trust_7 + hc,
           data=data.test)

# effect size
library(EMAtools)
test.1 <- lmer(CS ~ PSS + SSS_faml + age + gender + relationship_status + work_location + 
                 cohabiting_adults_no + cohabiting_kids_no_1 + cohabiting_kids_no_2 + 
                 cohabiting_kids_no_3 + (1| residing_country) +trust_1 + 
                 trust_5 + trust_7 + hc,
               data=data.test)

ES <- EMAtools::lme.dscore(test.1,data.test,'lme4')

# result save
save.image('H2.RData')
