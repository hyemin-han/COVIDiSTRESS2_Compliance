# H1
library(Amelia)
library(semTools)

# MG CFA with multiple imputations

# variables
vars1 <- c('socialinfluence_nor1_1','socialinfluence_nor1_2','socialinfluence_nor1_4',
           'socialinfluence_nor1_5','socialinfluence_nor1_6','socialinfluence_nor1_8')

vars2 <- c('socialinfluence_nor2_1','socialinfluence_nor2_2','socialinfluence_nor2_4',
           'socialinfluence_nor2_5','socialinfluence_nor2_6','socialinfluence_nor2_8')

# load data
data <- read.csv('../data_aligned.csv')

# mark NAs
# remove complete NAs
testset.na <- rowSums(is.na(data[,vars1]))
testset.na2 <- rowSums(is.na(data[,vars2]))

# at least one each
testset <- data[(testset.na < 6) & (testset.na2<6),]

testset1 <- testset[,c(vars1,'UserLanguage','ResponseId')]
testset2 <- testset[,c(vars2,'UserLanguage','ResponseId')]

# do MI

testamelia1 <- amelia(testset1,m=20,cs='UserLanguage', seed=12345, idvars='ResponseId')
testamelia2 <- amelia(testset2,m=20,cs='UserLanguage', seed=12345, idvars='ResponseId')


testmodel<-'
S1 =~ socialinfluence_nor1_1+socialinfluence_nor1_2+socialinfluence_nor1_3+
socialinfluence_nor1_4+socialinfluence_nor1_5+socialinfluence_nor1_6+
socialinfluence_nor1_7+socialinfluence_nor1_8

S2 =~ socialinfluence_nor2_1+socialinfluence_nor2_2+socialinfluence_nor2_3+
socialinfluence_nor2_4+socialinfluence_nor2_5+socialinfluence_nor2_6+
socialinfluence_nor2_7+socialinfluence_nor2_8
'

testmodel1<-'
S1 =~ socialinfluence_nor1_1+socialinfluence_nor1_2+
socialinfluence_nor1_4+socialinfluence_nor1_5+socialinfluence_nor1_6+socialinfluence_nor1_8
'

testmodel2<-'
S2 =~ socialinfluence_nor2_1+socialinfluence_nor2_2+
socialinfluence_nor2_4+socialinfluence_nor2_5+socialinfluence_nor2_6+
socialinfluence_nor2_8
'

info <- c('rmsea.robust','srmr','cfi.robust')

#####
# configural
configural1 <- cfa.mi(testmodel1,testamelia1$imputations,group='UserLanguage',
                      estimator='MLR',m=20,seed=12345
                      
)

fits.configural1<-fitMeasures(configural1)



configural2 <- cfa.mi(testmodel2,testamelia2$imputations,group='UserLanguage',
                      estimator='MLR',m=20,seed=12345
                      
)

fits.configural2<-fitMeasures(configural2)

rbind(fits.configural1[info],fits.configural2[info])


#     rmsea.robust       srmr cfi.robust
#[1,]   0.02987918 0.03791284  0.9861843
#[2,]   0.00000000 0.04123133  1.0000000


#####
# metric

# metric

metric1 <- cfa.mi(testmodel1,testamelia1$imputations,group='UserLanguage',
                  estimator='MLR',m=20,seed=12345, group.equal='loadings'
)
fits.metric1<-fitMeasures(metric1)

metric2 <- cfa.mi(testmodel2,testamelia2$imputations,group='UserLanguage',
                  estimator='MLR',m=20,seed=12345, group.equal='loadings'
)
fits.metric2<-fitMeasures(metric2)
rbind(fits.metric1[info],fits.metric2[info])

#rmsea.robust       srmr cfi.robust
#[1,]   0.02144471 0.04145215  0.9890708
#[2,]   0.00000000 0.04432669  1.0000000

# scalar


scalar1 <- cfa.mi(testmodel1,testamelia1$imputations,group='UserLanguage',
                  estimator='MLR',m=20,seed=12345, group.equal=c('loadings',
                                                                 'intercepts')
)
fits.scalar1<-fitMeasures(scalar1)
scalar2 <- cfa.mi(testmodel2,testamelia2$imputations,group='UserLanguage',
                  estimator='MLR',m=20,seed=12345, group.equal=c('loadings',
                                                                 'intercepts')
)
fits.scalar2<-fitMeasures(scalar2)
rbind(fits.configural1[info],fits.configural2[info])
rbind(fits.metric1[info],fits.metric2[info])
rbind(fits.scalar1[info],fits.scalar2[info])

#     rmsea.robust       srmr cfi.robust
#[1,]   0.01653852 0.04354016   0.991232
#[2,]   0.00000000 0.04752250   1.000000


# get plausibla values for further analyses
test1 <- plausibleValues(scalar1,nDraws=1 )
test2 <- plausibleValues(scalar2,nDraws=1)

# ID matching
data.H1.temp <- data[(testset.na < 6) & (testset.na2<6),]

data.H1<-NULL
# match the ids
for (i in 1:20){
  data.H1[[i]] <- data.H1.temp
  data.H1[[i]]$S1 <- test1[[i]]$S1
  data.H1[[i]]$S2 <- test2[[i]]$S2
  
}



# save all
save.image('H1.amelia.RData')

save(data.H1,file='H1.data.RData')

