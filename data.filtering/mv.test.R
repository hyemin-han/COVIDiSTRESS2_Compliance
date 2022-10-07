data <- read.csv ('../data_aligned.csv')

test.model <- '
HC =~ moral.values_1_midneutral + moral.values_7_midneutral
FR =~ moral.values_2_midneutral + moral.values_8_midneutral
IL =~ moral.values_3_midneutral + moral.values_9_midneutral
AR =~ moral.values_4_midneutral + moral.values_10_midneutral
PS =~  moral.values_5_midneutral + moral.values_11_midneutral

HC~~0*FR
HC~~0*IL
HC~~0*AR
HC~~0*PS
FR~~0*IL
FR~~0*AR
FR~~0*PS
IL~~0*AR
IL~~0*PS
AR~~0*PS'

library(lavaan)
test.fit <- cfa(test.model,data,estimator='WLSMV')
test.configural <- cfa(test.model,data,estimator='WLSMV', group = 'residing_country')
test.metric <- cfa(test.model,data,estimator='WLSMV', group = 'residing_country',group.equal
                   ='loadings')
fitMeasures(test.metric)
