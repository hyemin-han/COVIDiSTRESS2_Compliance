# filter data

# N >= 100: PSS Social norms Compliance
# N >= 30: Moral values Trust SSS-fam

# load original csv
data <- read.csv('../Final_COVIDiSTRESS_Vol2_cleaned.csv')

# social norms
soc.vars1 <- c('socialinfluence_nor1_1','socialinfluence_nor1_2','socialinfluence_nor1_4',
           'socialinfluence_nor1_5','socialinfluence_nor1_6','socialinfluence_nor1_8')

soc.vars2 <- c('socialinfluence_nor2_1','socialinfluence_nor2_2','socialinfluence_nor2_4',
           'socialinfluence_nor2_5','socialinfluence_nor2_6','socialinfluence_nor2_8')

# compliance
CS <- c('compliance_1','compliance_2',
        'compliance_4','compliance_5','compliance_6',
        'compliance_8')

# pss
pss.vars <- c('perceived_stress_sca_1','perceived_stress_sca_2','perceived_stress_sca_3',
              'perceived_stress_sca_4','perceived_stress_sca_5','perceived_stress_sca_6',
              'perceived_stress_sca_7','perceived_stress_sca_8','perceived_stress_sca_9',
              'perceived_stress_sca_10')

# mark social norms NAs
data.soc1 <- rowSums(is.na(data[,soc.vars1]))
data.soc2 <- rowSums(is.na(data[,soc.vars2]))
data.soc.na <- (data.soc1 < 6) & (data.soc2<6)

# mark compliance NAs
data.cs.na <- rowSums(is.na(data[,CS])) > 0
# mark pss NAs
data.pss.na <- rowSums(is.na(data[,pss.vars])) > 0


#extract languages with n>=100
n.langs <- table(data$UserLanguage)
list.langs <- labels(n.langs)[[1]]
langs.include <- list.langs[n.langs>=100]

n.include <- n.langs[n.langs>=100]

#extract data
for (i in 1:length(langs.include)){
  if (i == 1){
    data.mi <-data[data$UserLanguage == langs.include[i],]
  }else{
    current <- data[data$UserLanguage == langs.include[i],]
    data.mi <- rbind(data.mi,current)
  }
}

# extract country wide >= 30
data.aligned <- data.mi

# filtering by country n>= 30
country.30 <-table(data.aligned$residing_country) >=30

n.country <-table(data.aligned$residing_country)
list.country <- labels(n.country)[[1]]
country.include <- list.country[n.country>=30]
n.include.c <- n.country[n.country>=30]


#extract data
for (i in 1:length(country.include)){
  if (i == 1){
    data.filtered <- data.aligned[(data.aligned$residing_country == 
                                     country.include[i]) & !is.na(
                                       data.aligned$residing_country
                                     ),]
  }else{
    current <- data.aligned[(data.aligned$residing_country == country.include[i])
                            & !is.na(
                              data.aligned$residing_country
                            ),]
    data.filtered <- rbind(data.filtered,current)
  }
}

# save csv
write.csv(data.filtered,file='../data_filtered.csv',row.names = F)


# set and examine fitmeasures
fits <-c('rmsea.scaled','srmr','cfi.scaled','tli.scaled')



