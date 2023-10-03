## Meta-analysis ASMR per 100,000

setwd("C:/Users/shakirarodzlan/Desktop/PhD/PhD Chapters/Chapter 2-Meta analysis/Publication/Publish SR-MA ASMR/Analysis MA ASMR")

# Read data ----
install.packages("readxl")
library("readxl")
asmr <- read_excel("ma_asmr5.xlsx")

str (asmr)
summary(asmr)


# Packages ----
install.packages("tidyverse")
install.packages("meta")
#install.packages("dmetar")

library(tidyverse)
library(meta)
#library(dmetar)


# run meta analysis for ASMR

## log ASMR

asmr$ASMR_log <- log(asmr$ASMR)
summary(asmr)

## overall ASMR

meta_asmrall <- metagen(TE = ASMR_log, 
                      seTE = SE.asmr, 
                      sm = "RR", 
                      studlab = author_year,
                      common = F,
                      method.tau = "PM",
                      prediction = T,
                      hakn = T,
                      adhoc.hakn = "iqwig6",
                      data = asmr)
meta_asmrall

#Forest plot
forest(meta_asmrall, sortvar = TE, allstudies = F)


# Subgroup by CVD type

##meta regression by cvd type
metareg_type <- metareg(meta_asmrall, ~ cvd_type, 
                          hakn = T, 
                          method.tau = "REML", intercept = T) 
metareg_type

##subgroup 
metareg_type <- update(meta_asmrall, subgroup = cvd_type) 
metareg_type
#forest plot
forest(metareg_type, sortvar = TE, sort.subgroup = T, allstudies = F, 
       leftcols=c("author_year", "country_data", "year_data", "specific_type", "sex", "age"),
       leftlabs = c("Study or subgroup", "Country", "Time data", "Cause of death",  "Sex", "Age range"),
       rightlabs = c("ASMR per 100,000", "95% CI", "Weight"),
       bylab = c("Total CVD", "Stroke or Cerebrovascular disease", "IHD or Heart disease"),
       smlab = " ", just = "center")

# Subgroup by sex

##meta regression by sex
metareg_sex <- metareg(meta_asmrall, ~ sex, 
                        hakn = T, 
                        method.tau = "REML", intercept = T) 
metareg_sex

##subgroup 
metareg_sex <- update(meta_asmrall, subgroup = sex) 
metareg_sex
forest(metareg_sex, sortvar = TE, sort.subgroup = T, allstudies = F, 
       leftcols=c("author_year", "country_data", "year_data", "specific_type", "age"),
       leftlabs = c("Study or subgroup", "Country", "Time data", "Cause of death", "Age range"),
       rightlabs = c("ASMR per 100,000", "95% CI", "Weight"),
       bylab = c("Total CVD", "Stroke or Cerebrovascular disease", "IHD or Heart disease"),
       smlab = " ", just = "center")

##remove "both sex" from analysis
asmr_sex <- asmr %>%
  filter(sex != "Both sex") #exclude both sex
table (asmr_sex$sex)

metareg_sex2 <- metagen(TE = ASMR_log, 
                     seTE = SE.asmr, 
                     sm = "RR", 
                     studlab = author_year,
                     common = F,
                     method.tau = "PM",
                     prediction = T,
                     hakn = T,
                     adhoc.hakn = "iqwig6",
                     subgroup = sex,
                     data = asmr_sex)

forest(metareg_sex2, sortvar = TE, sort.subgroup = T, allstudies = F, 
       leftcols=c("author_year", "country_data", "year_data", "specific_type", "age"),
       leftlabs = c("Study or subgroup", "Country", "Time data", "Cause of death", "Age range"),
       rightlabs = c("ASMR per 100,000", "95% CI", "Weight"),
       bylab = c("Total CVD", "Stroke or Cerebrovascular disease", "IHD or Heart disease"),
       smlab = " ", just = "center")


# Subgroup by income country

##meta regression by income
metareg_income <- metareg(meta_asmrall, ~ income_country2, 
                        hakn = T, 
                        method.tau = "REML", intercept = T) 
metareg_income #significant

##subgroup 
metareg_income <- update(meta_asmrall, subgroup = income_country2) 
metareg_income
forest(metareg_income, sortvar = TE, sort.subgroup = T, allstudies = F, 
       leftcols=c("author_year", "country_data", "year_data", "specific_type", "sex", "age"),
       leftlabs = c("Study or subgroup", "Country", "Time data", "Cause of death", "Sex", "Age range"),
       rightlabs = c("ASMR per 100,000", "95% CI", "Weight"),
       smlab = " ", just = "center")


# Subgroup by WHO region

##meta regression by who region
metareg_who <- metareg(meta_asmrall, ~ regionWHO, 
                          hakn = T, 
                          method.tau = "REML", intercept = T) 
metareg_who #significant

##subgroup who region
metareg_who <- update(meta_asmrall, subgroup = regionWHO) 
metareg_who
forest(metareg_who, sortvar = TE, sort.subgroup = T, allstudies = F, 
       leftcols=c("author_year", "country_data", "year_data", "specific_type", "sex", "age"),
       leftlabs = c("Study or subgroup", "Country", "Time data", "Cause of death", "Sex", "Age range"),
       rightlabs = c("ASMR per 100,000", "95% CI", "Weight"),
       smlab = " ", just = "center")

##subgroup continent
metareg_conti <- update(meta_asmrall, subgroup = continents) 
metareg_conti
forest(metareg_conti, sortvar = TE, sort.subgroup = T, allstudies = F, 
       leftcols=c("author_year", "country_data", "year_data", "specific_type", "sex", "age"),
       leftlabs = c("Study or subgroup", "Country", "Time data", "Cause of death", "Sex", "Age range"),
       rightlabs = c("ASMR per 100,000", "95% CI", "Weight"),
       smlab = " ", just = "center")

# Subgroup by study time

asmr2 <- asmr %>%
  filter(year_grp2 != "2000-2019") #exclude mix year 2000-2009 (2 studies)
table (asmr2$ year_grp2)

names(asmr2)[names(asmr2) == "year_grp2"] <- "study_time"
summary(asmr2)

meta_time <- metagen(TE = ASMR_log, 
                     seTE = SE.asmr, 
                     sm = "RR", 
                     studlab = author_year,
                     common = F,
                     method.tau = "PM",
                     prediction = T,
                     hakn = T,
                     adhoc.hakn = "iqwig6",
                     subgroup = study_time,
                     data = asmr2)

forest(meta_time, sortvar = TE, sort.subgroup = T, allstudies = F, 
       leftcols=c("author_year", "country_data", "year_data", "specific_type", "sex", "age"),
       leftlabs = c("Study or subgroup", "Country", "Time data", "Cause of death", "Sex", "Age range"),
       rightlabs = c("ASMR per 100,000", "95% CI", "Weight"),
       smlab = " ", just = "center")



#select each study time and run meta - to check publication bias and heterogeneity later
#a) 1999-1999
time90_99 <- asmr2 %>%
  filter(study_time == "1990-1999") #select year 1900-1999 
meta_time90_99 <- metagen(TE = ASMR_log, 
                     seTE = SE.asmr, 
                     sm = "RR", 
                     studlab = author_year,
                     common = F,
                     method.tau = "PM",
                     prediction = T,
                     hakn = T,
                     adhoc.hakn = "iqwig6",
                     data = time90_99)
meta_time90_99
#b) 2000-2009
time00_09 <- asmr2 %>%
  filter(study_time == "2000-2009") #select year 2000-2009
meta_time00_09 <- metagen(TE = ASMR_log, 
                          seTE = SE.asmr, 
                          sm = "RR", 
                          studlab = author_year,
                          common = F,
                          method.tau = "PM",
                          prediction = T,
                          hakn = T,
                          adhoc.hakn = "iqwig6",
                          data = time00_09)
meta_time00_09
#c) 2010-2019
time10_19 <- asmr2 %>%
  filter(study_time == "2010-2019") #select year 2000-2009
meta_time10_19 <- metagen(TE = ASMR_log, 
                          seTE = SE.asmr, 
                          sm = "RR", 
                          studlab = author_year,
                          common = F,
                          method.tau = "PM",
                          prediction = T,
                          hakn = T,
                          adhoc.hakn = "iqwig6",
                          data = time10_19)
meta_time10_19




# Sub analysis for time study

# 1) among male

asmr_male <- asmr2 %>%  
  filter(sex == "Male")
table (asmr_male$ study_time)
#run total male
meta_male <- metagen(TE = ASMR_log, 
                          seTE = SE.asmr, 
                          sm = "RR", 
                          studlab = author_year,
                          common = F,
                          method.tau = "PM",
                          prediction = T,
                          hakn = T,
                          adhoc.hakn = "iqwig6",
                          data = asmr_male)
#subgroup
meta_male_time <- metagen(TE = ASMR_log, 
                         seTE = SE.asmr, 
                         sm = "RR", 
                         studlab = author_year,
                         common = F,
                         method.tau = "PM",
                         prediction = T,
                         hakn = T,
                         adhoc.hakn = "iqwig6",
                         subgroup = study_time,
                         data = asmr_male)

forest(meta_male_time, sortvar = TE, sort.subgroup = T, allstudies = F, 
       leftcols=c("author_year", "country_data", "year_data", "specific_type", "sex", "age"),
       leftlabs = c("Study or subgroup", "Country", "Time data", "Cause of death","Sex", "Age range"),
       rightlabs = c("ASMR per 100,000", "95% CI", "Weight"),
       smlab = " ", just = "center")

# 2) among female

asmr_female <- asmr2 %>%  
  filter(sex == "Female")
table (asmr_female$ study_time)

#run total female
meta_female <- metagen(TE = ASMR_log, 
                            seTE = SE.asmr, 
                            sm = "RR", 
                            studlab = author_year,
                            common = F,
                            method.tau = "PM",
                            prediction = T,
                            hakn = T,
                            adhoc.hakn = "iqwig6",
                            data = asmr_female)
#subgroup
meta_female_time <- metagen(TE = ASMR_log, 
                          seTE = SE.asmr, 
                          sm = "RR", 
                          studlab = author_year,
                          common = F,
                          method.tau = "PM",
                          prediction = T,
                          hakn = T,
                          adhoc.hakn = "iqwig6",
                          subgroup = study_time,
                          data = asmr_female)

forest(meta_female_time, sortvar = TE, sort.subgroup = T, allstudies = F, 
       leftcols=c("author_year", "country_data", "year_data", "specific_type", "sex", "age"),
       leftlabs = c("Study or subgroup", "Country", "Time data", "Cause of death","Sex", "Age range"),
       rightlabs = c("ASMR per 100,000", "95% CI", "Weight"),
       smlab = " ", just = "center")



# 3) among HIC

asmr_HIC <- asmr2 %>%  
  filter(income_country2 == "High-income country")
table (asmr_HIC$ study_time)
#run total HIC
meta_HIC <- metagen(TE = ASMR_log, 
                         seTE = SE.asmr, 
                         sm = "RR", 
                         studlab = author_year,
                         common = F,
                         method.tau = "PM",
                         prediction = T,
                         hakn = T,
                         adhoc.hakn = "iqwig6",
                         data = asmr_HIC)
#subgroup
meta_HIC_time <- metagen(TE = ASMR_log, 
                       seTE = SE.asmr, 
                       sm = "RR", 
                       studlab = author_year,
                       common = F,
                       method.tau = "PM",
                       prediction = T,
                       hakn = T,
                       adhoc.hakn = "iqwig6",
                       subgroup = study_time,
                       data = asmr_HIC)

forest(meta_HIC_time, sortvar = TE, sort.subgroup = T, allstudies = F, 
       leftcols=c("author_year", "country_data", "year_data", "specific_type", "sex", "age"),
       leftlabs = c("Study or subgroup", "Country", "Time data", "Cause of death","Sex", "Age range"),
       rightlabs = c("ASMR per 100,000", "95% CI", "Weight"),
       smlab = " ", just = "center")


# 4) among MIC

asmr_MIC <- asmr2 %>% 
  filter(income_country2 == "Middle Income Country") 
table (asmr_MIC$ study_time)
#run total MIC
meta_MIC <- metagen(TE = ASMR_log, 
                         seTE = SE.asmr, 
                         sm = "RR", 
                         studlab = author_year,
                         common = F,
                         method.tau = "PM",
                         prediction = T,
                         hakn = T,
                         adhoc.hakn = "iqwig6",
                         data = asmr_MIC)
#subgroup
meta_MIC_time <- metagen(TE = ASMR_log, 
                         seTE = SE.asmr, 
                         sm = "RR", 
                         studlab = author_year,
                         common = F,
                         method.tau = "PM",
                         prediction = T,
                         hakn = T,
                         adhoc.hakn = "iqwig6",
                         subgroup = study_time,
                         data = asmr_MIC)

forest(meta_MIC_time, sortvar = TE, sort.subgroup = T, allstudies = F, 
       leftcols=c("author_year", "country_data", "year_data", "specific_type", "sex", "age"),
       leftlabs = c("Study or subgroup", "Country", "Time data", "Cause of death","Sex", "Age range"),
       rightlabs = c("ASMR per 100,000", "95% CI", "Weight"),
       smlab = " ", just = "center")


# Sub analysis within CVD types

#1) Among total CVD (ICD-10 code: I00-I99 or ICD-9 codes: 350-459) 

asmr_all_CVD <- asmr %>% 
  filter(cvd_type == "All CVD") 

## overall ASMR
meta_allcvd <- metagen(TE = ASMR_log, 
                       seTE = SE.asmr, 
                       sm = "RR", 
                       studlab = author_year,
                       common = F,
                       method.tau = "PM",
                       prediction = T,
                       hakn = T,
                       adhoc.hakn = "iqwig6",
                       data = asmr_all_CVD)
meta_allcvd
forest(meta_allcvd, sortvar = TE, allstudies = F)


## overall ASMR
meta_allcvd <- metagen(TE = ASMR_log, 
                        seTE = SE.asmr, 
                        sm = "RR", 
                        studlab = author_year,
                        common = F,
                        method.tau = "PM",
                        prediction = T,
                        hakn = T,
                        adhoc.hakn = "iqwig6",
                        data = asmr_all_CVD)
meta_allcvd
forest(meta_allcvd, sortvar = TE, allstudies = F)

#meta regression by sex
asmr_all_CVD2 <- asmr_sex %>% 
  filter(cvd_type == "All CVD") #exclude both sex

metareg_CVDsex <- metagen(TE = ASMR_log, 
                         seTE = SE.asmr, 
                         sm = "RR", 
                         studlab = author_year,
                         common = F,
                         method.tau = "PM",
                         prediction = T,
                         hakn = T,
                         adhoc.hakn = "iqwig6",
                         subgroup = sex,
                         data = asmr_all_CVD2)

forest(metareg_CVDsex, sortvar = TE, sort.subgroup = T, allstudies = F, 
       leftcols=c("author_year", "country_data", "year_data", "age"),
       leftlabs = c("Study or subgroup", "Country", "Time data", "Age range"),
       rightlabs = c("ASMR per 100,000", "95% CI", "Weight"),
       smlab = " ", just = "center")


#subgroup analysis by income country
metareg_CVDincome <- update(meta_allcvd, subgroup = income_country2)
metareg_CVDincome
forest(metareg_CVDincome, sortvar = TE, sort.subgroup = T, allstudies = F, 
       leftcols=c("author_year", "country_data", "year_data", "sex", "age"),
       leftlabs = c("Study or subgroup", "Country", "Time data", "Sex", "Age range"),
       rightlabs = c("ASMR per 100,000", "95% CI", "Weight"),
       smlab = " ", just = "center")

#2) Among IHD

asmr_IHD <- asmr %>% 
  filter(cvd_type == "IHD") 

## overall ASMR
meta_IHD <- metagen(TE = ASMR_log, 
                       seTE = SE.asmr, 
                       sm = "RR", 
                       studlab = author_year,
                       common = F,
                       method.tau = "PM",
                       prediction = T,
                       hakn = T,
                       adhoc.hakn = "iqwig6",
                       data = asmr_IHD)
meta_IHD
forest(meta_IHD, sortvar = TE, allstudies = F)

#meta regression by sex
asmr_IHD2 <- asmr_sex %>% 
  filter(cvd_type == "IHD") #exclude both sex

metareg_IHDsex <- metagen(TE = ASMR_log, 
                          seTE = SE.asmr, 
                          sm = "RR", 
                          studlab = author_year,
                          common = F,
                          method.tau = "PM",
                          prediction = T,
                          hakn = T,
                          adhoc.hakn = "iqwig6",
                          subgroup = sex,
                          data = asmr_IHD2)

forest(metareg_IHDsex, sortvar = TE, sort.subgroup = T, allstudies = F, 
       leftcols=c("author_year", "country_data", "year_data", "age"),
       leftlabs = c("Study or subgroup", "Country", "Time data", "Age range"),
       rightlabs = c("ASMR per 100,000", "95% CI", "Weight"),
       smlab = " ", just = "center")



#2) Among stroke

asmr_stroke <- asmr %>% 
  filter(cvd_type == "Cerebrovascular Disease/Stroke") 

## overall ASMR
meta_stroke <- metagen(TE = ASMR_log, 
                    seTE = SE.asmr, 
                    sm = "RR", 
                    studlab = author_year,
                    common = F,
                    method.tau = "PM",
                    prediction = T,
                    hakn = T,
                    adhoc.hakn = "iqwig6",
                    data = asmr_stroke)
meta_stroke
forest(meta_stroke, sortvar = TE, allstudies = F)

#meta regression by sex
asmr_stroke2 <- asmr_sex %>% 
  filter(cvd_type == "Cerebrovascular Disease/Stroke")  #exclude both sex

asmr_strokesex <- metagen(TE = ASMR_log, 
                          seTE = SE.asmr, 
                          sm = "RR", 
                          studlab = author_year,
                          common = F,
                          method.tau = "PM",
                          prediction = T,
                          hakn = T,
                          adhoc.hakn = "iqwig6",
                          subgroup = sex,
                          data = asmr_stroke2)

forest(metareg_strokesex, sortvar = TE, sort.subgroup = T, allstudies = F, 
       leftcols=c("author_year", "country_data", "year_data", "age"),
       leftlabs = c("Study or subgroup", "Country", "Time data", "Age range"),
       rightlabs = c("ASMR per 100,000", "95% CI", "Weight"),
       smlab = " ", just = "center")


################

# Funnel plot ----

funnel(meta_asmrall, xlab = "ASMR per 100,000 (Overall study)") 
funnel(meta_allcvd, xlab = "ASMR per 100,000 (Total CVD)")
funnel(meta_IHD, xlab = "ASMR per 100,000 (IHD)")
funnel(meta_stroke, xlab = "ASMR per 100,000 (Stroke)")
funnel(meta_male, xlab = "ASMR per 100,000 (Male)")
funnel(meta_female, xlab = "ASMR per 100,000 (Female)")
funnel(meta_HIC, xlab = "ASMR per 100,000 (High income countries)")
funnel(meta_MIC, xlab = "ASMR per 100,000 (Middle income countries)")
funnel(meta_time90_99, xlab = "ASMR per 100,000 (study time: year 1990-1999)")
funnel(meta_time00_09, xlab = "ASMR per 100,000 (study time: year 2000-2009)")
funnel(meta_time10_19, xlab = "ASMR per 100,000 (study time: year 2010-2019)")


  #combine 2 funnel plot (overall study & total CVD) - present this

par(mfrow=c(1,2))
funnel(meta_asmrall, xlab = " (a) ASMR per 100,000 (Overall study)")
funnel(meta_allcvd, xlab = " (b) ASMR per 100,000 (Total CVD)")


# Publication bias ----

metabias(meta_asmrall, plotit = T, method.bias = "Egger")
metabias(meta_asmrall, plotit = T, method.bias = "Begg")

metabias(meta_allcvd, plotit = T, method.bias = "Egger")
metabias(meta_allcvd, plotit = T, method.bias = "Begg")

metabias(meta_IHD, plotit = T, method.bias = "Egger")
metabias(meta_IHD, plotit = T, method.bias = "Begg")

metabias(meta_stroke, plotit = T, method.bias = "Egger")
metabias(meta_stroke, plotit = T, method.bias = "Begg")

metabias(meta_HIC, plotit = T, method.bias = "Egger")
metabias(meta_HIC, plotit = T, method.bias = "Begg")
metabias(meta_MIC, plotit = T, method.bias = "Egger")
metabias(meta_MIC, plotit = T, method.bias = "Begg")

metabias(meta_male, plotit = T, method.bias = "Egger")
metabias(meta_male, plotit = T, method.bias = "Begg")
metabias(meta_female, plotit = T, method.bias = "Egger")
metabias(meta_female, plotit = T, method.bias = "Begg")

metabias(meta_time90_99, plotit = T, method.bias = "Egger")
metabias(meta_time90_99, plotit = T, method.bias = "Begg")
metabias(meta_time00_09, plotit = T, method.bias = "Egger")
metabias(meta_time00_09, plotit = T, method.bias = "Begg")
metabias(meta_time10_19, plotit = T, method.bias = "Egger")
metabias(meta_time10_19, plotit = T, method.bias = "Begg")


# Influential diagnostics ----
baujat(meta_asmrall, studlab = F) #to remove study name
baujat(meta_asmrall)
baujat(meta_allcvd)
baujat(meta_IHD)    #n study = 4, too small
baujat(meta_stroke) #n study = 3, too small
baujat(meta_HIC)
baujat(meta_MIC)
baujat(meta_male)
baujat(meta_female)
baujat(meta_time90_99)
baujat(meta_time00_09)
baujat(meta_time10_19)

#ma_inf <- InfluenceAnalysis(meta_asmrall, random = T) #cannot run
#plot(ma_inf, "baujat")
#plot(ma_inf, "influence")
#plot(ma_inf, "ES")
#plot(ma_inf, "I2")





##############################################################################

## Sensitivity analysis 

# removed outlier as suggested by baujat plot and re-run meta analysis

#1)	Overall study: Jin et.al, (2020) and Gómez-Martínez et al. (2018)

sen.asmr <- asmr %>%
  filter(author_year != "Jin et al. (2020)" & author_year != "Gómez-Martínez et al. (2018)"  ) 

sen.meta_asmrall <- metagen(TE = ASMR_log, 
                        seTE = SE.asmr, 
                        sm = "RR", 
                        studlab = author_year,
                        common = F,
                        method.tau = "PM",
                        prediction = T,
                        hakn = T,
                        adhoc.hakn = "iqwig6",
                        data = sen.asmr)
sen.meta_asmrall

#subgroup by cvd type
sen.meta_asmrall_type <- metagen(TE = ASMR_log, 
                            seTE = SE.asmr, 
                            sm = "RR", 
                            studlab = author_year,
                            common = F,
                            method.tau = "PM",
                            prediction = T,
                            hakn = T,
                            adhoc.hakn = "iqwig6",
                            subgroup = cvd_type,
                            data = sen.asmr)
sen.meta_asmrall_type


#2  Total CVD: Yang et al. (2021) and Gawryszewski & Souza (2014)

sen.asmr_all_CVD <- asmr_all_CVD %>% 
  filter (author_year != "Yang et al. (2021)" & author_year != "Gawryszewski & Souza (2014)")

## overall ASMR
sen.meta_allcvd <- metagen(TE = ASMR_log, 
                       seTE = SE.asmr, 
                       sm = "RR", 
                       studlab = author_year,
                       common = F,
                       method.tau = "PM",
                       prediction = T,
                       hakn = T,
                       adhoc.hakn = "iqwig6",
                       data = sen.asmr_all_CVD)
sen.meta_allcvd

#subgroup by sex

sen.meta_allcvd_sex <- metagen(TE = ASMR_log, 
                           seTE = SE.asmr, 
                           sm = "RR", 
                           studlab = author_year,
                           common = F,
                           method.tau = "PM",
                           prediction = T,
                           hakn = T,
                           adhoc.hakn = "iqwig6",
                           subgroup = sex,
                           data = sen.asmr_all_CVD)
sen.meta_allcvd_sex

#3) IHD: Dani et al. (2022)

sen.asmr_IHD <- asmr_IHD %>% 
  filter (author_year != "Dani et al. (2022)")

## overall ASMR
sen.meta_IHD <- metagen(TE = ASMR_log, 
                           seTE = SE.asmr, 
                           sm = "RR", 
                           studlab = author_year,
                           common = F,
                           method.tau = "PM",
                           prediction = T,
                           hakn = T,
                           adhoc.hakn = "iqwig6",
                           data = sen.asmr_IHD)
sen.meta_IHD

#subgroup by sex

sen.meta_IHD_sex <- metagen(TE = ASMR_log, 
                               seTE = SE.asmr, 
                               sm = "RR", 
                               studlab = author_year,
                               common = F,
                               method.tau = "PM",
                               prediction = T,
                               hakn = T,
                               adhoc.hakn = "iqwig6",
                               subgroup = sex,
                               data = sen.asmr_IHD)
sen.meta_IHD_sex

#4) Stroke: Moryson & Stawińska (2022)

sen.asmr_stroke <- asmr_stroke %>% 
  filter (author_year != "Moryson & Stawińska (2022)")

## overall ASMR
sen.meta_stroke <- metagen(TE = ASMR_log, 
                        seTE = SE.asmr, 
                        sm = "RR", 
                        studlab = author_year,
                        common = F,
                        method.tau = "PM",
                        prediction = T,
                        hakn = T,
                        adhoc.hakn = "iqwig6",
                        data = sen.asmr_stroke)
sen.meta_stroke

#subgroup by sex

sen.meta_stroke_sex <- metagen(TE = ASMR_log, 
                            seTE = SE.asmr, 
                            sm = "RR", 
                            studlab = author_year,
                            common = F,
                            method.tau = "PM",
                            prediction = T,
                            hakn = T,
                            adhoc.hakn = "iqwig6",
                            subgroup = sex,
                            data = sen.asmr_stroke)
sen.meta_stroke_sex

#5) Male and female: Best et al. (2018) and Gómez-Martínez et al. (2018)

sen.asmr_sex <- asmr %>%
  filter(author_year != "Best et al. (2018)" & author_year != "Gómez-Martínez et al. (2018)"  ) 

sen.meta_sex <- metagen(TE = ASMR_log, 
                            seTE = SE.asmr, 
                            sm = "RR", 
                            studlab = author_year,
                            common = F,
                            method.tau = "PM",
                            prediction = T,
                            hakn = T,
                            adhoc.hakn = "iqwig6",
                            subgroup = sex,
                            data = sen.asmr)
sen.meta_sex




#6) High income countries: Gómez-Martínez et al. (2018)

sen.asmr_HIC <- asmr_HIC %>% 
  filter (author_year != "Gómez-Martínez et al. (2018)")

## overall ASMR
sen.meta_HIC <- metagen(TE = ASMR_log, 
                           seTE = SE.asmr, 
                           sm = "RR", 
                           studlab = author_year,
                           common = F,
                           method.tau = "PM",
                           prediction = T,
                           hakn = T,
                           adhoc.hakn = "iqwig6",
                           data = sen.asmr_HIC)
sen.meta_HIC

#subgroup
sen.meta_HIC_time <- metagen(TE = ASMR_log, 
                        seTE = SE.asmr, 
                        sm = "RR", 
                        studlab = author_year,
                        common = F,
                        method.tau = "PM",
                        prediction = T,
                        hakn = T,
                        adhoc.hakn = "iqwig6",
                        subgroup = study_time, 
                        data = sen.asmr_HIC)
sen.meta_HIC_time


#7) Middle income countries: Yang et al. (2021)


sen.asmr_MIC <- asmr_MIC %>% 
  filter (author_year != "Yang et al. (2021)")

## overall ASMR
sen.meta_MIC <- metagen(TE = ASMR_log, 
                        seTE = SE.asmr, 
                        sm = "RR", 
                        studlab = author_year,
                        common = F,
                        method.tau = "PM",
                        prediction = T,
                        hakn = T,
                        adhoc.hakn = "iqwig6",
                        data = sen.asmr_MIC)
sen.meta_MIC

#subgroup
sen.meta_MIC_time <- metagen(TE = ASMR_log, 
                             seTE = SE.asmr, 
                             sm = "RR", 
                             studlab = author_year,
                             common = F,
                             method.tau = "PM",
                             prediction = T,
                             hakn = T,
                             adhoc.hakn = "iqwig6",
                             subgroup = study_time, 
                             data = sen.asmr_MIC)
sen.meta_MIC_time


#8)Time 1990-1999: Gómez-Martínez et al. (2018)

sen.time90_99 <- time90_99 %>% 
  filter (author_year != "Gómez-Martínez et al. (2018)")

## overall ASMR
sen.metatime90_99 <- metagen(TE = ASMR_log, 
                        seTE = SE.asmr, 
                        sm = "RR", 
                        studlab = author_year,
                        common = F,
                        method.tau = "PM",
                        prediction = T,
                        hakn = T,
                        adhoc.hakn = "iqwig6",
                        data = sen.time90_99)
sen.metatime90_99


#9)Time 2000-2009: Moryson & Stawińska (2022)

sen.time00_09 <- time00_09 %>% 
  filter (author_year != "Moryson & Stawińska (2022)")

## overall ASMR
sen.metatime00_09 <- metagen(TE = ASMR_log, 
                         seTE = SE.asmr, 
                         sm = "RR", 
                         studlab = author_year,
                         common = F,
                         method.tau = "PM",
                         prediction = T,
                         hakn = T,
                         adhoc.hakn = "iqwig6",
                         data = sen.time00_09)
sen.metatime00_09


#10)Time 2010-2019: Gómez-Martínez et al. (2018)

sen.time10_19 <- time10_19 %>% 
  filter (author_year != "Gómez-Martínez et al. (2018)")

## overall ASMR
sen.metatime10_19 <- metagen(TE = ASMR_log, 
                             seTE = SE.asmr, 
                             sm = "RR", 
                             studlab = author_year,
                             common = F,
                             method.tau = "PM",
                             prediction = T,
                             hakn = T,
                             adhoc.hakn = "iqwig6",
                             data = sen.time10_19)
sen.metatime10_19




################################### Meta-regression analysis ########################################


# Multiple meta-regression

# Packages
library(readxl)
library(metafor)
library(dmetar)
library(PerformanceAnalytics)
library(dplyr)

# Data
asmr <- read_excel("ma_asmr5.xlsx")
asmr2 <- asmr %>%
  mutate(ASMR_log = log(ASMR)) %>% 
  filter(year_grp2 != "2000-2019") %>%  #exclude mix year 2000-2009 (2 studies)
  filter(sex != "Both sex") %>% #exclude both sex
  rename(study_time = "year_grp2",
         sex2 = "sex")

table (asmr2$study_time)
table(asmr2$sex2)
table(asmr2$income_country2)
table(asmr2$cvd_type)
table(asmr2$age_grp)

# Reorder the levels
asmr2$cvd_type <- factor(asmr2$cvd_type, 
                         levels = c("Cerebrovascular Disease/Stroke", "IHD", 
                                    "Other heart disease", "All CVD"))


names (asmr2)

# Multicollinearity -------------------------------------------------------

# Pairwise correlation
asmr2 %>% 
  select(cvd_type, sex2, income_country2, study_time) %>% 
  mutate_all(as.factor) %>% 
  mutate_all(as.numeric) %>% # change all factor to numeric
  chart.Correlation()

# Very crude way to assess MC
# r > 0.8 may indicate MC

# Meta-regression ---------------------------------------------------------

# Backward variable selection

# Model 1 - full model
full_mod <- rma(yi = ASMR_log,
                sei = SE.asmr,
                data = asmr2,
                method = "ML",
                mods = ~ cvd_type + sex2 + income_country2 + study_time + age_grp,
                test = "knha")
full_mod # all significant 

# Model 2 - exclude study time based on the largest p val
mod2 <- rma(yi = ASMR_log,
            sei = SE.asmr,
            data = asmr2,
            method = "ML",
            mods = ~ cvd_type + sex2 + income_country2 + age_grp,
            test = "knha")
mod2 # all significant 

# Model comparison 1
anova(full_mod, mod2)
# P is not significant opt for the simpler model, the model with lower AICc (full_model)


# Pre-final model
mod2 # study time not include in the final model


# Interaction checking ----------------------------------------------------

mod_interaction <- rma(yi = ASMR_log,
                       sei = SE.asmr,
                       data = asmr2,
                       method = "ML",
                       mods = ~ cvd_type * sex2,
                       test = "knha")
mod_interaction # all not significant

mod_interaction2 <- rma(yi = ASMR_log,
                        sei = SE.asmr,
                        data = asmr2,
                        method = "ML",
                        mods = ~ cvd_type * income_country2,
                        test = "knha")
mod_interaction2 # all not significant

mod_interaction3 <- rma(yi = ASMR_log,
                        sei = SE.asmr,
                        data = asmr2,
                        method = "ML",
                        mods = ~ cvd_type * age_grp,
                        test = "knha")
mod_interaction3 # all not significant

mod_interaction4 <- rma(yi = ASMR_log,
                        sei = SE.asmr,
                        data = asmr2,
                        method = "ML",
                        mods = ~ sex2 * income_country2,
                        test = "knha")
mod_interaction4 # all not significant

mod_interaction5 <- rma(yi = ASMR_log,
                        sei = SE.asmr,
                        data = asmr2,
                        method = "ML",
                        mods = ~ sex2 * age_grp,
                        test = "knha")
mod_interaction5 # all not significant

mod_interaction6 <- rma(yi = ASMR_log,
                        sei = SE.asmr,
                        data = asmr2,
                        method = "ML",
                        mods = ~ age_grp * income_country2 ,
                        test = "knha")
mod_interaction6 # all not significant


# Final model -------------------------------------------------------------

mod2 # study time not include in the final model

# Permutation test --------------------------------------------------------

# Permutation test is done to ensure the robustness of the final model
permutest(mod2)

# Based on the permutation test:
# cvd_type and sex are still significant- thus can be consider as important predictors that influence the pooled effect size


# FINAL model

full_mod


