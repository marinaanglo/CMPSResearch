#library(tidyverse)
library(foreign)
library(MASS)
library(survey)
library(descr)
library(stargazer)
library(qualtRics)
library(tidyverse)
library(forestmangr)
library(broom)

### Data
df<-read.csv("/Volumes/GoogleDrive/My Drive/2020 CMPS/2020cmps.csv", as.is=T)


### race
table(df$S2_Race_Prime)
table(df$S2_Racer4) #3,935 

df$latino <- ifelse(df$S2_Racer2=="Hispanic or Latino",1,0)
table(df$latino)

df$asian <-ifelse(df$S2_Racer4=="Asian American",1,0)
table(df$asian)
## anti-blackness scale

prop.table(table(df$asian, df$Q213r2),1)
prop.table(table(df$latino, df$Q213r2),1)

### subset by Asian 

dat<- subset(df, df$S2_Racer4=="Asian American")


###gender
table(dat$S3b)
dat$woman <- NA
dat$woman[dat$S3b=="Man"] <- 0
dat$woman[dat$S3b=="Woman"] <- 1

table(dat$woman)

### yr born
table(dat$S5_Age)

dat$age <- NA
dat$age[dat$S5_Age=="18-29"] <- 0
dat$age[dat$S5_Age=="30-39"] <- 1
dat$age[dat$S5_Age=="40-49"] <- 2
dat$age[dat$S5_Age=="50-59"] <- 3
dat$age[dat$S5_Age=="60-69"] <- 4
dat$age[dat$S5_Age=="70 +"] <- 5

table(dat$age)

## reg vote
table(dat$S6)

dat$votereg <- ifelse(dat$S6=="Yes, registered to vote", 1, 0)
table(dat$votereg)


## foreign born
table(dat$S7)
dat$usborn <- ifelse(dat$S7== "United States" | dat$S7=="on the Island of Puerto Rico", 1,0)
table(dat$usborn)

### party
table(dat$Q21)
table(dat$Q22)

dat$dem <- ifelse(dat$Q21== "Democrat", 1, 0)
table(dat$dem)

dat$indep <- ifelse(dat$Q21=="Independent", 1, 0)
table(dat$indep)

dat$gop <- ifelse(dat$Q21=="Republican", 1, 0)
table(dat$gop)

### nat origin
table(dat$S8)

prop.table(table(dat$S8))

dat$chinese <- NA
dat$chinese<-ifelse(dat$S8=="Chinese" | dat$S8=="Taiwanese",1,0)
(table(dat$chinese))

dat$indian <- NA
dat$indian<-ifelse(dat$S8=="Indian",1,0)

prop.table(table(dat$indian))

dat$filipino <- ifelse(dat$S8=="Filipino",1,0)
table(dat$filipino)

dat$korean<-NA
dat$korean<-ifelse(dat$S8=="Korean",1,0)

dat$viet<- ifelse(dat$S8=="Vietnamese",1,0)
table(dat$viet)

dat$jpn <- ifelse(dat$S8=="Japanese",1,0)
table(dat$jpn)

dat$pakistani <-ifelse(dat$S8=="Pakistani",1,0)
table(dat$pakistani)

dat$otherasn <- ifelse(dat$S8!= "Chinese" & 
                         dat$S8!= "Indian" & dat$S8!="Filipino" &
                         dat$S8!="Korean" & dat$S8!="Vietnamese" & 
                         dat$S8!="Japanese" & dat$S8!="Pakistani", 1,0)
table(dat$otherasn)

### nat origin together
table(dat$S8)
dat$natorigin<-NA
dat$natorigin[dat$S8=="Chinese"] <- "CN"
dat$natorigin[dat$S8=="Indian"] <- "IND"
dat$natorigin[dat$S8=="Filipino"] <- "FIL"
dat$natorigin[dat$S8=="Korean"] <- "KR"
dat$natorigin[dat$S8=="Vietnamese"] <- "VN"
dat$natorigin[dat$S8=="Japanese"] <- "JP"
dat$natorigin[dat$S8=="Pakistani"] <- "South Asn"
dat$natorigin[dat$S8=="Bangladeshi"] <- "South Asn"
dat$natorigin[dat$S8=="Iranian"] <- "Other"
dat$natorigin[dat$S8=="Lao"] <- "Other"
dat$natorigin[dat$S8=="Cambodian"] <- "Other"
dat$natorigin[dat$S8=="Hmong"] <- "Other"
dat$natorigin[dat$S8=="Taiwanese"] <- "Other"
dat$natorigin[dat$S8=="Thai"] <- "Other"
dat$natorigin[dat$S8=="Pacific Islander (Samoa, Guam, Tonga\x85)"] <- "Other"
dat$natorigin[dat$S8==" Other (Specify) "] <- "Other"
table(dat$natorigin)

##mixed race
table(dat$S2_M)

## education
table(dat$S13)

dat$education<-NA
dat$education[dat$S13=="Grades 1-8"] <- 0
dat$education[dat$S13=="Some High School, but did not graduate"] <- 0
dat$education[dat$S13=="High School graduate or GED"] <- 0
dat$education[dat$S13=="Some college"] <- 0.33
dat$education[dat$S13=="Associates, 2-year degree"] <- 0.33
dat$education[dat$S13=="Bachelors, 4-year degree"] <- 0.66
dat$education[dat$S13=="Post-graduate degree"] <- 1

table(dat$education)
sum(table(dat$education))

### living 
table(dat$S14)

dat$metropolitan<-NA
dat$metropolitan[dat$S14=="Large urban area"] <-"City"
dat$metropolitan[dat$S14=="Large suburb near large city"] <- "Suburb"
dat$metropolitan[dat$S14=="Small suburb near small town or city"] <- "Suburb"
dat$metropolitan[dat$S14=="Small town or small city"] <- "Rural"
dat$metropolitan[dat$S14=="Rural area"] <- "Rural"


table(dat$metropolitan)

dat$suburb <- ifelse(dat$metropolitan=="Suburb",1,0)
dat$city <- ifelse(dat$metropolitan=="City",1,0)
dat$rural <-ifelse(dat$metropolitan=="Rural",1,0)

###current issues
table(dat$Q1r1)
table(dat$Q1r2)
table(dat$Q1r3)
table(dat$Q1r4)
table(dat$Q1r5)
table(dat$Q1r17)
table(dat$Q1r15)

### affect rating of politicians

#trump affect
table(dat$Q2r2)

dat$trump_approve <- ifelse(dat$Q2r2== "Somewhat favorable" 
                            | dat$Q2r2=="Very favorable",1,0)
table(dat$trump_approve)


## biden affect
table(dat$Q2r4)

dat$biden_approve<- 0
dat$biden_approve[dat$Q2r4=="Very favorable"] <- 1
dat$biden_approve[dat$Q2r4=="Somewhat favorable"] <- 1

table(dat$biden_approve)


#vote 2020
table(dat$Q12)
dat$vote2020<-ifelse(dat$Q12=="Yes, I am 100% certain I voted",1,0)
table(dat$vote2020)

#vote 2020 candidate
prop.table(table(dat$natorigin, dat$Q14),1)

table(dat$Q14)
dat$bidenvote<-ifelse(dat$Q14=="Democrats Joe Biden & Kamala Harris",1,0)
table(dat$bidenvote) #majority for biden, highest trump support in Fil, and VN


##strength of party
table(dat$Q22)
prop.table(table(dat$gop, dat$Q22),1)
prop.table(table(dat$dem, dat$Q22),1)

##independent lean
table(dat$Q23)

#party switch
table(dat$Q24)
table(dat$Q25)

#associate republicans with racists
table(dat$Q28r18)
dat$gop_racist<-ifelse(dat$Q28r18=="People who are racist",1,0)
table(dat$gop_racist)
prop.table(table(dat$gop_racist))

##associate Republicans with rich ppl
table(dat$Q28r15)
dat$gop_rich<-ifelse(dat$Q28r15=="Rich people",1,0)
table(dat$gop_rich)

##associate Republicans with old ppl
table(dat$Q28r1)
dat$gop_old <-ifelse(dat$Q28r1=="Older Americans",1,0)

##political interest
table(dat$Q29)
dat$poli_interest<-ifelse(dat$Q29=="Very interested in politics" | dat$Q29=="Somewhat interested",1,0)
table(dat$poli_interest)

##why not interested
table(dat$Q31)
dat$notrust<-ifelse(dat$Q31=="The system is not responsive to people like me" | dat$Q31=="I don\x92t trust politicians",1,0)
table(dat$notrust)

##2020 contact
 table(dat$Q38r1)
table(dat$Q38r2)

##contact by minority advocacy group
table(dat$Q40)

##ideology
table(dat$Q43)

dat$liberal<-ifelse(dat$Q43=="Very Liberal" | dat$Q43=="Somewhat Liberal",1,0)
dat$conservative<-ifelse(dat$Q43=="Somewhat Conservative" | dat$Q43=="Very Conservative",1,0)
dat$moderate<-ifelse(dat$Q43=="Moderate" | dat$Q43=="None of these",1,0)

table(dat$liberal)
table(dat$conservative)
table(dat$moderate)

## trust in gov
table(dat$Q44)
dat$gov_trust <- NA
dat$gov_trust[dat$Q44=="Always"] <-1
dat$gov_trust[dat$Q44=="Most of the time"] <- 0.66
dat$gov_trust[dat$Q44=="Only some of the time"] <- 0.33
dat$gov_trust[dat$Q44=="Never"] <- 0 

table(dat$gov_trust)
##jan 6
table(dat$Q56)

## religion
table(dat$Q58r1)
table(dat$Q58r2)
table(dat$Q58r4)
table(dat$Q58r5)
table(dat$Q58r6)

## neighborhood political homogeneity
table(dat$Q69)

dat$neighbor_similar <-ifelse(dat$Q69=="Somewhat similar" |
                                dat$Q69=="Very similar",1,0)
table(dat$neighbor_similar)

## gmy questions of interest
table(dat$Q630_Q632r1) #no english
table(dat$Q630_Q632r2) #good math
table(dat$Q630_Q632r3) #unamerican

table(dat$Q633) #discrimination impacted life 

##no english
table(dat$Q630_Q632r1)
dat$badeng<-ifelse(dat$Q630_Q632r1=="Yes",1,0)
table(dat$badeng)

#good math

dat$goodmath<-ifelse(dat$Q630_Q632r2=="Yes",1,0)
table(dat$goodmath)


## unamerican 
dat$notAmerican <-ifelse(dat$Q630_Q632r3=="Yes",1,0)
table(dat$notAmerican)

##discfrimination 
prop.table(table(dat$Q627))
table(dat$Q628)

dat$discrimination <- ifelse(dat$Q627=="Yes",1,0)
table(dat$discrimination)
table(dat$Q633)
dat$discrim[dat$Q633=="A lot"] <-1
dat$discrim[dat$Q633=="Some"] <-1
dat$discrim[dat$Q633=="A little"] <-0
dat$discrim[dat$Q633=="Not at all"] <-0
dat$discrim[dat$Q633=="Don\x92t know"] <-0
dat$discrim[dat$Q633=="Refused"] <-0

table(dat$discrim)


##discrim 281
table(dat$Q281r1)
table(dat$Q281r2)
table(dat$Q281r3)
table(dat$Q281r4)


### lf

table(dat$Q551_Q559r5) #asian am lf
dat$lf[dat$Q551_Q559r5=="A huge amount to do with what happens in my life"] <-1
dat$lf[dat$Q551_Q559r5=="A lot to do with what happens in my life"]<-0.75
dat$lf[dat$Q551_Q559r5=="Something to do with what happens in my life"]<-0.5
dat$lf[dat$Q551_Q559r5=="Only a little to do with what happens in my life"]<-0.25
dat$lf[dat$Q551_Q559r5=="Nothing to do with what happens in my life"]<-0

##lf binary
dat$lf_binary<-ifelse(dat$lf>=0.75,1,0)
table(dat$lf_binary)
table(dat$lf)

##micro_scale
dat$micro_scale<- dat$goodmath + dat$badeng
table(dat$micro_scale)

##income
table(dat$Q813)

dat$income[dat$Q813=="Less than $20,000"] <-0
dat$income[dat$Q813=="$20,000 to $29,999"] <- 0
dat$income[dat$Q813=="$30,000 to $39,999"] <- 0
dat$income[dat$Q813=="$40,000 to $49,999"] <- 0
dat$income[dat$Q813=="$50,000 to $59,999"] <- 0.5
dat$income[dat$Q813=="$60,000 to $69,999"] <- 0.5
dat$income[dat$Q813=="$70,000 to $79,999"] <- 0.5
dat$income[dat$Q813=="$80,000 to $89,999"] <- 0.5
dat$income[dat$Q813=="90,000 to $99,999"] <- 0.5
dat$income[dat$Q813=="$100,000 to $149,999"] <- 1
dat$income[dat$Q813=="$150,000 to $199,999"] <- 1
dat$income[dat$Q813=="$200,000 or more"] <- 1
dat$income[dat$Q813=="Prefer not to answer"] <- 0.5

###marital status

table(dat$Q741)

dat$married[dat$Q741=="Married"] <-1
dat$married[dat$Q741=="Single, not in a relationship"] <-0
dat$married[dat$Q741=="In a romantic relationship, but not living together"] <-0
dat$married[dat$Q741=="In a relationship and living together"] <-0
dat$married[dat$Q741=="Divorced"] <-0
dat$married[dat$Q741=="Widowed"] <-0

table(dat$lf_binary)
table(dat$micro_scale)

summary(is.na(dat$lf_binary))
summary(is.na(dat$micro_scale))
summary(is.na(dat$discrim))
summary(is.na(dat$usborn))
summary(is.na(dat$DEM))
summary(is.na(dat$woman))
summary(is.na(dat$married))

###model
model_lf<-glm(dat$lf_binary~dat$micro_scale + dat$discrimination + 
                dat$usborn + dat$dem + dat$woman + dat$income + 
                dat$married, 
              family=binomial(link='logit'), data=dat)

summary(model_lf)

model_lf2<-lm(dat$lf ~ dat$micro_scale + dat$discrimination + 
                dat$usborn + dat$dem + dat$woman + dat$income + 
                dat$married, data=dat)
summary(model_lf2)


model_lf3 <- glm(dat$lf_binary ~ dat$badeng + dat$goodmath + dat$discrim + 
                   dat$usborn + dat$dem + dat$woman + dat$income + 
                   dat$married,
                 family=binomial(link='logit'), data=dat)

summary(model_lf3)

###good math seems to be the driving thing behind lf?


##being asian am important to identity
table(dat$Q271)

dat$id_import[dat$Q271=="Extremely important"] <- 1
dat$id_import[dat$Q271=="Moderately important"] <- 0.5
dat$id_import[dat$Q271=="Not at all important"] <- 0
dat$id_import[dat$Q271=="Slightly important"] <- 0.25
dat$id_import[dat$Q271=="Very important"] <- 0.75

table(dat$id_import)

prop.table(table(dat$natorigin))
model_idimport<-lm(dat$id_import ~ dat$badeng + dat$goodmath + dat$discrim + 
                     + dat$chinese + dat$usborn + dat$dem + dat$woman + dat$income + dat$married,
                   data=dat )


summary(model_idimport)

model_lf_1<-lm(dat$lf_binary ~ dat$badeng + dat$goodmath + dat$discrim + dat$chinese + dat$usborn + dat$dem + dat$woman + dat$income + dat$married,data=dat )
table(dat$model_lf_1)

ggplot(dat, aes(x=id_import, y=bidenvote)) + geom_point() + theme_minimal()


table(dat$micro_scale)
table(dat$badeng)
# Plot Predicted data and original data points
ggplot(dat, aes(x=badeng, y=id_import)) + geom_point() +
  stat_smooth(method="glm", color="green", se=FALSE,
              method.args = list(family=binomial))

ggplot(dat, aes(x=goodmath, y=id_import)) + geom_point() +
  stat_smooth(method="glm", color="green", se=FALSE,
              method.args = list(family=binomial))

###forest plots 
library(broom.mixed)
plot_summs(model_idimport, scale=TRUE)

#broom to tidy model output
model_output <- tidy(model_lf2)
out_conf <- tidy(model_lf2, conf.int = TRUE)

#forest to remove intercept
lm_model_out <- round_df(out_conf, digits=2)
lm_model_out <- lm_model_out[-1,] #remove the intercept 

# ggplot
ggplot(lm_model_out, aes(x=estimate, y=estimate)) +
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high), 
                width = 0.2,size  = 1,
                position = "dodge", color="turquoise4") +
  geom_hline(yintercept = 0, color = "red", size = 1) +
  geom_point() + coord_flip() 

plot_summs(model_lf2, 
           coefs= c("Microaggressions" = micro_scale,
                    "Discrimination" = discrim,
                    "US Born" = usborn,
                    "Democrat" = dem,
                    "Woman" = woman,
                    "Income" = income,
                    "Married" = married), scale = TRUE, size=3) 

table(dat$model_lf2)

plot_summs(fit1, coefs = c("My pretty new name 1" = ugly_name_1, "My pretty new name 2" = ugly_name_2))
#cronbach alpha??????? 
#micro, discrim, usborn, national-origins, democrat, woman, marry, income
#outcome: LF and common political 


##plots for logit regression
####affirmative
table(df$Q417_Q424r7)
table(df$Q417_Q424r8)

