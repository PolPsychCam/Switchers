#this R script contains:
#
# 1) the code for the analysis of voters who switched from one of the 
#mainstream parties (Labour,Libdem or Conservative) to UKIP between the 2010 
#and 2015 general elections (from the beginning of this script to line 780 approximately)
#
# 2) the code for the analysis of voters who switched to any party,
#not only to UKIP (from line 780 to 1130 approximately)

#the data can be downloaded from this link:
#https://www.britishelectionstudy.com/data-object/british-election-study-combined-wave-internet-panel/
#when landing on the page, click "Download". The data files will appear in different formats
#download the file in SPSS format

library(haven)
library(plyr)
library(dplyr)
library(gridExtra)
library(corrplot)
library(VGAM)
library(car)

install.packages("corrplot")
install.packages("VGAM")
install.packages("car")

# load data
MyData1 = haven::read_sav("../Data/BES2017_W14_Panel_v0.4.sav")

#assign all "9999"s to "NA" for trust in MPs and authoritarianism scale
MyData1$trustMPsW6[MyData1$trustMPsW6 %in% 9999] = NA
MyData1$al_scaleW6[MyData1$al_scaleW6 %in% 9999] = NA

#set to "NA" all the 'don't knows' and 'prefer not to say' for the personal income variable
MyData1$profile_gross_personal[MyData1$profile_gross_personal %in% 9999] =  NA
MyData1$profile_gross_personal[MyData1$profile_gross_personal %in% 15] =  NA
MyData1$profile_gross_personal[MyData1$profile_gross_personal %in% 16] =  NA

# edit actively open minded variables so that a higher score indicates greater open-mindedness

MyData1$aom1W7[MyData1$aom1W7 %in% 9999] = NA
MyData1$aom2W7[MyData1$aom2W7 %in% 9999] = NA
MyData1$aom3W7[MyData1$aom3W7 %in% 9999] = NA
MyData1$aom4W7[MyData1$aom4W7 %in% 9999] = NA
MyData1$aom5W7[MyData1$aom5W7 %in% 9999] = NA
MyData1$aom6W7[MyData1$aom6W7 %in% 9999] = NA
MyData1$aom7W7[MyData1$aom7W7 %in% 9999] = NA

MyData1$aom4W7rev = 6-MyData1$aom4W7
MyData1$aom5W7rev = 6-MyData1$aom5W7
MyData1$aom6W7rev = 6-MyData1$aom6W7
MyData1$aom7W7rev = 6-MyData1$aom7W7

#take the average of the 7 actively open minded items

MyData1$aomaverage= (MyData1$aom1W7+
                       MyData1$aom2W7+
                       MyData1$aom3W7+
                       MyData1$aom4W7rev+
                       MyData1$aom5W7rev+
                       MyData1$aom6W7rev+
                       MyData1$aom7W7rev)/7

MyData1$aomaverage[MyData1$aomaverage %in% 9999] = NA

#recode authoritarianism so that there are 10  values instead of 20
#and so that there are only integer values

MyData1$al_scaleW6[MyData1$al_scaleW6 %in% 0.5] = 0
MyData1$al_scaleW6[MyData1$al_scaleW6 %in% 1.5] = 1
MyData1$al_scaleW6[MyData1$al_scaleW6 %in% 2.5] = 2
MyData1$al_scaleW6[MyData1$al_scaleW6 %in% 3.5] = 3
MyData1$al_scaleW6[MyData1$al_scaleW6 %in% 4.5] = 4
MyData1$al_scaleW6[MyData1$al_scaleW6 %in% 5.5] = 5
MyData1$al_scaleW6[MyData1$al_scaleW6 %in% 6.5] = 6
MyData1$al_scaleW6[MyData1$al_scaleW6 %in% 7.5] = 7
MyData1$al_scaleW6[MyData1$al_scaleW6 %in% 8.5] = 8
MyData1$al_scaleW6[MyData1$al_scaleW6 %in% 9.5] = 9

# rename Big 5 variables to shorten their labels (without creating new objects)

colnames(MyData1)[colnames(MyData1)=="personality_openness"]          <- "B5O"
colnames(MyData1)[colnames(MyData1)=="personality_conscientiousness"] <- "B5C"
colnames(MyData1)[colnames(MyData1)=="personality_extraversion"]      <- "B5E"
colnames(MyData1)[colnames(MyData1)=="personality_agreeableness"]     <- "B5A"
colnames(MyData1)[colnames(MyData1)=="personality_neuroticism"]       <- "B5N"

#create dataframe with only people who voted for Labour in 2010 

LabourVoters2010 = subset(MyData1, MyData1$profile_past_vote_2010 == 3)

#create dataframe with 2010 Labour voters who voted Labour or UKIP in 2015

LabourandUkipVoters2015 = subset(LabourVoters2010, LabourVoters2010$generalElectionVoteW6 == 2 #voted Labour
                                 | LabourVoters2010$generalElectionVoteW6 == 6) #voted UKIP

#create df with 2010 Libdem voters who switched to UKIP in 2015

LibdemVoters2010 = subset(MyData1, MyData1$profile_past_vote_2010 == 4)

LibdemandUkipVoters2015 = subset(LibdemVoters2010, LibdemVoters2010$generalElectionVoteW6 == 3 #voted Libdem
                                 | LibdemVoters2010$generalElectionVoteW6 == 6) #voted UKIP

#create df with 2010 Conservative voters who switched to UKIP in 2015

ConservativeVoters2010 = subset(MyData1, MyData1$profile_past_vote_2010 == 2)

ConservativeAndUkipVoters2015 = subset(ConservativeVoters2010, ConservativeVoters2010$generalElectionVoteW6 == 1 #voted Conservative
                                       | ConservativeVoters2010$generalElectionVoteW6 == 6) #voted UKIP

#change the number associated with each value of the 'general election vote' variable so that 
#voting in 2015 for the same party as in 2010 is equal to 0 and switching to another party is equal to 1

LabourandUkipVoters2015$generalElectionVoteW6[LabourandUkipVoters2015$generalElectionVoteW6 %in% 2]<-0
LabourandUkipVoters2015$generalElectionVoteW6[LabourandUkipVoters2015$generalElectionVoteW6 %in% 6]<-1

LibdemandUkipVoters2015$generalElectionVoteW6[LibdemandUkipVoters2015$generalElectionVoteW6 %in% 3]<-0
LibdemandUkipVoters2015$generalElectionVoteW6[LibdemandUkipVoters2015$generalElectionVoteW6 %in% 6]<-1

ConservativeAndUkipVoters2015$generalElectionVoteW6[ConservativeAndUkipVoters2015$generalElectionVoteW6 %in% 1]<-0
ConservativeAndUkipVoters2015$generalElectionVoteW6[ConservativeAndUkipVoters2015$generalElectionVoteW6 %in% 6]<-1

# to ensure that the assumption of normality was respected, the following checks were done 
# to make sure that the continuous predictors had normal distributions. 
# Predictors that did not have a normal distribution were removed. 
# The Big 5 personality traits predictors agreebleness and neuroticism were removed for this reason. 

#check that the predictors have normal distributions - Labour to UKIP switchers

hist(LabourandUkipVoters2015$trustMPsW6) #yes
hist(LabourandUkipVoters2015$al_scaleW6) #Yes
hist(LabourandUkipVoters2015$B5O) #Yes
hist(LabourandUkipVoters2015$B5C) #yes
hist(LabourandUkipVoters2015$B5E) # Yes
hist(LabourandUkipVoters2015$B5A) #No
hist(LabourandUkipVoters2015$B5N) #No
hist(LabourandUkipVoters2015$aomaverage) #yes

#check that the predictors have normal distributions - Libdem to UKIP

hist(LibdemandUkipVoters2015$trustMPsW6) #yes
hist(LibdemandUkipVoters2015$al_scaleW6) # Yes
hist(LibdemandUkipVoters2015$B5O) # Yes
hist(LibdemandUkipVoters2015$B5C) # yes
hist(LibdemandUkipVoters2015$B5E) # Yes
hist(LibdemandUkipVoters2015$B5A) # No
hist(LibdemandUkipVoters2015$B5N) # No
hist(LibdemandUkipVoters2015$aomaverage) # yes

#check that the predictors have normal distributions - Conservative to UKIP

hist(ConservativeAndUkipVoters2015$trustMPsW6) #Yes
hist(ConservativeAndUkipVoters2015$al_scaleW6) # yes
hist(ConservativeAndUkipVoters2015$B5O) # yes
hist(ConservativeAndUkipVoters2015$B5C) # no
hist(ConservativeAndUkipVoters2015$B5E) # yes
hist(ConservativeAndUkipVoters2015$B5A) # no
hist(ConservativeAndUkipVoters2015$B5N) # no
hist(ConservativeAndUkipVoters2015$aomaverage) # yes

#models with individual predictors. The purpose of this section is to identify and remove the predictors
#that individually do not have a significant effect on switching. Keeping predictors which were not significant 
#individually risked inflating R squared without increasing the model's predictive power.

#Labour to UKIP

TrustMPs = glm(generalElectionVoteW6~trustMPsW6, 
               LabourandUkipVoters2015, family=binomial(link = "logit") )

Al_Scale = glm(generalElectionVoteW6~al_scaleW6, 
               LabourandUkipVoters2015, family=binomial(link = "logit") )

Aomaverage = glm(generalElectionVoteW6~aomaverage, 
                 LabourandUkipVoters2015, family=binomial(link = "logit") )

Openness = glm(generalElectionVoteW6~B5O, 
               LabourandUkipVoters2015, family=binomial(link = "logit") )

Conscientiousness = glm(generalElectionVoteW6~B5C, 
                        LabourandUkipVoters2015, family=binomial(link = "logit") )

Extraversion = glm(generalElectionVoteW6~B5E, 
                   LabourandUkipVoters2015, family=binomial(link = "logit") )

WorkStatus = glm(generalElectionVoteW6~profile_work_statW7, 
                 LabourandUkipVoters2015, family=binomial(link = "logit") )

WorkType = glm(generalElectionVoteW6~profile_work_typeW7W10, 
               LabourandUkipVoters2015, family=binomial(link = "logit") )

SubjectiveClassPerception = glm(generalElectionVoteW6~subjClassW2_W4W7W9, 
                                LabourandUkipVoters2015, family=binomial(link = "logit") )

AgeGroup = glm(generalElectionVoteW6~ageGroup, 
               LabourandUkipVoters2015, family=binomial(link = "logit") )

Age = glm(generalElectionVoteW6~ageW7, 
          LabourandUkipVoters2015, family=binomial(link = "logit") )

Income = glm(generalElectionVoteW6~profile_gross_personal, 
             LabourandUkipVoters2015, family=binomial(link = "logit") )

Gender = glm(generalElectionVoteW6~gender, 
             LabourandUkipVoters2015, family=binomial(link = "logit") )

EducationLevel = glm(generalElectionVoteW6~edlevelW7, 
                     LabourandUkipVoters2015, family=binomial(link = "logit") )

Ethnicity = glm(generalElectionVoteW6~profile_ethnicity, 
                LabourandUkipVoters2015, family=binomial(link = "logit") )

Geography = glm(generalElectionVoteW6~gorW6, 
                LabourandUkipVoters2015, family=binomial(link = "logit") )

summary(TrustMPs) #No - not significant
summary(Al_Scale) # Yes - significant
summary(Aomaverage) # yes
summary(Openness) # Yes
summary(Conscientiousness) #No
summary(Extraversion) #No
summary(WorkStatus) # No
summary(WorkType) #Yes
summary(SubjectiveClassPerception) #No
summary(AgeGroup) #Yes
summary(Age) #yes
summary(Income) #No
summary(Gender) #Yes
summary(EducationLevel) # Yes
summary(Ethnicity) # Yes
summary(Geography) #Yes

#models with individual predictors. The purpose of this section is to identify and remove the predictors
#that individually do not have a significant effect on switching 
#Libdem to UKIP

TrustMPs2 = glm(generalElectionVoteW6~trustMPsW6, 
                LibdemandUkipVoters2015, family=binomial(link = "logit") )

Al_Scale2 = glm(generalElectionVoteW6~al_scaleW6, 
                LibdemandUkipVoters2015, family=binomial(link = "logit") )

Aomaverage2 = glm(generalElectionVoteW6~aomaverage, 
                  LibdemandUkipVoters2015, family=binomial(link = "logit") )

Openness2 = glm(generalElectionVoteW6~B5O, 
                LibdemandUkipVoters2015, family=binomial(link = "logit") )

Conscientiousness2 = glm(generalElectionVoteW6~B5C, 
                         LibdemandUkipVoters2015, family=binomial(link = "logit") )

Extraversion2 = glm(generalElectionVoteW6~B5E, 
                    LibdemandUkipVoters2015, family=binomial(link = "logit") )

WorkStatus2 = glm(generalElectionVoteW6~profile_work_statW7, 
                  LibdemandUkipVoters2015, family=binomial(link = "logit") )

WorkType2 = glm(generalElectionVoteW6~profile_work_typeW7W10, 
                LibdemandUkipVoters2015, family=binomial(link = "logit") )

SubjectiveClassPerception2 = glm(generalElectionVoteW6~subjClassW2_W4W7W9, 
                                 LibdemandUkipVoters2015, family=binomial(link = "logit") )

AgeGroup2 = glm(generalElectionVoteW6~ageGroup, 
                LibdemandUkipVoters2015, family=binomial(link = "logit") )

Income2 = glm(generalElectionVoteW6~profile_gross_personal, 
              LibdemandUkipVoters2015, family=binomial(link = "logit") )

Gender2 = glm(generalElectionVoteW6~gender, 
              LibdemandUkipVoters2015, family=binomial(link = "logit") )

EducationLevel2 = glm(generalElectionVoteW6~edlevelW7, 
                      LibdemandUkipVoters2015, family=binomial(link = "logit") )

Ethnicity2 = glm(generalElectionVoteW6~profile_ethnicity, 
                 LibdemandUkipVoters2015, family=binomial(link = "logit") )

Geography2 = glm(generalElectionVoteW6~gorW6, 
                 LibdemandUkipVoters2015, family=binomial(link = "logit") )

summary(TrustMPs2) # No - not significant
summary(Al_Scale2) # Yes - significant
summary(Aomaverage2) # No
summary(Openness2) # Yes
summary(Conscientiousness2) # No
summary(Extraversion2) #No
summary(WorkStatus2) # No
summary(WorkType2) # Yes
summary(SubjectiveClassPerception2) #No
summary(AgeGroup2) # Yes
summary(Income2) #No
summary(Gender2) #Yes
summary(EducationLevel2) # Yes
summary(Ethnicity2) # No
summary(Geography2) #Yes

#Conservative to UKIP

TrustMPs3 = glm(generalElectionVoteW6~trustMPsW6, 
                ConservativeAndUkipVoters2015, family=binomial(link = "logit") )

Al_Scale3 = glm(generalElectionVoteW6~al_scaleW6, 
                ConservativeAndUkipVoters2015, family=binomial(link = "logit") )

Aomaverage3 = glm(generalElectionVoteW6~aomaverage, 
                  ConservativeAndUkipVoters2015, family=binomial(link = "logit") )

Openness3 = glm(generalElectionVoteW6~B5O, 
                ConservativeAndUkipVoters2015, family=binomial(link = "logit") )

Extraversion3 = glm(generalElectionVoteW6~B5E, 
                    ConservativeAndUkipVoters2015, family=binomial(link = "logit") )

WorkStatus3 = glm(generalElectionVoteW6~profile_work_statW7, 
                  ConservativeAndUkipVoters2015, family=binomial(link = "logit") )

WorkType3 = glm(generalElectionVoteW6~profile_work_typeW7W10, 
                ConservativeAndUkipVoters2015, family=binomial(link = "logit") )

SubjectiveClassPerception3 = glm(generalElectionVoteW6~subjClassW2_W4W7W9, 
                                 ConservativeAndUkipVoters2015, family=binomial(link = "logit") )

AgeGroup3 = glm(generalElectionVoteW6~ageGroup, 
                ConservativeAndUkipVoters2015, family=binomial(link = "logit") )

Income3 = glm(generalElectionVoteW6~profile_gross_personal, 
              ConservativeAndUkipVoters2015, family=binomial(link = "logit") )

Gender3 = glm(generalElectionVoteW6~gender, 
              ConservativeAndUkipVoters2015, family=binomial(link = "logit") )

EducationLevel3 = glm(generalElectionVoteW6~edlevelW7, 
                      ConservativeAndUkipVoters2015, family=binomial(link = "logit") )

Ethnicity3 = glm(generalElectionVoteW6~profile_ethnicity, 
                 ConservativeAndUkipVoters2015, family=binomial(link = "logit") )

Geography3 = glm(generalElectionVoteW6~gorW6, 
                 ConservativeAndUkipVoters2015, family=binomial(link = "logit") )

summary(TrustMPs3) #yes - significant
summary(Al_Scale3) # yes
summary(Aomaverage3) # no - not significant
summary(Openness3) # no
summary(Extraversion3) #no
summary(WorkStatus3) # yes
summary(WorkType3) #yes
summary(SubjectiveClassPerception3) #no
summary(AgeGroup3) #yes
summary(Income3) #no
summary(Gender3) #yes
summary(EducationLevel3) # yes
summary(Ethnicity3) # yes
summary(Geography3) #yes

#the following GVIF sections give the Generalised Variance Inflation Factors (GVIF).
# A GVIF score above 10 indicates that a predictor presents a high risk of co-linearity 
#with another predictor, and should be removed. All predictors here had GVIF scores below 2 
#and passed this check.

#GVIF - Labour to UKIP

vif(glm(generalElectionVoteW6~al_scaleW6
        + B5O
        + profile_work_typeW7W10
        + ageGroup
        + gender
        + edlevelW7
        + profile_gross_personal
        + gorW6,
        LabourandUkipVoters2015, family = binomial(link = "logit")))

#GVIF - Libdem to UKIP

vif(glm(generalElectionVoteW6~al_scaleW6
        + B5O
        + profile_work_typeW7W10
        + ageGroup
        + gender
        + profile_ethnicity
        + edlevelW7
        + gorW6,
        LibdemandUkipVoters2015, family = binomial(link = "logit")))

#GVIF - Conservative to UKIP

vif(glm(generalElectionVoteW6~al_scaleW6
        + trustMPsW6
        + profile_work_statW7
        + profile_work_typeW7W10
        + ageGroup
        + gender
        + profile_ethnicity
        + edlevelW7
        + gorW6,
        ConservativeAndUkipVoters2015, family = binomial(link = "logit")))

#corrplots - the following corrplot sections show the correlations between pairs of predictors.
#In line with the literature's convention, predictors with a pairwise correlation above 0.5 were removed

#corrplot - Labour to UKIP

which(colnames(LabourandUkipVoters2015)=="al_scaleW6")
which(colnames(LabourandUkipVoters2015)=="aomaverage")
which(colnames(LabourandUkipVoters2015)=="B5O")
which(colnames(LabourandUkipVoters2015)=="profile_work_typeW7W10")
which(colnames(LabourandUkipVoters2015)=="gender")
which(colnames(LabourandUkipVoters2015)=="ageGroup")
which(colnames(LabourandUkipVoters2015)=="ageW7")
which(colnames(LabourandUkipVoters2015)=="edlevelW7")
which(colnames(LabourandUkipVoters2015)=="profile_ethnicity")
which(colnames(LabourandUkipVoters2015)=="gorW6")
which(colnames(LabourandUkipVoters2015)=="profile_gross_personal")

(corrplot(cor(LabourandUkipVoters2015[ ,c(5651,5709,5552,5488,5478,3088,5441,5526,5477,5530)], 
              LabourandUkipVoters2015[,c(5651,5709,5552,5488,5478,3088,5441,5526,5477,5530)],
              use = "complete.obs", method="spearman"), method = "number", tl.cex= 0.5, cex.axis=4))

#corrplot - Libdem to UKIP

which(colnames(LibdemandUkipVoters2015)=="al_scaleW6")
which(colnames(LibdemandUkipVoters2015)=="B5O")
which(colnames(LibdemandUkipVoters2015)=="profile_work_typeW7W10")
which(colnames(LibdemandUkipVoters2015)=="gender")
which(colnames(LibdemandUkipVoters2015)=="ageGroup")
which(colnames(LibdemandUkipVoters2015)=="edlevelW7")
which(colnames(LibdemandUkipVoters2015)=="gorW6")


(corrplot(cor(LibdemandUkipVoters2015[ ,c(5651,5552,5488,5478,5447,5441,5477)], 
              LibdemandUkipVoters2015[,c(5651,5552,5488,5478,5447,5441,5477)],
              use = "complete.obs", method="spearman"), method = "number", tl.cex= 1, cex.axis=4))

#corrplot - Conservative to UKIP

which(colnames(ConservativeAndUkipVoters2015)=="al_scaleW6")
which(colnames(ConservativeAndUkipVoters2015)=="trustMPsW6")
which(colnames(ConservativeAndUkipVoters2015)=="profile_work_typeW7W10")
which(colnames(ConservativeAndUkipVoters2015)=="profile_work_statW7")
which(colnames(ConservativeAndUkipVoters2015)=="gender")
which(colnames(ConservativeAndUkipVoters2015)=="ageGroup")
which(colnames(ConservativeAndUkipVoters2015)=="edlevelW7")
which(colnames(ConservativeAndUkipVoters2015)=="profile_ethnicity")
which(colnames(ConservativeAndUkipVoters2015)=="gorW6")

(corrplot(cor(ConservativeAndUkipVoters2015[ ,c(5651,2224,5488,5487,5478,5447,5441,5526,5477)], 
              ConservativeAndUkipVoters2015[,c(5651,2224,5488,5487,5478,5447,5441,5526,5477)],
              use = "complete.obs", method="spearman"), method = "number", tl.cex= 1, cex.axis=4))

#******************************************************************************************************

#TESTING ALL PREDICTORS IN THE SAME MODEL - all the predictors that passed the robustness checks 
#in the previous sections are included in the logistic regressions below. 

#full model with all variables predicting switching from Labour to UKIP - removed "work type" which was 
#above 0.5 correlation threshold with education level (-0.58 correlation)
#removed aomaverage because it has 4467 observations with missing values, which
#make the model delete too many observations due to missingness.
#This, in turn, biases the model's residual deviance by making it artificially low.

#create df with only the variables that will be used in order to rid them of NAs
#reference: https://bugsdb.com/_en/debug/0580fbef27e31bcfee588577033bdb79
#the reason this wasn't working was that you had '+' signs instead of commas
#to separate the arguments

library('dplyr')

dfLabour =  select(LabourandUkipVoters2015, generalElectionVoteW6,  
                   al_scaleW6 ,
                   B5O,
                   ageGroup ,
                   gender,
                   edlevelW7,
                   profile_work_typeW7W10,
                   profile_ethnicity ,
                   gorW6)

dfConservative =  select(ConservativeAndUkipVoters2015, generalElectionVoteW6, al_scaleW6,
                         trustMPsW6,
                         ageGroup,
                         gender,
                         edlevelW7,
                         profile_work_typeW7W10,
                         profile_ethnicity, 
                         gorW6)

dfLibdem =  select(LibdemandUkipVoters2015, generalElectionVoteW6, al_scaleW6,
                   B5O,
                   ageGroup, 
                   gender,
                   edlevelW7,
                   profile_work_typeW7W10,
                   gorW6)

#get rid of NAs as the likelihood ratio test won't work with them

dfLabour = na.omit(dfLabour)
dfConservative = na.omit(dfConservative)
dfLibdem = na.omit(dfLibdem)

FullModel = glm(generalElectionVoteW6~al_scaleW6 
                + B5O
                + ageGroup 
                + gender
                + edlevelW7
                + profile_work_typeW7W10
                + profile_ethnicity 
                + gorW6 , dfLabour, family=binomial (link = "logit") )

summary(FullModel)

#Labour model with demographic predictors only

LabDemo = glm(generalElectionVoteW6~ageGroup
              + gender
              + edlevelW7
              + profile_work_typeW7W10
              + profile_ethnicity 
              + gorW6 , dfLabour, family=binomial (link = "logit") )

summary(LabDemo)

#Labour model with psychological predictors only

LabPsycho = glm(generalElectionVoteW6~al_scaleW6
                + B5O, dfLabour, family=binomial (link = "logit") )

summary(LabPsycho)

#full model with all variables predicting switching from Conservative to UKIP 
#removed "work type" which reached the 0.5 correlation threshold with edlevel (-0.5 correlation) 
#and "work status" (0.53 correlation with age group)

FullModelConservative = glm(generalElectionVoteW6~al_scaleW6
                            + trustMPsW6
                            + ageGroup 
                            + gender
                            + edlevelW7
                            + profile_work_typeW7W10
                            + profile_ethnicity 
                            + gorW6 , dfConservative, family=binomial(link = "logit") )

summary(FullModelConservative)

ConDemo = glm(generalElectionVoteW6~ageGroup
              + gender
              + edlevelW7
              + profile_work_typeW7W10
              + profile_ethnicity 
              + gorW6 , dfConservative, family=binomial(link = "logit") )

summary(ConDemo)

ConPsycho = glm(generalElectionVoteW6~al_scaleW6
                + trustMPsW6, dfConservative, family=binomial(link = "logit") )

summary(ConPsycho)

# full model with all variables predicting switching from Libdem to UKIP
# removed "work type" which was above 0.5 correlation threshold with edlevel (-0.58 correlation)

FullModelLibdem = glm(generalElectionVoteW6~al_scaleW6
                      + B5O
                      + ageGroup 
                      + gender
                      + edlevelW7
                      + profile_work_typeW7W10
                      + gorW6 , dfLibdem, family=binomial(link = "logit") )

summary(FullModelLibdem)

LibDemo = glm(generalElectionVoteW6~ageGroup
              + gender
              + edlevelW7
              + profile_work_typeW7W10
              + gorW6 , dfLibdem, family=binomial(link = "logit") )

summary(LibDemo)

LibPsycho = glm(generalElectionVoteW6~al_scaleW6
                + B5O, dfLibdem, family=binomial(link = "logit") )

summary(LibPsycho)

#likelihood ratio tests comparing combined models to models with only demographic predictors

anova(FullModel,LabDemo,test="Chisq")

anova(FullModelConservative,ConDemo,test="Chisq")

anova(FullModelLibdem,LibDemo,test="Chisq")

#present all models in a single table

install.packages('jtools')
install.packages('huxtable')
install.packages('Rcpp')
install.packages('RInside')
install.packages('officer')
install.packages("flextable")
library(jtools)
library(huxtable)
library(Rcpp)
library(RInside)
library(officer)
library(flextable)

export_summs(FullModel, FullModelLibdem, FullModelConservative, ModelAllVoters,
             model.names = c("Labour to UKIP","Liberal Democrat to UKIP", 'Conservative to UKIP', 'All Voters'),
             scale = FALSE, #if you don't set this to false, it rescales your coefficients and makes them very confusing
             coefs = c('Authoritarianism' = 'al_scaleW6',
                       'Openness' = 'B5O',
                       'Trust in MPs' = 'trustMPsW6',
                       'Age Group' = 'ageGroup',
                       'Gender' = 'gender',
                       'Education Level' = 'edlevelW7',
                       'Work Type' = 'profile_work_typeW7W10',
                       'Work Status' = 'profile_work_statW7',
                       'Ethnicity' = 'profile_ethnicity',
                       'Geography' = 'gorW6'),
             summ.glm = c(N = "nobs", AIC = "AIC", BIC = "BIC"),
             to.file = 'Word',
             file.name = 'Export.docx')

#**************************************************************************************************************

#comparing Proportional Reduction of Deviance between psycho only and demo only models

#Labour

#model with only psychological predictors vs model with only demographic predictors
LabPsychoVSDemo = (LabDemo$deviance - LabPsycho$deviance)/LabDemo$deviance
LabPsychoVSDemo

LabChiPsychoVSDemo = LabDemo$deviance - LabPsycho$deviance
LabChiPsychoVSDemo

#demo model vs psycho model

(LabPsycho$deviance - LabDemo$deviance )/LabPsycho$deviance
LabPsycho$deviance - LabDemo$deviance

#Conservative

#model with only psychological predictors vs model with only demographic predictors
ConPsychoVSDemo = (ConDemo$deviance - ConPsycho$deviance)/ConDemo$deviance
ConPsychoVSDemo

ConChiPsychoVSDemo = ConDemo$deviance - ConPsycho$deviance
ConChiPsychoVSDemo

#demo model vs psycho model

(ConPsycho$deviance - ConDemo$deviance )/ConPsycho$deviance
ConPsycho$deviance - ConDemo$deviance

#Liberal Democrat

#model with only psychological predictors vs model with only demographic predictors
LibPsychoVSDemo = (LibDemo$deviance - LibPsycho$deviance)/LibDemo$deviance
LibPsychoVSDemo

LibChiPsychoVSDemo = LibDemo$deviance - LibPsycho$deviance
LibChiPsychoVSDemo

#demo model vs psycho model

(LibPsycho$deviance - LibDemo$deviance )/LibPsycho$deviance
LibPsycho$deviance - LibDemo$deviance

#***************************************************************************************

#OBTAINING PROBABILITIES 

#the purpose of the following section is to obtain probabilities of switching
#instead of the log odds of switching which are the output of the logistic regressions
#with all the predictors. I obtain switching probabilities with the R function predict()

#obtaining probabilities - Labour to UKIP

#I replace ageGroup with age so that age becomes a continuous rather than categorical predictor. 
#I exclude categorical variables here in order to look at switching behaviour across the entire 
#voting population rather than at specific geographic, gender, ethnic or age categories

FullModel2 = glm(generalElectionVoteW6~al_scaleW6
                 +aomaverage
                 +B5O
                 +edlevelW7
                 +ageW7, dfLabour, family=binomial (link = "logit") )

summary(FullModel2)

#probability of switching for Labour voters with low authoritarianism

#here I vary the value of the predictor of interest -authoritarianism-
#while keeping all other predictors at their means 

newdata1 = data.frame(al_scaleW6 = 1, #this is the authoritarianism score
                      aomaverage = mean(dfLabour$aomaverage, na.rm=TRUE),
                      B5O = mean(dfLabour$B5O, na.rm = TRUE),
                      edlevelW7 = mean(dfLabour$edlevelW7, na.rm = TRUE),
                      ageW7 = mean(dfLabour$ageW7, na.rm = TRUE))

predict(FullModel2, newdata1, type = "response")

#probability of switching for Labour voters with high authoritarianism

newdata2 = data.frame(al_scaleW6 = 10, 
                      aomaverage = mean(dfLabour$aomaverage, na.rm=TRUE),
                      B5O = mean(dfLabour$B5O, na.rm = TRUE),
                      edlevelW7 = mean(dfLabour$edlevelW7, na.rm = TRUE),
                      ageW7 = mean(dfLabour$ageW7, na.rm = TRUE))

predict(FullModel2, newdata2, type = "response")

#**********************************************************************************************

# obtaining probabilities of switching from Conservative to UKIP

FullModelConservative2 = glm(generalElectionVoteW6~al_scaleW6
                             + trustMPsW6
                             + ageW7
                             + edlevelW7,
                             dfConservative, family=binomial(link = "logit") )

summary(FullModelConservative2)

#probability of switching for Conservative voters with low authoritarianism

newdataCons1 = data.frame(al_scaleW6 = 1, 
                          trustMPsW6 = mean(dfConservative$trustMPsW6, na.rm = TRUE),
                          edlevelW7 = mean(dfConservative$edlevelW7, na.rm = TRUE),
                          ageW7 = mean(dfConservative$ageW7, na.rm = TRUE))

predict(FullModelConservative2, newdataCons1, type = "response")

#probability of switching for Conservative voters with high authoritarianism

newdataCons2 = data.frame(al_scaleW6 = 10, 
                          trustMPsW6 = mean(dfConservative$trustMPsW6, na.rm = TRUE),
                          edlevelW7 = mean(dfConservative$edlevelW7, na.rm = TRUE),
                          ageW7 = mean(dfConservative$ageW7, na.rm = TRUE))

predict(FullModelConservative2, newdataCons2, type = "response")

#************************************************************************************************

# obtaining probabilities - Libdem to UKIP

FullModelLibdem2 = glm(generalElectionVoteW6~al_scaleW6
                       + B5O
                       + ageW7 
                       + edlevelW7, dfLibdem, family=binomial(link = "logit") )

summary(FullModelLibdem2)

#probability of switching for Libdem voters with low authoritarianism

newdataLibdem1 = data.frame(al_scaleW6 = 1, 
                            B5O = mean(dfLibdem$B5O, na.rm = TRUE),
                            edlevelW7 = mean(dfLibdem$edlevelW7, na.rm = TRUE),
                            ageW7 = mean(dfLibdem$ageW7, na.rm = TRUE))

predict(FullModelLibdem2, newdataLibdem1, type = "response")

#probability of switching for Libdem voters with high authoritarianism

newdataLibdem2 = data.frame(al_scaleW6 = 10, 
                            B5O = mean(dfLibdem$B5O, na.rm = TRUE),
                            edlevelW7 = mean(dfLibdem$edlevelW7, na.rm = TRUE),
                            ageW7 = mean(dfLibdem$ageW7, na.rm = TRUE))

predict(FullModelLibdem2, newdataLibdem2, type = "response")

#-------------------------------------------------------------------------------------------------------------------

#create dataframe for all voters. This includes both those who voted for the same party and
#those who switched to another party (any party, not only UKIP)

#create dataframe with only people who voted SNP in 2010

SNPVoters2010 = subset(MyData1, MyData1$profile_past_vote_2010 == 5)

#create dataframe with only people who voted Plaid Cymru in 2010

PlaidCymruVoters2010 = subset(MyData1, MyData1$profile_past_vote_2010 == 6)

#create dataframe with only people who voted BNP in 2010

BNPVoters2010 = subset(MyData1, MyData1$profile_past_vote_2010 == 7)

#create dataframe with only people who voted Green in 2010

GreenVoters2010 = subset(MyData1, MyData1$profile_past_vote_2010 == 8)

#create dataframe with only people who voted UKIP in 2010

UKIPVoters2010 = subset(MyData1, MyData1$profile_past_vote_2010 == 10)

#create dataframes with all voters who switched from one party to another between 2010 and 2015
#in order: Labour, Conservatives, Libdem, SNP, Plaid Cymru, BNP, Greens

# create subset dataframe without abstainers and don't knows (all the values in the code below
# indicate voters who voted for Labour in 2010 and for Labour or another party in 2015)

LabourVotersAndSwitchers2015 = subset(LabourVoters2010, 
                                      LabourVoters2010$generalElectionVoteW6 == 1 | #switched to Conservative
                                        LabourVoters2010$generalElectionVoteW6 == 2 | #kept voting for Labour
                                        LabourVoters2010$generalElectionVoteW6 == 3 | #switched to Libdem
                                        LabourVoters2010$generalElectionVoteW6 == 4 | #switched to SNP
                                        LabourVoters2010$generalElectionVoteW6 == 5 | #switched to Plaid Cymru
                                        LabourVoters2010$generalElectionVoteW6 == 6 | #switched to UKIP
                                        LabourVoters2010$generalElectionVoteW6 == 7 | #switched to Green
                                        LabourVoters2010$generalElectionVoteW6 == 8 | #switched to BNP
                                        LabourVoters2010$generalElectionVoteW6 == 9 ) #switched to other

# create a new variable, "switch", so that we don't write over generalElectionVoteW6

LabourVotersAndSwitchers2015$switch<-LabourVotersAndSwitchers2015$generalElectionVoteW6

#create the new variable "switch" with all the other political parties so that it can be used 
#across parties and dataframes
#'1' means that the voter switched between 2010 and 2015, '0' means that the voter didn't switch

LabourVotersAndSwitchers2015$switch[LabourVotersAndSwitchers2015$switch %in% 2] = 0 #this line is for the voters who voted Labour both in 2010 and 2015
LabourVotersAndSwitchers2015$switch[LabourVotersAndSwitchers2015$switch %in% 1] = 1 #this line and all the following are for the voters who switched from Labour in 2010 to another party in 2015
LabourVotersAndSwitchers2015$switch[LabourVotersAndSwitchers2015$switch %in% 3] = 1
LabourVotersAndSwitchers2015$switch[LabourVotersAndSwitchers2015$switch %in% 4] = 1
LabourVotersAndSwitchers2015$switch[LabourVotersAndSwitchers2015$switch %in% 5] = 1
LabourVotersAndSwitchers2015$switch[LabourVotersAndSwitchers2015$switch %in% 6] = 1
LabourVotersAndSwitchers2015$switch[LabourVotersAndSwitchers2015$switch %in% 7] = 1
LabourVotersAndSwitchers2015$switch[LabourVotersAndSwitchers2015$switch %in% 8] = 1
LabourVotersAndSwitchers2015$switch[LabourVotersAndSwitchers2015$switch %in% 9] = 1

#same operation as above with 2010 Conservative voters

ConservativeVotersAndSwitchers2015 = subset(ConservativeVoters2010, ConservativeVoters2010$generalElectionVoteW6 == 1 |
                                              ConservativeVoters2010$generalElectionVoteW6 == 2 |
                                              ConservativeVoters2010$generalElectionVoteW6 == 3 |
                                              ConservativeVoters2010$generalElectionVoteW6 == 4 |
                                              ConservativeVoters2010$generalElectionVoteW6 == 5 |
                                              ConservativeVoters2010$generalElectionVoteW6 == 6 |
                                              ConservativeVoters2010$generalElectionVoteW6 == 7 |
                                              ConservativeVoters2010$generalElectionVoteW6 == 8 |
                                              ConservativeVoters2010$generalElectionVoteW6 == 9)

ConservativeVotersAndSwitchers2015$switch = ConservativeVotersAndSwitchers2015$generalElectionVoteW6

ConservativeVotersAndSwitchers2015$switch[ConservativeVotersAndSwitchers2015$switch %in% 1] = 0
ConservativeVotersAndSwitchers2015$switch[ConservativeVotersAndSwitchers2015$switch %in% 2] = 1
ConservativeVotersAndSwitchers2015$switch[ConservativeVotersAndSwitchers2015$switch %in% 3] = 1
ConservativeVotersAndSwitchers2015$switch[ConservativeVotersAndSwitchers2015$switch %in% 4] = 1
ConservativeVotersAndSwitchers2015$switch[ConservativeVotersAndSwitchers2015$switch %in% 5] = 1
ConservativeVotersAndSwitchers2015$switch[ConservativeVotersAndSwitchers2015$switch %in% 6] = 1
ConservativeVotersAndSwitchers2015$switch[ConservativeVotersAndSwitchers2015$switch %in% 7] = 1
ConservativeVotersAndSwitchers2015$switch[ConservativeVotersAndSwitchers2015$switch %in% 8] = 1
ConservativeVotersAndSwitchers2015$switch[ConservativeVotersAndSwitchers2015$switch %in% 9] = 1

LibdemVotersAndSwitchers2015 = subset(LibdemVoters2010, LibdemVoters2010$generalElectionVoteW6 == 1 |
                                        LibdemVoters2010$generalElectionVoteW6 == 2 |
                                        LibdemVoters2010$generalElectionVoteW6 == 3 |
                                        LibdemVoters2010$generalElectionVoteW6 == 4 |
                                        LibdemVoters2010$generalElectionVoteW6 == 5 | 
                                        LibdemVoters2010$generalElectionVoteW6 == 6 |
                                        LibdemVoters2010$generalElectionVoteW6 == 7 |
                                        LibdemVoters2010$generalElectionVoteW6 == 8 |
                                        LibdemVoters2010$generalElectionVoteW6 == 9)

LibdemVotersAndSwitchers2015$switch<-LibdemVotersAndSwitchers2015$generalElectionVoteW6

LibdemVotersAndSwitchers2015$switch[LibdemVotersAndSwitchers2015$switch %in% 3] = 0
LibdemVotersAndSwitchers2015$switch[LibdemVotersAndSwitchers2015$switch %in% 1] = 1
LibdemVotersAndSwitchers2015$switch[LibdemVotersAndSwitchers2015$switch %in% 2] = 1
LibdemVotersAndSwitchers2015$switch[LibdemVotersAndSwitchers2015$switch %in% 4] = 1
LibdemVotersAndSwitchers2015$switch[LibdemVotersAndSwitchers2015$switch %in% 5] = 1
LibdemVotersAndSwitchers2015$switch[LibdemVotersAndSwitchers2015$switch %in% 6] = 1
LibdemVotersAndSwitchers2015$switch[LibdemVotersAndSwitchers2015$switch %in% 7] = 1
LibdemVotersAndSwitchers2015$switch[LibdemVotersAndSwitchers2015$switch %in% 8] = 1
LibdemVotersAndSwitchers2015$switch[LibdemVotersAndSwitchers2015$switch %in% 9] = 1

SNPVotersAndSwitchers2015 = subset(SNPVoters2010, SNPVoters2010$generalElectionVoteW6 == 1 |
                                     SNPVoters2010$generalElectionVoteW6 == 2 |
                                     SNPVoters2010$generalElectionVoteW6 == 3 |
                                     SNPVoters2010$generalElectionVoteW6 == 4 |
                                     SNPVoters2010$generalElectionVoteW6 == 5 | 
                                     SNPVoters2010$generalElectionVoteW6 == 6 | 
                                     SNPVoters2010$generalElectionVoteW6==  7 |
                                     SNPVoters2010$generalElectionVoteW6 == 8 |
                                     SNPVoters2010$generalElectionVoteW6 == 9 )

SNPVotersAndSwitchers2015$switch<-SNPVotersAndSwitchers2015$generalElectionVoteW6

SNPVotersAndSwitchers2015$switch[SNPVotersAndSwitchers2015$switch %in% 4] = 0
SNPVotersAndSwitchers2015$switch[SNPVotersAndSwitchers2015$switch %in% 1] = 1
SNPVotersAndSwitchers2015$switch[SNPVotersAndSwitchers2015$switch %in% 2] = 1
SNPVotersAndSwitchers2015$switch[SNPVotersAndSwitchers2015$switch %in% 3] = 1
SNPVotersAndSwitchers2015$switch[SNPVotersAndSwitchers2015$switch %in% 5] = 1
SNPVotersAndSwitchers2015$switch[SNPVotersAndSwitchers2015$switch %in% 6] = 1
SNPVotersAndSwitchers2015$switch[SNPVotersAndSwitchers2015$switch %in% 7] = 1
SNPVotersAndSwitchers2015$switch[SNPVotersAndSwitchers2015$switch %in% 8] = 1
SNPVotersAndSwitchers2015$switch[SNPVotersAndSwitchers2015$switch %in% 9] = 1

PlaidCymruVotersAndSwitchers2015 = subset(PlaidCymruVoters2010, PlaidCymruVoters2010$generalElectionVoteW6 == 1 |
                                            PlaidCymruVoters2010$generalElectionVoteW6 == 2 |
                                            PlaidCymruVoters2010$generalElectionVoteW6 == 3 |
                                            PlaidCymruVoters2010$generalElectionVoteW6 == 4 |
                                            PlaidCymruVoters2010$generalElectionVoteW6 == 5 |
                                            PlaidCymruVoters2010$generalElectionVoteW6 == 6 |
                                            PlaidCymruVoters2010$generalElectionVoteW6==  7 |
                                            PlaidCymruVoters2010$generalElectionVoteW6 == 8 | 
                                            PlaidCymruVoters2010$generalElectionVoteW6 == 9 )

PlaidCymruVotersAndSwitchers2015$switch<-PlaidCymruVotersAndSwitchers2015$generalElectionVoteW6

PlaidCymruVotersAndSwitchers2015$switch[PlaidCymruVotersAndSwitchers2015$switch %in% 5] = 0
PlaidCymruVotersAndSwitchers2015$switch[PlaidCymruVotersAndSwitchers2015$switch %in% 1] = 1
PlaidCymruVotersAndSwitchers2015$switch[PlaidCymruVotersAndSwitchers2015$switch %in% 2] = 1
PlaidCymruVotersAndSwitchers2015$switch[PlaidCymruVotersAndSwitchers2015$switch %in% 3] = 1
PlaidCymruVotersAndSwitchers2015$switch[PlaidCymruVotersAndSwitchers2015$switch %in% 4] = 1
PlaidCymruVotersAndSwitchers2015$switch[PlaidCymruVotersAndSwitchers2015$switch %in% 6] = 1
PlaidCymruVotersAndSwitchers2015$switch[PlaidCymruVotersAndSwitchers2015$switch %in% 7] = 1
PlaidCymruVotersAndSwitchers2015$switch[PlaidCymruVotersAndSwitchers2015$switch %in% 8] = 1
PlaidCymruVotersAndSwitchers2015$switch[PlaidCymruVotersAndSwitchers2015$switch %in% 9] = 1

BNPVotersAndSwitchers2015 = subset(BNPVoters2010, BNPVoters2010$generalElectionVoteW6 == 1 |
                                     BNPVoters2010$generalElectionVoteW6 == 2 |
                                     BNPVoters2010$generalElectionVoteW6 == 3 |
                                     BNPVoters2010$generalElectionVoteW6 == 4 |
                                     BNPVoters2010$generalElectionVoteW6 == 5 |
                                     BNPVoters2010$generalElectionVoteW6 == 6 |
                                     BNPVoters2010$generalElectionVoteW6 == 7 |
                                     BNPVoters2010$generalElectionVoteW6 == 8 |
                                     BNPVoters2010$generalElectionVoteW6 == 9 )

BNPVotersAndSwitchers2015$switch<-BNPVotersAndSwitchers2015$generalElectionVoteW6

BNPVotersAndSwitchers2015$switch[BNPVotersAndSwitchers2015$switch %in% 8] = 0
BNPVotersAndSwitchers2015$switch[BNPVotersAndSwitchers2015$switch %in% 1] = 1
BNPVotersAndSwitchers2015$switch[BNPVotersAndSwitchers2015$switch %in% 2] = 1
BNPVotersAndSwitchers2015$switch[BNPVotersAndSwitchers2015$switch %in% 3] = 1
BNPVotersAndSwitchers2015$switch[BNPVotersAndSwitchers2015$switch %in% 4] = 1
BNPVotersAndSwitchers2015$switch[BNPVotersAndSwitchers2015$switch %in% 5] = 1
BNPVotersAndSwitchers2015$switch[BNPVotersAndSwitchers2015$switch %in% 6] = 1
BNPVotersAndSwitchers2015$switch[BNPVotersAndSwitchers2015$switch %in% 7] = 1
BNPVotersAndSwitchers2015$switch[BNPVotersAndSwitchers2015$switch %in% 9] = 1

GreenVotersAndSwitchers2015 = subset(GreenVoters2010, GreenVoters2010$generalElectionVoteW6 == 1 |
                                       GreenVoters2010$generalElectionVoteW6 == 2 | 
                                       GreenVoters2010$generalElectionVoteW6 == 3 |
                                       GreenVoters2010$generalElectionVoteW6 == 4 |
                                       GreenVoters2010$generalElectionVoteW6 == 5 |
                                       GreenVoters2010$generalElectionVoteW6 == 6 |
                                       GreenVoters2010$generalElectionVoteW6 == 7 |
                                       GreenVoters2010$generalElectionVoteW6 == 8 | 
                                       GreenVoters2010$generalElectionVoteW6 == 9 )

GreenVotersAndSwitchers2015$switch<-GreenVotersAndSwitchers2015$generalElectionVoteW6

GreenVotersAndSwitchers2015$switch[GreenVotersAndSwitchers2015$switch %in% 7] = 0
GreenVotersAndSwitchers2015$switch[GreenVotersAndSwitchers2015$switch %in% 1] = 1
GreenVotersAndSwitchers2015$switch[GreenVotersAndSwitchers2015$switch %in% 2] = 1
GreenVotersAndSwitchers2015$switch[GreenVotersAndSwitchers2015$switch %in% 3] = 1
GreenVotersAndSwitchers2015$switch[GreenVotersAndSwitchers2015$switch %in% 4] = 1
GreenVotersAndSwitchers2015$switch[GreenVotersAndSwitchers2015$switch %in% 5] = 1
GreenVotersAndSwitchers2015$switch[GreenVotersAndSwitchers2015$switch %in% 6] = 1
GreenVotersAndSwitchers2015$switch[GreenVotersAndSwitchers2015$switch %in% 8] = 1
GreenVotersAndSwitchers2015$switch[GreenVotersAndSwitchers2015$switch %in% 9] = 1

UKIPVotersAndSwitchers2015 = subset(UKIPVoters2010, UKIPVoters2010$generalElectionVoteW6 == 1 |
                                      UKIPVoters2010$generalElectionVoteW6 == 2 |
                                      UKIPVoters2010$generalElectionVoteW6 == 3 |
                                      UKIPVoters2010$generalElectionVoteW6 == 4 |
                                      UKIPVoters2010$generalElectionVoteW6 == 5 |
                                      UKIPVoters2010$generalElectionVoteW6 == 6 |
                                      UKIPVoters2010$generalElectionVoteW6 == 7 | 
                                      UKIPVoters2010$generalElectionVoteW6 == 8 | 
                                      UKIPVoters2010$generalElectionVoteW6 == 9)

UKIPVotersAndSwitchers2015$switch<-UKIPVotersAndSwitchers2015$generalElectionVoteW6

UKIPVotersAndSwitchers2015$switch[UKIPVotersAndSwitchers2015$switch %in% 6] = 0
UKIPVotersAndSwitchers2015$switch[UKIPVotersAndSwitchers2015$switch %in% 1] = 1
UKIPVotersAndSwitchers2015$switch[UKIPVotersAndSwitchers2015$switch %in% 2] = 1
UKIPVotersAndSwitchers2015$switch[UKIPVotersAndSwitchers2015$switch %in% 3] = 1
UKIPVotersAndSwitchers2015$switch[UKIPVotersAndSwitchers2015$switch %in% 4] = 1
UKIPVotersAndSwitchers2015$switch[UKIPVotersAndSwitchers2015$switch %in% 5] = 1
UKIPVotersAndSwitchers2015$switch[UKIPVotersAndSwitchers2015$switch %in% 7] = 1
UKIPVotersAndSwitchers2015$switch[UKIPVotersAndSwitchers2015$switch %in% 8] = 1
UKIPVotersAndSwitchers2015$switch[UKIPVotersAndSwitchers2015$switch %in% 9] = 1

# merge all the voters and switchers dataframes into one

AllVotersAndSwitchers = rbind(LabourVotersAndSwitchers2015, 
                              ConservativeVotersAndSwitchers2015, 
                              LibdemVotersAndSwitchers2015,
                              SNPVotersAndSwitchers2015,
                              PlaidCymruVotersAndSwitchers2015,
                              BNPVotersAndSwitchers2015,
                              GreenVotersAndSwitchers2015,
                              UKIPVotersAndSwitchers2015)

#create df with all 2010 voters

voters2010 = rbind(LabourVoters2010, 
                   ConservativeVoters2010, 
                   LibdemVoters2010,
                   SNPVoters2010,
                   PlaidCymruVoters2010,
                   BNPVoters2010,
                   GreenVoters2010,
                   UKIPVoters2010)

#subset to have df with the 2010 voters who also voted in 2015, excluding those who voted "Other"

Voters2010and2015 = subset(voters2010, voters2010$generalElectionVoteW6 == 1 
                           |voters2010$generalElectionVoteW6 == 2
                           |voters2010$generalElectionVoteW6 == 3
                           |voters2010$generalElectionVoteW6 == 4
                           |voters2010$generalElectionVoteW6 == 5
                           |voters2010$generalElectionVoteW6 == 6
                           |voters2010$generalElectionVoteW6 == 7
                           |voters2010$generalElectionVoteW6 == 8)

#df with all voters

dfAllVoters =  select(AllVotersAndSwitchers, switch,
                      al_scaleW6, 
                      B5O,
                      profile_work_statW7, 
                      ageGroup ,
                      ageW7,
                      edlevelW7,
                      profile_work_typeW7W10,
                      profile_ethnicity, 
                      gor)

#remove NAs from dataframe
dfAllVoters = na.omit(dfAllVoters)

ModelAllVoters = glm(switch~al_scaleW6 
                     + B5O
                     + profile_work_statW7 
                     + ageGroup 
                     + edlevelW7
                     + profile_work_typeW7W10
                     + profile_ethnicity 
                     + gor , dfAllVoters, family=binomial(link = "logit") )

summary(ModelAllVoters)

AllVotersDemo = glm(switch~ profile_work_statW7 
                    + ageGroup 
                    + edlevelW7
                    + profile_work_typeW7W10
                    + profile_ethnicity 
                    + gor , dfAllVoters, family=binomial(link = "logit") )

summary(AllVotersDemo)

AllVotersPsycho = glm(switch~al_scaleW6 
                      + B5O, dfAllVoters, family=binomial(link = "logit") )

summary(AllVotersPsycho)

#likelihood ratio test for all voters

anovaAllVoters = anova(ModelAllVoters, AllVotersDemo,test="Chisq")

anovaAllVoters

#----------------------------------------------------------------------------------------------------


#OBTAINING PROBABILITIES. Here we obtain probabilities of switching 
#instead of log odds of switching using the predict() function.
#The reason we do this is that probabilities are more intuitive to 
#understand than log odds

ModelAllVoters2 = glm(switch~al_scaleW6 
                      + B5O
                     #+ profile_work_statW7 
                      + ageW7 
                      + edlevelW7, dfAllVoters, family=binomial(link = "logit") )

summary(ModelAllVoters2)

#probability of switching for voters with low openness

newdata1 = data.frame(al_scaleW6 = mean(dfAllVoters$al_scaleW6, na.rm = TRUE),
                      B5O = 1,
                      edlevelW7 = mean(dfAllVoters$edlevelW7, na.rm = TRUE),
                      ageW7 = mean(dfAllVoters$ageW7, na.rm = TRUE))

predict(ModelAllVoters2, newdata1, type = "response")

#probability of switching for voters with high openness

newdata2 = data.frame(al_scaleW6 = mean(AllVotersAndSwitchers$al_scaleW6, na.rm = TRUE),
                      B5O = 10,
                      edlevelW7 = mean(AllVotersAndSwitchers$edlevelW7, na.rm = TRUE),
                      ageW7 = mean(AllVotersAndSwitchers$ageW7, na.rm = TRUE))

predict(ModelAllVoters2, newdata2, type = "response")



#--------------------------------------------------------------------------------------------------------------------------------


# robustness checks

#check if we find same effect of authoritarianism on probability of switching 
#with switchers to other parties than UKIP

#the below dataframes are used for robustness checks, not for the main regressions

#create df with Labour to SNP switchers
LSNPVoters = subset(LabourVoters2010, LabourVoters2010$generalElectionVoteW6 == 2 | #voted Labour
                      LabourVoters2010$generalElectionVoteW6 == 4) #voted SNP

#create df with Labour to Conservative switchers
LConVoters = subset(LabourVoters2010, LabourVoters2010$generalElectionVoteW6 == 2 | #voted Labour
                      LabourVoters2010$generalElectionVoteW6 == 1) #voted Conservative

#create df with Libdem to Labour switchers
LibdemandLabourVoters = subset(LibdemVoters2010, LibdemVoters2010$generalElectionVoteW6 == 3 #voted Libdem
                               | LibdemVoters2010$generalElectionVoteW6 == 2) #voted Labour

#create df with Libdem to Conservative switchers
LibdemandConVoters = subset(LibdemVoters2010, LibdemVoters2010$generalElectionVoteW6 == 3 #voted Libdem
                            | LibdemVoters2010$generalElectionVoteW6 == 1) #voted Conservative

#change the number associated with each value of the 'general election vote' variable so that 
#voting in 2015 for the same party as in 2010 is equal to 0 and switching to another party is equal to 1

LSNPVoters$generalElectionVoteW6[LSNPVoters$generalElectionVoteW6 %in% 2]<-0
LSNPVoters$generalElectionVoteW6[LSNPVoters$generalElectionVoteW6 %in% 4]<-1

LConVoters$generalElectionVoteW6[LConVoters$generalElectionVoteW6 %in% 2]<-0
LConVoters$generalElectionVoteW6[LConVoters$generalElectionVoteW6 %in% 1]<-1

LibdemandLabourVoters$generalElectionVoteW6[LibdemandLabourVoters$generalElectionVoteW6 %in% 3]<-0
LibdemandLabourVoters$generalElectionVoteW6[LibdemandLabourVoters$generalElectionVoteW6 %in% 2]<-1

LibdemandConVoters$generalElectionVoteW6[LibdemandConVoters$generalElectionVoteW6 %in% 3]<-0
LibdemandConVoters$generalElectionVoteW6[LibdemandConVoters$generalElectionVoteW6 %in% 1]<-1

#check that the predictors have normal distributions - Labour to SNP

hist(LSNPVoters$trustMPsW6) # yes
hist(LSNPVoters$al_scaleW6) # yes
hist(LSNPVoters$B5O) # yes
hist(LSNPVoters$B5C) #no
hist(LSNPVoters$B5E) # yes
hist(LSNPVoters$B5A) # no
hist(LSNPVoters$B5N) # yes
hist(LSNPVoters$aomaverage) #yes  

#check that the predictors have normal distributions - Labour to Conservative

hist(LConVoters$trustMPsW6) #yes
hist(LConVoters$al_scaleW6) # yes
hist(LConVoters$B5O) # yes
hist(LConVoters$B5C) # no
hist(LConVoters$B5E) # yes
hist(LConVoters$B5A) # no
hist(LConVoters$B5N) # no
hist(LConVoters$aomaverage) # 

#check that the predictors have normal distributions - Libdem to Labour

hist(LibdemandLabourVoters$trustMPsW6) #yes
hist(LibdemandLabourVoters$al_scaleW6) # yes
hist(LibdemandLabourVoters$B5O) # yes
hist(LibdemandLabourVoters$B5C) #no
hist(LibdemandLabourVoters$B5E) # yes
hist(LibdemandLabourVoters$B5A) # no
hist(LibdemandLabourVoters$B5N) # yes
hist(LibdemandLabourVoters$aomaverage) # yes

#check that the predictors have normal distributions - Libdem to Conservative

hist(LibdemandConVoters$trustMPsW6) #yes
hist(LibdemandConVoters$al_scaleW6) #yes
hist(LibdemandConVoters$B5O) #yes
hist(LibdemandConVoters$B5C) #no
hist(LibdemandConVoters$B5E) #yes
hist(LibdemandConVoters$B5A) #no
hist(LibdemandConVoters$B5N) #no
hist(LibdemandConVoters$aomaverage) #yes

#models with individual predictors. The purpose of this section is to identify and remove the predictors
#that individually do not have a significant effect on switching  
#Labour to SNP

TrustMPs4 = glm(generalElectionVoteW6~trustMPsW6, 
                LSNPVoters, family=binomial(link = "logit") )

Al_Scale4 = glm(generalElectionVoteW6~al_scaleW6, 
                LSNPVoters, family=binomial(link = "logit") )

Aomaverage4 = glm(generalElectionVoteW6~aomaverage, 
                  LSNPVoters, family=binomial(link = "logit") )

Openness4 = glm(generalElectionVoteW6~B5O, 
                LSNPVoters, family=binomial(link = "logit") )

Extraversion4 = glm(generalElectionVoteW6~B5E, 
                    LSNPVoters, family=binomial(link = "logit") )

Neuroticism4 = glm(generalElectionVoteW6~B5N, 
                   LSNPVoters, family=binomial(link = "logit") )

WorkStatus4 = glm(generalElectionVoteW6~profile_work_statW7, 
                  LSNPVoters, family=binomial(link = "logit") )

WorkType4 = glm(generalElectionVoteW6~profile_work_typeW7W10, 
                LSNPVoters, family=binomial(link = "logit") )

SubjectiveClassPerception4 = glm(generalElectionVoteW6~subjClassW2_W4W7W9, 
                                 LSNPVoters, family=binomial(link = "logit") )

AgeGroup4 = glm(generalElectionVoteW6~ageGroup, 
                LSNPVoters, family=binomial(link = "logit") )

Income4 = glm(generalElectionVoteW6~profile_gross_personal, 
              LSNPVoters, family=binomial(link = "logit") )

Gender4 = glm(generalElectionVoteW6~gender, 
              LSNPVoters, family=binomial(link = "logit") )

EducationLevel4 = glm(generalElectionVoteW6~edlevelW7, 
                      LSNPVoters, family=binomial(link = "logit") )

Ethnicity4 = glm(generalElectionVoteW6~profile_ethnicity, 
                 LSNPVoters, family=binomial(link = "logit") )

Geography4 = glm(generalElectionVoteW6~gorW6, 
                 LSNPVoters, family=binomial(link = "logit") )

summary(TrustMPs4) #no
summary(Al_Scale4) # yes
summary(Aomaverage4) # no
summary(Openness4) # yes
summary(Extraversion4) #no
summary(Neuroticism4) #no
summary(WorkStatus4) # yes
summary(WorkType4) #no
summary(SubjectiveClassPerception4) #no
summary(AgeGroup4) #yes
summary(Income4) #no
summary(Gender4) #no
summary(EducationLevel4) # no
summary(Ethnicity4) # yes
summary(Geography4) #no

#models with individual predictors. The purpose of this section is to identify and remove the predictors
#that individually do not have a significant effect on switching 
#Labour to Conservative

TrustMPs5 = glm(generalElectionVoteW6~trustMPsW6, 
                LConVoters, family=binomial(link = "logit") )

Al_Scale5 = glm(generalElectionVoteW6~al_scaleW6, 
                LConVoters, family=binomial(link = "logit") )

Aomaverage5 = glm(generalElectionVoteW6~aomaverage, 
                  LConVoters, family=binomial(link = "logit") )

Openness5 = glm(generalElectionVoteW6~B5O, 
                LConVoters, family=binomial(link = "logit") )

Extraversion5 = glm(generalElectionVoteW6~B5E, 
                    LConVoters, family=binomial(link = "logit") )

WorkStatus5 = glm(generalElectionVoteW6~profile_work_statW7, 
                  LConVoters, family=binomial(link = "logit") )

WorkType5 = glm(generalElectionVoteW6~profile_work_typeW7W10, 
                LConVoters, family=binomial(link = "logit") )

SubjectiveClassPerception5 = glm(generalElectionVoteW6~subjClassW2_W4W7W9, 
                                 LConVoters, family=binomial(link = "logit") )

AgeGroup5 = glm(generalElectionVoteW6~ageGroup, 
                LConVoters, family=binomial(link = "logit") )

Income5 = glm(generalElectionVoteW6~profile_gross_personal, 
              LConVoters, family=binomial(link = "logit") )

Gender5 = glm(generalElectionVoteW6~gender, 
              LConVoters, family=binomial(link = "logit") )

EducationLevel5 = glm(generalElectionVoteW6~edlevelW7, 
                      LConVoters, family=binomial(link = "logit") )

Ethnicity5 = glm(generalElectionVoteW6~profile_ethnicity, 
                 LConVoters, family=binomial(link = "logit") )

Geography5 = glm(generalElectionVoteW6~gorW6, 
                 LConVoters, family=binomial(link = "logit") )

summary(TrustMPs5) #yes
summary(Al_Scale5) # no
summary(Aomaverage5) # no
summary(Openness5) # yes
summary(Extraversion5) #no
summary(WorkStatus5) # yes
summary(WorkType5) #no
summary(SubjectiveClassPerception5) #no
summary(AgeGroup5) #yes
summary(Income5) #no
summary(Gender5) #no
summary(EducationLevel5) # no
summary(Ethnicity5) # no
summary(Geography5) #no

#models with individual predictors. The purpose of this section is to identify and remove the predictors
#that individually do not have a significant effect on switching 
#Libdem to Labour

TrustMPs6 = glm(generalElectionVoteW6~trustMPsW6, 
                LibdemandLabourVoters, family=binomial(link = "logit") )

Al_Scale6 = glm(generalElectionVoteW6~al_scaleW6, 
                LibdemandLabourVoters, family=binomial(link = "logit") )

Aomaverage6 = glm(generalElectionVoteW6~aomaverage, 
                  LibdemandLabourVoters, family=binomial(link = "logit") )

Openness6 = glm(generalElectionVoteW6~B5O, 
                LibdemandLabourVoters, family=binomial(link = "logit") )

Extraversion6 = glm(generalElectionVoteW6~B5E, 
                    LibdemandLabourVoters, family=binomial(link = "logit") )

Neuroticism6 = glm(generalElectionVoteW6~B5N, 
                   LibdemandLabourVoters, family=binomial(link = "logit") )

WorkStatus6 = glm(generalElectionVoteW6~profile_work_statW7, 
                  LibdemandLabourVoters, family=binomial(link = "logit") )

WorkType6 = glm(generalElectionVoteW6~profile_work_typeW7W10, 
                LibdemandLabourVoters, family=binomial(link = "logit") )

SubjectiveClassPerception6 = glm(generalElectionVoteW6~subjClassW2_W4W7W9, 
                                 LibdemandLabourVoters, family=binomial(link = "logit") )

AgeGroup6 = glm(generalElectionVoteW6~ageGroup, 
                LibdemandLabourVoters, family=binomial(link = "logit") )

Income6 = glm(generalElectionVoteW6~profile_gross_personal, 
              LibdemandLabourVoters, family=binomial(link = "logit") )

Gender6 = glm(generalElectionVoteW6~gender, 
              LibdemandLabourVoters, family=binomial(link = "logit") )

EducationLevel6 = glm(generalElectionVoteW6~edlevelW7, 
                      LibdemandLabourVoters, family=binomial(link = "logit") )

Ethnicity6 = glm(generalElectionVoteW6~profile_ethnicity, 
                 LibdemandLabourVoters, family=binomial(link = "logit") )

Geography6 = glm(generalElectionVoteW6~gorW6, 
                 LibdemandLabourVoters, family=binomial(link = "logit") )

summary(TrustMPs6) #yes
summary(Al_Scale6) # yes
summary(Aomaverage6) # no
summary(Openness6) # yes
summary(Extraversion6) #no
summary(Neuroticism6) #no
summary(WorkStatus6) # no
summary(WorkType6) #yes
summary(SubjectiveClassPerception6) #no
summary(AgeGroup6) #yes
summary(Income6) #yes
summary(Gender6) #no
summary(EducationLevel6) # yes
summary(Ethnicity6) # no
summary(Geography6) #yes

#models with individual predictors. The purpose of this section is to identify and remove the predictors
#that individually do not have a significant effect on switching 
#Libdem to Conservative

TrustMPs7 = glm(generalElectionVoteW6~trustMPsW6, 
                LibdemandConVoters, family=binomial(link = "logit") )

Al_Scale7 = glm(generalElectionVoteW6~al_scaleW6, 
                LibdemandConVoters, family=binomial(link = "logit") )

Aomaverage7 = glm(generalElectionVoteW6~aomaverage, 
                  LibdemandConVoters, family=binomial(link = "logit") )

Openness7 = glm(generalElectionVoteW6~B5O, 
                LibdemandConVoters, family=binomial(link = "logit") )

Extraversion7 = glm(generalElectionVoteW6~B5E, 
                    LibdemandConVoters, family=binomial(link = "logit") )

Neuroticism7 = glm(generalElectionVoteW6~B5N, 
                   LibdemandConVoters, family=binomial(link = "logit") )

WorkStatus7 = glm(generalElectionVoteW6~profile_work_statW7, 
                  LibdemandConVoters, family=binomial(link = "logit") )

WorkType7 = glm(generalElectionVoteW6~profile_work_typeW7W10, 
                LibdemandConVoters, family=binomial(link = "logit") )

SubjectiveClassPerception7 = glm(generalElectionVoteW6~subjClassW2_W4W7W9, 
                                 LibdemandConVoters, family=binomial(link = "logit") )

AgeGroup7 = glm(generalElectionVoteW6~ageGroup, 
                LibdemandConVoters, family=binomial(link = "logit") )

Age7 =    glm(generalElectionVoteW6~ageW7, 
              LibdemandConVoters, family=binomial(link = "logit") )

Income7 = glm(generalElectionVoteW6~profile_gross_personal, 
              LibdemandConVoters, family=binomial(link = "logit") )

Gender7 = glm(generalElectionVoteW6~gender, 
              LibdemandConVoters, family=binomial(link = "logit") )

EducationLevel7 = glm(generalElectionVoteW6~edlevelW7, 
                      LibdemandConVoters, family=binomial(link = "logit") )

Ethnicity7 = glm(generalElectionVoteW6~profile_ethnicity, 
                 LibdemandConVoters, family=binomial(link = "logit") )

Geography7 = glm(generalElectionVoteW6~gorW6, 
                 LibdemandConVoters, family=binomial(link = "logit") )

summary(TrustMPs7) #yes
summary(Al_Scale7) # yes
summary(Aomaverage7) #yes
summary(Openness7) #no
summary(Extraversion7) #no
summary(WorkStatus7) # no
summary(WorkType7) #no
summary(SubjectiveClassPerception7) #yes
summary(AgeGroup7) #no
summary(Age7) #no
summary(Income7) #yes
summary(Gender7) #no
summary(EducationLevel7) # yes
summary(Ethnicity7) # no
summary(Geography7) #yes

#corrplot - Labour to SNP

which(colnames(LSNPVoters)=="al_scaleW6")
which(colnames(LSNPVoters)=="B5O")
which(colnames(LSNPVoters)=="workingStatusW6_W12")
which(colnames(LSNPVoters)=="ageGroup")
which(colnames(LSNPVoters)=="profile_ethnicity")

(corrplot(cor(LSNPVoters[ ,c(5651,5552,5608,5447,5526)], 
              LSNPVoters[,c(5651,5552,5608,5447,5526)],
              use = "complete.obs", method="spearman"), method = "number", tl.cex= 0.5, cex.axis=4))

#corrplot - Labour to Conservative

which(colnames(LConVoters)=="trustMPsW6")
which(colnames(LConVoters)=="B5O")
which(colnames(LConVoters)=="workingStatusW6_W12")
which(colnames(LConVoters)=="ageGroup")
which(colnames(LConVoters)=="locus2W8")

(corrplot(cor(LConVoters[ ,c(2224,5552,5608,5447,3428)], 
              LConVoters[,c(2224,5552,5608,5447,3428)],
              use = "complete.obs", method="spearman"), method = "number", tl.cex= 0.5, cex.axis=4))

#corrplot - Libdem to Labour

which(colnames(LibdemandLabourVoters)=="al_scaleW6")
which(colnames(LibdemandLabourVoters)=="trustMPsW6")
which(colnames(LibdemandLabourVoters)=="B5O")
which(colnames(LibdemandLabourVoters)=="profile_work_typeW7W10")
which(colnames(LibdemandLabourVoters)=="ageGroup")
which(colnames(LibdemandLabourVoters)=="profile_gross_personal")
which(colnames(LibdemandLabourVoters)=="edlevelW7")
which(colnames(LibdemandLabourVoters)=="gorW6")

(corrplot(cor(LibdemandLabourVoters[ ,c(5651,2224,5552,5488,5447,5530,5441,5477)], 
              LibdemandLabourVoters[,c(5651,2224,5552,5488,5447,5530,5441,5477)],
              use = "complete.obs", method="spearman"), method = "number", tl.cex= 0.5, cex.axis=4))

#corrplot - Libdem to Conservative

which(colnames(LibdemandConVoters)=="al_scaleW6")
which(colnames(LibdemandConVoters)=="aomaverage")
which(colnames(LibdemandConVoters)=="trustMPsW6")
which(colnames(LibdemandConVoters)=="profile_gross_personal")
which(colnames(LibdemandConVoters)=="edlevelW7")
which(colnames(LibdemandConVoters)=="subjClassW2_W4W7W9")
which(colnames(LibdemandConVoters)=="gorW6")

(corrplot(cor(LibdemandConVoters[ ,c(5651,5709,2224,5530,5441,5626,5477)], 
              LibdemandConVoters[,c(5651,5709,2224,5530,5441,5626,5477)],
              use = "complete.obs", method="spearman"), method = "number", tl.cex= 0.5, cex.axis=4))

#GVIF - Labour to SNP

vif(glm(generalElectionVoteW6~al_scaleW6
        + B5O
        + workingStatusW6_W12
        + ageGroup
        + profile_ethnicity,
        LSNPVoters, family = binomial(link = "logit")))

#GVIF - Labour to Conservative

vif(glm(generalElectionVoteW6~trustMPsW6
        +  B5O
        + workingStatusW6_W12
        + ageGroup
        + locus2W8,
        LConVoters, family = binomial(link = "logit")))

#GVIF - Libdem to Labour 

vif(glm(generalElectionVoteW6~al_scaleW6
        + trustMPsW6
        + B5O
        + profile_work_typeW7W10
        + ageGroup
        + profile_gross_personal
        + edlevelW7
        + gorW6,
        LibdemandLabourVoters, family = binomial(link = "logit")))

#GVIF - Libdem to Conservative 

vif(glm(generalElectionVoteW6~al_scaleW6
        + trustMPsW6
        + aomaverage
        + subjClassW2_W4W7W9
        + profile_gross_personal
        + edlevelW7
        + gorW6,
        LibdemandConVoters, family = binomial(link = "logit")))


#----------------------------------------------------------------------------------


#full model - Labour to SNP

FullModelLSNP = glm(generalElectionVoteW6~al_scaleW6
                    + B5O
                    + workingStatusW6_W12
                    + ageGroup
                    + profile_ethnicity,
                    LSNPVoters, family = binomial(link = "logit"))

summary(FullModelLSNP)

#full model - Labour to Conservatives

FullModelLCon = glm(generalElectionVoteW6~trustMPsW6
                    +  B5O
                    + workingStatusW6_W12
                    + ageGroup,
                    LConVoters, family = binomial(link = "logit"))

summary(FullModelLCon)

#Full model - Libdem to Labour

FullModelLibdemL = glm(generalElectionVoteW6~al_scaleW6
                       + trustMPsW6
                       + B5O
                       + profile_work_typeW7W10
                       + ageGroup
                       + profile_gross_personal
                       + edlevelW7
                       + gorW6,
                       LibdemandLabourVoters, family = binomial(link = "logit"))

summary(FullModelLibdemL)

#Full model - Libdem to Conservative

FullModelLibdemCon = glm(generalElectionVoteW6~al_scaleW6
                         + trustMPsW6
                         + subjClassW2_W4W7W9
                         + profile_gross_personal
                         + edlevelW7
                         + gorW6,
                         LibdemandConVoters, family = binomial(link = "logit"))

summary(FullModelLibdemCon)

export_summs(FullModelLSNP, FullModelLCon, FullModelLibdemL, FullModelLibdemCon,
             model.names = c("Labour to SNP","Labour to Conservatives", 'Liberal Democrat to Labour', 'Liberal Democrat to Conservative'),
             scale = FALSE, #if you don't set this to false, it rescales your coefficients and makes them very confusing
             coefs = c('Authoritarianism' = 'al_scaleW6',
                       'Openness' = 'B5O',
                       'Age Group' = 'ageGroup',
                       'Education Level' = 'edlevelW7',
                       'Work Type' = 'profile_work_typeW7W10',
                       'Ethnicity' = 'profile_ethnicity',
                       'Geography' = 'gorW6',
                       'Trust in MPs' = 'trustMPsW6',
                       'Income' = 'profile_gross_personal'),
             summ.glm = c(N = "nobs", AIC = "AIC", BIC = "BIC"),
             to.file = 'Word',
             file.name = 'Export.docx')
