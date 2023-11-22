##############################################################################

### ESSEX COUNTY COUNCIL - TEST

##############################################################################


a<-read.csv("titanic.csv")


#############################


# LIBRARY

##############################

library(rlang)

library(haven)

library(foreign)

library(ggplot2)

library(ggthemes)

library(survey)

library("jtools")

library("remotes")

library("dplyr")

library(cowplot)

library(car)

library(dplyr)

library(lmtest)

library(sandwich)

library(tidyr)

library(psy)

library(heatmaply)

library(ymlthis)

library(kableExtra)

library(knitr)

library(brant)

library(MASS)
require(Hmisc)
require(reshape2)

library(brglm2)

library(sjstats)

library(generalhoslem)

library(ordinal)

library(devtools)

library(rmarkdown)

library(rms)

library(VGAM)

library(effects)

library(effectsize)

library(olsrr)

library(mgcv)

library(tidyverse)

library(dplyr)

library(descr)

library(ggplot2)

library(coefplot)

library(olsrr)

library(lmtest)

library(car)

library(carData)

library(factoextra)
library(gridExtra)

library(reshape2)

library(corrplot)

library(stats)

library(broom)

library(pscl)

library(pROC)

library(measures)

library(caret)

library(randomForest)

if(!require(texreg)) install.packages("texreg")

if(!require(sjPlot)) install.packages("sjPlot")

library(sjPlot)

if(!require(sjmisc)) install.packages("sjmisc")

library(sjmisc)

if(!require(sjlabelled)) install.packages("sjlabelled")

library(sjlabelled)

if(!require(gtsummary)) install.packages("gtsummary")

library(gtsummary)

library(survival)

library(stargazer)

library(lmtest)
library(sandwich)

library(mctest)

## remove missing values

a<-read.csv("titanic.csv")


b <- a[which(complete.cases(a[,c('Pclass', 'Sex', 'Survived', 'Age', 'Fare', 'SibSp', 'ParCh', 'Embarked')])),]


####################

# Pclass - Ticket class

####################

table(b$Pclass)

# rename

b$Pclass<-as.factor(b$Pclass)


levels(b$Pclass)<-c("1st", "2nd", "3rd")

table(b$Pclass)


##########################

# survival 

table(a$Survived)

####################

# Sex

####################

table(b$Sex)


####################

# Survived

####################

table(b$Survived)

b$Survived<-as.factor(b$Survived)

levels(b$Survived)<-c("No", "Yes")

table(b$Survived)


####################


# Age

####################

table(b$Age)

b$Age[b$Age<1]<-NA


####################


####################


####################


####################


####################

####################


##########################

# Graphs

###########################


# survival by port embarked from 

b%>%
  filter(!is.na(Survived)) %>%
  filter(!is.na(Embarked)) %>%
  ggplot() +
  geom_bar(aes(x = (Embarked), fill = Survived),
           position = "fill") +
  labs(title = "Survival by Port Embarked from",
       caption = "Data provided by Essex County Council")+
  scale_y_continuous(breaks = seq(0, 1, .2),
                     label = scales::percent)


####### survival by sex

b%>%
  filter(!is.na(Survived)) %>%
  filter(!is.na(Sex)) %>%
  ggplot() +
  geom_bar(aes(x = (Survived), fill = Sex),
           position = "fill") +
  labs(title = "Survival by Sex",
       caption = "Data provided by Essex County Council")+
  scale_y_continuous(breaks = seq(0, 1, .2),
                     label = scales::percent)

# Cherbourg port embarkation more likely to survive by some distance - why?

class(b$Embarked)

b$Embarked<-as.factor(b$Embarked)

table(b$Embarked)

##############################

# Cherbourg + Class of ticket share

b%>%
  filter(!is.na(Embarked)) %>%
  filter(!is.na(Pclass)) %>%
  ggplot() +
  geom_bar(aes(x = (Embarked), fill = Pclass),
           position = "fill") +
  labs(title = "Embarked from by Ticket Class",
       caption = "Data provided by Essex County Council")+
  scale_y_continuous(breaks = seq(0, 1, .2),
                     label = scales::percent)


#### many more of those on the Cherbourg had 1st class tickets 

b%>%
  filter(!is.na(Embarked)) %>%
  filter(!is.na(Survived)) %>%
  ggplot() +
  geom_bar(aes(x = (Embarked), fill = Survived),
           position = "fill") +
  labs(title = "Survival by Port Embarked from",
       caption = "Data provided by Essex County Council")+
  scale_y_continuous(breaks = seq(0, 1, .2),
                     label = scales::percent)

# Class of ticket by survival 

b%>%
  filter(!is.na(Survived)) %>%
  filter(!is.na(Pclass)) %>%
  ggplot() +
  geom_bar(aes(x = (Pclass), fill = Survived),
           position = "fill") +
  labs(title = "Survival by Class of Tickets",
       caption = "Data provided by Essex County Council")+
  scale_y_continuous(breaks = seq(0, 1, .2),
                     label = scales::percent)


b%>%
  filter(!is.na(Embarked)) %>%
  filter(!is.na(Survived)) %>%
  ggplot() +
  geom_bar(aes(x = (Embarked), fill = Sex),
           position = "fill") +
  labs(title = "Sex and Port Embarked from",
       caption = "Data provided by Essex County Council")+
  scale_y_continuous(breaks = seq(0, 1, .2),
                     label = scales::percent)


b%>%
  filter(!is.na(Embarked)) %>%
  filter(!is.na(Survived)) %>%
  ggplot() +
  geom_bar(aes(x = (Sex), fill=Pclass),
           position = "fill") +
  labs(title = "Sex and Class of Ticket",
       caption = "Data provided by Essex County Council")+
  scale_y_continuous(breaks = seq(0, 1, .2),
                     label = scales::percent)

# Price of ticket by survival 

ggplot(b, aes (x=Survived, y=Fare))+
  geom_boxplot()+
  theme_bw()+
  labs(title = "Survival rate by Fare Price paid",
       caption = "Data provided by Essex County Council",  y = "Fare Price",
       x = "Survival")+
  guides(fill=FALSE)


b %>% 
  ggplot(aes(x = Fare, fill = Survived)) +
  geom_histogram() +
  theme_bw()+
  labs(title = "Survival rate by Fare",
       caption = "Data provided by Essex County Council",  y = "Count",
       x = "Fare Price")


b %>% 
  ggplot(aes(x = Fare, fill = Pclass)) +
  geom_histogram() +
  theme_bw()+
  labs(title = "Ticket Class and Fare Price",
       caption = "Data provided by Essex County Council",  y = "Count",
       x = "Fare Price")

# age by survival rate

# Price of ticket by survival 

ggplot(b, aes (x=Survived, y=Age))+
  geom_boxplot()+
  theme_bw()+
  labs(title = "Survival rate by Age",
       caption = "Data provided by Essex County Council",  y = "Age",
       x = "Survival")+
  guides(fill=FALSE)

b %>% 
  ggplot(aes(x = Pclass, fill=Survived)) +
  geom_bar() +
  facet_wrap(~Sex)+
  theme_test()+
  labs(title = "Survival rate by Class of ticket and Sex",
       caption = "Data provided by Essex County Council",  y = "Count",
       x = "Ticket Class")

b%>%
  filter(!is.na(Embarked)) %>%
  filter(!is.na(Pclass)) %>%
  ggplot() +
  geom_bar(aes(x = (Pclass), fill = Survived),
           position = "fill") +
  facet_wrap(~Sex)+
labs(title = "Survival rate by Class of ticket and Sex",
     caption = "Data provided by Essex County Council",  y = "Count",
     x = "Ticket Class")+
  scale_y_continuous(breaks = seq(0, 1, .2),
                     label = scales::percent)



############# Age, Sex, and Class of Ticket

b %>% 
  ggplot(aes(x = Age, fill = Survived)) +
  geom_histogram(bins = 22) +
  facet_wrap(~Sex + Pclass) +
  theme_test()+
  labs(title="Survival rates by Sex, Class of Ticket, and Age")




############### Number of siblings/ Class of ticket

b%>%
  filter(!is.na(SibSp)) %>%
  filter(!is.na(Pclass)) %>%
  ggplot() +
  geom_bar(aes(x = (SibSp), fill = Pclass),
           position = "fill") +
  labs(title = "Number of Siblings and Class of Tickets",
       caption = "Data provided by Essex County Council")+
  scale_y_continuous(breaks = seq(0, 1, .2),
                     label = scales::percent)


################### no. siblings and survival

table(b$SibSp)

# lower no. of those with 3,4,5 siblings so
# small sample of those with more siblings

b%>%
  filter(!is.na(SibSp)) %>%
  filter(!is.na(Survived)) %>%
  ggplot() +
  geom_bar(aes(x = (SibSp), fill = Survived),
           position = "fill") +
  labs(title = "Number of Siblings/Spouses and Survival",
       caption = "Data provided by Essex County Council")+
  scale_y_continuous(breaks = seq(0, 1, .2),
                     label = scales::percent)


############### Number of parents/ children 

b%>%
  filter(!is.na(ParCh)) %>%
  filter(!is.na(Pclass)) %>%
  ggplot() +
  geom_bar(aes(x = (ParCh), fill = Pclass),
           position = "fill") +
  labs(title = "Number of Parents/ Children and Class of Tickets",
       caption = "Data provided by Essex County Council")+
  scale_y_continuous(breaks = seq(0, 1, .2),
                     label = scales::percent)


##################################

table(b$SibSp)

table(b$ParCh)






#####################################

# REGRESSION

#####################################

class(b$Survived)

table(b$Survived)

## null model

nmod <- glm(Survived~1, data=b, family = 'binomial')

glm0=glm(Survived~1, data=b, family=binomial)


m1 <- glm(Survived ~ Pclass + Sex + Age + Fare + SibSp + ParCh + Embarked, data = b, family = "binomial")

summary(m1)

exp(coef(m1))

stargazer(m1,
          type="html",
          out="star_linear_2.doc",
          intercept.bottom = F,
          intercept.top = T,
          ci = T, digits=2, model.names = T,
          single.row = T)

# the odds of an individual on the Titanic with a 2nd class ticket 
# Surviving on the Titanic decreases by 70% over an individual
# with a 1st class ticket 

vif(m1)

tidy_m1 <- tidy(m1)

tidy_m1

# r-squared 

pR2(m1)


########## anova test between null and real

anova(m1, glm0, test = "Chisq")



########## model 2

m2 <- glm(Survived ~ Pclass + Sex + Age + SibSp, data = b, family = "binomial")

summary(m2)

exp(coef(m2))

pR2(m2)


stargazer(m2,
          type="html",
          out="Model 2 Regression Output.doc",
          intercept.bottom = F,
          intercept.top = T,
          ci = T, digits=2, model.names = T,
          single.row = T)

# mc fadden - shows the model explains 33.8% of the variance in Survival of passengers 


# cooks distance 

BinaryModel_CooksDistance2<-plot(m2, which = 4, id.n = 3)



############## standardized residuals


model.data <- augment(m2) %>% 
  mutate(index = 1:n()) 

model.data %>% top_n(3, .cooksd)

ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = Survived), alpha = .5) +
  labs(title = "Standardized Residuals of Survival Outcome Regression")+
  theme_bw()

Total_StandardizedResiduals<-ggplot(model.data, aes(index, .std.resid)) + 
  geom_point(aes(color = Survived), alpha = .5) +
  labs(title = "Standardized Residuals of Binary Regression")+
  theme_bw()

# cooks distance 

BinaryModel_CooksDistance<-plot(m1, which = 4, id.n = 3)

## quite a few points above 0.10


##### removing those above 0.10

# removing values 3 times more than mean of all residuals

cooksD <- cooks.distance(m2)
influential <- cooksD[(cooksD > (5 * mean(cooksD, na.rm = TRUE)))]
influential

# identifies 37 residual points with mean 3x of all the residuals

names_of_influential <- names(influential)
outliers <- b[names_of_influential,]
Total_without_outliers <- b %>% anti_join(outliers)


#### re-run model with influential outliers removed 

m3 <- glm(Survived ~ Pclass + Sex + Age + SibSp, data = Total_without_outliers, family = "binomial")

summary(m3)

exp(coef(m3))


##### M3 Cooks Distance

BinaryModel_CooksDistance3<-plot(m3, which = 4, id.n = 3)

BinaryModel_CooksDistance3

plot(m3, which = 4, id.n = 3)

## t-test

varImp(m3)

# Being Male compared to female as most important predictor in the model at 12.57



anova(m2, m3, test = "Chisq")









######### predicted probability plots

# make variables numeric


# Select only numeric predictors

probabilities <- predict(m1, type = "response")
predicted.classes <- ifelse(probabilities > 0.5, "pos", "neg")
head(predicted.classes)


mydata <- b %>%
  dplyr::select_if(is.numeric) 
predictors <- colnames(mydata)

mydata <- mydata %>%
  mutate(logit = log(probabilities/(1-probabilities))) %>%
  gather(key = "predictors", value = "predictor.value", -logit)


ggplot(mydata, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")
