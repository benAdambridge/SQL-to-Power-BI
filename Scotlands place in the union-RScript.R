################################################################

# loading packages 

library(haven)

library(foreign)

library(ggplot2)

library(ggthemes)

library(survey)

library("jtools")

library("remotes")

library("dplyr")

library(cowplot)
library(coefplot)


library(dplyr)

library(MASS)

library(broom)
library(purrr)

################################################################


################################################################

df <- read.csv("bsa19_for_ukda.tab", sep = "\t")

######

# RECODING COUNTRY

######

# recode Country variable as Tab loads it in as 1, 2, 3 - it should be England, Scotland etc.

df$Country<-as.factor(df$Country)

levels(df$Country) <- c("England", "Scotland", "Wales")

table(df$Country)

# removes Wales' values, and then second part of code deletes Wales from dataset

df <- df %>% filter(Country=="England" | Country=="Scotland") %>% 
  mutate(Country=droplevels(Country))

table(df$Country)

################################################################

############################ INCOME 

################################################################

# Pos. = 478	Variable = fairdist	Variable label = How fair or unfair do you think the income distribution is in Britain?: SC B, C

# Value = 1.0	Label = Very fair
# Value = 2.0	Label = Fair
# Value = 3.0	Label = Unfair
# Value = 4.0	Label = Very unfair
# Value = 8.0	Label = Can't choose
#	Value = 9.0	Label = Not answered
#	Value = -1.0	Label = skip, didn`t return SC questionnaire
#	Value = -2.0	Label = skip, version off route

table(df$fairdist)

table(df$fairdist, df$Country)

df$fairdist[df$fairdist > 5] <- NA

df$fairdist[df$fairdist < 1] <- NA

table(df$fairdist, df$Country)

df$fairdist<-as.factor(df$fairdist)

levels(df$fairdist)<-c("Very fair", "Fair", "Unfair", "Very unfair")

table(df$fairdist)


# Pos. = 449	Variable = incdiff	Variable label = Govt responsibility to reduce income difference between high+low income?: SC B, C

# Value = 1.0	Label = Strongly agree
# Value = 2.0	Label = Agree
# Value = 3.0	Label = Neither agree nor disagree
# Value = 4.0	Label = Disagree
# Value = 5.0	Label = Strongly disagree
# Value = 8.0	Label = Can't choose
#	Value = 9.0	Label = Not answered
#	Value = -1.0	Label = skip, didn`t return SC questionnaire
#	Value = -2.0	Label = skip, version off route

table(df$incdiff)

df$incdiff[df$incdiff > 5] <- NA

df$incdiff[df$incdiff < 1] <- NA

table(df$incdiff, df$Country)

df$incdiff<-as.factor(df$incdiff)

levels(df$incdiff)<-c("Agree Strongly", "Agree", "Neither", "Disagree", "Disagree Strongly")

table(df$incdiff)


##### socdiag2 - What do you think GB ought to be like - which do you prefer?

# Value = 1.0	Label = Type A
# Value = 2.0	Label = Type B
# Value = 3.0	Label = Type C
# Value = 4.0	Label = Type D
# Value = 5.0	Label = Type E
# Value = 8.0	Label = Can't choose
#	Value = 9.0	Label = Not answered
#	Value = -1.0	Label = skip, didn`t return SC questionnaire
#	Value = -2.0	Label = skip, version off route

"Type A
A small elite at the 
top, very few people 
in the middle and 
the great mass of 
people at the 
bottom."

"Type B
A society like a 
pyramid with a 
small elite at the 
top, more people 
in the middle, and 
most at the 
bottom."

"Type C
A pyramid 
except that just 
a few people 
are at the 
bottom."

"Type D
A society with 
most people in 
the middle."

"Type E
Many people 
near the top, 
and only a few 
near the 
bottom."

table(df$socdiag2)

df$socdiag2[df$socdiag2 > 5] <- NA

df$socdiag2[df$socdiag2 < 1] <- NA

df$socdiag2<-as.factor(df$socdiag2)

levels(df$socdiag2)<-c("Type A", "Type B", "Type C", "Type D", "Type E")

table(df$socdiag2)

table(df$Country, df$socdiag2)



########################################################################## 

######### WELFARE 

##########################################################################

############## Around here, most unemployed people could find a job if they really
############## wanted one. [UnempJob]

# Value = 1.0	Label = Agree strongly
# Value = 2.0	Label = Agree
# Value = 3.0	Label = Neither agree nor disagree
# Value = 4.0	Label = Disagree
# Value = 5.0	Label = Disagree strongly


table(df$unempjob)

df$unempjob[df$unempjob > 5] <- NA

df$unempjob[df$unempjob < 1] <- NA

table(df$unempjob)

df$unempjob<-as.factor(df$unempjob)

levels(df$unempjob)<-c("Agree Strongly", "Agree", "Neither", "Disagree", "Disagree Strongly")

table(df$unempjob)


#################### The government should spend more money on welfare benefits for the
#################### poor, even if it leads to higher taxes. [MoreWelf]

## the actual economic actioning of welfare logic

# Value = 1.0	Label = Agree strongly
# Value = 2.0	Label = Agree
# Value = 3.0	Label = Neither agree nor disagree
# Value = 4.0	Label = Disagree
# Value = 5.0	Label = Disagree strongly


table(df$morewelf)

df$morewelf[df$morewelf > 5] <- NA

df$morewelf[df$morewelf < 1] <- NA

table(df$morewelf)

df$morewelf<-as.factor(df$morewelf)

levels(df$morewelf)<-c("Agree Strongly", "Agree", "Neither", "Disagree", "Disagree Strongly")

table(df$morewelf)


############## [sochelp] many people who get social help do not deserve it 

# Value = 1.0	Label = Agree strongly
# Value = 2.0	Label = Agree
# Value = 3.0	Label = Neither agree nor disagree
# Value = 4.0	Label = Disagree
# Value = 5.0	Label = Disagree strongly

table(df$sochelp)

df$sochelp[df$sochelp > 5] <- NA

df$sochelp[df$sochelp < 1] <- NA

table(df$sochelp)

df$sochelp<-as.factor(df$sochelp)

levels(df$sochelp)<-c("Agree Strongly", "Agree", "Neither", "Disagree", "Disagree Strongly")

table(df$sochelp)

################## [Redistrb] - Government should redistribute income from the better off to those who
# are less well off. 

## speaks to taxation i think - would expect liberals to be willing to pay more, to redistribute 

# Value = 1.0	Label = Agree strongly
# Value = 2.0	Label = Agree
# Value = 3.0	Label = Neither agree nor disagree
# Value = 4.0	Label = Disagree
# Value = 5.0	Label = Disagree strongly

table(df$redistrb)

df$redistrb[df$redistrb > 5] <- NA

df$redistrb[df$redistrb < 1] <- NA

table(df$redistrb)

df$redistrb<-as.factor(df$redistrb)

levels(df$redistrb)<-c("Agree Strongly", "Agree", "Neither", "Disagree", "Disagree Strongly")

table(df$redistrb)


########################################################################################## 

######################### BUSINESS 

###########################################################################################


# Big business benefits owners at the expense of workers. [BigBusnN]

# Value = 1.0	Label = Agree strongly
# Value = 2.0	Label = Agree
# Value = 3.0	Label = Neither agree nor disagree
# Value = 4.0	Label = Disagree
# Value = 5.0	Label = Disagree strongly

table(df$bigbusnn)

df$bigbusnn[df$bigbusnn > 5] <- NA

df$bigbusnn[df$bigbusnn < 1] <- NA

table(df$bigbusnn)

df$bigbusnn<-as.factor(df$bigbusnn)

levels(df$bigbusnn)<-c("Agree Strongly", "Agree", "Neither", "Disagree", "Disagree Strongly")

table(df$bigbusnn)


###### Indust4 - do management take advantage of employees 

# Value = 1.0	Label = Agree strongly
# Value = 2.0	Label = Agree
# Value = 3.0	Label = Neither agree nor disagree
# Value = 4.0	Label = Disagree
# Value = 5.0	Label = Disagree strongly

table(df$indust4)

df$indust4[df$indust4 > 5] <- NA

df$indust4[df$indust4 < 1] <- NA

table(df$indust4)

df$indust4<-as.factor(df$indust4)

levels(df$indust4)<-c("Agree Strongly", "Agree", "Neither", "Disagree", "Disagree Strongly")

table(df$indust4)




#############################################################################################################################

######## IMMIGRATION 

#############################################################################################################################

# 7. CITIZENSHIP - speaks to social acceptance of immigrants - would confirm civic nat and the focus of residency rather than wherre you were born

# MigWBenB How long should migrants from outside the EU have lived in Britain before they can receive the same welfare benefits as British citizens?

df$MigWBenB [df$MigWBenB > 7] <- NA

df$MigWBenB<-as.factor(df$MigWBenB)

levels(df$MigWBenB)<-c("Less than one", "One", "Two", "Three", "Four", "More than 4", "Never")


table(df$MigWBenB)


# MiEcon

# On a scale of 0 to 10, where 0 is extremely bad and 10 is extremely good, 
# would you say it is generally bad or good for Britain's economy that migrants 
# come to Britain from other countries?

df$MiEcono[df$MiEcono > 10] <- NA

########################################################## [MiCultur]

# social Q - 'cultural life' key distinction in the Q that this is a social Q

# remove missing values 

df$MiCultur[df$MiCultur > 10] <- NA



#################################

# GLOBAL 

#################################

########################## all self-completion - so, different sample

# Pos. = 460	Variable = wrlddiff	Variable label = Present economic differences between rich and poor countries are too large?: SC B, C

# Value = 1.0	Label = Strongly agree
# Value = 2.0	Label = Agree
# Value = 3.0	Label = Neither agree nor disagree
# Value = 4.0	Label = Disagree
# Value = 5.0	Label = Strongly disagree
# Value = 8.0	Label = Can't choose
#	Value = 9.0	Label = Not answered
#	Value = -1.0	Label = skip, didn`t return SC questionnaire
#	Value = -2.0	Label = skip, version off route

table(df$wrlddiff)

df$wrlddiff[df$wrlddiff > 5] <- NA

df$wrlddiff[df$wrlddiff < 1] <- NA

table(df$wrlddiff)

df$wrlddiff<-as.factor(df$wrlddiff)

levels(df$wrlddiff)<-c("Agree Strongly", "Agree", "Neither", "Disagree", "Disagree Strongly")

table(df$wrlddiff)


# Pos. = 461	Variable = helppoor	Variable label = People in wealthy countries should make an additional tax contribution to help people in poor countries?: SC B, C

#	Value = 1.0	Label = Strongly agree
#	Value = 2.0	Label = Agree
#	Value = 3.0	Label = Neither agree nor disagree
#	Value = 4.0	Label = Disagree
#	Value = 5.0	Label = Strongly disagree
#	Value = 8.0	Label = Can't choose
# Value = 9.0	Label = Not answered
# Value = -1.0	Label = skip, didn`t return SC questionnaire
# Value = -2.0	Label = skip, version off route

table(df$helppoor)

df$helppoor[df$helppoor > 5] <- NA

df$helppoor[df$helppoor < 1] <- NA

table(df$helppoor)

df$helppoor<-as.factor(df$helppoor)

levels(df$helppoor)<-c("Agree Strongly", "Agree", "Neither", "Disagree", "Disagree Strongly")

table(df$helppoor)




##################################################################

# Pos. = 78	Variable = TaxSpend	Variable label = If it had to choose, should govt reduce/increase/maintain levels of taxation and spending?

# Value = 1.0	Label = Reduce taxes and spend less on health, education and social benefits
# Value = 2.0	Label = Keep taxes and spending on these services at the same level as now
# Value = 3.0	Label = Increase taxes and spend more on health, education and social benefits
# Value = 4.0	Label = (None)
# Value = 8.0	Label = Don`t know
# Value = 9.0	Label = Refusal
# Value = -1.0	Label = Item not applicable
# Value = -2.0	Label = Skip, version off route

table(df$TaxSpend)

table(df$Country, df$TaxSpend)



###################################################################

# Pos. = 49	Variable = Partyid1	Variable label = Rs political party identification dv

# Value = 1.0	Label = Conservative
# Value = 2.0	Label = Labour
# Value = 3.0	Label = Liberal Democrat
# Value = 6.0	Label = Scottish National Party
# Value = 7.0	Label = Plaid Cymru
# Value = 8.0	Label = Other party
# Value = 9.0	Label = Other answer
# Value = 10.0	Label = None
# Value = 11.0	Label = UK Independence Party (UKIP)
# Value = 12.0	Label = British National Party (BNP)/ National Front
# Value = 13.0	Label = RESPECT/ Scottish Socialist Party (SSP)/ Socialist Party
# Value = 14.0	Label = Brexit Party
#Value = 15.0	Label = Change UK - The Independent Group
#Value = 95.0	Label = Green Party
##Value = 98.0	Label = Don`t  know
#Value = 99.0	Label = Refusal
#Value = -2.0	Label = Skip, version off route
#Value = -1.0	Label = Not applicable


table(df$Partyid1)

table(df$Country, df$Partyid1)

df$Partyid1[df$Partyid1>15]<-NA

df$Partyid1[df$Partyid1<1]<-NA


table(df$Partyid1)

# MORE CONSERVATIVES IN SCOTLAND THAN ONE WOULD IMAGINE 

# Pos. = 57	Variable = Politics	Variable label = How much interest do you have in politics?

# Value = 1.0	Label = ... a great deal,
# Value = 2.0	Label = quite a lot,
# Value = 3.0	Label = some,
# Value = 4.0	Label = not very much,
# Value = 5.0	Label = or, none at all?
#  Value = 8.0	Label = Don`t know
#Value = 9.0	Label = Refusal
#Value = -1.0	Label = Item not applicable
#Value = -2.0	Label = Skip, version off route


table(df$Politics)



#############################################

# APPLYING WEIGHTS 

# "The variables to include in csplan/svyset are 'spoint' for PSU,'stratid' for strata and 'wtfactor' for weighting the variables." p.16

df$WtFactor <- as.numeric(df$WtFactor)

df$StratID <- as.numeric(df$WtFactor)

bsa <-svydesign(id=~1, weights=~WtFactor, strata =~StratID, PSU =~spoint, data=df)

# PSU error

options(survey.lonely.psu="adjust")

# there are lonely PSUs where there is only 1 PSU present within a stratum 



##############################################################################



#############################################################################


# GRAPHS 


##############################################################################

################################################################


################################################################

############################ INCOME 

################################################################

# Pos. = 478	Variable = fairdist	Variable label = How fair or unfair do you think the income distribution is in Britain?: SC B, C

# Value = 1.0	Label = Very fair
# Value = 2.0	Label = Fair
# Value = 3.0	Label = Unfair
# Value = 4.0	Label = Very unfair
# Value = 8.0	Label = Can't choose
#	Value = 9.0	Label = Not answered
#	Value = -1.0	Label = skip, didn`t return SC questionnaire
#	Value = -2.0	Label = skip, version off route

tabfairdist <-svytable(~Country+fairdist, design = bsa) 

tabfairdist

# Add conditional proportion of levels of Redistribution for each Identity of Country

tab_fairdist <- as.data.frame(tabfairdist) %>%
  group_by(Country) %>%
  mutate(n_Country = sum(Freq), Prop_fairdist = Freq/n_Country) %>%
  ungroup()

tab_fairdist

# grouped

ggplot(data = tab_fairdist,
       aes(x = fairdist, fill = Country, y = Prop_fairdist)) + 
  geom_bar(stat="identity", position = "dodge")+
  labs(title = "How fair/unfair is income distribution in Britain", 
       x = "Fairness of Income Distribution", y = "Proportion of Fairness of Income responses", 
       caption = "Data from the British Social Attitudes Survey (2019)")

a<-ggplot(data = tab_fairdist,
          aes(x = fairdist, fill = Country, y = Prop_fairdist)) + 
  geom_bar(stat="identity", position = "dodge")+
  labs(title = "How fair/unfair is income distribution in Britain?", 
       x = "Fairness of Income Distribution", y = "Proportion of Fairness of Income responses", 
       caption = "Data from the British Social Attitudes Survey (2019)")

save_plot("Fairincome_plot.png", a)


# Pos. = 449	Variable = incdiff	Variable label = Govt responsibility to reduce income difference between high+low income?: SC B, C

# Value = 1.0	Label = Strongly agree
# Value = 2.0	Label = Agree
# Value = 3.0	Label = Neither agree nor disagree
# Value = 4.0	Label = Disagree
# Value = 5.0	Label = Strongly disagree
# Value = 8.0	Label = Can't choose
#	Value = 9.0	Label = Not answered
#	Value = -1.0	Label = skip, didn`t return SC questionnaire
#	Value = -2.0	Label = skip, version off route

tabincdiff <-svytable(~Country+incdiff, design = bsa) 

tabincdiff

# Add conditional proportion of levels of Redistribution for each Identity of Country

tab_incdiff <- as.data.frame(tabincdiff) %>%
  group_by(Country) %>%
  mutate(n_Country = sum(Freq), Prop_incdiff = Freq/n_Country) %>%
  ungroup()

tab_incdiff

# grouped

ggplot(data = tab_incdiff,
       aes(x = incdiff, fill = Country, y = Prop_incdiff)) + 
  geom_bar(stat="identity", position = "dodge")+
  labs(title = "It is the government's job to reduce income between 
       high and low incomes", 
       x = "Government reduce income inequality positioning", y = "Proportion of Government,income inequality responses", 
       caption = "Data from the British Social Attitudes Survey (2019)")

b<-ggplot(data = tab_incdiff,
          aes(x = incdiff, fill = Country, y = Prop_incdiff)) + 
  geom_bar(stat="identity", position = "dodge")+
  labs(title = "It is the government's job to reduce income between 
       high and low incomes", 
       x = "Government reduce income inequality positioning", y = "Proportion of Gov income inequality responses", 
       caption = "Data from the British Social Attitudes Survey (2019)")

save_plot("Gov_FairIncome_plot.png", b)

########################################################################## 

######### WELFARE 

##########################################################################

############## Around here, most unemployed people could find a job if they really
############## wanted one. [UnempJob]

# Value = 1.0	Label = Agree strongly
# Value = 2.0	Label = Agree
# Value = 3.0	Label = Neither agree nor disagree
# Value = 4.0	Label = Disagree
# Value = 5.0	Label = Disagree strongly

tabunempjob <-svytable(~Country+unempjob, design = bsa) 

tabunempjob

# Add conditional proportion of levels of Redistribution for each Identity of Country

tab_unempjob <- as.data.frame(tabunempjob) %>%
  group_by(Country) %>%
  mutate(n_Country = sum(Freq), Prop_unempjob = Freq/n_Country) %>%
  ungroup()

tab_unempjob

# grouped

ggplot(data = tab_unempjob,
       aes(x = unempjob, fill = Country, y = Prop_unempjob)) + 
  geom_bar(stat="identity", position = "dodge")+
  labs(title = "Most unemployed people could find a job if they really wanted", 
       x = "Finding Job positioning", y = "Proportion of Finding Job responses", 
       caption = "Data from the British Social Attitudes Survey (2019)")


c<-ggplot(data = tab_unempjob,
          aes(x = unempjob, fill = Country, y = Prop_unempjob)) + 
  geom_bar(stat="identity", position = "dodge")+
  labs(title = "Most unemployed people could find a job if they really wanted", 
       x = "Finding Job positioning", y = "Proportion of Finding Job responses", 
       caption = "Data from the British Social Attitudes Survey (2019)")


save_plot("unemployed_sympathy_plot.png", c)


#################### The government should spend more money on welfare benefits for the
#################### poor, even if it leads to higher taxes. [MoreWelf]

## the actual economic actioning of welfare logic

# Value = 1.0	Label = Agree strongly
# Value = 2.0	Label = Agree
# Value = 3.0	Label = Neither agree nor disagree
# Value = 4.0	Label = Disagree
# Value = 5.0	Label = Disagree strongly

tabmorewelf <-svytable(~Country+morewelf, design = bsa) 

tabmorewelf

# Add conditional proportion of levels of Redistribution for each Identity of Country

tab_morewelf <- as.data.frame(tabmorewelf) %>%
  group_by(Country) %>%
  mutate(n_Country = sum(Freq), Prop_morewelf = Freq/n_Country) %>%
  ungroup()

tab_morewelf

# grouped

ggplot(data = tab_morewelf,
       aes(x = morewelf, fill = Country, y = Prop_morewelf)) + 
  geom_bar(stat="identity", position = "dodge")+
  labs(title = "The government should spend more money on welfare benefits for the poor", 
       x = "More Welfare spending Positioning", y = "Proportion of More Welfare spending responses", 
       caption = "Data from the British Social Attitudes Survey (2019)")


d<-ggplot(data = tab_morewelf,
          aes(x = morewelf, fill = Country, y = Prop_morewelf)) + 
  geom_bar(stat="identity", position = "dodge")+
  labs(title = "Government should spend more money on welfare for the poor
                even if it leads to higher taxes", 
       x = "More Welfare spending Positioning", y = "Proportion of More Welfare spending responses", 
       caption = "Data from the British Social Attitudes Survey (2019)")

save_plot("gov_poor_plot.png", d)


############## [sochelp] many people who get social help do not deserve it 

# Value = 1.0	Label = Agree strongly
# Value = 2.0	Label = Agree
# Value = 3.0	Label = Neither agree nor disagree
# Value = 4.0	Label = Disagree
# Value = 5.0	Label = Disagree strongly

tabsochelp <-svytable(~Country+sochelp, design = bsa) 

tabsochelp

# Add conditional proportion of levels of Redistribution for each Identity of Country

tab_sochelp <- as.data.frame(tabsochelp) %>%
  group_by(Country) %>%
  mutate(n_Country = sum(Freq), Prop_sochelp = Freq/n_Country) %>%
  ungroup()

tab_sochelp

# grouped

ggplot(data = tab_sochelp,
       aes(x = sochelp, fill = Country, y = Prop_sochelp)) + 
  geom_bar(stat="identity", position = "dodge")+
  labs(title = "Many people who get social security do not really deserve any help", 
       x = "Benefits help positioning", y = "Proportion of Benefits Help responses", 
       caption = "Data from the British Social Attitudes Survey (2019)")

################## [Redistrb] - Government should redistribute income from the better off to those who
# are less well off. 

## speaks to taxation i think - would expect liberals to be willing to pay more, to redistribute 

# Value = 1.0	Label = Agree strongly
# Value = 2.0	Label = Agree
# Value = 3.0	Label = Neither agree nor disagree
# Value = 4.0	Label = Disagree
# Value = 5.0	Label = Disagree strongly

tabredistrb <-svytable(~Country+redistrb, design = bsa) 

tabredistrb

# Add conditional proportion of levels of Redistribution for each Identity of Country

tab_redistrb <- as.data.frame(tabredistrb) %>%
  group_by(Country) %>%
  mutate(n_Country = sum(Freq), Prop_redistrb = Freq/n_Country) %>%
  ungroup()

tab_redistrb

# grouped

ggplot(data = tab_redistrb,
       aes(x = redistrb, fill = Country, y = Prop_redistrb)) + 
  geom_bar(stat="identity", position = "dodge")+
  labs(title = "Government should redistribute income from the better-off to the less well-off", 
       x = "Redistribution positioning", y = "Proportion of Redistribution responses", 
       caption = "Data from the British Social Attitudes Survey (2019)")


########################################################################################## 

######################### BUSINESS 

###########################################################################################


# Big business benefits owners at the expense of workers. [BigBusnN]

# Value = 1.0	Label = Agree strongly
# Value = 2.0	Label = Agree
# Value = 3.0	Label = Neither agree nor disagree
# Value = 4.0	Label = Disagree
# Value = 5.0	Label = Disagree strongly

tabbigbusnn <-svytable(~Country+bigbusnn, design = bsa) 

tabbigbusnn

# Add conditional proportion of levels of Redistribution for each Identity of Country

tab_bigbusnn <- as.data.frame(tabbigbusnn) %>%
  group_by(Country) %>%
  mutate(n_Country = sum(Freq), Prop_bigbusnn = Freq/n_Country) %>%
  ungroup()

tab_bigbusnn

# grouped

ggplot(data = tab_bigbusnn,
       aes(x = bigbusnn, fill = Country, y = Prop_bigbusnn)) + 
  geom_bar(stat="identity", position = "dodge")+
  labs(title = "Big business benefits owners at the expense of workers", 
       x = "Big Business Positioning", y = "Proportion of Big Business responses", 
       caption = "Data from the British Social Attitudes Survey (2019)")

e<-ggplot(data = tab_bigbusnn,
          aes(x = bigbusnn, fill = Country, y = Prop_bigbusnn)) + 
  geom_bar(stat="identity", position = "dodge")+
  labs(title = "Big business benefits owners at the expense of workers", 
       x = "Big Business Positioning", y = "Proportion of Big Business responses", 
       caption = "Data from the British Social Attitudes Survey (2019)")

save_plot("business_plot.png", e)


###### Indust4 - do management take advantage of employees 

# Value = 1.0	Label = Agree strongly
# Value = 2.0	Label = Agree
# Value = 3.0	Label = Neither agree nor disagree
# Value = 4.0	Label = Disagree
# Value = 5.0	Label = Disagree strongly


tabIndust4 <-svytable(~Country+indust4, design = bsa) 

tabIndust4

# Add conditional proportion of levels of Redistribution for each Identity of Country

tab_Indust4 <- as.data.frame(tabIndust4) %>%
  group_by(Country) %>%
  mutate(n_Country = sum(Freq), Prop_Indust4 = Freq/n_Country) %>%
  ungroup()

tab_BigBusnN

# grouped

ggplot(data = tab_Indust4,
       aes(x = indust4, fill = Country, y = Prop_Indust4)) + 
  geom_bar(stat="identity", position = "dodge")+
  labs(title = "Management will always try to get the better of employees if it gets the chance", 
       x = "Management Positioning", y = "Proportion of Management responses", 
       caption = "Data from the British Social Attitudes Survey (2019)")

#############################################################################################################################

######## IMMIGRATION 

#############################################################################################################################

# 7. CITIZENSHIP - speaks to social acceptance of immigrants - would confirm civic nat and the focus of residency rather than wherre you were born

# MigWBenB How long should migrants from outside the EU have lived in Britain before they can receive the same welfare benefits as British citizens?

tabMigWBenB <-svytable(~Country+MigWBenB, design = bsa) 

tabMigWBenB

tab_MigWBenB <- as.data.frame(tabMigWBenB) %>%
  group_by(Country) %>%
  mutate(n_Country = sum(Freq), Prop_MigWBenB = Freq/n_Country) %>%
  ungroup()

tab_MigWBenB

# grouped

ggplot(data = tab_MigWBenB,
       aes(x = MigWBenB, fill = Country, y = Prop_MigWBenB)) + 
  geom_bar(stat="identity", position = "dodge")+
  labs(title = "How long until migrants outside of EU should recieve same benefits as British people?", 
       x = "Migrant Benefit Positioning", y = "Proportion of Migrant Benefit rights responses", 
       caption = "Data from the British Social Attitudes Survey (2019)")


f<-ggplot(data = tab_MigWBenB,
          aes(x = MigWBenB, fill = Country, y = Prop_MigWBenB)) + 
  geom_bar(stat="identity", position = "dodge")+
  labs(title = "When should migrants from outside of the EU recieve the 
       same benefits as British people?", 
       x = "Migrant Benefit Positioning (years)", y = "Proportion of Migrant Benefit rights responses", 
       caption = "Data from the British Social Attitudes Survey (2019)")

save_plot("citizenship_plot.png", f)



# MiEcon

# On a scale of 0 to 10, where 0 is extremely bad and 10 is extremely good, 
# would you say it is generally bad or good for Britain's economy that migrants 
# come to Britain from other countries?

tabMiEcono <-svytable(~Country+MiEcono, design = bsa) 

tabMiEcono

# Add conditional proportion of levels of HomoSex for each Identity of Country

tab_MiEcono <- as.data.frame(tabMiEcono) %>%
  group_by(Country) %>%
  mutate(n_Country = sum(Freq), Prop_MiEcono = Freq/n_Country) %>%
  ungroup()

tab_MiEcono

# grouped

ggplot(data = tab_MiEcono,
       aes(x = MiEcono, fill = Country, y = Prop_MiEcono)) + 
  geom_bar(stat="identity", position = "dodge")+
  labs(title = "Generally, are migrants good or bad for Britain's economy", subtitle = "0 = Extremely Bad, 10 = Extremely good",
       x = "Migrant Economic Benefit Positioning", y = "Proportion of Migrant Economic Benefit rights responses", 
       caption = "Data from the British Social Attitudes Survey (2019)")


g<-ggplot(data = tab_MiEcono,
          aes(x = MiEcono, fill = Country, y = Prop_MiEcono)) + 
  geom_bar(stat="identity", position = "dodge")+
  labs(title = "Generally, are migrants good or bad for Britain's economy", subtitle = "0 = Extremely Bad, 10 = Extremely good",
       x = "Migrant Economic Positioning", y = "Proportion of Migrant Economic responses", 
       caption = "Data from the British Social Attitudes Survey (2019)")

save_plot("immmigration_economic.png", g)


########################################################## [MiCultur]

# social Q - 'cultural life' key distinction in the Q that this is a social Q

# remove missing values 

df$MiCultur[df$MiCultur > 10] <- NA

# And on a scale of 0 to 10, would you say that Britain's cultural life is generally 
# undermined or enriched by migrants coming to live here from other 
# countries?

tabMiCultur <-svytable(~Country+MiCultur, design = bsa) 

tabMiCultur

# Add conditional proportion of levels of MICultur for each Identity of Country

tabMiCultur <- as.data.frame(tabMiCultur) %>%
  group_by(Country) %>%
  mutate(n_Country = sum(Freq), Prop_MiCultur = Freq/n_Country) %>%
  ungroup()


# grouped

ggplot(data = tabMiCultur,
       aes(x = MiCultur, fill = Country, y = Prop_MiCultur)) + 
  geom_bar(stat="identity", position = "dodge")+
  labs(title = "Generally, are migrants good or bad for Britain's culture", subtitle = "0 = Extremely Bad, 10 = Extremely good",
       x = "Migrant Culture Benefit Positioning", y = "Proportion of Migrant Culture Benefit rights responses", 
       caption = "Data from the British Social Attitudes Survey (2019)")


h<-ggplot(data = tabMiCultur,
          aes(x = MiCultur, fill = Country, y = Prop_MiCultur)) + 
  geom_bar(stat="identity", position = "dodge")+
  labs(title = "Generally, are migrants good or bad for Britain's culture", subtitle = "0 = Extremely Bad, 10 = Extremely good",
       x = "Migrant Culture Positioning", y = "Proportion of Migrant Culture responses", 
       caption = "Data from the British Social Attitudes Survey (2019)")

save_plot("immmigration_culture.png", h)


#################################

# GLOBAL 

#################################

########################## all self-completion - so, different sample

# Pos. = 460	Variable = wrlddiff	Variable label = Present economic differences between rich and poor countries are too large?: SC B, C

# Value = 1.0	Label = Strongly agree
# Value = 2.0	Label = Agree
# Value = 3.0	Label = Neither agree nor disagree
# Value = 4.0	Label = Disagree
# Value = 5.0	Label = Strongly disagree
# Value = 8.0	Label = Can't choose
#	Value = 9.0	Label = Not answered
#	Value = -1.0	Label = skip, didn`t return SC questionnaire
#	Value = -2.0	Label = skip, version off route

tabwrlddiff <-svytable(~Country+wrlddiff, design = bsa) 

tabwrlddiff

# Add conditional proportion of levels of MICultur for each Identity of Country

tabwrlddiff <- as.data.frame(tabwrlddiff) %>%
  group_by(Country) %>%
  mutate(n_Country = sum(Freq), Prop_wrlddiff = Freq/n_Country) %>%
  ungroup()


# grouped

ggplot(data = tabwrlddiff,
       aes(x = wrlddiff, fill = Country, y = Prop_wrlddiff)) + 
  geom_bar(stat="identity", position = "dodge")+
  labs(title = "Differences between rich and poor countries are too large", 
       x = "Global Rich/Poor Positioning", y = "Proportion of Global Rich/Poor responses", 
       caption = "Data from the British Social Attitudes Survey (2019)")

##################### Pos. = 461	Variable = 

###### helppoor	People in wealthy countries should make an additional tax contribution to help people in poor countries?: SC B, C

#	Value = 1.0	Label = Strongly agree
#	Value = 2.0	Label = Agree
#	Value = 3.0	Label = Neither agree nor disagree
#	Value = 4.0	Label = Disagree
#	Value = 5.0	Label = Strongly disagree
#	Value = 8.0	Label = Can't choose
# Value = 9.0	Label = Not answered
# Value = -1.0	Label = skip, didn`t return SC questionnaire
# Value = -2.0	Label = skip, version off route

tabhelppoor <-svytable(~Country+helppoor, design = bsa) 

tabhelppoor

# Add conditional proportion of levels of MICultur for each Identity of Country

tabhelppoor <- as.data.frame(tabhelppoor) %>%
  group_by(Country) %>%
  mutate(n_Country = sum(Freq), Prop_helppoor = Freq/n_Country) %>%
  ungroup()


# grouped

ggplot(data = tabhelppoor,
       aes(x = helppoor, fill = Country, y = Prop_helppoor)) + 
  geom_bar(stat="identity", position = "dodge")+
  labs(title = "People in wealthy countries should make an additional tax contribution to help people in poor countries",
       x = "Global tax contribution Positioning", y = "Proportion of Global tax contribution responses", 
       caption = "Data from the British Social Attitudes Survey (2019)")


i<-ggplot(data = tabhelppoor,
          aes(x = helppoor, fill = Country, y = Prop_helppoor)) + 
  geom_bar(stat="identity", position = "dodge")+
  labs(title = "Wealthy countries should tax residents more to help poor countries",
       x = "Global tax contribution Positioning", y = "Proportion of Global tax contribution responses", 
       caption = "Data from the British Social Attitudes Survey (2019)")


save_plot("global_plot.png", i)

