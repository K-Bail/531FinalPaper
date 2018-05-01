library(readr)
bail2 <- read_csv("C:/Users/KBail/Dropbox/PhD/Quant 2/bail2.csv")

install.packages("DescTools")
library(ggplot2)
library(DescTools)

library(grid)
library(gridExtra)

names(bail2)
Freq(bail2$real)
mean(bail2$real)
summary(bail2$real)


##Knowledge questions with four bins
bail2$realbins1 <- with(bail2, ifelse(real == 1, 1, 
                                      ifelse(real == 2, 1, ifelse(real == 3, 2, 
                                                                  ifelse(real==4,2, ifelse(real==5,3, 
                                                                                           ifelse(real==6,3, 
                                                                                                  ifelse(real==7,4, ifelse(real==8,4, 
                                                                                                                           ifelse(real==9,4, NA))))))))))
hist(bail2$realbins1)

##Knowledge questions with two bins, high and low
bail2$realbins2 <- with(bail2, ifelse(real == 1, 1, 
                                      ifelse(real == 2, 1, ifelse(real == 3, 1, 
                                                                  ifelse(real==4,1, ifelse(real==5, 1, 
                                                                                           ifelse(real==6,1, 
                                                                                                  ifelse(real==7,2, ifelse(real==8,2, 
                                                                                                                           ifelse(real==9,2, NA))))))))))

hist(bail2$realbins2)


##Plot of general ability to understand
abilitygeneral<- bail2$ability
abilityplot<-ggplot(data.frame(abilitygeneral), aes(x=bail2$ability)) +
  geom_bar()

abilityplot


##Rate your general ability to understand politics relative to other UI undergrads
relativeplot<-ggplot(data.frame(bail2), aes(x=bail2$others)) +
  geom_bar()
  
  
#geom_text(aes(label="178", "19", "70","2"))

relativeplot

#both grids
g1<- grid.arrange(abilityplot, relativeplot, top="Descriptive")



##Average of how much I think I know
self1<-((bail2$self_1+bail2$self_2+bail2$self_3+bail2$self_4)/4)
hist(self1)

##Two bins of how much I think I know

bail2$realbins3 <- with(bail2, ifelse(self1 <=6, 1, ifelse(self1 >6, 2, NA)))

hist(bail2$realbins3)
View(bail2$realbins3)

##recoding of perceived knowledge/ability relative to others
#run for and sit on the UI student council

bail2$cm_run[bail2$cm_run=="Have much more knowledge and ability"]<-4
bail2$cm_run[bail2$cm_run=="Have somewhat more knowledge and ability"]<-3
bail2$cm_run[bail2$cm_run=="Have somewhat less knowledge and ability"]<-2
bail2$cm_run[bail2$cm_run=="Have much less knowledge and ability"]<-1

bail2$cm_run<- as.numeric(as.factor(bail2$cm_run))

summary(bail2$cm_run)
hist(bail2$cm_run)

##work on a 10-member committee

bail2$cm_comm[bail2$cm_comm=="Have much more knowledge and ability"]<-4
bail2$cm_comm[bail2$cm_comm=="Have somewhat more knowledge and ability"]<-3
bail2$cm_comm[bail2$cm_comm=="Have somewhat less knowledge and ability"]<-2
bail2$cm_comm[bail2$cm_comm=="Have much less knowledge and ability"]<-1

bail2$cm_comm<- as.numeric(as.factor(bail2$cm_comm))

hist(bail2$cm_comm)

##Represent the undergraduate students on the UI Board of Trustees
bail2$cm_board[bail2$cm_board=="Have much more knowledge and ability"]<-4
bail2$cm_board[bail2$cm_board=="Have somewhat more knowledge and ability"]<-3
bail2$cm_board[bail2$cm_board=="Have somewhat less knowledge and ability"]<-2
bail2$cm_board[bail2$cm_board=="Have much less knowledge and ability"]<-1

bail2$cm_board<- as.numeric(as.factor(bail2$cm_board))

hist(bail2$cm_board)

##average of perceived knowledge and ability relative to others
perceived<-((bail2$cm_board+bail2$cm_comm+bail2$cm_run)/4)

##two bins of perceived knowledge and ability relative
perceivedbins <- with(bail2, ifelse(perceived <=2, 1, ifelse(perceived >2, 2, NA)))
hist(perceivedbins)


##recoding of perceived self knowledge/ability 
#run for and sit on the UI student council

bail2$ab_council[bail2$ab_council=="Definitely have the knowledge and ability"]<-4
bail2$ab_council[bail2$ab_council=="Probably have the knowledge and ability"]<-3
bail2$ab_council[bail2$ab_council=="Probably do not have the knowledge and ability"]<-2
bail2$ab_council[bail2$ab_council=="Definitely do not have the knowledge and ability"]<-1

bail2$ab_council<- as.numeric(as.factor(bail2$ab_council))

summary(bail2$ab_council)
hist(bail2$ab_council)

##work on a 10-member committee
bail2$ab_comm[bail2$ab_comm=="Definitely have the knowledge and ability"]<-4
bail2$ab_comm[bail2$ab_comm=="Probably have the knowledge and ability"]<-3
bail2$ab_comm[bail2$ab_comm=="Probably do not have the knowledge and ability"]<-2
bail2$ab_comm[bail2$ab_comm=="Definitely do not have the knowledge and ability"]<-1

bail2$ab_comm<- as.numeric(as.factor(bail2$ab_comm))

summary(bail2$ab_comm)
hist(bail2$ab_comm)


##Represent the undergraduate students on the UI Board of Trustees
bail2$ab_board[bail2$ab_board=="Definitely have the knowledge and ability"]<-4
bail2$ab_board[bail2$ab_board=="Probably have the knowledge and ability"]<-3
bail2$ab_board[bail2$ab_board=="Probably do not have the knowledge and ability"]<-2
bail2$ab_board[bail2$ab_board=="Definitely do not have the knowledge and ability"]<-1

bail2$ab_board<- as.numeric(as.factor(bail2$ab_board))

summary(bail2$ab_board)
hist(bail2$ab_board)

##average of perceived self knowledge and ability
perceivedself<-((bail2$ab_board+bail2$ab_comm+bail2$ab_council)/4)
summary(perceivedself)

##two bins of perceived knowledge and ability relative
perceivedselfbins <- with(bail2, ifelse(perceivedself <=2, 1, ifelse(perceivedself >2, 2, NA)))
hist(perceivedselfbins)

plot(perceived, perceivedself)

##participation

#likely to participation
campuslike<-


##ALL THE BINS

sum(perceivedselfbins== 2 & perceivedbins==2 & bail2$realbins2==2)

sum(perceivedselfbins== 2 & perceivedbins==1 & bail2$realbins2==2)

sum(perceivedselfbins== 2 & perceivedbins==2 & bail2$realbins2==1)

sum(perceivedselfbins== 2 & perceivedbins==1 & bail2$realbins2==1)
sum(perceivedselfbins== 1 & perceivedbins==2 & bail2$realbins2==1, na.rm = TRUE)
head(bail2)
