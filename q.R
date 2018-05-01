library(readr)
bail2 <- read_csv("~/PhD/Quant 2/bail2.csv")
library(grid)
library(gridExtra)

run<- c(bail2$cm_run)

real<-bail2$real
ovrl<- bail2$ovrl
ability<-bail2$others
otherundergrad<- c(bail2$others)
install.packages("ggplot2")
library(ggplot2)
par(mfrow=(2,2))

g1<- grid.arrange(run, abilityg,othersg,ovrlg, ncol=2, nrow=2, top="Descriptive")

mean(real)
median(real)
mode(real)
run<- ggplot(data.frame(run), aes(x=run)) +
  geom_bar()+
  theme_classic()+

abilityg<-ggplot(data.frame(ability), aes(x=ability)) +
  geom_bar()

othersg<- ggplot(data.frame(otherundergrad), aes(x=otherundergrad)) +
  geom_bar()

ovrlg<- ggplot(data.frame(ovrl), aes(x=ovrl)) +
  geom_bar()

ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity", width=0.5)
# Change colors
ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity", color="blue", fill="white")
# Minimal theme + blue fill color
p<-ggplot(data=df, aes(x=dose, y=len)) +
  geom_bar(stat="identity", fill="steelblue")+
  theme_minimal()

summary(bail2$real)

hist(bail2$real, ylim = c(0,80), main = "Number of Questions Correct",
     xlab = "Number", breaks=9, xlim = c(1, 9))
par(mfrow=c(2, 2))
library(MASS)

res1<- lm(bail2$real~otherundergrad)
res1
scatter.smooth(res1)


small.all1<-lm(CPI~Dmag+parties+parl.,data = small.all)
small.all2<-lm(CPI~Dmag+parties+parl.,data = subset(small.all,open.list==1))
small.all3<-lm(CPI~Dmag+parties+parl.,data = subset(small.all,open.list==0))
small.all4<-lm(CPI~Dmag*open.list+parties+parl.,data = small.all)

k3<-lm(CPI~dmag, data=so2)
k1<-lm(CPI~dmag, data=small.open)
k4<-lm(CPI~dmag, data=sc2)
k2<-lm(CPI~dmag, data=small.closed)


central limit theorem of p-value and bootstrapping
OLS doesn't have a normality assumption'
