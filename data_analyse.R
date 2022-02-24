#partie 1 sur FIFA
library(tidyverse)
library(stringr)
library(lubridate)

FIFA<-read_csv("C:/Users/WhiteFox/Desktop/SUPINFO191219/5RBIG/TPs/4FIFA/FIFA.csv")


glimpse(FIFA)
summary(FIFA)

FIFA2<-FIFA

unique(FIFA2$Height)

FIFA2<-separate(FIFA2,Height,c('Height','Height2'),"'")
FIFA2$Height <- as.numeric(FIFA2$Height)
FIFA2$Height2 <- as.numeric(FIFA2$Height2)
glimpse(FIFA2)

FIFA2$Height<-FIFA2$Height*30.48+FIFA2$Height2*2.54
FIFA2 <- FIFA2[,-c(28)]
unique(FIFA2$Height)

noHeight<-which(is.na(FIFA2$Height))
FIFA2 <- FIFA2[-noHeight,]
glimpse(FIFA2)

unique(FIFA2$Weight)
FIFA2$Weight<-str_remove(FIFA2$Weight, "lbs")
FIFA2$Weight<-as.numeric(FIFA2$Weight)
FIFA2$Weight<- 0.45359237 * FIFA2$Weight
glimpse(FIFA2)

ggplot(data=FIFA2, mapping = aes(x=,y=Weight)) + geom_boxplot()
table(FIFA2$Weight)
plot(table(FIFA2$Weight))

ggplot(data=FIFA2, mapping = aes(x=,y=Height)) + geom_boxplot()

unique(FIFA2$`Body Type`)
table(FIFA$`Body Type`)

NoBodyType<-which(FIFA2$`Body Type`=="PLAYER_BODY_TYPE_25")
FIFA2 <- FIFA2[-NoBodyType,]

unique(FIFA2$`Work Rate`)
#recherche sur work rate : https://www.fifplay.com/encyclopedia/work-rate/
FIFA2<-separate(FIFA2,`Work Rate`,c('AttackingWorkRate','DefensiveWorkRate'),"/ ")
unique(FIFA2$AttackingWorkRate)
unique(FIFA2$DefensiveWorkRate)
FIFA2$DefensiveWorkRate<-as.factor(FIFA2$DefensiveWorkRate)
FIFA2$AttackingWorkRate<-as.factor(FIFA2$AttackingWorkRate)

length(which(FIFA2$AttackingWorkRate=="High"))
AttackingWorkRateCount=tibble('Rate'=c("Low", "Medium", "High"), 
                              'Count'=c(length(which(FIFA2$AttackingWorkRate=="Low")),
                                length(which(FIFA2$AttackingWorkRate=="Medium")),
                                length(which(FIFA2$AttackingWorkRate=="High"))))

glimpse(AttackingWorkRateCount)
AttackingWorkRateCount$Rate<-as.factor(AttackingWorkRateCount$Rate)

ggplot(data=AttackingWorkRateCount, mapping = aes(x=Rate,y=Count)) + geom_point()

plot(table(FIFA2$AttackingWorkRate))

#netoyage de la collone Wage
unique(FIFA2$Wage)
colnames(FIFA2)[13]<-"WageInEuro"
FIFA2$WageInEuro <- str_remove(FIFA2$WageInEuro, "K")
FIFA2$WageInEuro <- str_remove(FIFA2$WageInEuro, "€")
FIFA2$WageInEuro <- as.numeric(FIFA2$WageInEuro)*1000
unique(FIFA2$WageInEuro)
summary(FIFA2$WageInEuro)

##positions points

FIFA2$LS<-str_remove(FIFA2$LS,"\\+")
#str_sub(FIFA2$LS,3,3)<-""  #version alternative
FIFA2$LS<-as.integer(str_sub(FIFA2$LS,1,2))+as.integer(str_sub(FIFA2$LS,3,3))
summary(FIFA2$LS)

#on devrait itérer sur les collones suivantes :
# :(

#############
#2Ã¨me partie des exercices


#1. etude de variable quantitative

summary(FIFA2$Age)
# effectifs
table(FIFA2$Age)
#frÃ©quences
length(FIFA2$Age)
frequences<-round(100*(table(FIFA2$Age))/length(FIFA2$Age),2)
frequences

#diagramme bÃ¢ton des effectifs
plot(table(FIFA2$Age),main ="Nb de joueur par age",ylim=c(0,1500), ylab="effectifs")
hist((FIFA2$Age))
hist((FIFA2$Age),breaks=5)
hist((FIFA2$Age),breaks=40)
#moyenne
mean(FIFA2$Age)
#mediane
median(FIFA2$Age)
#etendue
max(FIFA2$Age)-min(FIFA2$Age)
#quartiles
quantile(FIFA2$Age)
#boite Ã  moustache
boxplot(FIFA2$Age, horizontal=T)
#variance
var(FIFA2$Age)
#ecart type 
sqrt(var(FIFA2$Age))

#densitÃ©
hist((FIFA2$Age),prob=TRUE,ylim=c(0,0.1))
curve(dnorm(x,mean=mean(FIFA2$Age),sd=sd(FIFA2$Age)),add=TRUE)


#proportion test : doit se faire sur une unique proportion
nbMajeur=length(subset(FIFA2$Age,FIFA2$Age>=18))
nbTotal=length(FIFA2$Age)
nbMajeur
nbTotal

prop.test(nbMajeur,nbTotal)

#mean test
t.test(FIFA2$Age)
t.test(FIFA2$Age,mu=10)
t.test(FIFA2$Age,mu=25)
t.test(FIFA2$Age,mu=25.12)

#test chi2
frequences
result<-chisq.test(frequences)
result
result$observed
result$expected
result$residuals

#robust descrptives
mean(FIFA2$Age)
mean(FIFA2$Age,trim=0.05) #sans les 5% max et 5% min

sd(FIFA2$Age)
mad(FIFA2$Age)
IQR(FIFA2$Age)#espace interquartile



#2.
#on se concentre sur les 100 premiers
order(FIFA2$Overall, decreasing=TRUE)
FIFA_ordered=FIFA2[order(FIFA2$Overall, decreasing=TRUE),]
FIFA_top100=subset(FIFA_ordered[1:100,])

summary(FIFA_top100$Nationality)
unique(FIFA_top100$Nationality)

barplot(table(FIFA_top100$Nationality), las=2)
barplot(sort(table(FIFA_top100$Nationality),decreasing=TRUE),las=2)

barplot(sort(table(FIFA_top100$Club),decreasing=TRUE),las=2)

mean(FIFA_top100$WageInEuro)
FIFA_moreThan100=subset(FIFA_ordered[101:length(FIFA_ordered$X1),])
mean(FIFA_moreThan100$WageInEuro)

MoyenneTop100VSOthers=mean(FIFA_top100$WageInEuro)/mean(FIFA_moreThan100$WageInEuro)
MoyenneTop100VSOthers

proportionTotalWageTop100=sum(FIFA_top100$WageInEuro)/sum(FIFA2$WageInEuro)
proportionTotalWageTop100
proportionJoueursTop100=length(FIFA_top100$WageInEuro)/length(FIFA2$WageInEuro)
proportionJoueursTop100

#Study the age dependance of main quantitative variables.

boxplot(FIFA2$Age~FIFA2$`Preferred Foot`)

AgeVSFoot=table(FIFA2$Age,FIFA2$`Preferred Foot`)
result=chisq.test(AgeVSFoot)
result
result$observed
result$expected 
result$residuals

unique(FIFA2$`Body Type`)
research=which(FIFA2$`Body Type`=="Lean"|FIFA2$`Body Type`=="Normal" |FIFA2$`Body Type`=="Stocky")
FIFABodyClean=FIFA2[research,]
unique(FIFABodyClean$`Body Type`)

AgeVSBody=table(FIFABodyClean$Age,FIFABodyClean$`Body Type`)
boxplot(FIFABodyClean$Age~FIFABodyClean$`Body Type`)
result=chisq.test(AgeVSBody)
result
result$observed
result$expected 
result$residuals



install.packages("car")
library("car")
sp(FIFA2$WageInEuro~FIFA2$Age)
sp(FIFA2$WageInEuro~FIFA2$Overall)


cor(FIFA2$Age,FIFA2$WageInEuro)
plot(FIFA2$Age,FIFA2$WageInEuro)


#correlations
glimpse(FIFA2)
FIFA2[c('Overall',"WageInEuro")]

cor(FIFA2[c('Age','Overall',"WageInEuro",'Height','Weight')])

plot(FIFA2$Age~FIFA2$Overall+FIFA2$WageInEuro)

#pour plusieurs graphes
pairs(~Age+Overall+WageInEuro+Height+Weight, data= FIFA2)
#tout en un



#################
#partie FIFA3
#################
cor(FIFA2[c('Age','Overall',"WageInEuro",'Height','Weight')])
matrix<-cor(FIFA2[c('Age','Overall',"WageInEuro",'Height','Weight')])
# heat map voir aussi fichier excel
heatmap(matrix)

install.packages("Hmisc")
library("Hmisc")
#une autre solution pour calculer la correlation marlgr? les lignes avec un NA, via le package
result=rcorr(as.matrix(FIFA2[c('Age','Overall',"WageInEuro",'Height','Weight')]))
result[["r"]]
heatmap(result[["r"]])

#une correlation
plot(FIFA2[c('Height','Weight')])
result=lm(FIFA2$Weight~FIFA2$Height)
abline(result[["coefficients"]])
b=result[["coefficients"]][1]
a=result[["coefficients"]][2]
a
b
#estimation du poid pour quelqu'un de 190 cm
a*190+b

#toutes les correlations deux par deux :
install.packages("psych")
library("psych")
pairs.panels(FIFA2[c('Age','Overall',"WageInEuro",'Height','Weight')], gap = 0)

#Correlation d'une par rapport a plusieurs
lm(FIFA2$Age~FIFA2$Overall+FIFA2$WageInEuro+FIFA2$Weight+FIFA2$Height)

install.packages("plot3D")
library("plot3D")
lm(FIFA2$Weight~FIFA2$Age+FIFA2$Height)
scatter3D(FIFA2$Age,FIFA2$Height,FIFA2$Weight)

install.packages("rgl")
library(rgl)
plot3d(FIFA2$Age,FIFA2$Height,FIFA2$Weight)

#rajoutons des couleurs pour le salaire
#fonction récupérée sur le net : #http://www.sthda.com/english/wiki/a-complete-guide-to-3d-visualization-device-system-in-r-r-software-and-data-visualization
get_colors <- function(groups, group.col = palette()){
  groups <- as.factor(groups)
  ngrps <- length(levels(groups))
  if(ngrps > length(group.col)) 
    group.col <- rep(group.col, ngrps)
  color <- group.col[as.numeric(groups)]
  names(color) <- as.vector(groups)
  return(color)
}


cols <- get_colors(FIFA2$WageInEuro, c("Blue", "Green", "Red", "black"))
unique(FIFA2$WageInEuro)
cols <- get_colors(FIFA2$WageInEuro, heat.colors(145,rev=TRUE))

barplot(c(5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5,5),col=heat.colors(20, rev=TRUE))


plot3d(FIFA2$Age,FIFA2$Height,FIFA2$Weight, col = cols)
rgl.bg(color = "grey")
plot3d(FIFA2$Age,FIFA2$Overall,FIFA2$`International Reputation`, col = cols, size=10)


#conseil pour les cluster : etudier le fichier Ex09_03.R  Final du cours Essential R training

#on change d'abord les noms des lignes pour les joueurs
row.names(FIFA_top100)<-FIFA_top100$Name

d <- dist(FIFA_top100)
d

c <- hclust(d)
c
plot(c)


rect.hclust(c, k = 3, border = "blue")
g3 <- cutree(c, k = 3) #sépare en trois groupes les joueurs
g3 #montre quel joueur dans quel groupe (1,2 ou3)

gm <- cutree(c, k = 2:5)
gm #donne plusieurs niveaux de groupes, le chemin dans le graphe

FIFA_top100[c("Age","Overall")]
FIFA_top100_clean<-FIFA_top100[c("Age","Overall","Weight","Height","WageInEuro")]
row.names(FIFA_top100_clean)<-FIFA_top100$Name

km <- kmeans(FIFA_top100_clean,3) #le chiffre est le nombre de groupes
km
require(cluster)
clusplot(FIFA_top100_clean, km$cluster,color = TRUE, lines = 3,  labels = 2) 


km <- kmeans(FIFA_top100_clean,7) #le chiffre est le nombre de groupes
km
clusplot(FIFA_top100_clean, km$cluster,color = TRUE, lines = 3,  labels = 2) 

km <- kmeans(FIFA_top100_clean,6) #le chiffre est le nombre de groupes
km
clusplot(FIFA_top100_clean, km$cluster,color = TRUE, lines = 3,  labels = 2) 

FIFA_top100_clean<-FIFA_top100[c("Age","Overall","WageInEuro")]
row.names(FIFA_top100_clean)<-FIFA_top100$Name
km <- kmeans(FIFA_top100_clean,4) #le chiffre est le nombre de groupes
km
clusplot(FIFA_top100_clean, km$cluster,color = TRUE, lines = 3,  labels = 2) 

FIFA_top100_clean<-FIFA_top100[c("Jersey Number","Overall")]
row.names(FIFA_top100_clean)<-FIFA_top100$Name
km <- kmeans(FIFA_top100_clean,4) #le chiffre est le nombre de groupes
km
clusplot(FIFA_top100_clean, km$cluster,color = TRUE, lines = 3,  labels = 2) 
