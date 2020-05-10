install.packages("readr")
library(readr)
install.packages("diplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("stringr")
library(stringr)
install.packages('tidytext')
library(tidytext)
install.packages('questionr') #odds.ratio
library(questionr)
install.packages("tibble")
library("tibble")


All_COMMUNES<- read.delim('Desktop/S2/Trifou/data2011.csv', header = TRUE, #base de donnée
                       "\t", fileEncoding='latin3')
View(All_COMMUNES)


#separation de la base test 
set.seed(322)
test_COMMUNES_id<-sample((1:36209),size=3620,replace=F)
test_COMMUNES<-All_COMMUNES[test_COMMUNES_id,1:10]
COMMUNES<-All_COMMUNES[-test_COMMUNES_id,]

#Préparation des tables logistiques 
logistic_table.initialization_basic<-function(p,data_set){
  logic_table=matrix(nrow=length(data_set$nom),ncol=length(p))
  for(i in (1:length(p))){
    chr=p[i]
    logic_table[,i]<-grepl(chr,data_set$nom)
  }
  colnames(logic_table)<-p
  return(logic_table)
}
abc<-c('a','z','e','r','t','y','u','i','o','p','q','s','d','f','g','h',
       'j','k','l','m','w','x','c','v','b','n','é','tiret','è','under','ç','à',
       'ê','ô','ë','ÿ','î','â','û','ü','A','Z','E','R','T','Y','U','I',
       'O','P','Q','S','D','F','G','H','J','K','L','M','W','X','C','V','B','N')


#seulement présence ou non des caractères
tableau.logistic.allcommunes<-logistic_table.initialization_basic(abc,COMMUNES)*1
colnames(tableau.logistic.allcommunes)<-abc


tableau.logistic.testcommunes<-logistic_table.initialization_basic(abc,test_COMMUNES)*1
colnames(tableau.logistic.testcommunes)<-abc


logistic_table.initialization<-function(p,data_set){
  logic_table=matrix(nrow=length(data_set$nom),ncol=length(p))
  for(i in (1:length(p))){
    chr=p[i]
    for(j in (1:length(data_set$nom))){
      logic_table[j,i]<-str_count(data_set$nom[j],chr)
    }
  }
  colnames(logic_table)<-p
  return(logic_table)
}

#avec modalités des caractères
tableau.logistic.allcommunes.iter<-logistic_table.initialization(abc,COMMUNES)*1
colnames(tableau.logistic.allcommunes.iter)<-abc

tableau.logistic.testcommunes.iter<-logistic_table.initialization(abc,test_COMMUNES)*1
colnames(tableau.logistic.testcommunes.iter)<-abc


###REGRESSION LOGISTIQUE

#Vecteur regions
regi<-c('AL','AQ','AU','BN','BO','BR','CA','CE',
        'FC','HN','IF','LI','LO','LR','MP','NP','PA',
        'PC','PI','PL','RA')

#Tableau logistique 1 si la ville appartient à la region 0 sinon
logistic_table.initialization_region<-function(R,data_set){
  logic_table=matrix(nrow=length(data_set$region),ncol=length(R))
  for(i in (1:length(R))){
    chr=R[i]
    logic_table[,i]<-grepl(chr,data_set$region)
  }
  colnames(logic_table)<-R
  return(logic_table)
}

#base d'apprentissage
tableau.logistic.region<-logistic_table.initialization_region(regi,COMMUNES)*1

#base test 
tableau.logistic.testregion <- logistic_table.initialization_region(regi,test_COMMUNES)*1

data.lm.region<-cbind(tableau.logistic.region,tableau.logistic.allcommunes)
data.lm.region.test<-cbind(tableau.logistic.testregion,tableau.logistic.testcommunes)

data.lm.region.iter<-cbind(tableau.logistic.region,tableau.logistic.allcommunes.iter)
data.lm.regiontest.iter<-cbind(tableau.logistic.testregion,tableau.logistic.testcommunes.iter)


#Regression par Region sans modalité
# Variable d'interet la region, variables explicatives les charactères presents dans le nom des villes 

regi<-c('AL','AQ','AU','BN','BO','BR','CA','CE',
        'FC','HN','IF','LI','LO','LR','MP','NP','PA',
        'PC','PI','PL','RA')

AL.logi<-tableau.logistic.region[,1]
AQ.logi <- tableau.logistic.region[,2]
AU.logi <- tableau.logistic.region[,3]
BN.logi <- tableau.logistic.region[,4]
BO.logi <- tableau.logistic.region[,5]
BR.logi <-tableau.logistic.region[,6]
CA.logi <- tableau.logistic.region[,7]
CE.logi <- tableau.logistic.region[,8]
FC.logi <-tableau.logistic.region[,9]
HN.logi <- tableau.logistic.region[,10]
IF.logi <- tableau.logistic.region[,11]
LI.logi <- tableau.logistic.region[,12]
LO.logi <- tableau.logistic.region[,13]
LR.logi <- tableau.logistic.region[,14]
MP.logi <- tableau.logistic.region[,15]
NP.logi <- tableau.logistic.region[,16]
PA.logi <- tableau.logistic.region[,17]
PC.logi <- tableau.logistic.region[,18]
PI.logi <- tableau.logistic.region[,19]
PL.logi <- tableau.logistic.region[,20]
RA.logi <- tableau.logistic.region[,21]

RA.logi

#On crée les modèles de regression sans modalités des caractères  pour chaque region
Reg.regionAL<-glm(AL ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                        A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,as.data.frame(data.lm.region),
                  family=binomial(logit))

Reg.regionAQ<-glm(AQ ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region),family=binomial(logit))

Reg.regionAU<-glm(AU ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region),family=binomial(logit))

Reg.regionBN<-glm(BN ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region),family=binomial(logit))

Reg.regionBO<-glm(BO ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region),family=binomial(logit))

Reg.regionBR<-glm(BR ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region),family=binomial(logit))

Reg.regionCA<-glm(CA ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region),family=binomial(logit))

Reg.regionCE<-glm(CE ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region),family=binomial(logit))

Reg.regionFC<-glm(FC ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region),family=binomial(logit))

Reg.regionHN<-glm(HN ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region),family=binomial(logit))

Reg.regionIF<-glm(IF ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region),family=binomial(logit))

Reg.regionLI<-glm(LI ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region),family=binomial(logit))

Reg.regionLO<-glm(LO ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region),family=binomial(logit))

Reg.regionLR<-glm(LR ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region),family=binomial(logit))

Reg.regionMP<-glm(MP ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region),family=binomial(logit))

Reg.regionNP<-glm(NP ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region),family=binomial(logit))

Reg.regionPA<-glm(PA ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region),family=binomial(logit))

Reg.regionPC<-glm(PC ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region),family=binomial(logit))

Reg.regionPI<-glm(PI ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region),family=binomial(logit))

Reg.regionPL<-glm(PL ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region),family=binomial(logit))


Reg.regionRA<-glm(RA ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region),family=binomial(logit))


list.modl.glm <- list(Reg.regionAL,Reg.regionAQ,Reg.regionAU,Reg.regionBN,Reg.regionBO,Reg.regionBR,Reg.regionCA,Reg.regionCE,Reg.regionFC,Reg.regionHN,Reg.regionIF,Reg.regionLI,Reg.regionLO,Reg.regionLR,Reg.regionMP,Reg.regionNP,Reg.regionPA,Reg.regionPC,Reg.regionPI,Reg.regionPL,Reg.regionRA)


# matrice de confusion, c’est-à-dire le tableau croisé des valeurs observées 
#et celles des valeurs prédites en appliquant le modèle aux données d’origine
# regle d'affectation simple 0.5

table.AL.predi.glm <- table(AL.predi>0.5,AL.logi)
table.AQ.predi.glm <- table(AQ.predi>0.5,AQ.logi)
table.AU.predi.glm <- table(AU.predi>0.5,AU.logi)
table.BN.predi.glm <- table(BN.predi>0.5,BN.logi)
table.BO.predi.glm <- table(BO.predi>0.5,BO.logi)
table.BR.predi.glm <- table(BR.predi>0.5,BR.logi)
table.CA.predi.glm <- table(CA.predi>0.5,CA.logi)
table.CE.predi.glm <- table(CE.predi>0.5,CE.logi)
table.FC.predi.glm <- table(FC.predi>0.5,FC.logi)
table.HN.predi.glm <- table(HN.predi>0.5,HN.logi)
table.IF.predi.glm <- table(IF.predi>0.5,IF.logi)
table.LI.predi.glm <- table(LI.predi>0.5,LI.logi)
table.LO.predi.glm <- table(LO.predi>0.5,LO.logi)
table.LR.predi.glm <- table(LR.predi>0.5,LR.logi)
table.MP.predi.glm <- table(MP.predi>0.5,MP.logi)
table.NP.predi.glm <- table(NP.predi>0.5,NP.logi)
table.PA.predi.glm <- table(PA.predi>0.5,PA.logi) 
table.PC.predi.glm <- table(PC.predi>0.5,PC.logi)
table.PI.predi.glm <- table(PI.predi>0.5,PI.logi)
table.PL.predi.glm <- table(PL.predi>0.5,PL.logi)
table.RA.predi.glm <- table(RA.predi>0.5,RA.logi)
##

Err.f <- function(A){
  if(nrow(A)==1){
    e <- (A[2]*19/20)/32589
  }
  else{e<-(A[1,2]*19/20+A[2,1]/20)/32589}
  return(e)
}


#optimiser la règle d'affectation
taux<-seq(0.01,0.99,0.01)

#optim.tx renvoi la table de confusion avec la règle d'affectation qui minimise l'erreur sur base d'apprentissage
optim.tx<-function(A,B,tx){
  m<-c()
  for(i in 1:length(tx)){
    t<-table(A>tx[i],B)
    if (nrow(t)==1){
      e<-(t[2]*19/20)/32589
    }
    else{ e<-(t[1,2]*(19/20)+t[2,1]/20)/32589}
    m<-c(m,e)}
  if(nrow(table(A>tx[which.min(m)],B))==1){
    b <- rbind(table(A>tx[which.min(m)],B),c(0,0))
    r <-cbind(b,c(tx[which.min(m)],0),c(min(m),0))
  }
  else{
  r<-cbind(table(A>tx[which.min(m)],B),c(tx[which.min(m)],0),c(min(m),0))}
  colnames(r)<-c("FALSE","TRUE","taux opti","erreur")
  return(r)
}



optim.test.tx <- function(A,B,tx){
  m<-c()
  t<-table(A>tx[1,3],B)
    if (nrow(t)==1){
      e<-(t[2]*19/20)/3620
      b <- rbind(t,c(0,0))
      r <-cbind(b,c(tx[1,3],0),c(e,0))
    }
    else{ e<-(t[1,2]*(19/20)+t[2,1]/20)/3620
    r <-cbind(t,c(tx[1,3],0),c(e,0))}
  colnames(r)<-c("FALSE","TRUE","taux opti","erreur")
  return(r)
}


AL.logi.test<-tableau.logistic.testregion[,1]
AQ.logi.test <- tableau.logistic.testregion[,2]
AU.logi.test <- tableau.logistic.testregion[,3]
BN.logi.test <- tableau.logistic.testregion[,4]
BO.logi.test <- tableau.logistic.testregion[,5]
BR.logi.test <-tableau.logistic.testregion[,6]
CA.logi.test <- tableau.logistic.testregion[,7]
CE.logi.test<- tableau.logistic.testregion[,8]
FC.logi.test <-tableau.logistic.testregion[,9]
HN.logi.test <- tableau.logistic.testregion[,10]
IF.logi.test <- tableau.logistic.testregion[,11]
LI.logi.test <- tableau.logistic.testregion[,12]
LO.logi.test <- tableau.logistic.testregion[,13]
LR.logi.test <- tableau.logistic.testregion[,14]
MP.logi.test <- tableau.logistic.testregion[,15]
NP.logi.test<- tableau.logistic.testregion[,16]
PA.logi.test <- tableau.logistic.testregion[,17]
PC.logi.test <- tableau.logistic.testregion[,18]
PI.logi.test <- tableau.logistic.testregion[,19]
PL.logi.test <- tableau.logistic.testregion[,20]
RA.logi.test <- tableau.logistic.testregion[,21]



AL.drop<-drop1(Reg.regionAL, test = "Chisq")
AQ.drop <- drop1(Reg.regionAQ, test = "Chisq")
AU.drop <-drop1(Reg.regionAU, test = "Chisq")
BN.drop <-drop1(Reg.regionBN, test = "Chisq")
BO.drop <-drop1(Reg.regionBO, test = "Chisq")
BR.drop <-drop1(Reg.regionBR, test = "Chisq")
CA.drop <-drop1(Reg.regionCA, test = "Chisq")
CE.drop <-drop1(Reg.regionCE, test = "Chisq")
FC.drop <-drop1(Reg.regionFC, test = "Chisq")
HN.drop <-drop1(Reg.regionHN, test = "Chisq")
IF.drop <-drop1(Reg.regionIF, test = "Chisq")
LI.drop <-drop1(Reg.regionLI, test = "Chisq")
LO.drop <-drop1(Reg.regionLO, test = "Chisq")
LR.drop <-drop1(Reg.regionLR, test = "Chisq")
MP.drop <-drop1(Reg.regionMP, test = "Chisq")
NP.drop <-drop1(Reg.regionNP, test = "Chisq")
PA.drop <-drop1(Reg.regionPA, test = "Chisq")
PC.drop <-drop1(Reg.regionPC, test = "Chisq")
PI.drop <-drop1(Reg.regionPI, test = "Chisq")
PL.drop <-drop1(Reg.regionPL, test = "Chisq")
RA.drop <-drop1(Reg.regionRA, test = "Chisq")


##Avant l'AIC on veut reduire le modele aux variables significative du drop1
#On extrait les variable significative 
AL.select.drop<-rownames(AL.drop[which(AL.drop[,5]<0.05),])
AQ.select.drop<-rownames(AQ.drop[which(AQ.drop[,5]<0.05),])
AU.select.drop<-rownames(AU.drop[which(AU.drop[,5]<0.05),])
BN.select.drop<-rownames(BN.drop[which(BN.drop[,5]<0.05),])
BO.select.drop<-rownames(BO.drop[which(BO.drop[,5]<0.05),])
BR.select.drop<-rownames(BR.drop[which(BR.drop[,5]<0.05),])
CA.select.drop<-rownames(CA.drop[which(CA.drop[,5]<0.05),])
CE.select.drop<-rownames(CE.drop[which(CE.drop[,5]<0.05),])
FC.select.drop<-rownames(FC.drop[which(FC.drop[,5]<0.05),])
HN.select.drop<-rownames(HN.drop[which(HN.drop[,5]<0.05),])
IF.select.drop<-rownames(IF.drop[which(IF.drop[,5]<0.05),])
LI.select.drop<-rownames(LI.drop[which(LI.drop[,5]<0.05),])
LO.select.drop<-rownames(LO.drop[which(LO.drop[,5]<0.05),])
LR.select.drop<-rownames(LR.drop[which(LR.drop[,5]<0.05),])
MP.select.drop<-rownames(MP.drop[which(MP.drop[,5]<0.05),])
NP.select.drop<-rownames(NP.drop[which(NP.drop[,5]<0.05),])
PA.select.drop<-rownames(PA.drop[which(PA.drop[,5]<0.05),])
PC.select.drop<-rownames(PC.drop[which(PC.drop[,5]<0.05),])
PI.select.drop<-rownames(PI.drop[which(PI.drop[,5]<0.05),])
PL.select.drop<-rownames(PL.drop[which(PL.drop[,5]<0.05),])
RA.select.drop<-rownames(RA.drop[which(RA.drop[,5]<0.05),])


#on refait les modèles avec seulement les var significatives
AL.Reg.Select.drop<-glm(formula(paste('AL~',paste(AL.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region),family=binomial(logit))  
  
AQ.Reg.Select.drop<-glm(formula(paste('AQ~',paste(AQ.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region),family=binomial(logit))  

AU.Reg.Select.drop<-glm(formula(paste('AU~',paste(AU.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region),family=binomial(logit))  

BN.Reg.Select.drop<-glm(formula(paste('BN~',paste(BN.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region),family=binomial(logit))  

BO.Reg.Select.drop<-glm(formula(paste('BO~',paste(BO.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region),family=binomial(logit))  

BR.Reg.Select.drop<-glm(formula(paste('BR~',paste(BR.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region),family=binomial(logit))  


CA.Reg.Select.drop<-glm(formula(paste('CA~',paste(CA.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region),family=binomial(logit))  

CE.Reg.Select.drop<-glm(formula(paste('CE~',paste(CE.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region),family=binomial(logit))  

FC.Reg.Select.drop<-glm(formula(paste('FC~',paste(FC.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region),family=binomial(logit))  

HN.Reg.Select.drop<-glm(formula(paste('HN~',paste(HN.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region),family=binomial(logit))  

IF.Reg.Select.drop<-glm(formula(paste('IF~',paste(IF.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region),family=binomial(logit))  

LI.Reg.Select.drop<-glm(formula(paste('LI~',paste(LI.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region),family=binomial(logit))  

LO.Reg.Select.drop<-glm(formula(paste('LO~',paste(LO.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region),family=binomial(logit))  

LR.Reg.Select.drop<-glm(formula(paste('LR~',paste(LR.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region),family=binomial(logit))  

MP.Reg.Select.drop<-glm(formula(paste('MP~',paste(MP.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region),family=binomial(logit))  

NP.Reg.Select.drop<-glm(formula(paste('NP~',paste(NP.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region),family=binomial(logit))  

PA.Reg.Select.drop<-glm(formula(paste('PA~',paste(PA.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region),family=binomial(logit))  

PC.Reg.Select.drop<-glm(formula(paste('PC~',paste(PC.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region),family=binomial(logit))  

PI.Reg.Select.drop<-glm(formula(paste('PI~',paste(PI.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region),family=binomial(logit))  

PL.Reg.Select.drop<-glm(formula(paste('PL~',paste(PL.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region),family=binomial(logit))  

RA.Reg.Select.drop<-glm(formula(paste('RA~',paste(RA.select.drop,collapse='+'))),
                         as.data.frame(data.lm.region),family=binomial(logit))  


step.AL.Reg.Select.drop <- step(AL.Reg.Select.drop, direction = "backward")
step.AQ.Reg.Select.drop <- step(AQ.Reg.Select.drop, direction = "backward")
step.AU.Reg.Select.drop <- step(AU.Reg.Select.drop, direction = "backward")
step.BN.Reg.Select.drop <- step(BN.Reg.Select.drop, direction ="backward")
step.BO.Reg.Select.drop <- step(BO.Reg.Select.drop, direction = "backward")
step.BR.Reg.Select.drop <- step(BR.Reg.Select.drop, direction = "backward")
step.CA.Reg.Select.drop <- step(CA.Reg.Select.drop, direction ="backward")
step.CE.Reg.Select.drop <- step(CE.Reg.Select.drop, direction = "backward")
step.FC.Reg.Select.drop <- step(FC.Reg.Select.drop, direction = "backward")
step.HN.Reg.Select.drop <- step(HN.Reg.Select.drop, direction ="backward")
step.IF.Reg.Select.drop <- step(IF.Reg.Select.drop, direction = "backward")
step.LI.Reg.Select.drop <- step(LI.Reg.Select.drop, direction = "backward")
step.LO.Reg.Select.drop <- step(LO.Reg.Select.drop, direction = "backward")
step.LR.Reg.Select.drop <- step(LR.Reg.Select.drop, direction = "backward")
step.MP.Reg.Select.drop <- step(MP.Reg.Select.drop, direction = "backward")
step.NP.Reg.Select.drop <- step(NP.Reg.Select.drop, direction = "backward")
step.PA.Reg.Select.drop <- step(PA.Reg.Select.drop, direction = "backward")
step.PC.Reg.Select.drop <- step(PC.Reg.Select.drop, direction = "backward")
step.PI.Reg.Select.drop <- step(PI.Reg.Select.drop, direction = "backward")
step.PL.Reg.Select.drop <- step(PL.Reg.Select.drop, direction = "backward")
step.RA.Reg.Select.drop <- step(RA.Reg.Select.drop, direction = "backward")


#Regression par Region avec modalité
#On crée les modèles (avec modalité) de regression pour chaque region
Reg.iter.regionAL<-glm(AL ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,as.data.frame(data.lm.region.iter),
                  family=binomial(logit))

Reg.iter.regionAQ<-glm(AQ ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region.iter),family=binomial(logit))

Reg.iter.regionAU<-glm(AU ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region.iter),family=binomial(logit))

Reg.iter.regionBN<-glm(BN ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region.iter),family=binomial(logit))

Reg.iter.regionBO<-glm(BO ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region.iter),family=binomial(logit))

Reg.iter.regionBR<-glm(BR ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region.iter),family=binomial(logit))

Reg.iter.regionCA<-glm(CA ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region.iter),family=binomial(logit))

Reg.iter.regionCE<-glm(CE ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region.iter),family=binomial(logit))

Reg.iter.regionFC<-glm(FC ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region.iter),family=binomial(logit))

Reg.iter.regionHN<-glm(HN ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region.iter),family=binomial(logit))

Reg.iter.regionIF<-glm(IF ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region.iter),family=binomial(logit))

Reg.iter.regionLI<-glm(LI ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region.iter),family=binomial(logit))

Reg.iter.regionLO<-glm(LO ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region.iter),family=binomial(logit))

Reg.iter.regionLR<-glm(LR ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region.iter),family=binomial(logit))

Reg.iter.regionMP<-glm(MP ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region.iter),family=binomial(logit))

Reg.iter.regionNP<-glm(NP ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region.iter),family=binomial(logit))

Reg.iter.regionPA<-glm(PA ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region.iter),family=binomial(logit))

Reg.iter.regionPC<-glm(PC ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region.iter),family=binomial(logit))

Reg.iter.regionPI<-glm(PI ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region.iter),family=binomial(logit))

Reg.iter.regionPL<-glm(PL ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region.iter),family=binomial(logit))


Reg.iter.regionRA<-glm(RA ~ a+b+c+d+e+f+g+h+i+j+k+l+m+n+o+p+q+r+s+t+u+v+w+x+y+z+
                    A+B+C+D+E+F+G+H+I+J+K+L+M+N+O+P+Q+R+S+T+U+V+W+X+Y+Z+
                    é+tiret+è+under+ç+à+ê+ô+ë+ÿ+î+â+û+ü,
                  as.data.frame(data.lm.region.iter),family=binomial(logit))


AL.iter.drop <- drop1(Reg.iter.regionAL, test = "Chisq")
AQ.iter.drop <- drop1(Reg.iter.regionAQ, test = "Chisq")
AU.iter.drop <- drop1(Reg.iter.regionAU, test = "Chisq")
BN.iter.drop <- drop1(Reg.iter.regionBN, test = "Chisq")
BO.iter.drop <- drop1(Reg.iter.regionBO, test = "Chisq")
BR.iter.drop <- drop1(Reg.iter.regionBR, test = "Chisq")
CA.iter.drop <- drop1(Reg.iter.regionCA, test = "Chisq")
CE.iter.drop <- drop1(Reg.iter.regionCE, test = "Chisq")
FC.iter.drop <- drop1(Reg.iter.regionFC, test = "Chisq")
HN.iter.drop <- drop1(Reg.iter.regionHN, test = "Chisq")
IF.iter.drop <- drop1(Reg.iter.regionIF, test = "Chisq")
LI.iter.drop <- drop1(Reg.iter.regionLI, test = "Chisq")
LO.iter.drop <- drop1(Reg.iter.regionLO, test = "Chisq")
LR.iter.drop <- drop1(Reg.iter.regionLR, test = "Chisq")
MP.iter.drop <- drop1(Reg.iter.regionMP, test = "Chisq")
NP.iter.drop <- drop1(Reg.iter.regionNP, test = "Chisq")
PA.iter.drop <- drop1(Reg.iter.regionPA, test = "Chisq")
PC.iter.drop <- drop1(Reg.iter.regionPC, test = "Chisq")
PI.iter.drop <- drop1(Reg.iter.regionPI, test = "Chisq")
PL.iter.drop <- drop1(Reg.iter.regionPL, test = "Chisq")
RA.iter.drop <- drop1(Reg.iter.regionRA, test = "Chisq")


AL.iter.select.drop<-rownames(AL.iter.drop[which(AL.iter.drop[,5]<0.05),])
AQ.iter.select.drop<-rownames(AQ.iter.drop[which(AQ.iter.drop[,5]<0.05),])
AU.iter.select.drop<-rownames(AU.iter.drop[which(AU.iter.drop[,5]<0.05),])
BN.iter.select.drop<-rownames(BN.iter.drop[which(BN.iter.drop[,5]<0.05),])
BO.iter.select.drop<-rownames(BO.iter.drop[which(BO.iter.drop[,5]<0.05),])
BR.iter.select.drop<-rownames(BR.iter.drop[which(BR.iter.drop[,5]<0.05),])
CA.iter.select.drop<-rownames(CA.iter.drop[which(CA.iter.drop[,5]<0.05),])
CE.iter.select.drop<-rownames(CE.iter.drop[which(CE.iter.drop[,5]<0.05),])
FC.iter.select.drop<-rownames(FC.iter.drop[which(FC.iter.drop[,5]<0.05),])
HN.iter.select.drop<-rownames(HN.iter.drop[which(HN.iter.drop[,5]<0.05),])
IF.iter.select.drop<-rownames(IF.iter.drop[which(IF.iter.drop[,5]<0.05),])
LI.iter.select.drop<-rownames(LI.iter.drop[which(LI.iter.drop[,5]<0.05),])
LO.iter.select.drop<-rownames(LO.iter.drop[which(LO.iter.drop[,5]<0.05),])
LR.iter.select.drop<-rownames(LR.iter.drop[which(LR.iter.drop[,5]<0.05),])
MP.iter.select.drop<-rownames(MP.iter.drop[which(MP.iter.drop[,5]<0.05),])
NP.iter.select.drop<-rownames(NP.iter.drop[which(NP.iter.drop[,5]<0.05),])
PA.iter.select.drop<-rownames(PA.iter.drop[which(PA.iter.drop[,5]<0.05),])
PC.iter.select.drop<-rownames(PC.iter.drop[which(PC.iter.drop[,5]<0.05),])
PI.iter.select.drop<-rownames(PI.iter.drop[which(PI.iter.drop[,5]<0.05),])
PL.iter.select.drop<-rownames(PL.iter.drop[which(PL.iter.drop[,5]<0.05),])
RA.iter.select.drop<-rownames(RA.iter.drop[which(RA.iter.drop[,5]<0.05),])


#on glm avec seulement les var significatives
AL.Reg.iter.Select.drop<-glm(formula(paste('AL~',paste(AL.iter.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region.iter),family=binomial(logit))  

AQ.Reg.iter.Select.drop<-glm(formula(paste('AQ~',paste(AQ.iter.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region.iter),family=binomial(logit))  

AU.Reg.iter.Select.drop<-glm(formula(paste('AU~',paste(AU.iter.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region.iter),family=binomial(logit))  

BN.Reg.iter.Select.drop<-glm(formula(paste('BN~',paste(BN.iter.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region.iter),family=binomial(logit))  

BO.Reg.iter.Select.drop<-glm(formula(paste('BO~',paste(BO.iter.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region.iter),family=binomial(logit))  

BR.Reg.iter.Select.drop<-glm(formula(paste('BR~',paste(BR.iter.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region.iter),family=binomial(logit))  


CA.Reg.iter.Select.drop<-glm(formula(paste('CA~',paste(CA.iter.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region.iter),family=binomial(logit))  

CE.Reg.iter.Select.drop<-glm(formula(paste('CE~',paste(CE.iter.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region.iter),family=binomial(logit))  

FC.Reg.iter.Select.drop<-glm(formula(paste('FC~',paste(FC.iter.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region.iter),family=binomial(logit))  

HN.Reg.iter.Select.drop<-glm(formula(paste('HN~',paste(HN.iter.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region.iter),family=binomial(logit))  

IF.Reg.iter.Select.drop<-glm(formula(paste('IF~',paste(IF.iter.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region.iter),family=binomial(logit))  

LI.Reg.iter.Select.drop<-glm(formula(paste('LI~',paste(LI.iter.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region.iter),family=binomial(logit))  

LO.Reg.iter.Select.drop<-glm(formula(paste('LO~',paste(LO.iter.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region.iter),family=binomial(logit))  

LR.Reg.iter.Select.drop<-glm(formula(paste('LR~',paste(LR.iter.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region.iter),family=binomial(logit))  

MP.Reg.iter.Select.drop<-glm(formula(paste('MP~',paste(MP.iter.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region.iter),family=binomial(logit))  

NP.Reg.iter.Select.drop<-glm(formula(paste('NP~',paste(NP.iter.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region.iter),family=binomial(logit))  

PA.Reg.iter.Select.drop<-glm(formula(paste('PA~',paste(PA.iter.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region.iter),family=binomial(logit))  

PC.Reg.iter.Select.drop<-glm(formula(paste('PC~',paste(PC.iter.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region.iter),family=binomial(logit))  

PI.Reg.iter.Select.drop<-glm(formula(paste('PI~',paste(PI.iter.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region.iter),family=binomial(logit))  

PL.Reg.iter.Select.drop<-glm(formula(paste('PL~',paste(PL.iter.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region.iter),family=binomial(logit))  

RA.Reg.iter.Select.drop<-glm(formula(paste('RA~',paste(RA.iter.select.drop,collapse='+'))),
                        as.data.frame(data.lm.region.iter),family=binomial(logit))  

step.AL.Reg.iter.Select.drop <- step(AL.Reg.iter.Select.drop, direction = "backward")
step.AQ.Reg.iter.Select.drop <- step(AQ.Reg.iter.Select.drop, direction = "backward")
step.AU.Reg.iter.Select.drop <- step(AU.Reg.iter.Select.drop, direction = "backward")
step.BN.Reg.iter.Select.drop <- step(BN.Reg.iter.Select.drop, direction ="backward")
step.BO.Reg.iter.Select.drop <- step(BO.Reg.iter.Select.drop, direction = "backward")
step.BR.Reg.iter.Select.drop <- step(BR.Reg.iter.Select.drop, direction = "backward")
step.CA.Reg.iter.Select.drop <- step(CA.Reg.iter.Select.drop, direction ="backward")
step.CE.Reg.iter.Select.drop <- step(CE.Reg.iter.Select.drop, direction = "backward")
step.FC.Reg.iter.Select.drop <- step(FC.Reg.iter.Select.drop, direction = "backward")
step.HN.Reg.iter.Select.drop <- step(HN.Reg.iter.Select.drop, direction ="backward")
step.IF.Reg.iter.Select.drop <- step(IF.Reg.iter.Select.drop, direction = "backward")
step.LI.Reg.iter.Select.drop <- step(LI.Reg.iter.Select.drop, direction = "backward")
step.LO.Reg.iter.Select.drop <- step(LO.Reg.iter.Select.drop, direction = "backward")
step.LR.Reg.iter.Select.drop <- step(LR.Reg.iter.Select.drop, direction = "backward")
step.MP.Reg.iter.Select.drop <- step(MP.Reg.iter.Select.drop, direction = "backward")
step.NP.Reg.iter.Select.drop <- step(NP.Reg.iter.Select.drop, direction = "backward")
step.PA.Reg.iter.Select.drop <- step(PA.Reg.iter.Select.drop, direction = "backward")
step.PC.Reg.iter.Select.drop <- step(PC.Reg.iter.Select.drop, direction = "backward")
step.PI.Reg.iter.Select.drop <- step(PI.Reg.iter.Select.drop, direction = "backward")
step.PL.Reg.iter.Select.drop <- step(PL.Reg.iter.Select.drop, direction = "backward")
step.RA.Reg.iter.Select.drop <- step(RA.Reg.iter.Select.drop, direction = "backward")



## On crée une liste de modèle par region pour chaque type de modèle
#Modèle plein sans modalités
list.modl.glm <- list(Reg.regionAL,Reg.regionAQ,Reg.regionAU,Reg.regionBN,Reg.regionBO,Reg.regionBR,Reg.regionCA,Reg.regionCE,Reg.regionFC,Reg.regionHN,Reg.regionIF,Reg.regionLI,Reg.regionLO,Reg.regionLR,Reg.regionMP,Reg.regionNP,Reg.regionPA,Reg.regionPC,Reg.regionPI,Reg.regionPL,Reg.regionRA)
names(list.modl.glm) <- regi

#Modèle réduit sans modalités
list.modl.glm.redui <- list(step.AL.Reg.Select.drop,step.AQ.Reg.Select.drop,step.AU.Reg.Select.drop,step.BN.Reg.Select.drop,step.BO.Reg.Select.drop,step.BR.Reg.Select.drop,step.CA.Reg.Select.drop,step.CE.Reg.Select.drop,step.FC.Reg.Select.drop,step.HN.Reg.Select.drop,step.IF.Reg.Select.drop,step.LI.Reg.Select.drop,step.LO.Reg.Select.drop,step.LR.Reg.Select.drop,step.MP.Reg.Select.drop,step.NP.Reg.Select.drop,step.PA.Reg.Select.drop,step.PC.Reg.Select.drop,step.PI.Reg.Select.drop,step.PL.Reg.Select.drop,step.RA.Reg.Select.drop)
names(list.modl.glm.redui) <- regi

#Modèle plein avec modalités
list.modl.moda <- list(Reg.iter.regionAL,Reg.iter.regionAQ,Reg.iter.regionAU,Reg.iter.regionBN,Reg.iter.regionBO,Reg.iter.regionBR,Reg.iter.regionCA,Reg.iter.regionCE,Reg.iter.regionFC,Reg.iter.regionHN,Reg.iter.regionIF,Reg.iter.regionLI,Reg.iter.regionLO,Reg.iter.regionLR,Reg.iter.regionMP,Reg.iter.regionNP,Reg.iter.regionPA,Reg.iter.regionPC,Reg.iter.regionPI,Reg.iter.regionPL,Reg.iter.regionRA)
names(list.modl.moda ) <- regi

#Modèle réduit avec modalités
list.modl.moda.redui <- list(step.AL.Reg.iter.Select.drop,step.AQ.Reg.iter.Select.drop
                              ,step.AU.Reg.iter.Select.drop,step.BN.Reg.iter.Select.drop
                              ,step.BO.Reg.iter.Select.drop,step.BR.Reg.iter.Select.drop
                              ,step.CA.Reg.iter.Select.drop,step.CE.Reg.iter.Select.drop
                              ,step.FC.Reg.iter.Select.drop,step.HN.Reg.iter.Select.drop
                              ,step.IF.Reg.iter.Select.drop,step.LI.Reg.iter.Select.drop
                              ,step.LO.Reg.iter.Select.drop,step.LR.Reg.iter.Select.drop
                              ,step.MP.Reg.iter.Select.drop,step.NP.Reg.iter.Select.drop
                              ,step.PA.Reg.iter.Select.drop,step.PC.Reg.iter.Select.drop
                              ,step.PI.Reg.iter.Select.drop,step.PL.Reg.iter.Select.drop
                              ,step.RA.Reg.iter.Select.drop) 
names(list.modl.moda.redui) <- regi


# On crée une fonction qui va nous permettre de prédire et retourner un seul tableau de résultat par list de modèle


# A=list de modèle de meme type 
#B=tableau logi, 
#tx= 
    #Si on veut predire sur base d'apprentissage: 0.01,...,0.99 
    #Si on veut predire sur base test: 
        #resultat de prédiction sur base d'apprentissage pour extraire regle d'affectation avec cette meme fonction 
#C= new data pour predict

fct.predi.pregion <- function(A,B,tx,C){ 
  s <- c()
  e <- c()
  if(nrow(B)==3620){
    tx <- as.numeric(paste(tx[which(as.vector(tx[-43,3])>0),3]))
    m <- lapply(A, predict,newdata=as.data.frame(C),type='response')
    for(i in 1:length(A)){
      c <- unlist(m[i])
      f <- optim.test.tx.bis(c,B[,i],tx[i])
      v <- cbind(f,regi[i])
      s <- rbind(s,v)
      e <- c(e,f[1,4])
    }}
  else{m <- lapply(A, predict,type='response')
  for(i in 1:length(A)){
    c <- unlist(m[i])
    f <- optim.tx(c,B[,i],tx)
    v <- cbind(f,regi[i])
    s <- rbind(s,v)
    e <- c(e,f[1,4])
  }}
  sf <- rbind(s,c(mean(e),NA,NA,NA,NA))
  sfe <- as.data.frame(sf)
  return(sfe)
}


optim.test.tx.bis <- function(A,B,tx){
  m<-c()
  t<-table(A>tx,B)
  if (nrow(t)==1){
    e<-(t[2]*19/20)/3620
    b <- rbind(t,c(0,0))
    r <-cbind(b,c(tx,0),c(e,0))
  }
  else{ e<-(t[1,2]*(19/20)+t[2,1]/20)/3620
  r <-cbind(t,c(tx,0),c(e,0))}
  colnames(r)<-c("FALSE","TRUE","taux opti","erreur")
  return(r)
}

#Resultat des predictions pour chaque type de modèle

##Sur Base d'apprentissage
#Modèle plein sans modalité
predi.glm <- fct.predi.pregion(list.modl.glm,tableau.logistic.region,taux,data.lm.region)
#Modèle réduit sans modalité
predi.glm.redui <- fct.predi.pregion(list.modl.glm.redui,tableau.logistic.region,taux,data.lm.region)
#Modèle plein avec modalité
predi.moda <- fct.predi.pregion(list.modl.moda,tableau.logistic.region,taux,data.lm.regiontest.iter)
#Modèle réduit avec modalité
predi.moda.redui <- fct.predi.pregion(liste.modl.moda.redui,tableau.logistic.region,taux,data.lm.regiontest.iter)

#Sur Base test
predi.glm.test <- fct.predi.pregion(list.modl.glm,tableau.logistic.testregion,predi.glm,data.lm.region.test)
predi.glm.redui.test <- fct.predi.pregion(list.modl.glm.redui,tableau.logistic.testregion,predi.glm.redui,data.lm.region.test)
predi.moda.test <- fct.predi.pregion(list.modl.moda,tableau.logistic.testregion,predi.moda,data.lm.regiontest.iter)
predi.moda.redui.test <- fct.predi.pregion(list.modl.moda.redui,tableau.logistic.testregion,predi.moda.redui,data.lm.regiontest.iter)

# On crée un modèle qui selectionne pour chaque région le type de modèle qui a produit l'erreur la plus faible

# permet de retrouver les modèles par région qui ont eu l'erreur la plus faible à partir des prédictions faites précedemment
#A,B,C,D les 4 liste de modèles 
selct.modl <- function(A,B,C,D){
  m <- cbind(as.vector(A[-43,4]),as.vector(B[-43,4]),as.vector(C[-43,4]),as.vector(D[-43,4]))
  m <- m[which(as.vector(A[-43,4])>0),]
  r <- c()
  e <- c()
  for (i in 1:nrow(m)){
    r <- c(r,which.min(m[i,]))
    e <- c(e,as.numeric(min(m[i,])))
  }
  v <- rbind(r,e)
  return(v)
}
model.fin.region <- selct.modl(predi.glm,predi.glm.redui,predi.moda,predi.moda.redui)

#on regroupe donc les resultats
predi.model.opti.err <- function(A,bt){
  m <- A[1,]
  r <- c()
  if(bt=='base'){
    for(i in 1:ncol(A)){
      if(m[i]==1){c <- predi.glm[(2*i-1):(2*i),]}
      else if (m[i]==2){ c <- predi.glm.redui[(2*i-1):(2*i),]}
      else if (m[i]==3){ c <- predi.moda[(2*i-1):(2*i),]}
      else {c <- predi.moda.redui[(2*i-1):(2*i),]}
      r <- rbind(r,c)
    }}
  else{
    for(i in 1:ncol(A)){
      if(m[i]==1){c <- predi.glm.test[(2*i-1):(2*i),]}
      else if (m[i]==2){ c <- predi.glm.redui.test[(2*i-1):(2*i),]}
      else if (m[i]==3){ c <- predi.moda.test[(2*i-1):(2*i),]}
      else {c <- predi.moda.redui.test[(2*i-1):(2*i),]}
      r <- rbind(r,c)
    }}
  return(r)
}

# Sur base d'apprentissage
predi.model.opti <- predi.model.opti.err(model.fin.region,'base')
err.moy.predi.model.opti <- mean(as.numeric(as.vector(predi.model.opti[,4]))>0)#PB
err.moy.predi.model.opti

#Sur base test
predi.model.opti.test <- predi.model.opti.err(model.fin.region,'test')
err.moy.predi.model.opti.test <-  mean(as.numeric(as.vector(predi.model.opti.test[,4]))>0)
err.moy.predi.model.opti.test



##graph evolution de l'erreur moy 
# Sur base d'apprentissage
Err.b <- c(as.numeric(as.vector(predi.glm[43,1])),as.numeric(as.vector(predi.glm.redui[43,1])),
       as.numeric(as.vector(predi.moda[43,1])),as.numeric(as.vector(predi.moda.redui[43,1])),
       mean(model.fin.region[2,]))
# Sur base test
Err.t <- c(as.numeric(as.vector(predi.glm.test[43,1])),as.numeric(as.vector(predi.glm.redui.test[43,1])),
       as.numeric(as.vector(predi.moda.test[43,1])),as.numeric(as.vector(predi.moda.redui.test[43,1])),
       mean(as.numeric(as.vector(predi.model.opti.test[vect.ind,4]))))

ErrBT <- cbind(Err.b,Err.t)
matplot(ErrBT,type='b',main='evolution erreur') #rouge base test, noir base d'apprentissage



#On crée le modèle multi-région


mdl.max <- function(A,bt){
  r <- c()
  if(bt=="t"){
    glm <- lapply(list.modl.glm,predict,as.data.frame(data.lm.region.test),type='response')
    glm.r <- lapply(list.modl.glm.redui,predict,as.data.frame(data.lm.region.test),type='response')
    moda <- lapply(list.modl.moda,predict,as.data.frame(data.lm.regiontest.iter),type='response')
    moda.redui <- lapply(list.modl.moda.redui,predict,as.data.frame(data.lm.regiontest.iter),type='response')
    c <- tableau.logistic.testregion}
  else{
    glm <- lapply(list.modl.glm,predict,type='response')
    glm.r <- lapply(list.modl.glm.redui,predict,type='response')
    moda <- lapply(list.modl.moda,predict,type='response')
    moda.redui <- lapply(list.modl.moda.redui,predict,type='response')
    c <- tableau.logistic.region}
  
  for(i in 1:length(A)){
    if (A[i]==1){
      r<- cbind(r,unlist(glm[i]))
    }
    else if (A[i]==2){
      r <- cbind(r,unlist(glm.r[i]))
    }
    else if (A[i]==3){
      r <- cbind(r,unlist(moda[i]))
    }
    else{ r <- cbind(r,unlist(moda.redui[i]))
    }
  }
  res <- matrix(0,nrow(r),21)
  for(i in 1:nrow(r)){
    w <- which.max(r[i,])
    res[i,w] <- 1
  }
  colnames(res) <- regi
  re <- c()
  er <- c()
  for(i in 1:ncol(res)){
    tabl <- table(res[,i],c[,i])
    e <- cbind(tabl,c((tabl[1,2]*19/20+tabl[2,1]/20)/nrow(res),0),regi[i])
    re <- rbind(re,e)
    er <- c(er,re[(2*i-1),3])
  }
  print(mean(as.numeric(as.vector(er))))
  re <- rbind(re,mean(as.numeric(as.vector(er))))
  re <- as.data.frame(re)
  return(re)
}

vect.ind <- which(as.numeric(as.vector(predi.model.opti.test[,4]))>0)

md.max.predi <- mdl.max(model.fin.region,'b')
md.max.predi.test <- mdl.max(model.fin.region,'t')


#On ajoute les chaines de 3 et 4 caractères les plus présentes dans la base d'apprentissage 
#prepare la base pour la fonctio unnest_tokens
tib.COMMUNES <- as_tibble(COMMUNES)
tib.COMMUNES$nom <- as.character(tib.COMMUNES$nom)
tib.test <- as_tibble(test_COMMUNES)
tib.test$nom <- as.character(tib.test$nom)

#on extrait les chaines de 3-4 caractères sur base d'apprentissage 
Trichr.base <- unnest_tokens(tib.COMMUNES,bi,nom,token = "character_shingles", n = 3,drop=FALSE,collapse=FALSE)
freq.tri <- count(Trichr.base,bi,sort=TRUE)
freq.tri <- freq.tri[order(freq.tri$n,decreasing = TRUE),]


Quatr.chr.base <- unnest_tokens(tib.COMMUNES,bi,nom,token = "character_shingles", n = 4,drop=FALSE,collapse=FALSE)
freq.quatr <- count(Quatr.chr.base,bi,sort=TRUE)
freq.quatr <- freq.quatr[order(freq.quatr$n,decreasing = TRUE),]



# on crée des nouveaux tableaux logistiques avec les chaines de 3-4 caractères et le nb de caractères total
# on selectionne les 20 chaines de 3-4 caractères les plus frèquent de la base d'apprentissage

freq20.3.4 <- rbind(freq.tri[1:20,1],freq.quatr[1:20,1])


logistic.f.ngram <- function(A,B){ # renvoie un tableau avec le nb d'apparition de chaines de caractères dans les noms des villes 
  r <- c()
  cr <- c()
  for (i in 1:length(A)){
    t <- c()
    cr <- c(cr,nchar(A[i]))
    for(j in 1:length(B)){
      t <- c(t,str_count(A[i],B[j]))
      
    }
    r <- rbind(r,t)}
  r <- cbind(r,cr)
  colnames(r) <- c(B,"nbChar")
  rownames(r) <- c(1:nrow(r))
  return(r)
}


logistic.tri.quatr <- logistic.f.ngram(as.character(COMMUNES$nom),unlist(freq20.3.4))
logistic.tri.quatr.test <- logistic.f.ngram(as.character(test_COMMUNES$nom),unlist(freq20.3.4))

data.lm.region.tri.quatr <- cbind(data.lm.region.iter,logistic.tri.quatr)
data.lm.region.tri.quatr.test <- cbind(data.lm.regiontest.iter,logistic.tri.quatr.test)

#on prépare également Triffouillis-les-Oies
trifou <- list(nom="Triffouillis-les-Oies",0,0)
trifou.data <- logistic_table.initialization(abc,trifou)*1
trifou.ngram.data <- logistic.f.ngram(as.character(trifou$nom),unlist(freq20.3.4))
trifou.ngram.data <- cbind(trifou.data,trifou.ngram.data)

regi.ti <- str_c(regi,c('~'),sep="")


#On crée la liste de modèle par region avec ces nouvelles covariables
list.glm.ngram <- list()
for (i in 1:length(regi)) {
  modl <- glm(formula(paste(regi.ti[i],paste(c(abc,unlist(freq20.3.4),'nbChar'),collapse='+'))),
              as.data.frame(data.lm.region.tri.quatr),family=binomial(logit))
  list.glm.ngram[[regi[i]]] <-modl
}


predi.Trifou.ngram <-lapply(list.glm.ngram,predict,newdata=as.data.frame(trifou.ngram.data),type='response')

#On effectue les prédictions sur base d'apprentissage
predi.glm.ngram <- fct.predi.pregion(list.glm.ngram,tableau.logistic.region,taux,data.lm.region.tri.quatr)
#Sur base test
predi.glm.ngram.test <- fct.predi.pregion(list.glm.ngram,tableau.logistic.testregion,predi.glm.ngram,data.lm.region.tri.quatr.test)

# de même avec le modele multi-region sur base test
predi.max.ngram <-lapply(list.glm.ngram,predict,newdata=as.data.frame(data.lm.region.tri.quatr.test),type='response')
matrice.predi <- c()
for (i in 1:21){
  matrice.predi <- cbind(matrice.predi,as.numeric(as.vector(unlist(predi.max.ngram[i]))))
}
matrice.predi
res.max.ngram <- c() 
for (i in 1:nrow(matrice.predi)){
  r <- which.max(matrice.predi[i,])
  res.max.ngram <- c(res.max.ngram,regi[r])
} 

res.max.ngram  #resultat prediction multi-region sur base test
Mat.confusion.max.ngram <- table(as.factor(res.max.ngram),test_COMMUNES$region)

r <- c()
ef <- c()
for (i in 1:nrow(Mat.confusion.max.ngram)) {
  r <- c(r,Mat.confusion.max.ngram[i,i])
  ef <- c(ef,sum(Mat.confusion.max.ngram[,i]))
}
resulta.mat <- rbind(ef,r)
colnames(resulta.mat) <- regi
rownames(resulta.mat) <- c("Effectif région","bien placé")
resulta.mat #resultat par label
Mat.confusion.max.ngram #details des resultats du modèle multi-region



#Ll

COMMUNES
logistic.tri.quatr
tableau.logistic.allcommunes.iter
tableau.logi.ngram <- cbind(tableau.logistic.allcommunes.iter,logistic.tri.quatr)
data.lm.ngram <- cbind(COMMUNES$longitude,COMMUNES$latitude,tableau.logi.ngram)
colnames(data.lm.ngram) <- c('Longitude','lattitude',colnames(tableau.logi.ngram))
data.lm.ngram


#foret aleatoire
install.packages('plotly')
install.packages('randomForest')
library(randomForest)
library(plotly)

Sys.setenv('R_MAX_VSIZE'=32000000000)


RF.L <- randomForest(Longitude~., data=as.data.frame(data.lm.ngram[,-2]))
RF.l <- randomForest(lattitude~., data=as.data.frame(data.lm.ngram[,-1]))

Predi.RF.L <- predict(RF.L, newdata = as.data.frame(data.lm.region.tri.quatr.test[,-(1:21)]))
Predi.RF.l <- predict(RF.l, newdata = as.data.frame(data.lm.region.tri.quatr.test[,-(1:21)]))

Trifou.RF.L.predi <- predict(RF.L, newdata = as.data.frame(trifou.ngram.data))
Trifou.RF.l.predi <- predict(RF.l, newdata = as.data.frame(trifou.ngram.data))


Erreur.L <- (test_COMMUNES$longitude-Predi.RF.L)^2
Erreur.l <- (test_COMMUNES$latitude-Predi.RF.l)^2
Erreur <- (Erreur.L+Erreur.l)

Erreur <- cbind(ind.ville,Erreur)
Erreur <- Erreur[order(Erreur[,2]),]

ind.ville <- c(1:3620)

Erreur.L <- cbind(ind.ville,Erreur.L) #pour garder l'indice des villes après arrangement croissant
Erreur.l <- cbind(ind.ville,Erreur.l)
colnames(Erreur.L) <- c("ind","ErrL")
colnames(Erreur.l) <- c("ind","Errl")

Erreur.L <- Erreur.L[order(Erreur.L[,2]),]

Erreur.l <- Erreur.l[order(Erreur.l[,2]),]


#on va placer quelques villes sur la carte de France
install.packages('maptools')
library(maptools)

install.packages('raster')
library(raster)
FranceFormes <- getData(name="GADM", country="FRA", level=1)
plot(FranceFormes, main="FR")

#On cherche les meilleurs predictions
MeilleurL <- Predi.RF.L[Erreur[1:10,1]]
Meilleurl <- Predi.RF.l[Erreur[1:10,1]]
MeilleurLl <- cbind(MeilleurL,Meilleurl)*180/pi

Meilleur <- test_COMMUNES[Erreur[1:10,1],c(2,5,6)]
Meilleur <- Meilleur[,c(2,3)]*180/pi

#on les places vert position reelle, et rouge prédiction
plot(FranceFormes, main="Top 10")
for (i in 1:10){
  points(Meilleur[i,1],Meilleur[i,2],pch=20,col="green",cex=1)
  points(MeilleurLl[i,1],MeilleurLl[i,2],pch=1,col="red",cex=1)
}


