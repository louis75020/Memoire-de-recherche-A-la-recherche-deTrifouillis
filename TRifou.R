
install.packages("maps")
install.packages("mapdata")
library(maps)
library(mapdata)  # Map databases.
library(Hmisc)
install.packages("readr")
install.packages("diplyr")
library(dplyr)
library(readr)
install.packages("tidyverse")
library(tidyverse)
install.packages("stats")
install.packages("base")
library(tibble)
library(tidyr)
library(dplyr)
install.packages("stringr")
library(stringr)
install.packages('tidytext')
library(tidytext)


load("Documents/ALL/S2/Trifou/Environment.RData")
ls()


#CUT all_communes nom, longitud, latitude code postal departement region
#500 lettre rare
#lettre particulierement regroupé
#pi/180
setwd('Desktop/S2/Trifou')

All_COMMUNES<- read.delim('Desktop/S2/Trifou/data2011.csv', header = TRUE,
                       "\t", fileEncoding='latin3')
View(All_COMMUNES)


#separation de la base test 
set.seed(322)
test_COMMUNES_id<-sample((1:36209),size=3620,replace=F)
test_COMMUNES<-All_COMMUNES[test_COMMUNES_id,1:10]
COMMUNES<-All_COMMUNES[-test_COMMUNES_id,]

test_COMMUNES
COMMUNES
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



tableau.logistic.allcommunes<-logistic_table.initialization_basic(abc,COMMUNES)*1
View(tableau.logistic.allcommunes)

colnames(tableau.logistic.allcommunes)<-abc


tableau.logistic.testcommunes<-logistic_table.initialization_basic(abc,test_COMMUNES)*1
colnames(tableau.logistic.testcommunes)<-abc
view(tableau.logistic.testcommunes)

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

tableau.logistic.allcommunes.iter<-logistic_table.initialization(abc,COMMUNES)*1
view(tableau.logistic.allcommunes.iter)
colnames(tableau.logistic.allcommunes.iter)<-abc

tableau.logistic.testcommunes.iter<-logistic_table.initialization(abc,test_COMMUNES)*1
colnames(tableau.logistic.testcommunes.iter)<-abc
tableau.logistic.testcommunes.iter

###REGRESSION LOGISTIQUE
table(COMMUNES$region)

#Vecteur des regions
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

#Verification
tableau.logistic.region<-logistic_table.initialization_region(regi,COMMUNES)*1
tableau.logistic.region


tableau.logistic.testregion <- logistic_table.initialization_region(regi,test_COMMUNES)*1
tableau.logistic.testregion
test_COMMUNES
COMMUNES

#Tableau avec la première colonne 1 ou 0 si la ville est dans la region
#et le reste des colonnes 1 ou 0 si les lettres et caractère spéciaux 'a',...,'z','A',...,'Z' dans le nom de la ville

data.lm.region<-cbind(tableau.logistic.region,tableau.logistic.allcommunes)
data.lm.region

data.lm.region.test<-cbind(tableau.logistic.testregion,tableau.logistic.testcommunes)
data.lm.region.test

data.lm.region.iter<-cbind(tableau.logistic.region,tableau.logistic.allcommunes.iter)
data.lm.region.iter

data.lm.regiontest.iter<-cbind(tableau.logistic.testregion,tableau.logistic.testcommunes.iter)
data.lm.regiontest.iter

tableau.logistic.region
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

#On crée les modèles sans freq des caractères de regression pour chaque region
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
length(list.modl.glm)

summary(Reg.regionCE)

#Test de la qualité du Modèle
#Dans un premier temps on effectue les predictions sur les data qui ont servi à créer le modèle 
AL.predi<-predict(Reg.regionAL,type="response")
AQ.predi<-predict(Reg.regionAQ,type="response")
AU.predi<-predict(Reg.regionAU,type="response")
BN.predi<-predict(Reg.regionBN,type="response")
BO.predi<-predict(Reg.regionBO,type="response")
BR.predi<-predict(Reg.regionBR,type="response")
CA.predi<-predict(Reg.regionCA,type="response")
CE.predi<-predict(Reg.regionCE,type="response")
FC.predi<-predict(Reg.regionFC,type="response")
HN.predi<-predict(Reg.regionHN,type="response")
IF.predi<-predict(Reg.regionIF,type="response")
LI.predi<-predict(Reg.regionLI,type="response")
LO.predi<-predict(Reg.regionLO,type="response")
LR.predi<-predict(Reg.regionLR,type="response")
MP.predi<-predict(Reg.regionMP,type="response")
NP.predi<-predict(Reg.regionNP,type="response")
PA.predi<-predict(Reg.regionPA,type="response")
PC.predi<-predict(Reg.regionPC,type="response")
PI.predi<-predict(Reg.regionPI,type="response")
PL.predi<-predict(Reg.regionPL,type="response")
RA.predi<-predict(Reg.regionRA,type="response")

list.predi.glm <- list(AL.predi,AQ.predi,AU.predi,BN.predi,BO.predi,BR.predi,CA.predi,CE.predi,FC.predi
                       ,HN.predi,IF.predi,LI.predi,LR.predi,MP.predi,NP.predi,PA.predi,PC.predi,PI.predi,
                       PL.predi,RA.predi)


regi

##### IGNORER
#RASSS.predi<-predict(Reg.regionRA,newdata=RASSS,type="response")
#RASSS<-select(as.data.frame(data.lm.region),RA,a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,q,r,s,t,u,v,w,x,y,z,
         #A,B,C,D,E,F,G,H,I,J,K,L,M,N,O,P,Q,R,S,T,U,V,W,X,Y,Z)
##### IGNORER


# matrice de confusion, c’est-à-dire le tableau croisé des valeurs observées 
#et celles des valeurs prédites en appliquant le modèle aux données d’origine

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


Err.f <- function(A){
  if(nrow(A)==1){
    e <- (A[2]*19/20)/32589
  }
  else{e<-(A[1,2]*19/20+A[2,1]/20)/32589}
  return(e)
}

regi
Err.glm <- c(Err.f(table.AL.predi.glm),Err.f(table.AQ.predi.glm),Err.f(table.AU.predi.glm),
                Err.f(table.BN.predi.glm),Err.f(table.BO.predi.glm),Err.f(table.BR.predi.glm)
                ,Err.f(table.CA.predi.glm),Err.f(table.CE.predi.glm),Err.f(table.FC.predi.glm)
                ,Err.f(table.HN.predi.glm),Err.f(table.IF.predi.glm),Err.f(table.LI.predi.glm)
                ,Err.f(table.LO.predi.glm),Err.f(table.LR.predi.glm),Err.f(table.MP.predi.glm)
                ,Err.f(table.NP.predi.glm),Err.f(table.PA.predi.glm),Err.f(table.PC.predi.glm)
                ,Err.f(table.PI.predi.glm),Err.f(table.PL.predi.glm),Err.f(table.RA.predi.glm))
Err.moy.glm <- mean(Err.glm)
Err.f(table.AL.predi.glm)
Err.moy.glm
#essaie d'optimer le <0.5
taux<-seq(0.01,0.99,0.01)
length(taux)

#optim.tx renvoi la table de confusion avec le tx qui minimise l'erreur

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
AL.table.opti.glm
regi
##to be continued
AL.table.opti.glm<-optim.tx(AL.predi,AL.logi,taux)
AQ.table.opti.glm<-optim.tx(AQ.predi,AQ.logi,taux)
AU.table.opti.glm<-optim.tx(AU.predi,AU.logi,taux)
BN.table.opti.glm<-optim.tx(BN.predi,BN.logi,taux) #
BO.table.opti.glm<-optim.tx(BO.predi,BO.logi,taux)
BR.table.opti.glm<-optim.tx(BR.predi,BR.logi,taux)
CA.table.opti.glm<-optim.tx(CA.predi,CA.logi,taux)
CE.table.opti.glm<-optim.tx(CE.predi,CE.logi,taux)
FC.table.opti.glm<-optim.tx(FC.predi,FC.logi,taux)
HN.table.opti.glm<-optim.tx(HN.predi,HN.logi,taux)
IF.table.opti.glm<-optim.tx(IF.predi,IF.logi,taux)
LI.table.opti.glm<-optim.tx(LI.predi,LI.logi,taux)
LO.table.opti.glm<-optim.tx(LO.predi,LO.logi,taux)
LR.table.opti.glm<-optim.tx(LR.predi,LR.logi,taux)
MP.table.opti.glm<-optim.tx(MP.predi,MP.logi,taux)
NP.table.opti.glm<-optim.tx(NP.predi,NP.logi,taux)
PA.table.opti.glm<-optim.tx(PA.predi,PA.logi,taux)#
PC.table.opti.glm<-optim.tx(PC.predi,PC.logi,taux)#
PI.table.opti.glm<-optim.tx(PI.predi,PI.logi,taux)
PL.table.opti.glm<-optim.tx(PL.predi,PL.logi,taux)#
RA.table.opti.glm<-optim.tx(RA.predi,RA.logi,taux)
BR.table.opti.glm
Err.regio.opti.glm<-c(AL.table.opti.glm[1,4],AQ.table.opti.glm[1,4],AU.table.opti.glm[1,4],
                  BN.table.opti.glm[1,4],BO.table.opti.glm[1,4],BR.table.opti.glm[1,4],
                  CA.table.opti.glm[1,4],CE.table.opti.glm[1,4],FC.table.opti.glm[1,4],
                  HN.table.opti.glm[1,4],IF.table.opti.glm[1,4],LI.table.opti.glm[1,4],
                  LO.table.opti.glm[1,4],LR.table.opti.glm[1,4],MP.table.opti.glm[1,4],
                  NP.table.opti.glm[1,4],PA.table.opti.glm[1,4],PC.table.opti.glm[1,4],
                  PI.table.opti.glm[1,4],PL.table.opti.glm[1,4],RA.table.opti.glm[1,4])
Err.moy.opti.glm <- mean(Err.regio.opti.glm)
Err.moy.opti.glm
Err.moy.glm

AL.table.opti.glm


#on test le modèle sur la base test
test_COMMUNES
tableau.logistic.testregion


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

RA.logi.test


tableau.logistic.testcommunes
data.lm.region.test

AL.predi.test<-predict(Reg.regionAL,newdata=as.data.frame(data.lm.region.test),type="response")
AQ.predi.test<-predict(Reg.regionAQ,newdata=as.data.frame(data.lm.region.test),type="response")
AU.predi.test<-predict(Reg.regionAU,newdata=as.data.frame(data.lm.region.test),type="response")
BN.predi.test<-predict(Reg.regionBN,newdata=as.data.frame(data.lm.region.test),type="response")
BO.predi.test<-predict(Reg.regionBO,newdata=as.data.frame(data.lm.region.test),type="response")
BR.predi.test<-predict(Reg.regionBR,newdata=as.data.frame(data.lm.region.test),type="response")
CA.predi.test<-predict(Reg.regionCA,newdata=as.data.frame(data.lm.region.test),type="response")
CE.predi.test<-predict(Reg.regionCE,newdata=as.data.frame(data.lm.region.test),type="response")
FC.predi.test<-predict(Reg.regionFC,newdata=as.data.frame(data.lm.region.test),type="response")
HN.predi.test<-predict(Reg.regionHN,newdata=as.data.frame(data.lm.region.test),type="response")
IF.predi.test<-predict(Reg.regionIF,newdata=as.data.frame(data.lm.region.test),type="response")
LI.predi.test<-predict(Reg.regionLI,newdata=as.data.frame(data.lm.region.test),type="response")
LO.predi.test<-predict(Reg.regionLO,newdata=as.data.frame(data.lm.region.test),type="response")
LR.predi.test<-predict(Reg.regionLR,newdata=as.data.frame(data.lm.region.test),type="response")
MP.predi.test<-predict(Reg.regionMP,newdata=as.data.frame(data.lm.region.test),type="response")
NP.predi.test<-predict(Reg.regionNP,newdata=as.data.frame(data.lm.region.test),type="response")
PA.predi.test<-predict(Reg.regionPA,newdata=as.data.frame(data.lm.region.test),type="response")
PC.predi.test<-predict(Reg.regionPC,newdata=as.data.frame(data.lm.region.test),type="response")
PI.predi.test<-predict(Reg.regionPI,newdata=as.data.frame(data.lm.region.test),type="response")
PL.predi.test<-predict(Reg.regionPL,newdata=as.data.frame(data.lm.region.test),type="response")
RA.predi.test<-predict(Reg.regionRA,newdata=as.data.frame(data.lm.region.test),type="response")

AL.predi.test
AL.table.opti.glm
data.lm.region.test
table.AL.predi.test.glm
table.AL.predi.test.glm <- optim.test.tx(AL.predi.test,AL.logi.test,AL.table.opti.glm)
table.AQ.predi.test.glm <- optim.test.tx(AQ.predi.test,AQ.logi.test,AQ.table.opti.glm)
table.AU.predi.test.glm <- optim.test.tx(AU.predi.test,AU.logi.test,AU.table.opti.glm)
table.BN.predi.test.glm <- optim.test.tx(BN.predi.test,BN.logi.test,BN.table.opti.glm)
table.BO.predi.test.glm <- optim.test.tx(BO.predi.test,BO.logi.test,BO.table.opti.glm)
table.BR.predi.test.glm <- optim.test.tx(BR.predi.test,BR.logi.test,BR.table.opti.glm)
table.CA.predi.test.glm <- optim.test.tx(CA.predi.test,CA.logi.test,CA.table.opti.glm)
table.CE.predi.test.glm <- optim.test.tx(CE.predi.test,CE.logi.test,CE.table.opti.glm)
table.FC.predi.test.glm <- optim.test.tx(FC.predi.test,FC.logi.test,FC.table.opti.glm)
table.HN.predi.test.glm <- optim.test.tx(HN.predi.test,HN.logi.test,HN.table.opti.glm)
table.IF.predi.test.glm <- optim.test.tx(IF.predi.test,IF.logi.test,IF.table.opti.glm)
table.LI.predi.test.glm <- optim.test.tx(LI.predi.test,LI.logi.test,LI.table.opti.glm)
table.LO.predi.test.glm <- optim.test.tx(LO.predi.test,LO.logi.test,LO.table.opti.glm)
table.LR.predi.test.glm <- optim.test.tx(LR.predi.test,LR.logi.test,LR.table.opti.glm)
table.MP.predi.test.glm <- optim.test.tx(MP.predi.test,MP.logi.test,MP.table.opti.glm)
table.NP.predi.test.glm <- optim.test.tx(NP.predi.test,NP.logi.test,NP.table.opti.glm)
table.PA.predi.test.glm <- optim.test.tx(PA.predi.test,PA.logi.test,PA.table.opti.glm)
table.PC.predi.test.glm <- optim.test.tx(PC.predi.test,PC.logi.test,PC.table.opti.glm)
table.PI.predi.test.glm <- optim.test.tx(PI.predi.test,PI.logi.test,PI.table.opti.glm)
table.PL.predi.test.glm <- optim.test.tx(PL.predi.test,PL.logi.test,PL.table.opti.glm)
table.RA.predi.test.glm <- optim.test.tx(RA.predi.test,RA.logi.test,RA.table.opti.glm)

#Erreur sur base test (3620)
table.RA.predi.test.glm
table.AL.predi.test.glm
Err.test.glm<-c(table.AL.predi.test.glm[1,4],table.AQ.predi.test.glm[1,4],table.AU.predi.test.glm[1,4],
           table.BN.predi.test.glm[1,4],table.BO.predi.test.glm[1,4],table.BR.predi.test.glm[1,4],
           table.CA.predi.test.glm[1,4],table.CE.predi.test.glm[1,4],table.FC.predi.test.glm[1,4],
           table.HN.predi.test.glm[1,4],table.IF.predi.test.glm[1,4],table.LI.predi.test.glm[1,4],
           table.LO.predi.test.glm[1,4],table.LR.predi.test.glm[1,4],table.MP.predi.test.glm[1,4],
           table.NP.predi.test.glm[1,4],table.PA.predi.test.glm[1,4],table.PC.predi.test.glm[1,4],
           table.PI.predi.test.glm[1,4],table.PL.predi.test.glm[1,4],table.RA.predi.test.glm[1,4])
                       
Err.moy.test.glm<-mean(Err.test.glm)
Err.moy.test.glm
Err.moy.opti.glm
Err.moy.glm


table.AL.predi.test.glm
(55+7)/3620
3525+55+7+33
regi
### n'a pas beaucoup de sens le modèle rejette tout,
#baisser le critère <0.5 augmente l'erreur donc autant rien faire


#Predire une ville sur chacun des modele et garder la proba max


#identification de variable signigicative 
# drop1 va tour à tour supprimer chaque variable du modèle 
#et réaliser une analyse de variance (ANOVA, voir fonction anova) 
#pour voir si la variance change significativement.
#####ATTENTION LONG#####
#test=AIC
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



PL.drop
##Avant l'AIC on veut reduire le modele aux variables significative du drop1

regi

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

PL.select.drop
#on glm avec seulement les var significatives

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

summary(AL.Reg.Select.drop)
PL.Reg.Select.drop
#AIC
#on regarde s’il est possible d’améliorer le modèle en supprimant une des variables du modèle. 
#Si plusieurs variables permettent d’améliorer le modèle, on supprimera la variable 
#dont la suppression améliorera le plus le modèle. Puis on recommence le même procédé 
#pour voir si la suppression d’une seconde variable peut encore améliorer le modèle et ainsi de suite.
#####ATTENTION LONG#####

#garder les var significatives et faire step backward

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


length(list.glm.redui)
tableau.logi.regi.iter<-cbind(tableau.logistic.region,tableau.logistic.allcommunes.iter)
tableau.logi.regi.iter

#Regression par Region avec freq
data.lm.region.iter
data.lm.regiontest.iter
tableau.logi.regi.iter
#file encording=latin3

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



summary(Reg.iter.regionAL)

#Test de la qualité du Modèle
#Dans un premier temps on effectue les predictions sur les data qui ont servi à créer le modèle 
AL.iter.predi<-predict(Reg.iter.regionAL,type="response")
AQ.iter.predi<-predict(Reg.iter.regionAQ,type="response")
AU.iter.predi<-predict(Reg.iter.regionAU,type="response")
BN.iter.predi<-predict(Reg.iter.regionBN,type="response")
BO.iter.predi<-predict(Reg.iter.regionBO,type="response")
BR.iter.predi<-predict(Reg.iter.regionBR,type="response")
CA.iter.predi<-predict(Reg.iter.regionCA,type="response")
CE.iter.predi<-predict(Reg.iter.regionCE,type="response")
FC.iter.predi<-predict(Reg.iter.regionFC,type="response")
HN.iter.predi<-predict(Reg.iter.regionHN,type="response")
IF.iter.predi<-predict(Reg.iter.regionIF,type="response")
LI.iter.predi<-predict(Reg.iter.regionLI,type="response")
LO.iter.predi<-predict(Reg.iter.regionLO,type="response")
LR.iter.predi<-predict(Reg.iter.regionLR,type="response")
MP.iter.predi<-predict(Reg.iter.regionMP,type="response")
NP.iter.predi<-predict(Reg.iter.regionNP,type="response")
PA.iter.predi<-predict(Reg.iter.regionPA,type="response")
PC.iter.predi<-predict(Reg.iter.regionPC,type="response")
PI.iter.predi<-predict(Reg.iter.regionPI,type="response")
PL.iter.predi<-predict(Reg.iter.regionPL,type="response")
RA.iter.predi<-predict(Reg.iter.regionRA,type="response")

table.AL.iter.predi.glm <- optim.tx(AL.iter.predi,AL.logi,taux)
table.AQ.iter.predi.glm <- optim.tx(AQ.iter.predi,AQ.logi,taux)
table.AU.iter.predi.glm <- optim.tx(AU.iter.predi,AU.logi,taux)
table.BN.iter.predi.glm <- optim.tx(BN.iter.predi,BN.logi,taux)
table.BO.iter.predi.glm <- optim.tx(BO.iter.predi,BO.logi,taux)
table.BR.iter.predi.glm <- optim.tx(BR.iter.predi,BR.logi,taux)
table.CA.iter.predi.glm <- optim.tx(CA.iter.predi,CA.logi,taux)
table.CE.iter.predi.glm <- optim.tx(CE.iter.predi,CE.logi,taux)
table.FC.iter.predi.glm <- optim.tx(FC.iter.predi,FC.logi,taux)
table.HN.iter.predi.glm <- optim.tx(HN.iter.predi,HN.logi,taux)
table.IF.iter.predi.glm <- optim.tx(IF.iter.predi,IF.logi,taux)
table.LI.iter.predi.glm <- optim.tx(LI.iter.predi,LI.logi,taux)
table.LO.iter.predi.glm <- optim.tx(LO.iter.predi,LO.logi,taux)
table.LR.iter.predi.glm <- optim.tx(LR.iter.predi,LR.logi,taux)
table.MP.iter.predi.glm <- optim.tx(MP.iter.predi,MP.logi,taux)
table.NP.iter.predi.glm <- optim.tx(NP.iter.predi,NP.logi,taux)
table.PA.iter.predi.glm <- optim.tx(PA.iter.predi,PA.logi,taux)
table.PC.iter.predi.glm <- optim.tx(PC.iter.predi,PC.logi,taux)
table.PI.iter.predi.glm <- optim.tx(PI.iter.predi,PI.logi,taux)
table.PL.iter.predi.glm <- optim.tx(PL.iter.predi,PL.logi,taux)
table.RA.iter.predi.glm <- optim.tx(RA.iter.predi,RA.logi,taux)



#erreur modele plein avec moda 
  #sur base 
table.AL.iter.predi.glm
table.AL.predi.glm
AL.table.opti.glm


Err.regi.iter<-c(table.AL.iter.predi.glm[1,4],table.AQ.iter.predi.glm[1,4],table.AU.iter.predi.glm[1,4],table.BN.iter.predi.glm[1,4],table.BO.iter.predi.glm[1,4],table.BR.iter.predi.glm[1,4],table.CA.iter.predi.glm[1,4],
                 table.CE.iter.predi.glm[1,4],table.FC.iter.predi.glm[1,4],table.HN.iter.predi.glm[1,4],table.IF.iter.predi.glm[1,4],table.LI.iter.predi.glm[1,4],table.LO.iter.predi.glm[1,4],table.LR.iter.predi.glm[1,4],
                 table.MP.iter.predi.glm[1,4],table.NP.iter.predi.glm[1,4],table.PA.iter.predi.glm[1,4],table.PC.iter.predi.glm[1,4],table.PI.iter.predi.glm[1,4],table.PL.iter.predi.glm[1,4],table.RA.iter.predi.glm[1,4])

Err.moy.iter<-mean(Err.regi.iter)

table.AL.iter.predi.glm
Err.moy.glm
Err.moy.opti.glm
Err.moy.test.glm
Err.moy.iter


#on test le modèle sur la base test
test_COMMUNES
tableau.logistic.testregion
RA.logi.test

data.lm.region.iter
data.lm.regiontest.iter
tableau.logi.regi.iter

AL.predi.iter.test<-predict(Reg.iter.regionAL,newdata=as.data.frame(data.lm.regiontest.iter),type="response")
AQ.predi.iter.test<-predict(Reg.iter.regionAQ,newdata=as.data.frame(data.lm.regiontest.iter),type="response")
AU.predi.iter.test<-predict(Reg.iter.regionAU,newdata=as.data.frame(data.lm.regiontest.iter),type="response")
BN.predi.iter.test<-predict(Reg.iter.regionBN,newdata=as.data.frame(data.lm.regiontest.iter),type="response")
BO.predi.iter.test<-predict(Reg.iter.regionBO,newdata=as.data.frame(data.lm.regiontest.iter),type="response")
BR.predi.iter.test<-predict(Reg.iter.regionBR,newdata=as.data.frame(data.lm.regiontest.iter),type="response")
CA.predi.iter.test<-predict(Reg.iter.regionCA,newdata=as.data.frame(data.lm.regiontest.iter),type="response")
CE.predi.iter.test<-predict(Reg.iter.regionCE,newdata=as.data.frame(data.lm.regiontest.iter),type="response")
FC.predi.iter.test<-predict(Reg.iter.regionFC,newdata=as.data.frame(data.lm.regiontest.iter),type="response")
HN.predi.iter.test<-predict(Reg.iter.regionHN,newdata=as.data.frame(data.lm.regiontest.iter),type="response")
IF.predi.iter.test<-predict(Reg.iter.regionIF,newdata=as.data.frame(data.lm.regiontest.iter),type="response")
LI.predi.iter.test<-predict(Reg.iter.regionLI,newdata=as.data.frame(data.lm.regiontest.iter),type="response")
LO.predi.iter.test<-predict(Reg.iter.regionLO,newdata=as.data.frame(data.lm.regiontest.iter),type="response")
LR.predi.iter.test<-predict(Reg.iter.regionLR,newdata=as.data.frame(data.lm.regiontest.iter),type="response")
MP.predi.iter.test<-predict(Reg.iter.regionMP,newdata=as.data.frame(data.lm.regiontest.iter),type="response")
NP.predi.iter.test<-predict(Reg.iter.regionNP,newdata=as.data.frame(data.lm.regiontest.iter),type="response")
PA.predi.iter.test<-predict(Reg.iter.regionPA,newdata=as.data.frame(data.lm.regiontest.iter),type="response")
PC.predi.iter.test<-predict(Reg.iter.regionPC,newdata=as.data.frame(data.lm.regiontest.iter),type="response")
PI.predi.iter.test<-predict(Reg.iter.regionPI,newdata=as.data.frame(data.lm.regiontest.iter),type="response")
PL.predi.iter.test<-predict(Reg.iter.regionPL,newdata=as.data.frame(data.lm.regiontest.iter),type="response")
RA.predi.iter.test<-predict(Reg.iter.regionRA,newdata=as.data.frame(data.lm.regiontest.iter),type="response")

table.AL.iter.predi.test.glm <- optim.test.tx(AL.predi.iter.test,AL.logi.test,table.AL.iter.predi.glm)
table.AQ.iter.predi.test.glm <- optim.test.tx(AQ.predi.iter.test,AQ.logi.test,table.AQ.iter.predi.glm)
table.AU.iter.predi.test.glm <- optim.test.tx(AU.predi.iter.test,AU.logi.test,table.AU.iter.predi.glm)
table.BN.iter.predi.test.glm <- optim.test.tx(BN.predi.iter.test,BN.logi.test,table.BN.iter.predi.glm)
table.BO.iter.predi.test.glm <- optim.test.tx(BO.predi.iter.test,BO.logi.test,table.BO.iter.predi.glm)
table.BR.iter.predi.test.glm <- optim.test.tx(BR.predi.iter.test,BR.logi.test,table.BR.iter.predi.glm)
table.CA.iter.predi.test.glm <- optim.test.tx(CA.predi.iter.test,CA.logi.test,table.CA.iter.predi.glm)
table.CE.iter.predi.test.glm <- optim.test.tx(CE.predi.iter.test,CE.logi.test,table.CE.iter.predi.glm)
table.FC.iter.predi.test.glm <- optim.test.tx(FC.predi.iter.test,FC.logi.test,table.FC.iter.predi.glm)
table.HN.iter.predi.test.glm <- optim.test.tx(HN.predi.iter.test,HN.logi.test,table.HN.iter.predi.glm)
table.IF.iter.predi.test.glm <- optim.test.tx(IF.predi.iter.test,IF.logi.test,table.IF.iter.predi.glm)
table.LI.iter.predi.test.glm <- optim.test.tx(LI.predi.iter.test,LI.logi.test,table.LI.iter.predi.glm)
table.LO.iter.predi.test.glm <- optim.test.tx(LO.predi.iter.test,LO.logi.test,table.LO.iter.predi.glm)
table.LR.iter.predi.test.glm <- optim.test.tx(LR.predi.iter.test,LR.logi.test,table.LR.iter.predi.glm)
table.MP.iter.predi.test.glm <- optim.test.tx(MP.predi.iter.test,MP.logi.test,table.MP.iter.predi.glm)
table.NP.iter.predi.test.glm <- optim.test.tx(NP.predi.iter.test,NP.logi.test,table.NP.iter.predi.glm)
table.PA.iter.predi.test.glm <- optim.test.tx(PA.predi.iter.test,PA.logi.test,table.PA.iter.predi.glm)
table.PC.iter.predi.test.glm <- optim.test.tx(PC.predi.iter.test,PC.logi.test,table.PC.iter.predi.glm)
table.PI.iter.predi.test.glm <- optim.test.tx(PI.predi.iter.test,PI.logi.test,table.PI.iter.predi.glm)
table.PL.iter.predi.test.glm <- optim.test.tx(PL.predi.iter.test,PL.logi.test,table.PL.iter.predi.glm)
table.RA.iter.predi.test.glm <- optim.test.tx(RA.predi.iter.test,RA.logi.test,table.RA.iter.predi.glm)
table.RA.iter.predi.test.glm

table.RA.iter.predi.test.glm
table.RA.iter.predi.glm
#erreur sur test  modele plein avec moda 


regi
Err.regi.iter.test<-c(table.AL.iter.predi.test.glm[1,4],table.AQ.iter.predi.test.glm[1,4],
                      table.AU.iter.predi.test.glm[1,4],table.BN.iter.predi.test.glm[1,4],
                      table.BO.iter.predi.test.glm[1,4],table.BR.iter.predi.test.glm[1,4],
                      table.CA.iter.predi.test.glm[1,4],table.CE.iter.predi.test.glm[1,4],
                      table.FC.iter.predi.test.glm[1,4],table.HN.iter.predi.test.glm[1,4],
                      table.IF.iter.predi.test.glm[1,4],table.LI.iter.predi.test.glm[1,4],
                      table.LO.iter.predi.test.glm[1,4],table.LR.iter.predi.test.glm[1,4],
                      table.MP.iter.predi.test.glm[1,4],table.NP.iter.predi.test.glm[1,4],
                      table.PA.iter.predi.test.glm[1,4],table.PC.iter.predi.test.glm[1,4],
                      table.PI.iter.predi.test.glm[1,4],table.PL.iter.predi.test.glm[1,4],
                      table.RA.iter.predi.test.glm[1,4])

Err.moy.iter.test<-mean(Err.regi.iter.test)

Err.moy.glm
Err.moy.opti.glm
Err.moy.test.glm
Err.moy.iter
Err.moy.iter.test


#identification de variable signigicative 
# drop1 va tour à tour supprimer chaque variable du modèle 
#et réaliser une analyse de variance (ANOVA, voir fonction anova) 
#pour voir si la variance change significativement.
#####ATTENTION LONG#####

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


AL.iter.drop

##Avant l'AIC on veut reduire le modele aux variables significative du drop1 ( reduira le cout de l'AIC)
regi

#On extrait les variable significative 
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


AL.iter.select.drop

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

summary(AL.Reg.iter.Select.drop)
AL.iter.drop

#AIC
#on regarde s’il est possible d’améliorer le modèle en supprimant une des variables du modèle. 
#Si plusieurs variables permettent d’améliorer le modèle, on supprimera la variable 
#dont la suppression améliorera le plus le modèle. Puis on recommence le même procédé 
#pour voir si la suppression d’une seconde variable peut encore améliorer le modèle et ainsi de suite.
#####ATTENTION LONG#####

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




#####################
####################

#on effectue le prediction sur modele redui sans moda 
  #sur data base 
AL.predi.redui<-predict(step.AL.Reg.Select.drop,type="response")
AQ.predi.redui<-predict(step.AQ.Reg.Select.drop,type="response")
AU.predi.redui<-predict(step.AU.Reg.Select.drop,type="response")
BN.predi.redui<-predict(step.BN.Reg.Select.drop,type="response")
BO.predi.redui<-predict(step.BO.Reg.Select.drop,type="response")
BR.predi.redui<-predict(step.BR.Reg.Select.drop,type="response")
CA.predi.redui<-predict(step.CA.Reg.Select.drop,type="response")
CE.predi.redui<-predict(step.CE.Reg.Select.drop,type="response")
FC.predi.redui<-predict(step.FC.Reg.Select.drop,type="response")
HN.predi.redui<-predict(step.HN.Reg.Select.drop,type="response")
IF.predi.redui<-predict(step.IF.Reg.Select.drop,type="response")
LI.predi.redui<-predict(step.LI.Reg.Select.drop,type="response")
LO.predi.redui<-predict(step.LO.Reg.Select.drop,type="response")
LR.predi.redui<-predict(step.LR.Reg.Select.drop,type="response")
MP.predi.redui<-predict(step.MP.Reg.Select.drop,type="response")
NP.predi.redui<-predict(step.NP.Reg.Select.drop,type="response")
PA.predi.redui<-predict(step.PA.Reg.Select.drop,type="response")
PC.predi.redui<-predict(step.PC.Reg.Select.drop,type="response")
PI.predi.redui<-predict(step.PI.Reg.Select.drop,type="response")
PL.predi.redui<-predict(step.PL.Reg.Select.drop,type="response")
RA.predi.redui<-predict(step.RA.Reg.Select.drop,type="response")


regi
table.BR.predi.redui
table.AL.predi.redui<-optim.tx(AL.predi.redui,AL.logi,taux)
table.AQ.predi.redui<-optim.tx(AQ.predi.redui,AQ.logi,taux)
table.AU.predi.redui<-optim.tx(AU.predi.redui,AU.logi,taux)
table.BN.predi.redui<-optim.tx(BN.predi.redui,BN.logi,taux)
table.BO.predi.redui<-optim.tx(BO.predi.redui,BO.logi,taux)
table.BR.predi.redui<-optim.tx(BR.predi.redui,BR.logi,taux)
table.CA.predi.redui<-optim.tx(CA.predi.redui,CA.logi,taux)
table.CE.predi.redui<-optim.tx(CE.predi.redui,CE.logi,taux)
table.FC.predi.redui<-optim.tx(FC.predi.redui,FC.logi,taux)
table.HN.predi.redui<-optim.tx(HN.predi.redui,HN.logi,taux)
table.IF.predi.redui<-optim.tx(IF.predi.redui,IF.logi,taux)
table.LI.predi.redui<-optim.tx(LI.predi.redui,LI.logi,taux)
table.LO.predi.redui<-optim.tx(LO.predi.redui,LO.logi,taux)
table.LR.predi.redui<-optim.tx(LR.predi.redui,LR.logi,taux)
table.MP.predi.redui<-optim.tx(MP.predi.redui,MP.logi,taux)
table.NP.predi.redui<-optim.tx(NP.predi.redui,NP.logi,taux)
table.PA.predi.redui<-optim.tx(PA.predi.redui,PA.logi,taux)
table.PC.predi.redui<-optim.tx(PC.predi.redui,PC.logi,taux)
table.PI.predi.redui<-optim.tx(PI.predi.redui,PI.logi,taux)
table.PL.predi.redui<-optim.tx(PL.predi.redui,PL.logi,taux)
table.RA.predi.redui<-optim.tx(RA.predi.redui,RA.logi,taux)
table.RA.predi.redui

table.AL.predi.redui #predi modele reduit sans moda data base 

Err.redui.glm<-c(table.AL.predi.redui[1,4],table.AQ.predi.redui[1,4],table.AU.predi.redui[1,4],
                 table.BN.predi.redui[1,4],table.BO.predi.redui[1,4],table.BR.predi.redui[1,4],
                 table.CA.predi.redui[1,4],table.CE.predi.redui[1,4],table.FC.predi.redui[1,4],
                 table.HN.predi.redui[1,4],table.IF.predi.redui[1,4],table.LI.predi.redui[1,4],
                 table.LO.predi.redui[1,4],table.LR.predi.redui[1,4],table.MP.predi.redui[1,4],
                 table.NP.predi.redui[1,4],table.PA.predi.redui[1,4],table.PC.predi.redui[1,4],
                 table.PI.predi.redui[1,4],table.PL.predi.redui[1,4],table.RA.predi.redui[1,4])
Err.moy.redui.glm <- mean(Err.redui.glm)
Err.moy.redui.glm

Err.moy.glm
Err.moy.opti.glm
Err.moy.test.glm
Err.moy.redui.glm
Err.moy.iter
Err.moy.iter.test

####



data.lm.region.test
regi

#on effectue le prediction sur modele redui sans moda 
  #sur base test
AL.predi.redui.test<-predict(step.AL.Reg.Select.drop,newdata=as.data.frame(data.lm.region.test),type="response")
AQ.predi.redui.test<-predict(step.AQ.Reg.Select.drop,newdata=as.data.frame(data.lm.region.test),type="response")
AU.predi.redui.test<-predict(step.AU.Reg.Select.drop,newdata=as.data.frame(data.lm.region.test),type="response")
BN.predi.redui.test<-predict(step.BN.Reg.Select.drop,newdata=as.data.frame(data.lm.region.test),type="response")
BO.predi.redui.test<-predict(step.BO.Reg.Select.drop,newdata=as.data.frame(data.lm.region.test),type="response")
BR.predi.redui.test<-predict(step.BR.Reg.Select.drop,newdata=as.data.frame(data.lm.region.test),type="response")
CA.predi.redui.test<-predict(step.CA.Reg.Select.drop,newdata=as.data.frame(data.lm.region.test),type="response")
CE.predi.redui.test<-predict(step.CE.Reg.Select.drop,newdata=as.data.frame(data.lm.region.test),type="response")
FC.predi.redui.test<-predict(step.FC.Reg.Select.drop,newdata=as.data.frame(data.lm.region.test),type="response")
HN.predi.redui.test<-predict(step.HN.Reg.Select.drop,newdata=as.data.frame(data.lm.region.test),type="response")
IF.predi.redui.test<-predict(step.IF.Reg.Select.drop,newdata=as.data.frame(data.lm.region.test),type="response")
LI.predi.redui.test<-predict(step.LI.Reg.Select.drop,newdata=as.data.frame(data.lm.region.test),type="response")
LO.predi.redui.test<-predict(step.LO.Reg.Select.drop,newdata=as.data.frame(data.lm.region.test),type="response")
LR.predi.redui.test<-predict(step.LR.Reg.Select.drop,newdata=as.data.frame(data.lm.region.test),type="response")
MP.predi.redui.test<-predict(step.MP.Reg.Select.drop,newdata=as.data.frame(data.lm.region.test),type="response")
NP.predi.redui.test<-predict(step.NP.Reg.Select.drop,newdata=as.data.frame(data.lm.region.test),type="response")
PA.predi.redui.test<-predict(step.PA.Reg.Select.drop,newdata=as.data.frame(data.lm.region.test),type="response")
PC.predi.redui.test<-predict(step.PC.Reg.Select.drop,newdata=as.data.frame(data.lm.region.test),type="response")
PI.predi.redui.test<-predict(step.PI.Reg.Select.drop,newdata=as.data.frame(data.lm.region.test),type="response")
PL.predi.redui.test<-predict(step.PL.Reg.Select.drop,newdata=as.data.frame(data.lm.region.test),type="response")
RA.predi.redui.test<-predict(step.RA.Reg.Select.drop,newdata=as.data.frame(data.lm.region.test),type="response")


table.AL.predi.redui.test <- optim.test.tx(AL.predi.redui.test,AL.logi.test,table.AL.predi.redui)
table.AQ.predi.redui.test <- optim.test.tx(AQ.predi.redui.test,AQ.logi.test,table.AQ.predi.redui)
table.AU.predi.redui.test <- optim.test.tx(AU.predi.redui.test,AU.logi.test,table.AU.predi.redui)
table.BN.predi.redui.test <- optim.test.tx(BN.predi.redui.test,BN.logi.test,table.BN.predi.redui)
table.BO.predi.redui.test <- optim.test.tx(BO.predi.redui.test,BO.logi.test,table.BO.predi.redui)
table.BR.predi.redui.test <- optim.test.tx(BR.predi.redui.test,BR.logi.test,table.BR.predi.redui)
table.CA.predi.redui.test <- optim.test.tx(CA.predi.redui.test,CA.logi.test,table.CA.predi.redui)
table.CE.predi.redui.test <- optim.test.tx(CE.predi.redui.test,CE.logi.test,table.CE.predi.redui)
table.FC.predi.redui.test <- optim.test.tx(FC.predi.redui.test,FC.logi.test,table.FC.predi.redui)
table.HN.predi.redui.test <- optim.test.tx(HN.predi.redui.test,HN.logi.test,table.HN.predi.redui)
table.IF.predi.redui.test <- optim.test.tx(IF.predi.redui.test,IF.logi.test,table.IF.predi.redui)
table.LI.predi.redui.test <- optim.test.tx(LI.predi.redui.test,LI.logi.test,table.LI.predi.redui)
table.LO.predi.redui.test <- optim.test.tx(LO.predi.redui.test,LO.logi.test,table.LO.predi.redui)
table.LR.predi.redui.test <- optim.test.tx(LR.predi.redui.test,LR.logi.test,table.LR.predi.redui)
table.MP.predi.redui.test <- optim.test.tx(MP.predi.redui.test,MP.logi.test,table.MP.predi.redui)
table.NP.predi.redui.test <- optim.test.tx(NP.predi.redui.test,NP.logi.test,table.NP.predi.redui)
table.PA.predi.redui.test <- optim.test.tx(PA.predi.redui.test,PA.logi.test,table.PA.predi.redui)
table.PC.predi.redui.test <- optim.test.tx(PC.predi.redui.test,PC.logi.test,table.PC.predi.redui)
table.PI.predi.redui.test <- optim.test.tx(PI.predi.redui.test,PI.logi.test,table.PI.predi.redui)
table.PL.predi.redui.test <- optim.test.tx(PL.predi.redui.test,PL.logi.test,table.PL.predi.redui)
table.RA.predi.redui.test <- optim.test.tx(RA.predi.redui.test,RA.logi.test,table.RA.predi.redui)
table.RA.predi.redui.test 
table.AL.predi.redui.test #predi modele reduit sans moda data test 
table.AL.predi.redui
##Erreur modele reduit sur base test 
Err.regi.redui.test<-c(table.AL.predi.redui.test[1,4],table.AQ.predi.redui.test[1,4],
                       table.AU.predi.redui.test[1,4],table.BN.predi.redui.test[1,4],
                       table.BO.predi.redui.test[1,4],table.BR.predi.redui.test[1,4],
                       table.CA.predi.redui.test[1,4],table.CE.predi.redui.test[1,4],
                       table.FC.predi.redui.test[1,4],table.HN.predi.redui.test[1,4],
                       table.IF.predi.redui.test[1,4],table.LI.predi.redui.test[1,4],
                       table.LO.predi.redui.test[1,4],table.LR.predi.redui.test[1,4],
                       table.MP.predi.redui.test[1,4],table.NP.predi.redui.test[1,4],
                       table.PA.predi.redui.test[1,4],table.PC.predi.redui.test[1,4],
                       table.PI.predi.redui.test[1,4],table.PL.predi.redui.test[1,4],
                       table.RA.predi.redui.test[1,4])

regi

Err.moy.redui.test<-mean(Err.regi.redui.test)

Err.moy.glm
Err.moy.opti.glm
Err.moy.test.glm
Err.moy.redui.glm
Err.moy.redui.test
Err.moy.iter
Err.moy.iter.test

#on effectue le prediction sur modele redui avec moda 
  #sur data base 
step.AL.Reg.iter.Select.drop #modele reduit avec mode 

AL.iter.predi.redui<-predict(step.AL.Reg.iter.Select.drop,type="response")
AQ.iter.predi.redui<-predict(step.AQ.Reg.iter.Select.drop,type="response")
AU.iter.predi.redui<-predict(step.AU.Reg.iter.Select.drop,type="response")
BN.iter.predi.redui<-predict(step.BN.Reg.iter.Select.drop,type="response")
BO.iter.predi.redui<-predict(step.BO.Reg.iter.Select.drop,type="response")
BR.iter.predi.redui<-predict(step.BR.Reg.iter.Select.drop,type="response")
CA.iter.predi.redui<-predict(step.CA.Reg.iter.Select.drop,type="response")
CE.iter.predi.redui<-predict(step.CE.Reg.iter.Select.drop,type="response")
FC.iter.predi.redui<-predict(step.FC.Reg.iter.Select.drop,type="response")
HN.iter.predi.redui<-predict(step.HN.Reg.iter.Select.drop,type="response")
IF.iter.predi.redui<-predict(step.IF.Reg.iter.Select.drop,type="response")
LI.iter.predi.redui<-predict(step.LI.Reg.iter.Select.drop,type="response")
LO.iter.predi.redui<-predict(step.LO.Reg.iter.Select.drop,type="response")
LR.iter.predi.redui<-predict(step.LR.Reg.iter.Select.drop,type="response")
MP.iter.predi.redui<-predict(step.MP.Reg.iter.Select.drop,type="response")
NP.iter.predi.redui<-predict(step.NP.Reg.iter.Select.drop,type="response")
PA.iter.predi.redui<-predict(step.PA.Reg.iter.Select.drop,type="response")
PC.iter.predi.redui<-predict(step.PC.Reg.iter.Select.drop,type="response")
PI.iter.predi.redui<-predict(step.PI.Reg.iter.Select.drop,type="response")
PL.iter.predi.redui<-predict(step.PL.Reg.iter.Select.drop,type="response")
RA.iter.predi.redui<-predict(step.RA.Reg.iter.Select.drop,type="response")


list.predi.moda.redui <- list(AL.iter.predi.redui,AQ.iter.predi.redui)

table.AL.iter.predi.redui<- optim.tx(AL.iter.predi.redui,AL.logi,taux)
table.AQ.iter.predi.redui<- optim.tx(AQ.iter.predi.redui,AQ.logi,taux)
table.AU.iter.predi.redui<- optim.tx(AU.iter.predi.redui,AU.logi,taux)
table.BN.iter.predi.redui<- optim.tx(BN.iter.predi.redui,BN.logi,taux)
table.BO.iter.predi.redui<- optim.tx(BO.iter.predi.redui,BO.logi,taux)
table.BR.iter.predi.redui<- optim.tx(BR.iter.predi.redui,BR.logi,taux)
table.CA.iter.predi.redui<- optim.tx(CA.iter.predi.redui,CA.logi,taux)
table.CE.iter.predi.redui<- optim.tx(CE.iter.predi.redui,CE.logi,taux)
table.FC.iter.predi.redui<- optim.tx(FC.iter.predi.redui,FC.logi,taux)
table.HN.iter.predi.redui<- optim.tx(HN.iter.predi.redui,HN.logi,taux)
table.IF.iter.predi.redui<- optim.tx(IF.iter.predi.redui,IF.logi,taux)
table.LI.iter.predi.redui<- optim.tx(LI.iter.predi.redui,LI.logi,taux)
table.LO.iter.predi.redui<- optim.tx(LO.iter.predi.redui,LO.logi,taux)
table.LR.iter.predi.redui<- optim.tx(LR.iter.predi.redui,LR.logi,taux)
table.MP.iter.predi.redui<- optim.tx(MP.iter.predi.redui,MP.logi,taux)
table.NP.iter.predi.redui<- optim.tx(NP.iter.predi.redui,NP.logi,taux)
table.PA.iter.predi.redui<- optim.tx(PA.iter.predi.redui,PA.logi,taux)
table.PC.iter.predi.redui<- optim.tx(PC.iter.predi.redui,PC.logi,taux)
table.PI.iter.predi.redui<- optim.tx(PI.iter.predi.redui,PI.logi,taux)
table.PL.iter.predi.redui<- optim.tx(PL.iter.predi.redui,PL.logi,taux)
table.RA.iter.predi.redui<- optim.tx(RA.iter.predi.redui,RA.logi,taux)

table.AL.iter.predi.redui #predi modele reduit avec moda data base

Err.iter.redui <- c(table.AL.iter.predi.redui[1,4], table.AQ.iter.predi.redui[1,4],table.AU.iter.predi.redui[1,4], 
                    table.BN.iter.predi.redui[1,4], table.BO.iter.predi.redui[1,4],table.BR.iter.predi.redui[1,4],
                    table.CA.iter.predi.redui[1,4], table.CE.iter.predi.redui[1,4], table.FC.iter.predi.redui[1,4], 
                    table.HN.iter.predi.redui[1,4], table.IF.iter.predi.redui[1,4],table.LI.iter.predi.redui[1,4], 
                    table.LO.iter.predi.redui[1,4], table.LR.iter.predi.redui[1,4], table.MP.iter.predi.redui[1,4],
                    table.NP.iter.predi.redui[1,4], table.PA.iter.predi.redui[1,4],table.PC.iter.predi.redui[1,4],
                    table.PI.iter.predi.redui[1,4],table.PL.iter.predi.redui[1,4],table.RA.iter.predi.redui[1,4])


Err.moy.iter.redui <- mean(Err.iter.redui)

Err.moy.glm
Err.moy.opti.glm
Err.moy.test.glm
Err.moy.redui.glm
Err.moy.redui.test
Err.moy.iter
Err.moy.iter.test
Err.moy.iter.redui
#manque iter redui test 


#opti fonction à partir list region du modèle nous sort toute les prediction sur base

liste.modl.moda.redui <- list(step.AL.Reg.iter.Select.drop,step.AQ.Reg.iter.Select.drop
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

regi
tableau.logistic.region
step.AL.Reg.iter.Select.drop

prediction.f.base <- function(A,B,tx){
  m<-lapply(A,predict)
  r<-list()
  f <- c()
  for(i in 1:ncol(B)){
    e<-optim.tx(unlist(m[i]),B[,i],tx)
    r<-list(r,e)
    f <- c(f,e[1,4])
  }
  
  v <- rbind(r,mean(f))
  return(v)
}

pred.moda.redui.base <- prediction.f.base(liste.modl.moda.redui,tableau.logistic.region,taux)
##### le predict ne marche pas 



#on effectue le prediction sur modele redui avec moda 
  #sur base test
AL.predi.iter.redui.test<-predict(step.AL.Reg.iter.Select.drop,newdata=as.data.frame(data.lm.regiontest.iter),type="response") 
AQ.predi.iter.redui.test<-predict(step.AQ.Reg.iter.Select.drop,newdata=as.data.frame(data.lm.regiontest.iter),type="response") 
AU.predi.iter.redui.test<-predict(step.AU.Reg.iter.Select.drop,newdata=as.data.frame(data.lm.regiontest.iter),type="response") 
BN.predi.iter.redui.test<-predict(step.BN.Reg.iter.Select.drop,newdata=as.data.frame(data.lm.regiontest.iter),type="response") 
BO.predi.iter.redui.test<-predict(step.BO.Reg.iter.Select.drop,newdata=as.data.frame(data.lm.regiontest.iter),type="response") 
BR.predi.iter.redui.test<-predict(step.BR.Reg.iter.Select.drop,newdata=as.data.frame(data.lm.regiontest.iter),type="response") 
CA.predi.iter.redui.test<-predict(step.CA.Reg.iter.Select.drop,newdata=as.data.frame(data.lm.regiontest.iter),type="response") 
CE.predi.iter.redui.test<-predict(step.CE.Reg.iter.Select.drop,newdata=as.data.frame(data.lm.regiontest.iter),type="response") 
FC.predi.iter.redui.test<-predict(step.FC.Reg.iter.Select.drop,newdata=as.data.frame(data.lm.regiontest.iter),type="response") 
HN.predi.iter.redui.test<-predict(step.HN.Reg.iter.Select.drop,newdata=as.data.frame(data.lm.regiontest.iter),type="response") 
IF.predi.iter.redui.test<-predict(step.IF.Reg.iter.Select.drop,newdata=as.data.frame(data.lm.regiontest.iter),type="response") 
LI.predi.iter.redui.test<-predict(step.LI.Reg.iter.Select.drop,newdata=as.data.frame(data.lm.regiontest.iter),type="response") 
LO.predi.iter.redui.test<-predict(step.LO.Reg.iter.Select.drop,newdata=as.data.frame(data.lm.regiontest.iter),type="response") 
LR.predi.iter.redui.test<-predict(step.LR.Reg.iter.Select.drop,newdata=as.data.frame(data.lm.regiontest.iter),type="response") 
MP.predi.iter.redui.test<-predict(step.MP.Reg.iter.Select.drop,newdata=as.data.frame(data.lm.regiontest.iter),type="response") 
NP.predi.iter.redui.test<-predict(step.NP.Reg.iter.Select.drop,newdata=as.data.frame(data.lm.regiontest.iter),type="response") 
PA.predi.iter.redui.test<-predict(step.PA.Reg.iter.Select.drop,newdata=as.data.frame(data.lm.regiontest.iter),type="response") 
PC.predi.iter.redui.test<-predict(step.PC.Reg.iter.Select.drop,newdata=as.data.frame(data.lm.regiontest.iter),type="response") 
PI.predi.iter.redui.test<-predict(step.PI.Reg.iter.Select.drop,newdata=as.data.frame(data.lm.regiontest.iter),type="response") 
PL.predi.iter.redui.test<-predict(step.PL.Reg.iter.Select.drop,newdata=as.data.frame(data.lm.regiontest.iter),type="response") 
RA.predi.iter.redui.test<-predict(step.RA.Reg.iter.Select.drop,newdata=as.data.frame(data.lm.regiontest.iter),type="response") 


table.AL.iter.predi.redui.test<- table(AL.predi.iter.redui.test>0.5,AL.logi.test)
table.AQ.iter.predi.redui.test<- table(AQ.predi.iter.redui.test>0.5,AQ.logi.test)
table.AU.iter.predi.redui.test<- table(AU.predi.iter.redui.test>0.5,AU.logi.test)
table.BN.iter.predi.redui.test<- table(BN.predi.iter.redui.test>0.5,BN.logi.test)
table.BO.iter.predi.redui.test<- table(BO.predi.iter.redui.test>0.5,BO.logi.test)
table.BR.iter.predi.redui.test<- table(BR.predi.iter.redui.test>0.5,BR.logi.test)
table.CA.iter.predi.redui.test<- table(CA.predi.iter.redui.test>0.5,CA.logi.test)
table.CE.iter.predi.redui.test<- table(CE.predi.iter.redui.test>0.5,CE.logi.test)
table.FC.iter.predi.redui.test<- table(FC.predi.iter.redui.test>0.5,FC.logi.test)
table.HN.iter.predi.redui.test<- table(HN.predi.iter.redui.test>0.5,HN.logi.test)
table.IF.iter.predi.redui.test<- table(IF.predi.iter.redui.test>0.5,IF.logi.test)
table.LI.iter.predi.redui.test<- table(LI.predi.iter.redui.test>0.5,LI.logi.test)
table.LO.iter.predi.redui.test<- table(LO.predi.iter.redui.test>0.5,LO.logi.test)
table.LR.iter.predi.redui.test<- table(LR.predi.iter.redui.test>0.5,LR.logi.test)
table.MP.iter.predi.redui.test<- table(MP.predi.iter.redui.test>0.5,MP.logi.test)
table.NP.iter.predi.redui.test<- table(NP.predi.iter.redui.test>0.5,NP.logi.test)
table.PA.iter.predi.redui.test<- table(PA.predi.iter.redui.test>0.5,PA.logi.test)
table.PC.iter.predi.redui.test<- table(PC.predi.iter.redui.test>0.5,PC.logi.test)
table.PI.iter.predi.redui.test<- table(PI.predi.iter.redui.test>0.5,PI.logi.test)
table.PL.iter.predi.redui.test<- table(PL.predi.iter.redui.test>0.5,PL.logi.test)
table.RA.iter.predi.redui.test<- table(RA.predi.iter.redui.test>0.5,RA.logi.test)

table.AL.iter.predi.redui.test #predi modele reduit avec moda data test

table.RA.iter.predi.redui.test<- optim.test.tx(RA.predi.iter.redui.test,RA.logi.test,table.RA.iter.predi.redui)
table.RA.iter.predi.redui.test
##errreur modele avec moda reduit sur test 

Err.AL.iter.redui.test<-(51+16)/3620
Err.AQ.iter.redui.test<-(254)/3620
Err.AU.iter.redui.test<-(102)/3620
Err.BN.iter.redui.test<-(180)/3620
Err.BO.iter.redui.test<-(212)/3620
Err.BR.iter.redui.test<-(132)/3620
Err.CA.iter.redui.test<-(168)/3620
Err.CE.iter.redui.test<-(178)/3620
Err.FC.iter.redui.test<-(169)/3620
Err.HN.iter.redui.test<-(143+1)/3620
Err.IF.iter.redui.test<-(132)/3620
Err.LI.iter.redui.test<-(75)/3620
Err.LO.iter.redui.test<-(217)/3620
Err.LR.iter.redui.test<-(162)/3620
Err.MP.iter.redui.test<-(300+2)/3620
Err.NP.iter.redui.test<-(167+2)/3620
Err.PA.iter.redui.test<-(88)/3620
Err.PC.iter.redui.test<-(170)/3620
Err.PI.iter.redui.test<-(241)/3620
Err.PL.iter.redui.test<-(140)/3620
Err.RA.iter.redui.test<-(288)/3620


Err.regi.iter.redui.test <-c(Err.AL.iter.redui.test,Err.AQ.iter.redui.test,Err.AU.iter.redui.test,Err.BN.iter.redui.test,Err.BO.iter.redui.test,Err.BR.iter.redui.test,Err.CA.iter.redui.test,
                             Err.CE.iter.redui.test,Err.FC.iter.redui.test,Err.HN.iter.redui.test,Err.IF.iter.redui.test,Err.LI.iter.redui.test,Err.LO.iter.redui.test,Err.LR.iter.redui.test,
                             Err.MP.iter.redui.test,Err.NP.iter.redui.test,Err.PA.iter.redui.test,Err.PC.iter.redui.test,Err.PI.iter.redui.test,Err.PL.iter.redui.test,Err.RA.iter.redui.test)
Err.moy.iter.redui.test<-mean(Err.regi.iter.redui.test)






#Compare modèles sans modalité plein vs réduit, pour chaque region 
#AL 
#sans moda
table.AL.predi.glm # predi modele plein sans moda sur data base 
table.AL.predi.test.glm # predi modele plein sans moda sur data test 
table.AL.predi.redui #predi modele reduit sans moda data base 
table.AL.predi.redui.test #predi modele reduit sans moda data test

#avec moda
table.AL.iter.predi.glm #predi modele plein avec moda data base
table.AL.iter.predi.test.glm #predi modele plein avec moda data test
table.AL.iter.predi.redui #predi modele reduit avec moda data base
table.AL.iter.predi.redui.test #predi modele reduit avec moda data test

                

#on cree le modèle final
Reg.regionAL # 1 plein sans moda
step.AL.Reg.Select.drop # 2 redui sans moda
Reg.iter.regionAL # 3 plein avec moda
step.AL.Reg.iter.Select.drop # 4 redui avec moda



###TEST

#créé modèle pour preduire la region
#1 Reg.regionAL
#2 AL.Reg.Select.drop
#3 Reg.iter.regionAL
#4 step.AL.Reg.iter.Select.drop


##statistiques descriptives 

C<-180/pi
plot(C*All_COMMUNES$longitude,C*All_COMMUNES$latitude,pch='.')


regi<-c('AL','AQ','AU','BN','BO','BR','CA','CE',
        'FC','HN','IF','LI','LO','LR','MP','NP','PA',
        'PC','PI','PL','RA')

data.lm.region

#frequence ville/Region
barplot(data.lm.region[,1:21],col="darkred",space=1)

#frequence caractère par region
barplot(data.lm.region[data.lm.region[,"AL"]==1,22:87],col="darkred",space=1,main="AL")
barplot(data.lm.region[data.lm.region[,"AQ"]==1,22:87],col="darkred",space=1,main="AQ")
barplot(data.lm.region[data.lm.region[,"AU"]==1,22:87],col="darkred",space=1,main="AU")
barplot(data.lm.region[data.lm.region[,"BN"]==1,22:87],col="darkred",space=1,main="BN")
barplot(data.lm.region[data.lm.region[,"BO"]==1,22:87],col="darkred",space=1,main="BO")
barplot(data.lm.region[data.lm.region[,"BR"]==1,22:87],col="darkred",space=1,main="BR")
barplot(data.lm.region[data.lm.region[,"CA"]==1,22:87],col="darkred",space=1,main="CA")
barplot(data.lm.region[data.lm.region[,"CE"]==1,22:87],col="darkred",space=1,main="CE")
barplot(data.lm.region[data.lm.region[,"FC"]==1,22:87],col="darkred",space=1,main="FC")
barplot(data.lm.region[data.lm.region[,"HN"]==1,22:87],col="darkred",space=1,main="HN")
barplot(data.lm.region[data.lm.region[,"IF"]==1,22:87],col="darkred",space=1,main="IF")
barplot(data.lm.region[data.lm.region[,"LI"]==1,22:87],col="darkred",space=1,main="LI")
barplot(data.lm.region[data.lm.region[,"LO"]==1,22:87],col="darkred",space=1,main="LO")
barplot(data.lm.region[data.lm.region[,"LR"]==1,22:87],col="darkred",space=1,main="LR")
barplot(data.lm.region[data.lm.region[,"MP"]==1,22:87],col="darkred",space=1,main="MP")
barplot(data.lm.region[data.lm.region[,"NP"]==1,22:87],col="darkred",space=1,main="NP")
barplot(data.lm.region[data.lm.region[,"PA"]==1,22:87],col="darkred",space=1,main="PA")
barplot(data.lm.region[data.lm.region[,"PC"]==1,22:87],col="darkred",space=1,main="PC")
barplot(data.lm.region[data.lm.region[,"PI"]==1,22:87],col="darkred",space=1,main="PI")
barplot(data.lm.region[data.lm.region[,"PL"]==1,22:87],col="darkred",space=1,main="PL")
barplot(data.lm.region[data.lm.region[,"RA"]==1,22:87],col="darkred",space=1,main="RA")


table(data.lm.region[,"BQ"])
table(data.lm.region.test[,"BQ"])

COMMUNES


####optimisation du code


liste.modl.moda.redui <- list(step.AL.Reg.iter.Select.drop,step.AQ.Reg.iter.Select.drop
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
names(liste.modl.moda.redui) <- regi

list.modl.glm <- list(Reg.regionAL,Reg.regionAQ,Reg.regionAU,Reg.regionBN,Reg.regionBO,Reg.regionBR,Reg.regionCA,Reg.regionCE,Reg.regionFC,Reg.regionHN,Reg.regionIF,Reg.regionLI,Reg.regionLO,Reg.regionLR,Reg.regionMP,Reg.regionNP,Reg.regionPA,Reg.regionPC,Reg.regionPI,Reg.regionPL,Reg.regionRA)
names(list.modl.glm) <- regi

list.modl.glm.redui <- list(step.AL.Reg.Select.drop,step.AQ.Reg.Select.drop,step.AU.Reg.Select.drop,step.BN.Reg.Select.drop,step.BO.Reg.Select.drop,step.BR.Reg.Select.drop,step.CA.Reg.Select.drop,step.CE.Reg.Select.drop,step.FC.Reg.Select.drop,step.HN.Reg.Select.drop,step.IF.Reg.Select.drop,step.LI.Reg.Select.drop,step.LO.Reg.Select.drop,step.LR.Reg.Select.drop,step.MP.Reg.Select.drop,step.NP.Reg.Select.drop,step.PA.Reg.Select.drop,step.PC.Reg.Select.drop,step.PI.Reg.Select.drop,step.PL.Reg.Select.drop,step.RA.Reg.Select.drop)
names(list.modl.glm.redui) <- regi

list.modl.moda <- list(Reg.iter.regionAL,Reg.iter.regionAQ,Reg.iter.regionAU,Reg.iter.regionBN,Reg.iter.regionBO,Reg.iter.regionBR,Reg.iter.regionCA,Reg.iter.regionCE,Reg.iter.regionFC,Reg.iter.regionHN,Reg.iter.regionIF,Reg.iter.regionLI,Reg.iter.regionLO,Reg.iter.regionLR,Reg.iter.regionMP,Reg.iter.regionNP,Reg.iter.regionPA,Reg.iter.regionPC,Reg.iter.regionPI,Reg.iter.regionPL,Reg.iter.regionRA)
names(list.modl.moda ) <- regi

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

fct.predi.pregion <- function(A,B,tx,C){ # A=list de modèle de meme type, B=tableau logi
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

predi.glm <- fct.predi.pregion(list.modl.glm,tableau.logistic.region,taux,data.lm.region)
predi.glm.redui <- fct.predi.pregion(list.modl.glm.redui,tableau.logistic.region,taux,data.lm.region)
predi.moda <- fct.predi.pregion(list.modl.moda,tableau.logistic.region,taux,data.lm.regiontest.iter)#
predi.moda.redui <- fct.predi.pregion(liste.modl.moda.redui,tableau.logistic.region,taux,data.lm.regiontest.iter)

predi.glm.test <- fct.predi.pregion(list.modl.glm,tableau.logistic.testregion,predi.glm,data.lm.region.test)
predi.glm.redui.test <- fct.predi.pregion(list.modl.glm.redui,tableau.logistic.testregion,predi.glm.redui,data.lm.region.test)
predi.moda.test <- fct.predi.pregion(list.modl.moda,tableau.logistic.testregion,predi.moda,data.lm.regiontest.iter)
predi.moda.redui.test <- fct.predi.pregion(list.modl.moda.redui,tableau.logistic.testregion,predi.moda.redui,data.lm.regiontest.iter)

#MODEL OPTI ERREUR
predi.model.opti <- predi.model.opti.err(model.fin.region,'base')
err.moy.predi.model.opti <- mean(as.numeric(as.vector(predi.model.opti[,4]))>0)#PB
err.moy.predi.model.opti

predi.model.opti.test <- predi.model.opti.err(model.fin.region,'test')
err.moy.predi.model.opti.test <-  mean(as.numeric(as.vector(predi.model.opti.test[,4]))>0)
err.moy.predi.model.opti.test


#Modèle max predict


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
mean(model.fin.region[2,])



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


modl.max <- function(A,B){
  
}


##graph evolution de l'erreur moy / region, sur base et test 
predi.glm
regi
AL.errb <- c(as.numeric(as.vector(predi.glm[1,4])),as.numeric(as.vector(predi.glm.redui[1,4])),as.numeric(as.vector(predi.moda[1,4])),as.numeric(as.vector(predi.moda.redui[1,4])))
AQ.errb <- c(as.numeric(as.vector(predi.glm[3,4])),as.numeric(as.vector(predi.glm.redui[3,4])),as.numeric(as.vector(predi.moda[3,4])),as.numeric(as.vector(predi.moda.redui[3,4])))
AU.errb <- c(as.numeric(as.vector(predi.glm[5,4])),as.numeric(as.vector(predi.glm.redui[5,4])),as.numeric(as.vector(predi.moda[5,4])),as.numeric(as.vector(predi.moda.redui[5,4])))
BN.errb <- c(as.numeric(as.vector(predi.glm[7,4])),as.numeric(as.vector(predi.glm.redui[7,4])),as.numeric(as.vector(predi.moda[7,4])),as.numeric(as.vector(predi.moda.redui[7,4])))
BO.errb <- c(as.numeric(as.vector(predi.glm[9,4])),as.numeric(as.vector(predi.glm.redui[9,4])),as.numeric(as.vector(predi.moda[9,4])),as.numeric(as.vector(predi.moda.redui[9,4])))
BR.errb <- c(as.numeric(as.vector(predi.glm[11,4])),as.numeric(as.vector(predi.glm.redui[11,4])),as.numeric(as.vector(predi.moda[11,4])),as.numeric(as.vector(predi.moda.redui[11,4])))
CA.errb <- c(as.numeric(as.vector(predi.glm[13,4])),as.numeric(as.vector(predi.glm.redui[13,4])),as.numeric(as.vector(predi.moda[13,4])),as.numeric(as.vector(predi.moda.redui[13,4])))
CE.errb <- c(as.numeric(as.vector(predi.glm[15,4])),as.numeric(as.vector(predi.glm.redui[15,4])),as.numeric(as.vector(predi.moda[15,4])),as.numeric(as.vector(predi.moda.redui[15,4])))
FC.errb <- c(as.numeric(as.vector(predi.glm[17,4])),as.numeric(as.vector(predi.glm.redui[17,4])),as.numeric(as.vector(predi.moda[17,4])),as.numeric(as.vector(predi.moda.redui[17,4])))
HN.errb <- c(as.numeric(as.vector(predi.glm[19,4])),as.numeric(as.vector(predi.glm.redui[19,4])),as.numeric(as.vector(predi.moda[19,4])),as.numeric(as.vector(predi.moda.redui[19,4])))
IF.errb <- c(as.numeric(as.vector(predi.glm[21,4])),as.numeric(as.vector(predi.glm.redui[21,4])),as.numeric(as.vector(predi.moda[21,4])),as.numeric(as.vector(predi.moda.redui[21,4])))
LI.errb <- c(as.numeric(as.vector(predi.glm[23,4])),as.numeric(as.vector(predi.glm.redui[23,4])),as.numeric(as.vector(predi.moda[23,4])),as.numeric(as.vector(predi.moda.redui[23,4])))
LO.errb <- c(as.numeric(as.vector(predi.glm[25,4])),as.numeric(as.vector(predi.glm.redui[25,4])),as.numeric(as.vector(predi.moda[25,4])),as.numeric(as.vector(predi.moda.redui[25,4])))
LR.errb <- c(as.numeric(as.vector(predi.glm[27,4])),as.numeric(as.vector(predi.glm.redui[27,4])),as.numeric(as.vector(predi.moda[27,4])),as.numeric(as.vector(predi.moda.redui[27,4])))
MP.errb <- c(as.numeric(as.vector(predi.glm[29,4])),as.numeric(as.vector(predi.glm.redui[29,4])),as.numeric(as.vector(predi.moda[29,4])),as.numeric(as.vector(predi.moda.redui[29,4])))
NP.errb <- c(as.numeric(as.vector(predi.glm[31,4])),as.numeric(as.vector(predi.glm.redui[31,4])),as.numeric(as.vector(predi.moda[31,4])),as.numeric(as.vector(predi.moda.redui[31,4])))
PA.errb <- c(as.numeric(as.vector(predi.glm[33,4])),as.numeric(as.vector(predi.glm.redui[33,4])),as.numeric(as.vector(predi.moda[33,4])),as.numeric(as.vector(predi.moda.redui[33,4])))
PC.errb <- c(as.numeric(as.vector(predi.glm[35,4])),as.numeric(as.vector(predi.glm.redui[35,4])),as.numeric(as.vector(predi.moda[35,4])),as.numeric(as.vector(predi.moda.redui[35,4])))
PI.errb <- c(as.numeric(as.vector(predi.glm[37,4])),as.numeric(as.vector(predi.glm.redui[37,4])),as.numeric(as.vector(predi.moda[37,4])),as.numeric(as.vector(predi.moda.redui[37,4])))
PL.errb <- c(as.numeric(as.vector(predi.glm[39,4])),as.numeric(as.vector(predi.glm.redui[39,4])),as.numeric(as.vector(predi.moda[39,4])),as.numeric(as.vector(predi.moda.redui[39,4])))
RA.errb <- c(as.numeric(as.vector(predi.glm[41,4])),as.numeric(as.vector(predi.glm.redui[41,4])),as.numeric(as.vector(predi.moda[41,4])),as.numeric(as.vector(predi.moda.redui[41,4])))

AL.errt <- c(as.numeric(as.vector(predi.glm.test[1,4])),as.numeric(as.vector(predi.glm.redui.test[1,4])),as.numeric(as.vector(predi.moda.test[1,4])),as.numeric(as.vector(predi.moda.redui.test[1,4])))
AQ.errt <- c(as.numeric(as.vector(predi.glm.test[3,4])),as.numeric(as.vector(predi.glm.redui.test[3,4])),as.numeric(as.vector(predi.moda.test[3,4])),as.numeric(as.vector(predi.moda.redui.test[3,4])))
AU.errt <- c(as.numeric(as.vector(predi.glm.test[5,4])),as.numeric(as.vector(predi.glm.redui.test[5,4])),as.numeric(as.vector(predi.moda.test[5,4])),as.numeric(as.vector(predi.moda.redui.test[5,4])))
BN.errt <- c(as.numeric(as.vector(predi.glm.test[7,4])),as.numeric(as.vector(predi.glm.redui.test[7,4])),as.numeric(as.vector(predi.moda.test[7,4])),as.numeric(as.vector(predi.moda.redui.test[7,4])))
BO.errt <- c(as.numeric(as.vector(predi.glm.test[9,4])),as.numeric(as.vector(predi.glm.redui.test[9,4])),as.numeric(as.vector(predi.moda.test[9,4])),as.numeric(as.vector(predi.moda.redui.test[9,4])))
BR.errt <- c(as.numeric(as.vector(predi.glm.test[11,4])),as.numeric(as.vector(predi.glm.redui.test[11,4])),as.numeric(as.vector(predi.moda.test[11,4])),as.numeric(as.vector(predi.moda.redui.test[11,4])))
CA.errt <- c(as.numeric(as.vector(predi.glm.test[13,4])),as.numeric(as.vector(predi.glm.redui.test[13,4])),as.numeric(as.vector(predi.moda.test[13,4])),as.numeric(as.vector(predi.moda.redui.test[13,4])))
CE.errt <- c(as.numeric(as.vector(predi.glm.test[15,4])),as.numeric(as.vector(predi.glm.redui.test[15,4])),as.numeric(as.vector(predi.moda.test[15,4])),as.numeric(as.vector(predi.moda.redui.test[15,4])))
FC.errt <- c(as.numeric(as.vector(predi.glm.test[17,4])),as.numeric(as.vector(predi.glm.redui.test[17,4])),as.numeric(as.vector(predi.moda.test[17,4])),as.numeric(as.vector(predi.moda.redui.test[17,4])))
HN.errt <- c(as.numeric(as.vector(predi.glm.test[19,4])),as.numeric(as.vector(predi.glm.redui.test[19,4])),as.numeric(as.vector(predi.moda.test[19,4])),as.numeric(as.vector(predi.moda.redui.test[19,4])))
IF.errt <- c(as.numeric(as.vector(predi.glm.test[21,4])),as.numeric(as.vector(predi.glm.redui.test[21,4])),as.numeric(as.vector(predi.moda.test[21,4])),as.numeric(as.vector(predi.moda.redui.test[21,4])))
LI.errt <- c(as.numeric(as.vector(predi.glm.test[23,4])),as.numeric(as.vector(predi.glm.redui.test[23,4])),as.numeric(as.vector(predi.moda.test[23,4])),as.numeric(as.vector(predi.moda.redui.test[23,4])))
LO.errt <- c(as.numeric(as.vector(predi.glm.test[25,4])),as.numeric(as.vector(predi.glm.redui.test[25,4])),as.numeric(as.vector(predi.moda.test[25,4])),as.numeric(as.vector(predi.moda.redui.test[25,4])))
LR.errt <- c(as.numeric(as.vector(predi.glm.test[27,4])),as.numeric(as.vector(predi.glm.redui.test[27,4])),as.numeric(as.vector(predi.moda.test[27,4])),as.numeric(as.vector(predi.moda.redui.test[27,4])))
MP.errt <- c(as.numeric(as.vector(predi.glm.test[29,4])),as.numeric(as.vector(predi.glm.redui.test[29,4])),as.numeric(as.vector(predi.moda.test[29,4])),as.numeric(as.vector(predi.moda.redui.test[29,4])))
NP.errt <- c(as.numeric(as.vector(predi.glm.test[31,4])),as.numeric(as.vector(predi.glm.redui.test[31,4])),as.numeric(as.vector(predi.moda.test[31,4])),as.numeric(as.vector(predi.moda.redui.test[31,4])))
PA.errt <- c(as.numeric(as.vector(predi.glm.test[33,4])),as.numeric(as.vector(predi.glm.redui.test[33,4])),as.numeric(as.vector(predi.moda.test[33,4])),as.numeric(as.vector(predi.moda.redui.test[33,4])))
PC.errt <- c(as.numeric(as.vector(predi.glm.test[35,4])),as.numeric(as.vector(predi.glm.redui.test[35,4])),as.numeric(as.vector(predi.moda.test[35,4])),as.numeric(as.vector(predi.moda.redui.test[35,4])))
PI.errt <- c(as.numeric(as.vector(predi.glm.test[37,4])),as.numeric(as.vector(predi.glm.redui.test[37,4])),as.numeric(as.vector(predi.moda.test[37,4])),as.numeric(as.vector(predi.moda.redui.test[37,4])))
PL.errt <- c(as.numeric(as.vector(predi.glm.test[39,4])),as.numeric(as.vector(predi.glm.redui.test[39,4])),as.numeric(as.vector(predi.moda.test[39,4])),as.numeric(as.vector(predi.moda.redui.test[39,4])))
RA.errt <- c(as.numeric(as.vector(predi.glm.test[41,4])),as.numeric(as.vector(predi.glm.redui.test[41,4])),as.numeric(as.vector(predi.moda.test[41,4])),as.numeric(as.vector(predi.moda.redui.test[41,4])))


vect.errb <- rbind(AL.errb, AQ.errb,AU.errb,BN.errb,BO.errb ,BR.errb ,CA.errb ,CE.errb ,FC.errb ,HN.errb ,IF.errb ,LI.errb ,LO.errb ,LR.errb ,MP.errb ,NP.errb ,PA.errb ,PC.errb ,PI.errb ,PL.errb,RA.errb )
vect.errt <- rbind(AL.errt, AQ.errt,AU.errt,BN.errb,BO.errt ,BR.errt ,CA.errt ,CE.errt ,FC.errt ,HN.errt ,IF.errt ,LI.errt ,LO.errt ,LR.errt ,MP.errt ,NP.errt ,PA.errt ,PC.errt ,PI.errt ,PL.errt,RA.errt )

Err.b <- c(as.numeric(as.vector(predi.glm[43,1])),as.numeric(as.vector(predi.glm.redui[43,1])),
       as.numeric(as.vector(predi.moda[43,1])),as.numeric(as.vector(predi.moda.redui[43,1])),
       mean(model.fin.region[2,]))
Err.t <- c(as.numeric(as.vector(predi.glm.test[43,1])),as.numeric(as.vector(predi.glm.redui.test[43,1])),
       as.numeric(as.vector(predi.moda.test[43,1])),as.numeric(as.vector(predi.moda.redui.test[43,1])),
       mean(as.numeric(as.vector(predi.model.opti.test[vect.ind,4]))))
0.03911925
Err.b
ErrBT <- cbind(Err.b,Err.t)
matplot(ErrBT,type='b',main='evolution erreur')
vect.ind <- which(as.numeric(as.vector(predi.model.opti.test[,4]))>0)

#evolution pourcentage bien placé

as.numeric(as.vector(predi.glm[2,2]))/sum(AL.logi)
as.numeric(as.vector(predi.glm.redui[2,2]))/sum(AL.logi)
as.numeric(as.vector(predi.moda[2,2]))/sum(AL.logi)
as.numeric(as.vector(predi.moda.redui[2,2]))/sum(AL.logi)



par(mfrow=c(3,2)) 
for(i in 1:length(regi)){
  c <- c(as.numeric(as.vector(predi.glm[(2*i),2])),
         as.numeric(as.vector(predi.glm.redui[(2*i),2])),
         as.numeric(as.vector(predi.moda[(2*i),2])),
         as.numeric(as.vector(predi.moda.redui[(2*i),2])))/sum(tableau.logistic.region[,i])
  v <- c(as.numeric(as.vector(predi.glm.test[(2*i),2])),
         as.numeric(as.vector(predi.glm.redui.test[(2*i),2])),
         as.numeric(as.vector(predi.moda.test[(2*i),2])),
         as.numeric(as.vector(predi.moda.redui.test[(2*i),2])))/sum(tableau.logistic.testregion[,i])
  a <- regi[i]
  matplot(cbind(c,v),main=a)
}
 
#modl multi region

model.fin.region
regi
AL.modl <- list.modl.moda[1]
AQ.modl <- list.modl.moda[2]
AU.modl <- list.modl.glm[3]

md.max.predi <- mdl.max(model.fin.region,'b')
md.max.predi.test <- mdl.max(model.fin.region,'t')

md.max.predi

length(as.integer(as.vector(md.max.predi[,2])))

pp <- c()
for (i in 1:length(regi)) {
  pp <- c(pp,as.integer(as.vector(md.max.predi[2*i,2])))
}

sum(pp)/32589
md.max.predi
pp
sum(tableau.logistic.region[,21])

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


#ajouter des covariable

##test
apr <- list("bonjour","salutjour","comment")
rpa <- list("revoir","dimanche",'mental')
aprpa <- cbind(apr,rpa)
colnames(aprpa) <- c("A","B")
aprpa


apr_Df <- as_tibble(aprpa)
un <- unnest_tokens(apr_Df,ngram,A,token = "character_shingles", n = 3,drop=FALSE,collapse=FALSE)
un

count(un,ngram,sort=TRUE)
#test
#prepare la base pour la fonctio unnest_tokens
tib.COMMUNES <- as_tibble(COMMUNES)
tib.COMMUNES$nom <- as.character(tib.COMMUNES$nom)
tib.test <- as_tibble(test_COMMUNES)
tib.test$nom <- as.character(tib.test$nom)

#on extrait les chaines de 3-4 caractères sur base d'apprentissage et base test 
Trichr.base <- unnest_tokens(tib.COMMUNES,bi,nom,token = "character_shingles", n = 3,drop=FALSE,collapse=FALSE)
freq.tri <- count(Trichr.base,bi,sort=TRUE)
freq.tri <- freq.tri[order(freq.tri$n,decreasing = TRUE),]
freq.tri

Quatr.chr.base <- unnest_tokens(tib.COMMUNES,bi,nom,token = "character_shingles", n = 4,drop=FALSE,collapse=FALSE)
freq.quatr <- count(Quatr.chr.base,bi,sort=TRUE)
freq.quatr <- freq.quatr[order(freq.quatr$n,decreasing = TRUE),]
freq.quatr

save(freq.quatr,freq.tri,file="Desktop/S2/Trifou/chaine.RData")

#Test
ngram('bonjour',n=2)
unnest_token
#

# on crée des nouveaux tableaux logistiques avec les chaines de 3-4 caractères et le nb de caractères total
# on selectionne les 20 chaines de 3-4 caractères les plus frèquent de la base d'apprentissage
freq.bi[1:10,]
freq20.3.4 <- rbind(freq.tri[1:20,1],freq.quatr[1:20,1])
unlist(freq20.3.4)

logistic.tri.quatr <- logistic.f.ngram(as.character(COMMUNES$nom),unlist(freq20.3.4))
logistic.tri.quatr.test <- logistic.f.ngram(as.character(test_COMMUNES$nom),unlist(freq20.3.4))
logistic.tri.quatr 


data.lm.region.tri.quatr <- cbind(data.lm.region.iter,logistic.tri.quatr)
data.lm.region.tri.quatr
data.lm.region.tri.quatr.test <- cbind(data.lm.regiontest.iter,logistic.tri.quatr.test)
data.lm.region.tri.quatr.test
#
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

## on ajoute @ la fin des noms mais unnest_token ne reconnait pas les @
for (i in 1:nrow(tib.COMMUNES)){
  tib.COMMUNES[i,2] <- str_c(tib.COMMUNES[i,2],c('@'),sep="")
}

#test
logistic.f.ngram(c('bonjour','jetz'),c('ou','j','tz'))
#



Reg.regionAL.gram <- glm(formula(paste('AL~',paste(c(abc,unlist(freq20.3.4),'nbChar'),collapse='+'))),
                         as.data.frame(data.lm.region.tri.quatr),family=binomial(logit))


regi.ti <- str_c(regi,c('~'),sep="")
regi.ti

list.glm.ngram <- list()
for (i in 1:length(regi)) {
  modl <- glm(formula(paste(regi.ti[i],paste(c(abc,unlist(freq20.3.4),'nbChar'),collapse='+'))),
              as.data.frame(data.lm.region.tri.quatr),family=binomial(logit))
  list.glm.ngram[[regi[i]]] <-modl
  }


class(list.glm.ngram) 

length(list.glm.ngram)
lapply(list.glm.ngram, predict,type='response')
predi.glm.ngram <- fct.predi.pregion(list.glm.ngram,tableau.logistic.region,taux,data.lm.region.tri.quatr)
predi.glm
predi.glm.ngram 
#
err.moy.predi.model.opti
predi.model.opti.test

err.moy.predi.model.opti.test
predi.glm.ngram.test <- fct.predi.pregion(list.glm.ngram,tableau.logistic.testregion,predi.glm.ngram,data.lm.region.tri.quatr.test)
as.numeric(as.vector(predi.glm.ngram.test[,4]))


ppn <- c()
for (i in 1:length(regi)) {
  ppn <- c(ppn,as.integer(as.vector(predi.glm.ngram[2*i,2])))
}
ppn
sum(ppn)/32589

ppg <- c()
for (i in 1:length(regi)) {
  ppg <- c(ppg,as.integer(as.vector(predi.glm[2*i,2])))
}
ppg
sum(ppg)/32589



predi.ngramAL <- predict(Reg.regionAL.gram,type = 'response')
predi.glm.ngramAL <- optim.tx(predi.ngramAL,AL.logi,taux)
predi.glm.ngramAL


#Ll

COMMUNES
logistic.tri.quatr
tableau.logistic.allcommunes.iter
tableau.logi.ngram <- cbind(tableau.logistic.allcommunes.iter,logistic.tri.quatr)
data.lm.ngram <- cbind(COMMUNES$longitude,COMMUNES$latitude,tableau.logi.ngram)
colnames(data.lm.ngram) <- c('Longitude','lattitude',colnames(tableau.logi.ngram))
data.lm.ngram




L.lm <- lm(formula(paste('Longitude~',paste(c(abc,unlist(freq20.3.4),'nbChar'),collapse='+'))),
             as.data.frame(data.lm.ngram))
l.lm <- lm(formula(paste('lattitude~',paste(c(abc,unlist(freq20.3.4),'nbChar'),collapse='+'))),
           as.data.frame(data.lm.ngram))

predi.L.lm <- predict(L.lm)
err.L.lm <- sum((data.lm.ngram-predi.L.lm)^2)/nrow(data.lm.ngram)

sqrt(err.L.lm)
summary(COMMUNES$longitude)
summary(L.lm)
summary(l.lm)

#foret aleatoire
install.packages('plotly')
install.packages('randomForest')
library(randomForest)
library(plotly)

data.lm.ngram

RF.L <- randomForest(Longitude~., data=as.data.frame(data.lm.ngram))
Sys.setenv('R_MAX_VSIZE'=32000000000)
RF.L

RF.l <- randomForest(lattitude~., data=as.data.frame(data.lm.ngram))

Predi.RF.L <- predict(RF.L, newdata = )

summary(RF.L)

save.image("Documents/ALL/S2/Trifou/Environment.RData")
