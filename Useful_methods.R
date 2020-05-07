require(stringr)

##################
##Creation of logistic tables
#####################
#Arguments
#pattern: vecteur de chaines de caracteres a chercher dans le dataset exemple:('a','b',....'z')
#data_set: data set ou matrice de grande dimension ayant au moins une colonne nommée 'nom' ie les noms des communes
#Sortie
#logic_table: un tableau de taille (nbre de communes)*(taille du vecteur pattern) avec en colonne 1 si la ville i possede le pattern p dans son nom 0 sinon
logistic_table.initialization_basic<-function(pattern=p,data_set=COMMUNES){
  logic_table=matrix(nrow=length(data_set$nom),ncol=length(pattern))
  for(i in (1:length(pattern))){
    chr=pattern[i]
    logic_table[,i]<-grepl(chr,data_set$nom)
  }
  colnames(logic_table)<-p
  return(logic_table)
}

#pareil qu'avant mais avec en sortie le nbre d'iterations des lettres
logistic_table.initialization<-function(pattern=p,data_set=COMMUNES){
  logic_table=matrix(nrow=length(data_set$nom),ncol=length(pattern))
  for(i in (1:length(pattern))){
    chr=pattern[i]
    for(j in (1:length(data_set$nom))){
      logic_table[j,i]<-str_count(data_set$nom[j],chr)
    }
  }
  colnames(logic_table)<-p
  return(logic_table)
}

####################################
#Creation of datasets for linear models
####################################
#Arguments
#M a logistic table
#y variable of interest
#p list of patterns of interest
#Sortie
#Une matrice ou data set de format (nbre de lignes de M)*(nbre de colonnes de M+1) avec en premiere colonne la variable d'ineteret
data.lm<-function(M,y,p){
  nbOfVariables=length(p)+1
  lm.data<-matrix(nrow = length(y),ncol=nbOfVariables)
  lm.data[,1]=y
  lm.data[,2:nbOfVariables]<-M
  as.factor(lm.data[,2:nbOfVariables])
  lm.data=as.data.frame(lm.data)
  return(lm.data)
}

#######################
##Code permettant de separer base de tests/base d'apprentissage
####################
# set.seed(322)
# test_COMMUNES_id<-sample((1:n),size=3620,replace=F)
# test_COMMUNES<-ALL_COMMUNES[test_COMMUNES_id,1:6]
# COMMUNES<-ALL_COMMUNES[-test_COMMUNES_id,]