
from numpy import *

class strSimilarity(object):
    
    def __init__(self,distance,INPUT):
        self.distance=distance
        self.COMMUNES=array(INPUT)
        #print(self.COMMUNES)
        self.medians=self.median_per_region()
        
        
    ########################################
    #Greedy algorithm to find a median#####
  
    
    def compute_distances(self,INPUT):
        #INPUT: a list of strings
        
        OUTPUT=[]
        
        for i in INPUT:
            
            tmp=[]
            
            for j in INPUT:
                
                tmp.append(self.distance(i,j))
            OUTPUT.append(array(tmp))
            
        return(array(OUTPUT))
        #OUTPUT: a table of distances
        
    def set_median(self,INPUT):
        #INPUT: a list of strings
        
        tableOfDistances=self.compute_distances(INPUT)
        
        means=[mean(tableOfDistances[i,:]) for i in range(len(tableOfDistances))]
        #print(means)
        n=len(means)
        median=sort(means)[int(around(n/2))]
        #print(median)
        
        #print(INPUT[means.index(median)])
        return INPUT[means.index(median)]
        
    ####################################################
    #######Greedy Algorithm to find medians/regions#####

        
        
    def median_per_region(self):
        
        OUTPUT=[]
        regions=set(self.COMMUNES[:,9])
        
        for i in regions:
            
            print('current region ',i)
            tmp=self.COMMUNES[where(self.COMMUNES[:,9]==i)]
            tmp2=tmp[:,1]
            
            median=self.set_median(tmp2)
                
            print('The median is ',median)
            
            OUTPUT.append(median)
            print('\n','--------------')
            
        return OUTPUT
        #OUTPUT:list of medians
        
        
    def distances_data(self):
        
        DATA=self.COMMUNES[:,1]
        OUTPUT=[]
        
        for s in DATA:
            
            tmp=[]
            
            for m in self.medians:
                
                tmp.append(self.distance(s,m))
                
            OUTPUT.append(array(tmp))
            
        return array(OUTPUT)
        
    def distance_data(self,INPUT):
        
        DATA=INPUT[:,1]
        OUTPUT=[]
        
        for s in DATA:
            
            tmp=[]
            
            for m in self.medians:
                
                tmp.append(self.distance(s,m))
                
            OUTPUT.append(array(tmp))
            
        return array(OUTPUT)
        
    #############################################