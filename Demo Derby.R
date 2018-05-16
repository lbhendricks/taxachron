#### DATA DEMO DERBY, ELC AND ePANDDA, MAY 15 - 18 2018 ####
## WRITTEN BY GREGORY J. SMITH ##

rm(list=ls()) #clear environment
library(paleobioDB) #to read in fossil occurrence data from pbdb
library(dplyr)

#### Canidae ####
canidae<-  pbdb_occurrences (limit="all", base_name="canidae",show=c("coords", "phylo", "ident"))
canidae <- filter(canidae, !gnl == "", !fml == "") #Remove those occurrences without genus or family ID
canidae <- as.data.frame(canidae)

genera<-canidae$gnl #What are all occurrences appearing in this Period?
genus.names<-unique(genera) #What are the specific genus names appearing in this Period?
num.genera<-length(genus.names) #How many genera are there? (will be number of rows in the results matrix)
outTable<-matrix(NA,nrow=num.genera,ncol=5)
colnames(outTable)<-c("FAD","LAD","Duration","Family", "Order")
rownames(outTable)<-genus.names

t1<-Sys.time()
for(i in 1:nrow(canidae)){
  
  this.taxon <- canidae$gnl[i] #Define the taxa
  all.occ.this.taxon <- filter(canidae, gnl == this.taxon) #Select all rows of that taxon
  FAD <- max(all.occ.this.taxon$eag, na.rm = T) #Find the max value (FAD) of that taxon...
  outTable[this.taxon,1] <- FAD #...and insert it into column 1 (FAD)
  LAD <- min(all.occ.this.taxon$lag, na.rm = T) #Find the min value (LAD) of that taxon...
  outTable[this.taxon,2] <- LAD #...and insert it into column 2
  longevity <- FAD - LAD #Calulate the species longevity...
  outTable[this.taxon,3] <- longevity #...and insert it into column 3
  Family <- all.occ.this.taxon$fml[1] #Find the family that genus belongs to (the first one in case two are assigned)
  outTable[this.taxon,4] <- as.character(Family) #...and insert the Family name into column 4
  Order <- all.occ.this.taxon$odl[1] #Find the order that genus belongs to (the first one in case two are assigned)
  outTable[this.taxon,5] <- as.character(Order) #...and insert the Order name into column 5
  outTable<-outTable
}
df <- as.data.frame(outTable)

t2<-Sys.time()
t2-t1