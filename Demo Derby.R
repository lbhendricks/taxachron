#### DATA DEMO DERBY, ELC AND ePANDDA, MAY 15 - 17 2018 ####
## WRITTEN BY GREGORY J. SMITH ##

rm(list=ls()) #clear environment
library(paleobioDB) #to read in fossil occurrence data from pbdb
library(dplyr) #for data wrangling
library(ggplot2) #for plotting

#### LOAD IN OCCURRENCE DATA FROM PBDB ####
canidae <-  pbdb_occurrences (limit="all", base_name="canidae",show=c("coords", "phylo", "ident")) %>% #download pbdb canidae data and
  filter(!gnl == "", !fml == "") #Remove those occurrences without genus or family ID
felidae <-  pbdb_occurrences (limit="all", base_name="felidae",show=c("coords", "phylo", "ident")) %>% #download pbdb felidae data and
  filter(!gnl == "", !fml == "") #Remove those occurrences without genus or family ID

x <- list(canidae,felidae) #through each taxonomic group into a list

####  GENERATE LONGEVITY TABLES AND PLOT LONGEVITIES ####
GenerateLongevities <- function(x){ #where x is a matrix of pbdb occurrence data fetched via pbdb_occurrences
  
  x <- as.data.frame(x)
  genera<-x$gnl #What are all genus occurrences for this taxonomic group?
  genus.names<-unique(genera) #What are the specific genus names?
  num.genera<-length(genus.names) #How many genera are there? (will be number of rows in the results matrix)
  outTable<-matrix(NA,nrow=num.genera,ncol=5)
  colnames(outTable)<-c("FAD","LAD","Duration","Family", "Order")
  rownames(outTable)<-genus.names
  
for(i in 1:nrow(x)){
  
  this.taxon <- x$gnl[i] #Define the taxa
  all.occ.this.taxon <- filter(x, gnl == this.taxon) #Select all rows of that taxon
  FAD <- max(all.occ.this.taxon$eag, na.rm = T) #Find the max value (FAD) of that taxon...
  outTable[this.taxon,1] <- as.numeric(as.character(FAD)) #...and insert it into column 1 (FAD)
  LAD <- min(all.occ.this.taxon$lag, na.rm = T) #Find the min value (LAD) of that taxon...
  outTable[this.taxon,2] <- as.numeric(as.character(LAD)) #...and insert it into column 2
  longevity <- FAD - LAD #Calulate the species longevity...
  outTable[this.taxon,3] <- longevity #...and insert it into column 3
  Family <- all.occ.this.taxon$fml[1] #Find the family that genus belongs to (the first one in case two are assigned)
  outTable[this.taxon,4] <- as.character(Family) #...and insert the Family name into column 4
  Order <- all.occ.this.taxon$odl[1] #Find the order that genus belongs to (the first one in case two are assigned)
  outTable[this.taxon,5] <- as.character(Order) #...and insert the Order name into column 5
  outTable<-outTable
  
}

df <- as.data.frame(outTable) #Save Result to list

# Plot the longevities
plot(1,1, type = 'n',
     xlim = c(0.001, max(as.numeric(as.character(df$FAD)), na.rm = TRUE)),
     ylim = c(0, nrow(df)),
     xlab = "Millions of Years BP",
     ylab = "Genera",
     yaxt = "n")

colors <- c(rgb(0.1, 0.1, 0.1, 0.1, 0.5),
            rgb(1, 0, 0, 0, 0.5))

for (i in 1:nrow(df)) {
  segments(as.numeric(as.character(df$LAD[i])), i,
           as.numeric(as.character(df$FAD[i])), i)
           # col = colors[as.numeric(factor(df$Family))[i]])
}
}

LongevityTables <- lapply(x, GenerateLongevities)
names(LongevityTables) <- c("canid longevities","felid longevities")
list2env(LongevityTables, envir = .GlobalEnv)
