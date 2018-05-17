#### DATA DEMO DERBY, ELC AND ePANDDA, MAY 15 - 17 2018 ####
## WRITTEN BY GREGORY J. SMITH ##

rm(list=ls()) #clear environment
library(paleobioDB) #to read in fossil occurrence data from pbdb
library(dplyr) #for data wrangling
library(RColorBrewer) #to make pretty colors
library(ggplot2) #for plotting
library(scales) #for pretty scales

#### LOAD IN OCCURRENCE DATA FROM PBDB ####
canidae <-  pbdb_occurrences (limit="all", base_name="canidae",show=c("coords", "phylo", "ident")) %>% #download pbdb canidae data and
  filter(!gnl == "", !fml == "") #Remove those occurrences without genus or family ID

x <- list(canidae) #through each taxonomic group into a list
family.names <- c("Canidae")

####  GENERATE LONGEVITY TABLES AND PLOT LONGEVITIES ####
GenerateLongevities <- function(x){ #where x is a matrix of pbdb occurrence data fetched via pbdb_occurrences
  
  x <- as.data.frame(x)
  genera<-x$gnl #What are all genus occurrences for this taxonomic group?
  genus.names<-unique(genera) #What are the specific genus names?
  num.genera<-length(genus.names) #How many genera are there? (will be number of rows in the results matrix)
  outTable<-matrix(NA,nrow=num.genera,ncol=4)
  colnames(outTable)<-c("Genus","FAD","LAD","Longevity")
  outTable[,1] <- levels(genus.names)
  
for(i in 1:nrow(x)){
  
  this.taxon <- x$gnl[i] #Define the taxa
  all.occ.this.taxon <- filter(x, gnl == this.taxon) #Select all rows of that taxon
  FAD <- max(all.occ.this.taxon$eag, na.rm = T) #Find the max value (FAD) of that taxon...
  outTable[this.taxon,2] <- as.numeric(as.character(FAD)) #...and insert it into column 2 (FAD)
  LAD <- min(all.occ.this.taxon$lag, na.rm = T) #Find the min value (LAD) of that taxon...
  outTable[this.taxon,3] <- as.numeric(as.character(LAD)) #...and insert it into column 3 
  longevity <- FAD - LAD #Calulate the species longevity...
  outTable[this.taxon,4] <- longevity #...and insert it into column 4
  outTable<-outTable
  
}

df <- as.data.frame(outTable) #Save Result to list

# Plot the longevities
g <- ggplot(df, aes(x = Longevity, y = Genus)) +
  geom_segment(aes(x = as.numeric(as.character(LAD)), y = Genus, xend = as.numeric(as.character(FAD)), yend = Genus)) +
  geom_text(aes(x = as.numeric(as.character(FAD)), y = Genus, label = Genus), nudge_x = 1, nudge_y = 1) +
  theme(axis.text.y=element_blank(),
        axis.ticks.y=element_blank()) +
  ggtitle("Longevities")
g

}

Longevities <- lapply(x, GenerateLongevities)
Longevities

#### end ####