# code from Demo Derby.R
# testing if it works with the ELC API instead of the pbdb package

# decide what genus/family to get info about
input<-"felidae"

# download the data using the ELC API
occurrences<-read.csv(paste("http://earthlifeconsortium.org/api_v1/occ?taxon=",input,"&ageunits=Ka&output=csv",sep=""))

# split the taxon name on the space so we only end up with the genus name
x <- list(occurrences) #through each taxonomic group into a list


####  GENERATE LONGEVITY TABLES ####
GenerateLongevityTable <- function(x){ #where x is a matrix of pbdb occurrence data fetched via pbdb_occurrences
     
     x <- as.data.frame(x)
     genera<-x$taxon #What are all genus occurrences for this taxonomic group?
     genus.names<-unique(genera) #What are the specific genus names?
     num.genera<-length(genus.names) #How many genera are there? (will be number of rows in the results matrix)
     outTable<-matrix(NA,nrow=num.genera,ncol=5)
     colnames(outTable)<-c("FAD","LAD","Duration","Family", "Order")
     rownames(outTable)<-genus.names
     
     for(i in 1:nrow(x)){
          
          this.taxon <- x$taxon[i] #Define the taxa
          all.occ.this.taxon <- filter(x, taxon == this.taxon) #Select all rows of that taxon
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
     
     df <- as.data.frame(outTable) #Save Result to list
     
}

LongevityTables <- lapply(x, GenerateLongevityTable)
names(LongevityTables) <- paste(input,"longevities")
list2env(LongevityTables, envir = .GlobalEnv)
