# code from Demo Derby.R
# testing if it works with the ELC API instead of the pbdb package

# decide what genus/family to get info about
input<-"felidae"

# download the data using the ELC API
occurrences<-read.csv(paste("http://earthlifeconsortium.org/api_v1/occ?taxon=",input,"&ageunits=Ka&output=csv",sep=""))

# not necessary when we have only one taxa that we're showing
#x <- list(occurrences) # each taxonomic group into a list
x<-occurrences

####  function to GENERATE LONGEVITY TABLES ####
GenerateLongevityTable <- function(x){ #where x is a matrix of pbdb occurrence data fetched via pbdb_occurrences
      #x <- as.data.frame(x)  # turn x into a data frame (holdover from version where multiple families are included)
      x<-x[!is.na(c(x$min_age,x$max_age)),] # remove rows that have a NA for either min age or max age
      
      taxon_split<-strsplit(as.character(x$taxon)," ")  # split the taxon name on spaces
      genera<-NA       
      for(row in 1:length(taxon_split)) {
          genera[row]<-taxon_split[[row]][1]
          }
      x$genera<-genera  # add the genus column to the data frame
      #genera<-as.character(x$taxon) #What are all occurrences for this taxonomic group?
      
      genus.names<-unique(genera) #What are the unique genus names?
      num.genera<-length(genus.names) #How many genera are there? (will be number of rows in the results matrix)
      
      outTable<-data.frame(matrix(NA,nrow=num.genera,ncol=4))  # ncol=5 changed to 3 because we don't need family and order anymore
      colnames(outTable)<-c("Taxa","FAD","LAD","Duration")#,"Family", "Order")
      #rownames(outTable)<-genus.names
      outTable$Taxa<-genus.names
      
      for(species in 1:nrow(outTable)){
          this.taxon <- outTable[species,1] #Define the taxa
          all.occ.this.taxon <- dplyr::filter(x, x$genera == this.taxon) #Select all rows of that taxon
          FAD <- max(all.occ.this.taxon$max_age, na.rm = T) #Find the max value (FAD) of that taxon...
          outTable[species,2] <- FAD #...and insert it into column 1 (FAD)
          LAD <- min(all.occ.this.taxon$min_age, na.rm = T) #Find the min value (LAD) of that taxon...
          outTable[species,3] <- LAD #...and insert it into column 2
          longevity <- FAD - LAD #Calulate the species longevity...
          outTable[species,4] <- longevity #...and insert it into column 3
          # don't need this because it should all be the same family in the initial version of the API
          #Family <- all.occ.this.taxon$fml[1] #Find the family that genus belongs to (the first one in case two are assigned)
          #outTable[this.taxon,4] <- as.character(Family) #...and insert the Family name into column 4
          # don't need this because it should all be the same order in the initial version of the API
          #Order <- all.occ.this.taxon$odl[1] #Find the order that genus belongs to (the first one in case two are assigned)
          #outTable[this.taxon,5] <- as.character(Order) #...and insert the Order name into column 5
          outTable<-outTable
          }

df <- as.data.frame(outTable) #Save Result to list
}

felidae<-GenerateLongevityTable(x)

# dont't need these because we only have one API request  
# LongevityTables <- lapply(x, GenerateLongevityTable)
# names(LongevityTables) <- paste(input,"longevities")
# list2env(LongevityTables, envir = .GlobalEnv)