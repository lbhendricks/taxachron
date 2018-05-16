library(shiny)
library(dplyr)
library(ggplot2)

#reads time interval names, ages, colors
time_int<-read.csv("http://paleobiodb.org/data1.1/intervals/list.txt?scale=1&limit=all")
periods<-subset(time_int,time_int$level==3)

# Define server logic for random distribution application
taxachron_server<-(
shinyServer(function(input, output) {
     
     dataInput<-reactive({
          # specifies that we want the output to be in thousands of eyars
          occurrences<-read.csv(paste("http://earthlifeconsortium.org/api_v1/occ?taxon=",input,"&ageunits=Ka",sep=""))
          
          occurrences$mean_age<-rowMeans(cbind(occurrences$max_age,occurrences$min_age))
          
          return(occurrences)
     })
     
     
     x <- list(canidae,felidae) #through each taxonomic group into a list
     
     ####  GENERATE LONGEVITY TABLES ####
     GenerateLongevityTable <- function(x){ #where x is a matrix of pbdb occurrence data fetched via pbdb_occurrences
          
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
     
})
)

# Define UI for dataset viewer application
taxachron_ui<-(
shinyUI(fluidPage(
     
     # Application title.
     titlePanel("Taxachron: Visualizing Taxa Longevity"),
     
     sidebarLayout(
          sidebarPanel(
               img(src ="firstApp_logo.png",height=120,width=120),
               
               h4("Visualizing the timeline of a taxa"),
               
               textInput("taxon", "Enter a family or genus name:","Felidae"),
               
               helpText("Warning: large taxonomic groups may require
                        a few seconds (or more) to load."),
               
               submitButton("Submit"),
               
               h5("If you use this information in a publication, please acknowledge the Earth Life Consortium"),
               
               h6("Comments or suggestions? Email XXX"),
               
               a("XXXX")
               
               ),
          
          mainPanel(
               tabsetPanel(type = "tabs", 
                           tabPanel("Plot", plotOutput("plot"),
                                    downloadButton("downloadData", "Download occurrences")
                           ), 
                           tabPanel("Table", h3("Oldest occurrences"),
                                    tableOutput("table1"),
                                    h3("Oldest occurrences of classified species"),
                                    tableOutput("table2"))
               )
          )
     )
))
)

shinyApp(ui=taxachron_ui,server=taxachron_server)
