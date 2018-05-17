library(shiny)
library(dplyr)
library(ggplot2)

#reads time interval names, ages, colors
time_int<-read.csv("http://paleobiodb.org/data1.1/intervals/list.txt?scale=1&limit=all")
periods<-subset(time_int,time_int$level==3)


####  function to GENERATE LONGEVITY TABLES ####
GenerateLongevities <- function(x){ #where x is a matrix of pbdb occurrence data fetched via pbdb_occurrences
  
  x <- as.data.frame(x)  # turn x into a data frame (holdover from version where multiple families are included)
  x<-x[!is.na(x$min_age),] # remove rows that have a NA for either min age or max age
  x<-x[!is.na(x$max_age),] # remove rows that have a NA for either min age or max age
  
  taxon_split<-strsplit(as.character(x$taxon)," ")  # split the taxon name on spaces
  genera<-NA 
  
  for(row in 1:length(taxon_split)) {
    genera[row]<-taxon_split[[row]][1]
  }
  
  x$genera<-genera  # add the genus column to the data frame
  
  genus.names<-unique(genera) #What are the unique genus names?
  num.genera<-length(genus.names) #How many genera are there? (will be number of rows in the results matrix)
  
  outTable<-data.frame(matrix(NA,nrow=num.genera,ncol=4))  
  colnames(outTable)<-c("Taxa","FAD","LAD","Duration")
  outTable$Taxa<-genus.names
  
  for(species in 1:nrow(outTable)){
    this.taxon <- outTable[species,1] #Define the taxa
    all.occ.this.taxon <- dplyr::filter(x, x$genera == this.taxon) #Select all rows of that taxon
    FAD <- max(all.occ.this.taxon$max_age, na.rm = T) #Find the max value (FAD) of that taxon...
    outTable[species,2] <- FAD #...and insert it into column 2 (FAD)
    LAD <- min(all.occ.this.taxon$min_age, na.rm = T) #Find the min value (LAD) of that taxon...
    outTable[species,3] <- LAD #...and insert it into column 3
    longevity <- FAD - LAD #Calulate the species longevity...
    outTable[species,4] <- longevity #...and insert it into column 4
    outTable<-outTable
  }
  
  df <- as.data.frame(outTable) #Save Result to list
  
  # Plot the longevities
  g <- ggplot(df, aes(x = Longevity, y = Taxa)) +
    geom_segment(aes(x = as.numeric(as.character(LAD)), y = Taxa, xend = as.numeric(as.character(FAD)), yend = Taxa)) +
    geom_text(aes(x = as.numeric(as.character(FAD)), y = Taxa, label = Taxa), nudge_x = 1, nudge_y = 1) +
    theme(axis.text.y=element_blank(),
          axis.ticks.y=element_blank()) +
    ggtitle("Longevities")
  g
  
} #end function



# Define server logic for random distribution application
taxachron_server<-(
shinyServer(function(input, output) {
     
     dataInput<-reactive({
          # specifies that we want the output to be in thousands of eyars
          occurrences<-read.csv(paste("http://earthlifeconsortium.org/api_v1/occ?taxon=",input,"&ageunits=Ka",sep=""))
          
          occurrences$mean_age<-rowMeans(cbind(occurrences$max_age,occurrences$min_age))
          
          return(occurrences)
     })
     
     output$plot<-renderPlot({
     GenerateLongevities(occurrences)
     })
     
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
