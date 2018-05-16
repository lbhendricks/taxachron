library(shiny)

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
     
     
     output$plot<-renderPlot({
          dataset<-dataInput()
          hist_output<-hist(dataset$mean_age,breaks=seq(0,545,by=1),plot=F)
          hist_data<-data.frame(counts=hist_output$counts,mids=hist_output$mids)
          hist_data<-subset(hist_data,hist_data$counts>0)
          layout(matrix(c(1,2),ncol=2),widths=c(1,5))
          par(mar=c(5,4,1.25,0))
          plot(0,0,type="n",xlim=c(0,5),xaxt="n",xlab="",ylab="Age (Ma)",ylim=c(max(hist_data$mids),min(hist_data$mids)),bty="n")
          rect(0,periods$early_age,5,periods$late_age,col=paste(periods$color))
          text(rep(2.5,nrow(periods)),rowMeans(cbind(periods$early_age,periods$late_age)),periods$abbrev)
          mtext(input$taxon,side=3,adj=0,cex=1.5)
          par(mar=c(5,0,1.25,1))
          plot(hist_data$counts,hist_data$mids,type="n",xlab="Number of occurrences",xlim=c(0,max(hist_data$counts)),ylim=c(max(hist_data$mids),min(hist_data$mids)),yaxt="n",ylab="",bty="n")
          segments(rep(0,length(hist_data$mids)),hist_data$mids,hist_data$counts,hist_data$mids,lwd=3,col="steelblue3")
     })
     
     output$downloadData <- downloadHandler(
          
          filename = function() {
               paste(input$taxon, ".csv", sep = "")
          },
          
          content = function(file) {
               occurrences<-dataInput()
               write.csv(occurrences,file,row.names = FALSE)
          }
     )
     
     
     maxOcc<-reactive({
          dataset<-dataInput()
          subset(dataset,dataset$mean_age==max(dataset$mean_age))
     })
     
     maxOccSp<-reactive({
          dataset<-dataInput()
          species<-subset(dataset,dataset$matched_rank==3)
          species<-subset(species,is.na(species$genus_reso)==T | species$genus_reso=="n. gen.")
          subset(species,species$mean_age==max(species$mean_age))
     })
     
     output$table1<-renderTable({
          max_occ<-maxOcc()
          data.frame(taxon=max_occ$matched_name,
                     collection=max_occ$collection_no,
                     early_interval=max_occ$early_interval,
                     max_age=max_occ$early_age,
                     late_interval=max_occ$late_interval,
                     min_age=max_occ$late_age
          )
     })
     
     
     output$table2<-renderTable({
          max_occ<-maxOccSp()
          data.frame(taxon=max_occ$matched_name,
                     collection=max_occ$collection_no,
                     early_interval=max_occ$early_interval,
                     max_age=max_occ$early_age,
                     late_interval=max_occ$late_interval,
                     min_age=max_occ$late_age
          )
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
