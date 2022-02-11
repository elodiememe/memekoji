#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(data.table)
library(lubridate)
library(leaflet)
library(shiny)
library(DT)

#load data
customer<-fread("/Users/lodiee/Desktop/R/Day 5/data_customer.csv")
personal<-fread("/Users/lodiee/Desktop/R/Day 5/data_personal.csv")

customperson<-merge(customer, personal, by="CustomerId", all.x=TRUE)

as.factor(customperson$Exited)
as.factor(customperson$Gender)

churnp<-glm(Exited~CreditScore+Gender+Age+Tenure+Balance+NumOfProducts+HasCrCard+IsActiveMember+EstimatedSalary,family="binomial",data=customperson)

customperson$predictch<-predict(churnp,customperson,type="response")


#select the top 100 customers with the highest churn probability
z <- customperson
zsub1 <- z[order(z$predictch,decreasing = TRUE),]
hhhh <- zsub1[c(1:100),]
View(hhhh)
View(z)


#create map
zips <- as.matrix(hhhh[1:100, list(zip_longitude, zip_latitude)])

map <- leaflet();
map <- addTiles(map); 
map <- addMarkers(map, data = zips, clusterOptions = markerClusterOptions(),);
map <- setView(map, lat= 43, lng= -79, zoom = 3); # North America
map

#define function
myFun1<-function(Id){
  prob<-customperson[CustomerId==Id,list(predictch)]
  return(prob)
}

# ui part -----------------------------------------------------------------
# Define a map object as output
ui <-  fluidPage(
  #Add main panel with map
  titlePanel("Customer map with the top 100 highest churn probability"),
  sidebarLayout(position = "left",
                sidebarPanel(
                  fluidRow(
                    column(9, DT::dataTableOutput('x1')),
                    column(3, verbatimTextOutput('x2'))
                  ),width = 5
                ),
                mainPanel(leafletOutput(outputId="mymap"),width = 5)
  )
)
  
  
  # server part -------------------------------------------------------------
  
  server <- function(input, output, session) {
   
    customsub = z[, c(1,17)]
    output$x1 = DT::renderDataTable(customsub, server = TRUE)
    output$x2 = renderPrint({
      s = input$x1_rows_selected
      if (length(s)) {
        cat('These rows were selected:\n\n')
        cat(s, sep = ', ')
      }
    })
    
    # Define map and fill map with data points 
    output$mymap <-  renderLeaflet(map)
    
  }  


# End server

shinyApp(ui = ui, server = server)



