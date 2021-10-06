
library(shiny)
library(openxlsx)
library(dplyr)
library(RODBC)


ui <- fluidPage(class = "page",includeCSS("www/style.css"),
  
  
  fluidRow(class = "dash-header", 
           column(width = 8,titlePanel("Chemical Dependence Employment"), class = "title"),
           column(width = 4, class="header-image",
                  span(
                    #www folder seems to not load images when launching via Rscript.exe
         #           img(src = "soarbig.png", width=65, height=65 )
                      )
                     )
                    ),
  sidebarLayout(
    
    sidebarPanel(class = "dash-sidebar",
      
      # picking dates
      tags$head(tags$style(".myclass {background-color: #96dafb;}")),
      dateRangeInput("daterange","Hire Date", format = "mm/dd/yyyy",start = Sys.Date() - 120, end = Sys.Date()),
      checkboxInput("showAll", "Show All (ignore dates)", value = F),
      
      
      radioButtons("fileType","File Type",c("Excel" = ".xlsx", "Comma Separated Values" = ".csv"), selected = ".xlsx", inline = T),
      downloadButton('downloadData', 'Download'),
      actionButton("back", "Home", icon = icon("arrow-left"), onclick = "setTimeout(function(){window.close();},500); stopApp();")
    ),
    mainPanel(
      # plotlyOutput("plotOut"),
      column(width = 8,
             textOutput("textOut"),
             textOutput("count"),
             tableOutput("tableOut"),
      ),
    ),
  )
)

server <- function(input,output, session){
  
  
  data <- reactive({

    read.csv("cd.csv")
    
  })
  
  observeEvent(input$back,{
    setwd("..")
    
    setwd("main-menu")
    system("runMainMenu.bat")
    q("no")
  })
    session$onSessionEnded(function(){q("no")})
    
    output$textOut <- renderText({
      input$select1
    })
    
    output$tableOut <- renderTable({
      data <- data()
      

      data$MonthsEmployed <-
        as.numeric(as.Date(data$Termination.Date,"%Y-%m-%d") - as.Date(data$Hire.Date,"%Y-%m-%d")) / 30.4375


      #this will determine which rows are in our date range
      if(!input$showAll){
      data <- data[!is.na(data$Hire.Date),]
      rows_by_date <- between(as.Date(data$Hire.Date,"%Y-%m-%d"),input$daterange[1],input$daterange[2])
      data <- data[rows_by_date,]
      }

      
      data
      

      

    })
    
    
    
    output$downloadData <- downloadHandler(
      filename = function(){paste("Chemical-Dependency-employment", Sys.Date(),input$fileType, sep = "")},
      content = function(file){
        if(input$fileType == ".csv"){
          write.csv(data(), file = file)
        } else {
          write.xlsx(data(), file = file)
          
        }
      }
    )
  }
  
options(shiny.launch.browser = T)
  shinyApp(ui = ui, server = server)
  