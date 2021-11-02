
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
      
      # show cols
      selectInput(inputId = "showCols", "Show Columns", multiple = T, choices = c()),
      
      # genders
      selectInput(inputId = "gender", label = "Gender", choices = c("Male", "Female", "Trans", "Trans Man", "Trans Woman", "Non-Binary", "GenderQueer", "Other"), multiple = T),
      
      # race
      selectInput(inputId = "race", label = "Race", choices = c("White", "Black", "Asian", "Native American", "Hispanic", "Multi-Racial", "Native Hawaiian", "Unknown"), multiple = T),
      
      # education level
      selectInput(inputId = "educationCompleted",multiple = T, label = "Education Completed", choices = c("Less than a High School Degree", "High School Graduate", "GED", "Some College (No Degree)", "Post Secondary Certification", "Vocational School Graduate", "AA Degree", "4 Year Degree", "Post Graduate Degree", "Unknown")),
      
      # age
      #  textInput(inputId = "age", label = "Age", value = ""),
      sliderInput(inputId = "age", label = "Age", min = 0, max = 120, step = 1, value = c(0,120)),
      
      radioButtons("fileType","File Type",c("Excel" = ".xlsx", "Comma Separated Values" = ".csv"), selected = ".xlsx", inline = T),
      downloadButton('downloadData', 'Download'),
      actionButton("back", "Home", icon = icon("arrow-left"), onclick = "setTimeout(function(){window.close();},5000); stopApp();")
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
    


    tableData <- reactive({
        #get data
        data <- data()
        

        #this will determine which rows are in our date range
        if((!is.na(data$Last.Employment))){
        rows_by_date <- between(as.Date(data$Last.Employment,"%Y-%m-%d"),as.Date(input$daterange[1],"%m/%d/%Y"),as.Date(input$daterange[2],"%m/%d/%Y"))
        

        
        data <- data[rows_by_date,]
        }

     
    
        #calculate age
        data$Age <- floor((Sys.Date() - as.Date(data$Birth.Date,"%Y-%m-%d")) / 365.25)
        data$Age <- as.integer(data$Age)

        
        #this determines if the age is between our minimum and maximum range
        #performing comparisons across boolean vectors can be difficult, so we are adding them instead
        #since R defines T = 1, F = 0, an AND operation will have a value of 2 if both arguments are TRUE
    
        data <- data[{ {input$age[1] <= data$Age} + {data$Age <= input$age[2]} } == 2,]
        #filter data based on inputs
        if(!is.null(input$gender)){
            data <- data[data$Gender %in% input$gender,]
         
        }
        
        if(!is.null(input$race)){
            data <- data[data$Race %in% input$race,]

        }
        if(!is.null(input$educationCompleted)){
            data <- data[data$Education.Completed %in% input$educationCompleted,]

        }
        if(!is.null(input$funding)){
          data <- data[data$Funding %in% input$funding,]
          
        }
        if(!is.null(input$program)){
          data <- data[data$Program %in% input$program,]
          
        }
        
       # print(data)
      #  data <- data[data$Wage > input$wage[1] & data$Wage < input$Wage[2]]
        
        #get desired columns

        print(colnames(data))
        data <- data[c("Client.Id","First.Name","Last.Name","Gender","Race","Education.Completed.x","Age", "Last.Employment","Wage")]
        print(colnames(data))
        data$Wage <- paste("$",data$Wage) 
        colnames(data)[6] = "Education.Completed"
        colnames(data) <- gsub("."," ",colnames(data), fixed = T)
        data
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
  