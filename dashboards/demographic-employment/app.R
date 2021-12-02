#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(openxlsx)
library(DT)
library(shiny)
library(dplyr)
library(htmltools)
#?includeCSS
# Define UI for application
ui <- fluidPage(
    includeCSS("style.css"),
    # Application title
    
    fluidRow( class = "header",
      column(width = 8,
      class = "title", titlePanel("Demographic Employment Dashboard")
      ),
      column(width = 4,
             span(class="header-image",
 #                 img(src = "soarbig.png", width=65, height=65 )
                  ))
      ),
    
    # Sidebar 
    fluidRow(
     
        column(width = 6,class = "dash-sidebar",
            # app intro
            # genders
            selectInput(inputId = "showCols", "Show Columns", multiple = T, choices = c()),
            selectInput(inputId = "gender", label = "Gender", choices = c("Male", "Female", "Trans", "Trans Man", "Trans Woman", "Non-Binary", "GenderQueer", "Other"), multiple = T),
            
            # race
            selectInput(inputId = "ethnicity", label = "Ethnicity", choices = c("White", "Black", "Asian", "Native American", "Hispanic", "Multi-Racial", "Native Hawaiian", "Unknown"), multiple = T),
        
            # education level
            selectInput(inputId = "educationAtIntake",multiple = T, label = "Education At Intake", choices = c("Less than a High School Degree", "High School Graduate", "GED", "Some College (No Degree)", "Post Secondary Certification", "Vocational School Graduate", "AA Degree", "4 Year Degree", "Post Graduate Degree", "Unknown")),
            
            # age
            sliderInput(inputId = "age", label = "Age", min = 0, max = 120, step = 1, value = c(0,120)),
            
            # family size
            sliderInput(inputId = "cdbgCityFamilySize", label = "CDBG City Family Size", min = 0, max = 15, step = 1, value = c(0,15)),
            
            # MA
            selectInput(inputId = "ma", label = "MA", multiple = T, choices = c(0,1)),
            
            # MFIP
            selectInput(inputId = "mfip", label = "MFIP", multiple = T, choices = c(0,1)),
            
            # DWP
            selectInput(inputId = "dwp", label = "DWP", multiple = T, choices = c(0,1)),
            
            # funding
            # selectInput(inputId = "funding",multiple = T, label = "Funding", choices = c("City CDBG", "County CDBG", "DEED P2P", "DEED/WESA", "MFIP", "MN-OJP", "OYOD", "United Way", "United Way-RS", "VI-SPDAT", "Fee-Workshop", "Fee-Individual Services", "Other Agency", "No Funding")),
            
            #Services
            # selectInput(inputId = "program",multiple = T, label = "Program", choices = c("Career Quest", "Employment Services", "Mind Over Matters", "RS-Pre Release", "RS-Post Release", "RS-Aftercare", "Phase 3 - Training", "Phase 4 - DAW", "Phase 4 - Other", "Fee Services")),
          
          
            # picking dates
            # span(class = "date-range",
            # dateRangeInput("daterange","Last Employed", format = "mm/dd/yyyy",start = Sys.Date() - 30, end = Sys.Date())
            # ),

          radioButtons("fileType","File Type",c("Excel" = ".xlsx", "Comma Separated Values" = ".csv"), selected = ".xlsx", inline = T),
          downloadButton('downloadData', 'Download'),
          span(class = "report-button",
            actionButton("back", "Home", icon = icon("arrow-left"), onclick = "setTimeout(function(){window.close();},5000); stopApp();"),
            
            actionButton("info","", icon = icon("info-circle")),
          )
          
          
        ),


        # Main panel
        column(width = 8, class = "dash-body",
          # tabsetPanel(
               # tabPanel("Data Table", ),
                
                dataTableOutput("tbTable")
             #   radioButtons("tableSize","Entries per Page", choices = c("All" = -1,10,25,50,100), inline = T, selected = -1)
           # )
        )
    )
)



server <- function(input, output, session) {
    data <- reactive({

        read.csv("demographic.csv")
        })

    output$downloadData <- downloadHandler(
        #name of the file
        filename = function(){paste("Demographic-Employment", Sys.Date(),input$fileType, sep = "")},
        content = function(file){
            #write in desired format based on input
            if(input$fileType == ".csv"){
                write.csv(tableData(),file = file)
            } else {
            write.xlsx(tableData(), file = file)
            }
        }
    )
    
    
    updateInputs <- observeEvent(data(),{
        #this reactive updates the selectInputs so that only the options that are present in the data are shown 
        data <- data()
        updateSelectInput(session, "gender","Gender",choices = c(unique(data$Gender)))
        updateSelectInput(session, "ethnicity","Ethnicity",choices = c(unique(data$Ethnicity)))
        updateSelectInput(session, "educationAtIntake","Education At Intake",choices = c(unique(data$Education.At.Intake)))
        # updateSelectInput(session, "funding","Funding",choices = c(unique(data$Funding)))
        # updateSelectInput(session, "program","Program",choices = c(unique(data$Program)))
        
        is_not_na <- !unlist(lapply(data$CDBG.City.Family.Size,is.na))
        availableFS <- data$CDBG.City.Family.Size[is_not_na]
        
        updateSliderInput(session,"cdbgCityFamilySize", max = max(availableFS))
        
        is_not_na <- !unlist(lapply(data$Age,is.na))
        availableFS <- data$Age[is_not_na]
        
        updateSliderInput(session,"age", max = max(availableFS))
    })
    
   updateColumnInput <- observeEvent(tableData,{
     updateSelectInput(session, "showCols", choices = colnames(tableData()))
   })
    
   observeEvent(input$back,{
     setwd("..")

     setwd("main-menu")
     system("runMainMenu.bat")
     q("no")
   })
   session$onSessionEnded(function(){q("no")})
    
   observeEvent(input$info,{
       showModal(
           modalDialog(
           p("The employment reporting application allows you to report clients who became employed during a period of time by race, age, gender, and education level. Select each variable from the drop downs or select date or age ranges."),
           br(),
            p(" Use the download button to download the data that is currently displaying."),
           br()
           )
       )
   }) 
    
    
    tableData <- reactive({
        #get data
        data <- data()
        

        #this will determine which rows are in our date range
        # if((!is.na(data$Last.Employment))){
        # rows_by_date <- between(as.Date(data$Last.Employment,"%Y-%m-%d"),as.Date(input$daterange[1],"%m/%d/%Y"),as.Date(input$daterange[2],"%m/%d/%Y"))
        

        
        # data <- data[rows_by_date,]
        # }

     
    
        #calculate age
        # data$Age <- floor((Sys.Date() - as.Date(data$Birth.Date,"%Y-%m-%d")) / 365.25)
        # data$Age <- as.integer(data$Age)

        
        #this determines if the age is between our minimum and maximum range
        #performing comparisons across boolean vectors can be difficult, so we are adding them instead
        #since R defines T = 1, F = 0, an AND operation will have a value of 2 if both arguments are TRUE
    
        data <- data[{ {input$age[1] <= data$Age} + {data$Age <= input$age[2]} } == 2,]
        
        data <- data[{ {input$cdbgCityFamilySize[1] <= data$CDBG.City.Family.Size} + {data$CDBG.City.Family.Size <= input$cdbgCityFamilySize[2]} } == 2,]
        
        #filter data based on inputs
        if(!is.null(input$gender)){
            data <- data[data$Gender %in% input$gender,]
        }
        if(!is.null(input$ethnicity)){
            data <- data[data$Ethnicity %in% input$ethnicity,]
        }
        if(!is.null(input$educationAtIntake)){
            data <- data[data$Education.At.Intake %in% input$educationAtIntake,]
        }
        if(!is.null(input$ma)) {
          data <- data[data$MA %in% input$ma,]
        }
        if(!is.null(input$mfip)) {
            data <- data[data$MFIP %in% input$mfip,]
        }
        if(!is.null(input$dwp)) {
            data <- data[data$DWP %in% input$dwp,]
        }
        
        # if(!is.null(input$funding)){
        #   data <- data[data$Funding %in% input$funding,]
          
        # }
        # if(!is.null(input$program)){
        #   data <- data[data$Program %in% input$program,]
          
        # }
        
       # print(data)
      #  data <- data[data$Wage > input$wage[1] & data$Wage < input$Wage[2]]
        
        #get desired columns

        print(colnames(data))
        data <- data[c("Client.Id","Client","Gender","Ethnicity","Education.At.Intake","CDBG.City.Family.Size","Age", "CDBG.City.Income", "MA", "MFIP", "DWP")]
        print(colnames(data))
        # data$Wage <- paste("$",data$Wage) 
        # colnames(data)[6] = "Education.At.Intake"
        colnames(data) <- gsub("."," ",colnames(data), fixed = T)
        data
    })
    
    output$tbTable <- renderDataTable({
        data <- tableData()
        if(!is.null(input$showCols)){
        data <- data[input$showCols]
        }
        data
    },options = list(pageLength = -1))

}
options(shiny.launch.browser = T)
shinyApp(ui = ui, server = server)
