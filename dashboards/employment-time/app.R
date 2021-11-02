library(shiny)
library(plotly)
library(dplyr)
library(ggplot2)
library(htmlwidgets)
library(openxlsx)
library(DT)




ui <- fluidPage(class = "page",
                tags$style(type="text/css",
                           ".shiny-output-error { visibility: hidden; }",
                           ".shiny-output-error:before { visibility: hidden; }"
                ),
                
                        includeCSS("style.css"),
                        fluidRow(class = "dash-header", 
                                 column(width = 8,titlePanel("Time with SOAR Employment Dashboard"), class = "title"),
                                 column(width = 4, class="header-image",
                                        span(
                      #                    img(src = "soarbig.png", width=65, height=65 )
                                          ))),

                        fluidRow(
                          column(width = 4, class = "dash-sidebar",
                                 
                                 

                                 
                                # selectInput("showCols", multiple = T,label = "Show Columns", choices = "All"),
                                 
                                 selectInput("intakeJobs","Include Intake Jobs", choices = c("Yes" = T, "No" = F,  "Intake Jobs Only")),
                                 
                                 sliderInput("monthsWithSoar", min = 0, max = 10, value = c(0,5), step = 1, label = "Months with Soar"),
                                 
                                 selectInput(inputId = "employed", label = "Employed", choices = c("Employed","Unemployed", "Both"), selected = "Both"),
                                 # genders
                                 selectInput(inputId = "gender", label = "Gender", choices = c("Male", "Female", "Trans", "Trans Man", "Trans Woman", "Non-Binary", "GenderQueer", "Other"), multiple = T),
                                 
                                 # race
                                 selectInput(inputId = "race", label = "Race", choices = c("White", "Black", "Asian", "Native American", "Hispanic", "Multi-Racial", "Native Hawaiian", "Unknown"), multiple = T),
                                 
                                 # education level
                                 selectInput(inputId = "educationCompleted",multiple = T, label = "Education Completed", choices = c("Less than a High School Degree", "High School Graduate", "GED", "Some College (No Degree)", "Post Secondary Certification", "Vocational School Graduate", "AA Degree", "4 Year Degree", "Post Graduate Degree", "Unknown")),
                                 
                                 # age
                                 #  textInput(inputId = "age", label = "Age", value = ""),
                                 sliderInput(inputId = "age", label = "Age", min = 0, max = 120, step = 1, value = c(0,120)),
                                 
                                 dateRangeInput("intakeDate","Intake Date", format = "mm/dd/yyyy",start = Sys.Date() - 180, end = Sys.Date()),
                                 dateRangeInput("hireDate","Hire Date", format = "mm/dd/yyyy",start = Sys.Date() - 180, end = Sys.Date()),
                                 
                                 radioButtons("fileType","File Type",c("Excel" = ".xlsx", "Comma Separated Values" = ".csv"), selected = ".xlsx", inline = T),
                                 downloadButton('downloadData', 'Download'),
                                 span(class = "report-button",
                                      actionButton("back", "Home", icon = icon("arrow-left"), onclick = "setTimeout(function(){window.close();},5000); stopApp();"),
                                      actionButton("info","", icon = icon("info-circle")),
                                 )
                          ),
                          column(width = 6, class = "dash-body",
                                 plotlyOutput("plotOut"),
                                 dataTableOutput("tableOut")
                          ),
                          column(width = 2, class = "kpi",
                                      h5("Average Starting Wage"),
                                      textOutput("kpi1"),
                                      h5("Average Time to Employment"),
                                      textOutput("kpi2")
                                 )
                        ),
                fluidRow(class = "table-container",
                  column(width = 4),
                  column(width = 8,

                  )
                )
)


server <- function(input,output, session){
  data <- reactive({
    read.csv("employment.csv")
    
  })
  
  output$downloadData <- downloadHandler(
    #name of the file
    filename = function(){paste("Employment-SOARTime", Sys.Date(),input$fileType, sep = "")},
    content = function(file){
      #write in desired format based on input
      if(input$fileType == ".csv"){
        write.csv(tableData(),file = file)
      } else {
             write.xlsx(tableData(), file = file)
      }
    }
  )
  
  updateSlider <- observe({
    updateSliderInput(session = session, "monthsWithSoar", max = max(data2()$Months.with.SOAR))
    
    
  })
  observeEvent(input$back,{
    setwd("..")
    
    setwd("main-menu")
    system("runMainMenu.bat")
    q("no")
  })
  session$onSessionEnded(function(){q("no")})
  
  observeEvent(input$info,{
    showModal(modalDialog(
      tabsetPanel(
        tabPanel("Overview",
          p("This dashboard will let you explore employment data from the SOAR database"),
          br(),
          p("The right will show the average length until the currently selected grouping is employed, as well as the average wage"),
          br(),
          p("The plot shows employment by time spent with SOAR (see Data Definitions: Employed)")
        ),
        tabPanel("Controls",
                 p("Use the controls on the left to filter the data based on your selections"),
                 br(),
                 p("For some categories, you can choose one, or multiple inputs"),
                 br(),
                 p("Hover over the graph, and a small toolbar will appear.  Use the camera icon to save an image of the plot.")
                 ),
        tabPanel("Data Definitions",
                 p("This table was formed from a LEFT OUTER join on the Clients and Jobs table. This means that both tables are combined, and all clients are shown, even if they do not have a record in the Jobs table."),
                 br(),
                 p("Most columns are defined as they appear in the database.  Custom defined columns are as follows:"),
                 br(),
                 p("Age: uses Current Date - Birth Date"),
                 p("Employed: uses the Employed column from the Clients table; Employed if -1, Unemployed if 0  (in Microsoft Access, -1 is yes and 0 is no)"),
                 p("Months with SOAR: Uses Current Date - Intake Date"),
                 p("Employed After: Uses Hired Date - Intake Date")
                 )
      )

    ))
  })
  
  
  
  data2 <- reactive({
    barData <- data()
    barData[["Months.with.SOAR"]] <- floor(as.numeric(
      Sys.Date() - as.Date(barData[["Intake.Date"]], "%Y-%m-%d")
    ) / 30.4375)
    
    barData <- barData[!is.na(barData$Months.with.SOAR),]

    barData
  })
  barData <- reactive({
    
    barData <- tableData()
    
    barData <- barData[as.numeric(barData$Months.with.SOAR) >= input$monthsWithSoar[1] & as.numeric(barData$Months.with.SOAR) <= input$monthsWithSoar[2],]
    barData$Months.with.SOAR <- as.character(barData$Months.with.SOAR)
    barData <- barData %>% group_by(Employed, Months.with.SOAR) %>% tally()
    barData
    
    barData
    
  })
  
  
  
  output$plotOut <- renderPlotly({
    
    fig <- plot_ly(data = barData(), type = "bar", x = ~Months.with.SOAR, y = ~n, color = ~Employed, colors = c("#82b1ad", "#ff7f50")) %>%
      layout(yaxis = list(title = "Count"), xaxis = list(title = "Months with SOAR"))
    
    fig
    
  })
  
  output$tableOut <- renderDataTable({
    data <- tableData()
  #  if(!is.na(input$showCols)){
   # data <- data[input$showCols]
   # }
    data <- data[c("Client.Id","First.Name","Last.Name","Employed.After","Gender",
           "Age","Employed","Race","Education.Completed",
           "Hire.Date", "Months.with.SOAR", "Wage")]
    data$Wage <- paste("$",data$Wage, sep = "")
    colnames(data) <- gsub("."," ",colnames(data),fixed = T)
    datatable(data, rownames = F, filter = "none", options = list(paging = F, searching = F))
  })
  
  updateInputs <- reactive({
    #this reactive updates the selectInputs so that only the options that are present in the data are shown 
    data <- data()
    updateSelectInput(session, "gender","Gender",choices = c(unique(data$Age)))
    updateSelectInput(session, "race","Race",choices = c(unique(data$Race)))
    updateSelectInput(session, "educationCompleted","Education Completed",choices = c(unique(data$Education.Completed)))
  })
  updateSelectCols <- reactive({
    updateSelectInput(session, "showCols", "Show Columns", choices = colnames(data()), selected = c("Client.Id","First.Name","Last.Name","Employed.After","Gender",
                                                                                                    "Intake.Date","Age","Employed","Race","Education.Completed",
                                                                                                    "Hire.Date","Employer", "Months.with.SOAR", "Wage"))
  })
  
  
  tableData <- reactive({
    data <- data2()
    data <- data[as.numeric(data$Months.with.SOAR) >= input$monthsWithSoar[1] & as.numeric(data$Months.with.SOAR) <= input$monthsWithSoar[2],]
    data$Months.with.SOAR <- as.character(data$Months.with.SOAR)
    
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
      #   group <- c(group,"Gender")
    }
    
    if(!is.null(input$race)){
      data <- data[data$Race %in% input$race,]
      #   group <- c(group, "Race")
    }
    if(!is.null(input$educationCompleted)){
      data <- data[data$Education.Completed %in% input$educationCompleted,]
      #   group <- c(group, "Education.Completed")
    }
    if(input$intakeJobs == F){
      data <- data[data$Job.Number >= 3,]
    } else if(input$intakeJobs == "Intake Jobs Only"){
      data <- data[data$Job.Number == 2,]
    }
    if(input$employed != 'Both'){
      data <- data[data$Employed == input$employed,]
    }
    
    
   
    data$Employed.After.Val <- 
      floor(as.numeric(
      as.Date(data$Hire.Date, "%Y-%m-%d") - as.Date(data$Intake.Date, "%Y-%m-%d")
      )/30.4375)
    data$Employed.After <- ifelse(!is.na(data$Employed.After),paste(data$Employed.After.Val, "Months"),"NA")
    
   # data <- data[between(as.Date(data["Hire.Date"],"%Y-%m-%d"),as.Date(input$hireDate[1],"%m/%d/%Y"),as.Date(input$hireDate[2],"%m/%d/%Y")),]
  #  data <- data[between(as.Date(data["Intake.Date"],"%Y-%m-%d"),as.Date(input$intakeDate[1],"%m/%d/%Y"),as.Date(input$intakeDate[2],"%m/%d/%Y")),]
    
    
    #print(colnames(data))
    #print(data)
    print(colnames(data))
    data
  })
  output$kpi1 <- renderText({
    
    data <- tableData()$Wage
    #print(data())
    data <- data[!is.na(data)]
    avg <- mean(data)
    avg <- paste("$",round(avg, 2), sep = "")
    
  })
  output$kpi2 <- renderText({
    data <- tableData()$Employed.After.Val
    #print(data$Employed.After.Val)
    data <- data[!is.na(data)]
    #print(data)
    avg <- paste(round(mean(data),1), "Months")
  })
  # output$kpi3 <- renderText({
  #   "15.54"
  # })
  
}

#options(shiny.port = 5002)
#browseURL("http://127.0.0.1:5002/")
options(shiny.launch.browser = T)
shinyApp(ui = ui, server = server)

