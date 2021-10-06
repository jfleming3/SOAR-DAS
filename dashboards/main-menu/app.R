library(shiny)
library(plotly)
library(dplyr)
library(openxlsx)


#stopApp()

ui <- fluidPage(

  includeCSS("style.css"),
  fluidRow(class = "header",
           column(width = 8, class = "title",
                  titlePanel("SOAR Data Analytics Center")
                  ),
           column(width = 4, class = "header-logo",
                # img(src = "soarbig.png", width=85, height=85)
                 )
           ),
  fluidRow(class = "body",
  column(width = 5,
         br(),
         actionButton("demographic_employment", "Demographic Employment Dashboard", width = 350, onclick = "setTimeout(function(){window.close();},500);"),
         actionButton("employment_time", "Employment by Time with SOAR", width = 350, onclick = "setTimeout(function(){window.close();},500);"),
         actionButton("chemical_dependency", "Chemical Dependency", width = 350, onclick = "setTimeout(function(){window.close();},500);"),
         actionButton("barrier", "Barrier Dashboard", width = 350, onclick = "setTimeout(function(){window.close();},500);"),
         br(),br(),br(),
  actionButton(
    inputId = "bug_report",
    label = "Bug Report",
    icon = icon("bug"),
    href = "https://docs.google.com/forms/d/e/1FAIpQLSfVLm6WFBgrvg6lfwsPAmQHzWawOxRlUCgkRPtn7QfGSf0Vkg/viewform",
    onclick = "window.open('https://docs.google.com/forms/d/e/1FAIpQLSfVLm6WFBgrvg6lfwsPAmQHzWawOxRlUCgkRPtn7QfGSf0Vkg/viewform')",
  )
         ),
  column(width = 6, class = "kpi",
         dateRangeInput("intakeDate","Intake Date", start = Sys.Date() - 30, end = Sys.Date(), startview = 'year'),
         radioButtons("timeMode", label = "", choices = c("Default","Yearly"), inline = T),
         downloadButton('downloadData', 'Download Report'),
         plotlyOutput("intake", height = "50%")
         ),
  column(width = 1, class = "kpi2",
         p("Intake this Period"),
         textOutput("totalCases")
         )

  )
)




server <- function(input,output, session){

  intakeData <- reactive({
    print(getwd())
    read.csv("intake_data.csv")
  })
  
  output$downloadData <- downloadHandler(
    filename = function(){paste("Intake_Data", Sys.Date(),".xlsx", sep = "")},
    content = function(file){
        write.xlsx(downloadData(), file = file)
    }
  )
  
  
  
  
  observeEvent(input$demographic_employment,{
    if (input$demographic_employment > 0) {
    setwd("..")
    setwd("CD_Employment")
   # path <- "..\\demographic-employment\\runDemographicDashboard.bat"
    system("runCDEmploymentDashboard.bat")
    q("no")
    }
  })
  
  observeEvent(input$chemical_dependency,{
    if (input$chemical_dependency > 0) {
    setwd("..")
    setwd("CD_Employement")

    system("runCDEmploymentDashboard.bat")
    q("no")
    }
  })
  
  observeEvent(input$barrier,{
    if (input$barrier > 0) {
    setwd("..")
    setwd("barrier")

    system("runBarrierDashboard.bat")
    q("no")
    }
  })
  
  observeEvent(input$employment_time,{
    if(input$employment_time > 0){
    setwd("..")
    setwd("employment-time")
   # print(getwd())
   #path <- "..\\employment-time\\runEmploymentDash.bat"

    system("runEmploymentDash.bat")
    q("no")
    }
  })
  session$onSessionEnded(function(){q("no")})
  
  filtered_intake_data <- reactive({
    data <- intakeData()
    data$Intake.Date <- as.Date(data$Intake.Date,"%Y-%m-%d")
    data <- data %>% group_by(Intake.Date) %>% tally()
    
    print(input$intakeDate[1])
    showData <- between(data$Intake.Date, input$intakeDate[1],input$intakeDate[2])
    
    
    
    data <- data[showData,]
  })
  
  output$totalCases <- renderText({
    
    cases <- as.character(length(filtered_intake_data()$Intake.Date))
    
    paste(cases, "Cases")
  })
  
  yearly_data <- reactive({
    data <- filtered_intake_data()
    
    data$Intake.Year <- substring(data$Intake.Date,0,4)
    data <- data[!is.na(data$Intake.Date),]
    data <- data %>% group_by(Intake.Year) %>% tally() %>% arrange(Intake.Year)
    data.frame(data)
  })
  
  
  
  output$intake <- renderPlotly({
    
    
    if(input$timeMode == "Default"){
    data <- filtered_intake_data()
    
    fig <- plot_ly(data = data,type = 'scatter', x = ~Intake.Date, y = ~n, mode = "lines") %>%
      layout(xaxis <- list(title = "Intake Date"), yaxis = list(title = "New Clients"))
    }
    else if(input$timeMode == "Yearly"){
    data <- yearly_data()
    
    fig <- plot_ly(data = data,type = "bar", x = ~Intake.Year, y = ~n) %>%
      layout(xaxis <- list(title = "Intake Year"), yaxis = list(title = "New Clients"))
    }
    fig
  })
  
  downloadData <- reactive({
    if(input$timeMode == "Yearly"){
      data <- yearly_data()
    } else {
      data <- filtered_intake_data() %>% group_by(Intake.Date) %>% tally()
    }
    data
  })
  

}

options(shiny.launch.browser = T)
shinyApp(ui = ui, server = server)
