library(shinydashboard)
library(shiny)
library(plotly)
library(dplyr)
library(DT)
library(openxlsx)



ui <- dashboardPage(
  dashboardHeader(
    title = "Barriers Dashboard"
  ),
  dashboardSidebar(
    selectInput("showCols","Show Columns", choices = c(), multiple = T),
    sliderInput("monthsWithSoar","Months With SOAR", min = 0, max = 12, value =c(0,12) , step = 1),
    radioButtons("fileType","File Type",c("Excel" = ".xlsx", "Comma Separated Values" = ".csv"), selected = ".xlsx", inline = T),
    downloadButton('downloadData', 'Download'),
    actionButton("back", "Home", icon = icon("arrow-left"), onclick = "setTimeout(function(){window.close();},500); stopApp();")
  ),
  dashboardBody(
    tags$style(type="text/css",
               ".shiny-output-error { visibility: hidden; }",
               ".shiny-output-error:before { visibility: hidden; }"
    ),
    column(width = 6,
    plotlyOutput("plot_employed")
    ),
    column(width = 6,
    plotlyOutput("plot_unemployed")
    ),
    dataTableOutput("tableOut")
  )
)


#if you're reading this... I'm very sorry for what you're about to experience.
#defs not my best work
server <- function(input, output, session){
  
  data <- reactive({
    read.csv("barrier.csv")
  })
  # go back to main menu
  observeEvent(input$back,{
    setwd("..")
    
    setwd("main-menu")
    system("runMainMenu.bat")
    q("no")
  })
  #end session when window is closed
  session$onSessionEnded(function(){q("no")})
  
  #response to download button
  output$downloadData <- downloadHandler(
    #name of the file
    filename = function(){paste("Employment-SOARTime", Sys.Date(),input$fileType, sep = "")},
    content = function(file){
      #write in desired format based on input
      if(input$fileType == ".csv"){
        write.csv(filteredData(),file = file)
      } else {
        write.xlsx(filteredData(), file = file)
      }
    }
  )
  
  #intitial table data setup
  tableData <- reactive({
    data <- data()
    #calculate months with soar
    data[["Months.with.SOAR"]] <- floor(as.numeric(
      Sys.Date() - as.Date(data[["Intake.Date"]], "%Y-%m-%d")
    ) / 30.4375)
    
    #column for easy calculations later
    data$Unreliable.Transport <- ifelse(data$Transportation.Reliable == 0, 1, 0)
    # 6 = unknown
    data <- data[data$Employed.Status < 6,]
    # value < 4 is some type of employment, 5 = unemployed
    data$Employed <- ifelse(data$Employed.Status <= 4, 1, 0)
  #  print(data)
    #count total barriers for each client
    data$Barrier.Count <- data$Homeless + data$Rent.Evicted + data$Unreliable.Transport + data$Parenting.Time + data$Limited.English + data$Chemical.Dependence + data$Criminal.Record + data$Diagnosed.Disability + data$Mental.Health + data$Medical.Condition
    #filter unwanted columns
    data <- data[ !(colnames(data) %in% c("X","Sort.Seq","Education.Status","Employed.Status","Unreliable.Transport")) ]


    
    
    data
    
  })
  filteredData <- reactive({
    #filter by monthsWithSoar
    data <- tableData()
    monthFilter <- between(data$Months.with.SOAR,input$monthsWithSoar[1],input$monthsWithSoar[2])
    #set na values to false
    monthFilter[is.na(monthFilter)] = F
    data <- data[monthFilter,]
    data
  })
  
  updateInputChoices <- observeEvent(tableData(),{
    #update slider and selectinputs based on tableData output
    maxSlider <- max(tableData()["Months.with.SOAR"][!is.na(tableData()["Months.with.SOAR"])])
    updateSelectInput(session, "showCols", choices = colnames(tableData()))
    updateSliderInput(session, "monthsWithSoar", max = maxSlider)
  })
  
  
  
  
  output$plot_employed <- renderPlotly({
    
    data <- plotDataEmployed()
    #replace periods
    data$Barrier <- gsub("."," ",data$Barrier, fixed = T)
    
     plot_ly(data,type = "bar", x = ~Months.with.SOAR, y = ~n, color = ~Barrier) %>%
       layout(barmode = "group",title = "Employed Barrier Count",yaxis = list(title = "Count"), xaxis = list(tickmode = "linear", dtick = 1, title = "Months with SOAR"))

       
    
  })
  
  plotDataEmployed <- reactive({
    
    data <- filteredData()
    #set initial shown columns
    showCol <- colnames(data)
    if(!is.null(input$showCols)){
      #if we have input start to filter our data
      showCol <-unique(c(input$showCols,"Employed","Months.with.SOAR"))
      print(showCol)
      data <- data[showCol]
    }
    data <- data[data$Employed == 1,]
    #check if education completed is in our input.  This is later One-hot encoded
    if("Education.Completed" %in% showCol){
      data$Education.Completed[is.na(data$Education.Completed)] <- "Unknown"
      
      #one-hot encoding for Education.Completed
      for(val in unique(data$Education.Completed)){
        data[[val]] <- ifelse(data$Education.Completed == val, 1, 0)
      }
    }
    #counting with only numeric columns
    use_cols <- unlist(lapply(data,is.numeric), use.names = F)
    data <- data[use_cols]
    #get rid of unwanted columns
    data <- data[ !(colnames(data) %in% c("X","Sort.Seq","Barrier.Count","Employed","Unknown")) ]
    
    
    #set na data to 0's
    data[is.na(data)] <- 0
    
    #sum the data by month
    sums <- cbind(Months.with.SOAR = unique(data$Months.with.SOAR), rowsum(data[!{colnames(data) %in% "Months.with.SOAR"}], group = data$Months.with.SOAR))
    #create an empty dataframe to avoid some annoying errors
    reshapedsums <- data.frame(Months.with.SOAR = integer(), Barrier = character(), n = integer())
    
    #?I did this some time ago and I'm not sure
    #puts everything into 3 columns Months, Barrier, and count (n)
    for(column in 1:length(unique(sums$Months.with.SOAR))){
      temp <- data.frame(Months.with.SOAR = sums$Months.with.SOAR[column], Barrier = colnames(sums[-1]), n = unlist(sums[column,-1], use.names = F))
      reshapedsums <- rbind(reshapedsums, temp)
    }
    
    
    
    reshapedsums
   # reshapedsums <- reshapedsums[reshapedsums$Months.with.SOAR != 0,]
  })
  
  plotDataUnemployed <- reactive({
    #same process as previous function.
    data <- filteredData()
    showCol <- colnames(data)
    if(!is.null(input$showCols)){
      showCol <-unique(c(input$showCols,"Employed","Months.with.SOAR"))
      print(showCol)
      data <- data[showCol]
    }
    data <- data[data$Employed == 0,]
    
    if("Education.Completed" %in% showCol){
    data$Education.Completed[is.na(data$Education.Completed)] <- "Unknown"
    
    
    for(val in unique(data$Education.Completed)){
      data[[val]] <- ifelse(data$Education.Completed == val, 1, 0)
    }
    }
    use_cols <- unlist(lapply(data,is.numeric), use.names = F)
    data <- data[use_cols]
    data <- data[ !(colnames(data) %in% c("X","Sort.Seq","Barrier.Count","Employed","Unknown")) ]
    
    
    
    data[is.na(data)] <- 0
    
    
    sums <- cbind(Months.with.SOAR = unique(data$Months.with.SOAR), rowsum(data[!{colnames(data) %in% "Months.with.SOAR"}], group = data$Months.with.SOAR))
    
    reshapedsums <- data.frame(Months.with.SOAR = integer(), Barrier = character(), n = integer())
    
    
    for(column in 1:length(unique(sums$Months.with.SOAR))){
      temp <- data.frame(Months.with.SOAR = sums$Months.with.SOAR[column], Barrier = colnames(sums[-1]), n = unlist(sums[column,-1], use.names = F))
      reshapedsums <- rbind(reshapedsums, temp)
    }


    
    reshapedsums
   # reshapedsums <- reshapedsums[reshapedsums$Months.with.SOAR != 0,]
  })
  
  output$plot_unemployed <- renderPlotly({
    data <- plotDataUnemployed()
    data$Barrier <- gsub("."," ",data$Barrier, fixed = T)
    plot_ly(data,type = "bar", x = ~Months.with.SOAR, y = ~n, color = ~Barrier) %>%
      layout(barmode = "group",title = "Unemployed Barrier Count",yaxis = list(title = "Count"), xaxis = list(tickmode = "linear", dtick = 1, title = "Months with SOAR"))

  })
  
  
  
  
  
  output$tableOut <- renderDataTable({
    data <- filteredData()
    #filter desired columns
    if(!is.null(input$showCols)){
      data <- data[input$showCols]
    }
    colnames(data) <- gsub("."," ",colnames(data), fixed = T)
    data
  })
  
}
#oof
options(shiny.launch.browser = T)
shinyApp(ui = ui, server = server)