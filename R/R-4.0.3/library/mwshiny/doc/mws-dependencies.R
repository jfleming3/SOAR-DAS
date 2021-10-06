## ----setup, include = FALSE----------------------------------------------
knitr::opts_chunk$set(
  # collapse = FALSE,
  # comment = "#>"
  results = "hold"
)

## ----load libraries, results = 'hide', warning=F-------------------------

# our multi-window shiny friend
library(mwshiny)
# visNetwork is a package for visualization of graphs and networks, and is dependent on .js and .css scripts for visualizations in Shiny apps
library(visNetwork)


## ----specify app---------------------------------------------------------

# the titles of our windows
win_titles <- c("Controller", "Graph")

ui_win <- list()

# first we add what we want to see in the controller to the list
ui_win[[1]] <- fluidPage(
  titlePanel("visNetwork Dependency Example: Controller"),
  sidebarLayout(
    # where all our choices will go
    sidebarPanel(
      # choose the number of nodes, from 3 to 10
      numericInput(inputId = "num_nodes",
                   label = "How many nodes would you like to see?",
                   value = 3,
                   min = 3,
                   max = 10),
      # choose the number of edges, from 2 to 9
      numericInput(inputId = "num_edges",
                   label = "How many edges would you like to see?",
                   value = 2,
                   min = 2,
                   max = 9),
      # only build our graph when this button is clicked
      actionButton(inputId = "go",
                   label = "Graph!")
      
    ),
    # empty main panel
    mainPanel()
  )
)

# next we add what we want to see in the visualization window to the ui list
ui_win[[2]] <- fluidPage(
  titlePanel("visNetwork Dependency Example: Graph"),
  visNetworkOutput(outputId="network", 
                   height = "800px")
)

# now we do some calculations based on our inputs, to pass to our final rendering of our network
serv_calc <- list()

serv_calc[[1]] <- function(calc, sess){
  observeEvent(calc$go, {
    # create our nodes data frame for the visNetowrk render plot to use
    calc[["nodes"]] <- data.frame(id = 1:calc$num_nodes)
    
    # create our edges data frame, specifying a random set of nodes to connect
    calc[["edges"]] <- data.frame(from = sample(calc$num_edges, calc$num_edges, replace = T),
                                  to = sample(calc$num_edges, calc$num_edges, replace = T))
  })
}

# now we render our output!
serv_out <- list()

serv_out[["network"]] <- function(calc, sess){
  # this function renders our graph, for shiny purposes
  renderVisNetwork({
    # don't render anything if we haven't pressed the graph! button yet!
    if (!is.null(calc$nodes) & !is.null(calc$edges)){
      visNetwork(calc$nodes, calc$edges)
    }
  })
}


## ----mwsapp naive, eval=F------------------------------------------------
#  #run!
#  mwsApp(win_titles, ui_win, serv_calc, serv_out)

## ---- out.width="325px", fig.show="hold",echo=F--------------------------
knitr::include_graphics(c("figures/init_naive_controller.png", "figures/init_naive_vis.png"))

## ---- out.width="600px", fig.align='center', echo=F----------------------
knitr::include_graphics("figures/init_naive_console.PNG")

## ---- out.width="400px", fig.align='center', echo=F----------------------
knitr::include_graphics("figures/head_naive_console.PNG")

## ----shiny, eval=F-------------------------------------------------------
#  
#  # shiny is already loaded by mwshiny
#  
#  # our UI, which is going to just show a default graph
#  ui <- fluidPage(
#    visNetworkOutput(outputId="network",
#                     height = "800px")
#  )
#  
#  # our server function, which renders our graph
#  server <- function(input, output, session) {
#  
#    # the necessary components of a graph with 3 nodes and 2 edges
#    nodes <- data.frame(id = 1:3)
#    edges <- data.frame(from = c(1,2), to = c(1,3))
#  
#    # render our network for the UI
#    output$network <- renderVisNetwork({
#        visNetwork(nodes, edges)
#    })
#  }
#  
#  # run this app!
#  shinyApp(ui, server)

## ---- out.width="400px", fig.align='center', echo=F----------------------
knitr::include_graphics("figures/init_shiny.png")

## ---- out.width="400px", fig.align='center', echo=F----------------------
knitr::include_graphics("figures/shiny_head_console.PNG")

## ----htmlwidgets find----------------------------------------------------
# this command prints all files in the htmlwidgets package that end with .js
print(list.files(path=system.file(package = "htmlwidgets", mustWork = T),pattern = "\\.js$", recursive = T))

## ------------------------------------------------------------------------
# this command prints all files in the visNetwork package that end with .js
print(list.files(path=system.file(package = "visNetwork", mustWork = T),pattern = "\\.js$", recursive = T))

# for ease of viewing, print some blank lines to separate the javascript
cat("\n\n")

# this command prints all files in the visNetwork package that end with .css
print(list.files(path=system.file(package = "visNetwork", mustWork = T),pattern = "\\.css$", recursive = T))

## ----mwsapp all, eval=F--------------------------------------------------
#  # we're going to load htmlwidgets explicitly for clarity
#  library(htmlwidgets)
#  
#  # alloctate our named dependency list
#  depend <- list()
#  # names of the list correspond to the package we want to import
#  # we give each of them the value NA, because we're going to import all .js and .css files from our packages
#  depend[["htmlwidgets"]] <- NA
#  depend[["visNetwork"]] <- NA
#  
#  #run with dependencies!
#  mwsApp(win_titles, ui_win, serv_calc, serv_out, depend)

## ---- out.width="400px", fig.align='center', echo=F----------------------
knitr::include_graphics("figures/init_all_page.png")

## ----mwsapp specific, eval=F---------------------------------------------
#  # names of the list correspond to the package we want to import
#  # we give each of them the value of a vector of strings corresponding to the specific scripts we want to import
#  depend[["htmlwidgets"]] <- c("www/htmlwidgets.js")
#  depend[["visNetwork"]] <- c("htmlwidgets/lib/vis/vis.css", "htmlwidgets/lib/vis/vis.min.js", "htmlwidgets/visNetwork.js")
#  
#  #run with dependencies!
#  mwsApp(win_titles, ui_win, serv_calc, serv_out, depend)

## ---- out.width="400px", fig.align='center', echo=F----------------------
knitr::include_graphics("figures/click_vis_specific.png")

