# Data merge
library(shiny)
library(shinyFiles)
library(DT)
library(readxl)
library(tidyverse)
library(rhandsontable)
library(openxlsx)

# Data_merge function
data_merge <- function (origin, NA.removal = TRUE, page = 3) {
  
  filenames <- list.files(origin, pattern="*.xlsx", full.names=TRUE)
  
  # Merge the data
  for (file in filenames){
    # if the merged dataset doesn't exist, create it
    if (!exists("dataset")){
      dataset <- read_excel(file, sheet = page)
    }
    
    # if the merged dataset does exist, append to it
    if (exists("dataset")){
      temp_dataset <- read_excel(file, sheet = page)
      dataset<-merge(dataset, temp_dataset, all = TRUE)
      rm(temp_dataset)
    }
  }
  
  # Remove NAs and replace with Undet
  if (NA.removal == TRUE) {
    for (n in names(dataset)) {
      if (class(dataset[,n]) == "numeric") {
        y <- "Undet"
        levels(dataset[n]) <- c(dataset[n], y)
        dataset[n][is.na(dataset[n])] <- y
      }
    }
  }
  print(dataset)  
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Data merge for raw ELISA data"),
  
  sidebarLayout(
    sidebarPanel(
      shinyDirButton("folderInput","Choose Folder","Choose working directory"),
      br(),
      br(),
      radioButtons("radioInput", "Replace NA's?", 
                   choices = c("Yes" = TRUE, "No" = FALSE), 
                   selected = FALSE),
      radioButtons("sheetInput", "What sheet is your data on?", 
                   choices = c("Sheet 1" = 1,"Sheet 2" = 2, "Sheet 3" = 3), 
                   selected = 3),
      br(),
      h5("Download merged data as:"),
      downloadButton("downloadCSV", "csv"),
      downloadButton("downloadXL", "excel")
      ),
    
    mainPanel(
      textOutput("text1"),
      rHandsontableOutput('hot'))
  )
)

# Define server 
server <- function(input, output, session) {
  
  # Select Working Directory 
  volumes <- getVolumes()
  shinyDirChoose(input, 'folderInput', roots = volumes, session = session, 
                 restrictions = system.file(package = 'base'))
  
  #Setup to get datamerge to work
  folderInput1 <- reactive({
    req(input$folderInput)
    
    return(parseDirPath(volumes, input$folderInput))
  })
  
  path1 <- reactive({
    return(print(parseDirPath(volumes, input$folderInput)))
  })
  
 
  # Merge the data together, remove 0 values in first column (for ELISA this is Sample ID) 
  merged <- reactive({
              dataset <- data_merge(folderInput1(), 
                          input$radioInput, 
                          as.numeric(input$sheetInput))
              dataset %>%
                filter(dataset[,1] != 0)
  })
  
 

  
  output$text1 <- renderText({
    path1()
  })

  merged_table <- reactive ({ rhandsontable(merged(), stretchH = "all") %>%
                                            hot_context_menu(allowRowEdit = TRUE)
                                    })
  
  output$hot <- renderRHandsontable({merged_table()})
  
  
  # Downloadable files of selected dataset ----
  output$downloadCSV <- downloadHandler(
    filename = function() {
      paste("mergedData", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(hot_to_r(input$hot), file, row.names = FALSE)
    }
  )
  
  output$downloadXL <- downloadHandler(
    filename = function() {
      paste("mergedData", ".xlsx", sep = "")
    },
    content = function(file) {
      write.xlsx(hot_to_r(input$hot), file, row.names = FALSE)
    }
  )
}
  
# Run the application
shinyApp(ui = ui, server = server, options = list(port=7990))