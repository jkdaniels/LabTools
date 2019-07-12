# Data Explorer 
library(shiny)
library(shinyFiles)
library(shinyWidgets)
library(DT)
library(dplyr)
library(readr)


data_merge <- function (origin) {
  
  filenames <- list.files(origin, pattern="*.csv", full.names=TRUE)
  
  # Merge the data
  for (file in filenames){
    # if the merged dataset doesn't exist, create it
    if (!exists("dataset")){
      dataset <- read_csv(file)
    }
    
    # if the merged dataset does exist, append to it
    if (exists("dataset")){
      temp_dataset <- read_csv(file)
      commonNames <- names(dataset)[which(colnames(dataset) %in% colnames(temp_dataset))]
      commonNames <- commonNames[commonNames != c("Sample_ID", "Study", "Type")]
      dataset<- merge(dataset,temp_dataset, by = c("Sample_ID", "Study", "Type"), all=TRUE)
      for(i in commonNames){
        left <- paste(i, ".x", sep="")
        right <- paste(i, ".y", sep="")
        dataset[is.na(dataset[left]),left] <- dataset[is.na(dataset[left]),right]
        dataset[right]<- NULL
        colnames(dataset)[colnames(dataset) == left] <- i
      }
      rm(temp_dataset)
    }
  }
  print(dataset)  
}

replacement <- function(ds){
  for (n in names(ds)){
    if (is.element("Undet", ds[,n]) == TRUE){
      x <- as.numeric(min(ds[,n], na.rm = TRUE))
      y <- x/2
      levels(ds[n]) <- c(ds[n], y)
      ds[n][ds[n] == "Undet"] <- y   
    }
  }
  (print(ds))
}

elisalog <- function(ds){
  for (n in names(ds)){
    ds <- as.data.frame(ds)
    ds[,n]
    if (NA %in% ds[,n] == TRUE){
      ds[,n][is.na(ds[,n]) != TRUE] <- "-"
      ds[,n][is.na(ds[,n]) == TRUE] <- "Unrun"
    }
  }
  print(ds) 
}


ui <- fluidPage(
   
  # Application title
  titlePanel("Results Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      h3("Choose a working directory (csv files only):"),
      shinyDirButton("folderInput","Choose Folder","Choose working directory"),
      h3("Search criteria:"),
      uiOutput("studyOutput"),
      uiOutput("typeOutput"),
      uiOutput("columnOutput"),
      radioButtons("radioInput", "Replace Undets?", 
                   choices = c("Yes, 1/2 Min" = TRUE, "No" = FALSE), 
                   selected = FALSE),
      radioButtons("radioUnrun", "Show ELISA Log?", 
                   choices = c("Yes" = TRUE, "No" = FALSE), 
                   selected = FALSE)
      
    ),
    
    mainPanel(
      dataTableOutput("results")
      )
    )
  )

# Define server logic required to draw a histogram
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
  
  df <- reactive({
    dataset <- data_merge(folderInput1())
  })
  
  
# Study selection
  output$studyOutput <- renderUI({
    ds <- df()
    studies <- unique(ds$Study)
    studies <- studies[!is.na(studies)] 
    pickerInput("studyInput", "Study", 
                choices = studies, 
                selected = studies,
                options = list(`actions-box` = TRUE), 
                multiple = TRUE)
    })

# Sample type selection
  output$typeOutput <- renderUI({
    ds <- df()
    types <- unique(ds$Type)
    types <- types[!is.na(types)] 
    pickerInput("typeInput","Sample Type", 
                choices = types,
                selected = types,
                options = list(`actions-box` = TRUE), 
                multiple = TRUE)
    })

  # Column Selection
  output$columnOutput <- renderUI({
    ds <- df()
    columns <- colnames(df())
    a1 <- columns[columns != c("Sample_ID")]
    a2 <- a1[a1 != c("Study")]
    available <- a2[a2 != c("Type")]
    pickerInput("columnInput","Column selector", 
                choices = available, 
                selected = columns[columns != "Group"], 
                options = list(`actions-box` = TRUE),
                multiple = TRUE)
    })
  
  # Filter data
  filtered <- reactive({
    ds <- df()
    columns <- c("Sample_ID","Study", "Type", input$columnInput )
      filter(ds[,columns],
            Study %in% input$studyInput & 
            Type %in% input$typeInput
        )
      })
  
  # Remove Undets with 1/2 min
  undet <- reactive({
                if (input$radioInput == TRUE){
                      ds <- replacement(filtered())
                      return(ds)
                      } else {
                      return(filtered())
                      }
                    })
  
  
  final <- reactive({
              if (input$radioUnrun == TRUE){
                      ds <- undet()
                      ds <- elisalog(ds)
                      return(ds)
                      } else {
                      return(undet())
                      }
                    })
  
# Creating the table
  ELISA <- reactive({datatable(final(),
                                  class = 'cell-border stripe',
                                  extensions = c('Buttons', 'ColReorder'),
                                  rownames= FALSE,
                                  options = list(
                                    dom = 'Bfrtlip', 
                                    lengthMenu = list(c(5, 15, 100, -1), 
                                                      c('5', '15', '100', 'All')),
                                    pageLength = 15,
                                    buttons = c('csv', 'excel', 'pdf'),
                                    colReorder = TRUE,
                                    deferRender = TRUE
                                    ))
  })


  output$results <- renderDataTable(server = FALSE, {ELISA()})

  }

# Run the application 
shinyApp(ui = ui, server = server)

