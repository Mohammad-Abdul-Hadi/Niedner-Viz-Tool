library(shiny)
library(shinyWidgets)
library(shinyBS)
library(DT)
library(sqldf)

library(shinydashboard)
library(maps)
library(dplyr)
library(leaflet)
library(shinycssloaders)
library(shinythemes)
library(datadigest)
library(rio)
library(stargazer)
library(corrplot)
library(varImp)

dd <- mtcars

elements <- read.csv(file.path("./Snaking/Snaking_tres_0.2.csv"), stringsAsFactors=FALSE)
str(elements)
snakingE1Vals <- unique(elements[,2])
snakingE2Vals <- unique(elements[,3])
snakingAlVals <- unique(elements[,4])
snakingFcVals <- unique(elements[,5])
snakingCountVals <- unique(elements[,6])
snakingLenVals <- unique(elements[,7])
snakingAmpVals <- unique(elements[,8])

Assembly_elems <- read.csv(file.path("./Assembly/Assembly.csv"), stringsAsFactors=FALSE)
str(Assembly_elems)
assemblyMEI1Vals <- unique(Assembly_elems[,2])
assemblyMEI2Vals <- unique(Assembly_elems[,3])
assemblySMIVals <- unique(Assembly_elems[,4])
assemblyMEO1Vals <- unique(Assembly_elems[,5])
assemblyMEO2Vals <- unique(Assembly_elems[,6])
assemblySMOVals <- unique(Assembly_elems[,7])
assemblyFCVals <- unique(Assembly_elems[,8])
assemblyELVals <- unique(Assembly_elems[,9])
assemblyImageVals <- unique(Assembly_elems[,10])
assemblyImage2Vals <- unique(Assembly_elems[,11])

Bunching_elems <- read.csv(file.path("./Bunching/Bunching.csv"), stringsAsFactors=FALSE)
str(Bunching_elems)
bunchingMoEIWarp <- unique(Bunching_elems[,2])
bunchingMoEIWeft <- unique(Bunching_elems[,3])
bunchingSMI <- unique(Bunching_elems[,4])
bunchingMoEOWarp <- unique(Bunching_elems[,5])
bunchingMoEOWeft <- unique(Bunching_elems[,6])
bunchingSMO <- unique(Bunching_elems[,7])
bunchingFC <- unique(Bunching_elems[,8])
bunchingAL <- unique(Bunching_elems[,9])
bunchingPS <- unique(Bunching_elems[,10])
bunchingOutput <- unique(Bunching_elems[,12])
bunchingImageF <- unique(Bunching_elems[,13])
bunchingImageS1 <- unique(Bunching_elems[,14])
bunchingImageS2 <- unique(Bunching_elems[,15])


ui <- shinyUI(fluidPage(
  
    tags$head(tags$style(
      HTML('
           #sidebar {
              background-color: #ffffff;
          }
  
          body, label, input, button, select, sidebar, header { 
            font-family: "Calibri";
            font-size: 18px;
          }')
    )),

   sidebarLayout(
      sidebarPanel(id="sidebar", style = "overflow-y:scroll; max-height: 1000px; position:relative;",
        img(src = "niedner.jpg", width = "100%"),
        br(),
        br(),
        hr(),
        
        bsCollapse(id = "sidebarCollapse", multiple = FALSE, open = NULL,
                   bsCollapsePanel("Snaking", value = 1,
                           selectInput("p", 
                                       "Output Variable:",
                                       c("Snaking Count"='o1', #Sepal.Length
                                         "Snaking Length (m)"='o2', #Sepal.Width
                                         "Snaking Amplitude (m)"='o3')
                           ),
                           selectInput("q", 
                                       "Input Variable:",
                                       c("Modulus (E0)- GPa"='i1', #Sepal.Length
                                         "Modulus (E1)- GPa"='i2', #Sepal.Width
                                         "Friction"='i3', #Petal.Length
                                         "Load (N/mm^2)"='i4' #Petal.Width
                                         )
                           ),
                           sliderInput("bins",
                                       "Number of Divisions:",
                                       min = 1,
                                       max = 10,
                                       value = 7),
                           hr(),
                           hr(),

                           shinyWidgets::sliderTextInput(inputId = "n1", 
                                                         label = "Demonstration Image Slider", 
                                                         choices = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
                                                         animate = TRUE,
                                                         grid = TRUE,
                                                         hide_min_max = FALSE),
                           hr(),
                           shinyWidgets::sliderTextInput(inputId = "n1_1", 
                                                         label = "Modulus of Elasticity - inner: (GPa)", 
                                                         choices = snakingE1Vals,
                                                         animate = TRUE,
                                                         grid = TRUE,
                                                         hide_min_max = FALSE),
                           
                           shinyWidgets::sliderTextInput(inputId = "n1_2", 
                                                         label = "Modulus of Elasticity - outer: (GPa)", 
                                                         choices = snakingE2Vals,
                                                         animate = TRUE,
                                                         grid = TRUE,
                                                         hide_min_max = FALSE),
                           
                           shinyWidgets::sliderTextInput(inputId = "n1_3", 
                                                         label = "Axial Load (Water Pressure Drop): (N/mm^2)", 
                                                         choices = snakingAlVals,
                                                         animate = TRUE,
                                                         grid = TRUE,
                                                         hide_min_max = FALSE),
                           shinyWidgets::sliderTextInput(inputId = "n1_4", 
                                                         label = "Friction Coefficient", 
                                                         choices = snakingFcVals,
                                                         animate = TRUE,
                                                         grid = TRUE,
                                                         hide_min_max = FALSE),
                           style = "info"),

                   bsCollapsePanel("Bunching",  value = 2,
                           selectInput("p2", 
                                       "Output Variable:",
                                       c("Maximum bunching amplitude (mm)"='o2_1')
                           ),
                           selectInput("q2", 
                                       "Input Variables:",
                                       c("Modulus of Elasticity – Inner – Warp (MPa)"='i2_1', 
                                         "Modulus of Elasticity – Inner – Weft (MPa)"='i2_2', 
                                         "Modulus of Elasticity – Outer – Warp (MPa)"='i2_3', 
                                         "Modulus of Elasticity – Outer – Weft (MPa)"='i2_4', 
                                         "Shear Modulus - Inner (MPa)"='i2_5', 
                                         "Shear Modulus - Outer (MPa)"='i2_6', 
                                         "Friction Coefficient"='i2_7', 
                                         "Axial Load (water pressure drop) - N/mm^2"='i2_8', 
                                         "Pre-Strain (%)"='i2_9'
                                         )
                           ),
                           sliderInput("bins2",
                                       "Number of Divisions:",
                                       min = 1,
                                       max = 10,
                                       value = 7),
                           hr(),
                           hr(),

                           shinyWidgets::sliderTextInput(inputId = "n2", 
                                                         label = "Demonstration Image Slider", 
                                                         choices = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
                                                         animate = TRUE,
                                                         grid = TRUE,
                                                  hide_min_max = FALSE),
                          hr(), 
                          shinyWidgets::sliderTextInput(inputId = "n2_1", 
                                                        label = "Modulus of Elasticity - Inner - Warp (MPa):", 
                                                        choices = bunchingMoEIWarp,
                                                        animate = TRUE,
                                                        grid = TRUE,
                                                        hide_min_max = FALSE),
                          shinyWidgets::sliderTextInput(inputId = "n2_2", 
                                                        label = "Modulus of Elasticity - Inner - Weft (MPa):", 
                                                        choices = bunchingMoEIWeft,
                                                        animate = TRUE,
                                                        grid = TRUE,
                                                        hide_min_max = FALSE),
                          shinyWidgets::sliderTextInput(inputId = "n2_3", 
                                                        label = "Modulus of Elasticity - Outer - Warp (MPa):", 
                                                        choices = bunchingMoEOWarp,
                                                        animate = TRUE,
                                                        grid = TRUE,
                                                        hide_min_max = FALSE),
                          shinyWidgets::sliderTextInput(inputId = "n2_4", 
                                                        label = "Modulus of Elasticity - Outer - Weft (MPa):", 
                                                        choices = bunchingMoEOWeft,
                                                        animate = TRUE,
                                                        grid = TRUE,
                                                        hide_min_max = FALSE),
                          shinyWidgets::sliderTextInput(inputId = "n2_5", 
                                                        label = "Shear Modulus - Inner (MPa):", 
                                                        choices = bunchingSMI,
                                                        animate = TRUE,
                                                        grid = TRUE,
                                                        hide_min_max = FALSE),
                          shinyWidgets::sliderTextInput(inputId = "n2_6", 
                                                        label = "Shear Modulus - Outer (MPa):", 
                                                        choices = bunchingSMO,
                                                        animate = TRUE,
                                                        grid = TRUE,
                                                        hide_min_max = FALSE),
                          shinyWidgets::sliderTextInput(inputId = "n2_7", 
                                                        label = "Friction Coefficient:", 
                                                        choices = bunchingFC,
                                                        animate = TRUE,
                                                        grid = TRUE,
                                                        hide_min_max = FALSE),
                          shinyWidgets::sliderTextInput(inputId = "n2_8", 
                                                        label = "Axial Load (Water Pressure Drop): (N/mm^2):", 
                                                        choices = bunchingAL,
                                                        animate = TRUE,
                                                        grid = TRUE,
                                                        hide_min_max = FALSE), 
                          shinyWidgets::sliderTextInput(inputId = "n2_9", 
                                                        label = "Pre-Strain (%):", 
                                                        choices = bunchingPS,
                                                        animate = TRUE,
                                                        grid = TRUE,
                                                        hide_min_max = FALSE), style = "info"),

                   bsCollapsePanel("Assembly",  value = 3,
                           selectInput("p3", 
                                       "Output Variable:",
                                       c("Excessive Length (m)"='o3_1')
                           ),
                           selectInput("q3", 
                                       "Input Variables:",
                                       c("Modulus of Elasticity – Inner – Warp (GPa)"='i3_1', 
                                          "Modulus of Elasticity – Inner – Weft (GPa)"='i3_2', 
                                          "Modulus of Elasticity – Outer – Warp (GPa)"='i3_3', 
                                          "Modulus of Elasticity – Outer – Weft (GPa)"='i3_4', 
                                          "Shear Modulus - Inner (MPa)"='i3_5', 
                                          "Shear Modulus - Outer (MPa)"='i3_6', 
                                          "Friction Coefficient"='i3_7'
                                         )
                           ),
                           sliderInput("bins3",
                                       "Number of Divisions:",
                                       min = 1,
                                       max = 10,
                                       value = 7),
                           hr(),
                           hr(),

                           shinyWidgets::sliderTextInput(inputId = "n3", 
                                                         label = "Demonstration Image Slider", 
                                                         choices = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20),
                                                         animate = TRUE,
                                                         grid = TRUE,
                                        hide_min_max = FALSE),
                          hr(), 
                          shinyWidgets::sliderTextInput(inputId = "n3_1",
                                                label = "Modulus of Elasticity - inner - warp (GPa):",
                                                choices = assemblyMEI1Vals,
                                                animate = TRUE,
                                                grid = TRUE,
                                                hide_min_max = FALSE), 
                          shinyWidgets::sliderTextInput(inputId = "n3_2",
                                                label = "Modulus of Elasticity - inner - weft (GPa):",
                                                choices = assemblyMEI2Vals,
                                                animate = TRUE,
                                                grid = TRUE,
                                                hide_min_max = FALSE),
                          shinyWidgets::sliderTextInput(inputId = "n3_3",
                                                label = "Modulus of Elasticity - outer - warp (GPa):",
                                                choices = assemblyMEO1Vals,
                                                animate = TRUE,
                                                grid = TRUE,
                                                hide_min_max = FALSE),
                          shinyWidgets::sliderTextInput(inputId = "n3_4",
                                                label = "Modulus of Elasticity - outer - weft (GPa):",
                                                choices = assemblyMEO2Vals,
                                                animate = TRUE,
                                                grid = TRUE,
                                                hide_min_max = FALSE),
                          shinyWidgets::sliderTextInput(inputId = "n3_5",
                                                label = "Shear Modulus - inner (MPa):",
                                                choices = assemblySMIVals,
                                                animate = TRUE,
                                                grid = TRUE,
                                                hide_min_max = FALSE),
                          shinyWidgets::sliderTextInput(inputId = "n3_6",
                                                label = "Shear Modulus - outer (MPa):",
                                                choices = assemblySMOVals,
                                                animate = TRUE,
                                                grid = TRUE,
                                                hide_min_max = FALSE),
                          shinyWidgets::sliderTextInput(inputId = "n3_7",
                                                label = "Friction Coefficient:",
                                                choices = assemblyFCVals,
                                                animate = TRUE,
                                                grid = TRUE,
                                                hide_min_max = FALSE), 
                                   style = "info"),

                   bsCollapsePanel("ML Models", value = 4,
                           checkboxInput("lr", "Linear Regression", TRUE),
                           checkboxInput("rf", "Random Forest", FALSE),
                           checkboxInput("svm", "Support Vector Machine", FALSE),
                           sliderInput(
                                "Slider1",
                                label = h3("Train/Test Split %"),
                                min = 0,
                                max = 100,
                                value = 75
                              ),
                           textOutput("cntTrain"),
                           textOutput("cntTest"),
                           br(),
                           style = "info")
        ),
        br(),
        hr(),

        selectInput("dataset", "Download a dataset:",
                    choices = c("Snaking", "Bunching", "Assembly", "Presentation")),

        # Button
        downloadButton("downloadData", "Download"),

        downloadButton("downloadFile", "Download Presentation"),
        
        br(),
        hr(),
        fileInput("file1", "Choose JPG or PNG File",
                  multiple = FALSE,
                  accept = c("image/jpeg", 
                             "image/png",
                             "image/svg+xml")),
                  placeholder = "No file selected",
                  #verbatimTextOutput("clientdataText"),
                  width = 4
        
      ),
      

      mainPanel(
        tabsetPanel(id = "tabs",
          tabPanel("Snaking", value = 1,
                   bsCollapse(id = "SnakingCollapse", multiple = TRUE, open = c(2,3, 4),
                   bsCollapsePanel("Documentation", value = 1, # "This is a panel with just demo image ",
                                   fluidPage( 
                                     fluidRow(
                                       column(12, # offset = 3, 
                                              imageOutput("myImage1", width="100%", height="100%", inline = FALSE))
                                     )
                                   ),
                                   style = "primary"),
                   bsCollapsePanel("Plot",  value = 2, # "This is a panel demo data plot",
                                   fluidPage( 
                                     fluidRow(
                                       column(6, plotOutput("distPlot")),
                                       column(6, plotOutput("distPlot2"))
                                     )
                                   ),
                                   style = "primary"),
                   bsCollapsePanel("Snaking Images", value = 3,  # "This panel one image and one table",
                                   fluidPage( 
                                     fluidRow(
                                       column(12, #offset = 3, 
                                              imageOutput("myImage1_actual", width="100%", height="100%", inline = FALSE))#,
                                       # column(6, DT::dataTableOutput("mytable_snaking"))
                                     )
                                   ),
                                   style = "primary"),
                   bsCollapsePanel("Snaking Data Table", value = 4, # "This panel contains one table.",
                                   fluidPage(
                                     fluidRow(
                                       column(6, offset = 3, DT::dataTableOutput("mytable_snaking"))
                                     )
                                   ),
                                   style = "primary"))
                   ), 

          tabPanel("Bunching",  value = 2,
                   bsCollapse(id = "SnakingCollapse", multiple = TRUE, open = c(2,3,4),
                   bsCollapsePanel("Documentation",  value = 1,  # "This is a panel with just demo image ",
                                   fluidPage( 
                                      fluidRow(
                                         column(12, imageOutput("myImage2", width="100%", height="100%", inline = FALSE))
                                       )
                                   ),
                                   style = "primary"),
                   bsCollapsePanel("Plot",  value = 2, # "This is a panel demo data plot",
                                   fluidPage( 
                                     fluidRow(
                                       column(6, plotOutput("distPlot2_1")),
                                       column(6, plotOutput("distPlot2_2"))
                                     )
                                   ),
                                   style = "primary"),
                   bsCollapsePanel("Bunching Panel", value = 3, # "This panel has three images. ",
                                   fluidPage(
                                       fluidRow(
                                         column(6, div(style = "width:session$clientData$output_myImage_2_1_width;
                                                       text-align:center;", 
                                                       imageOutput("myImage2_1"))),
                                         column(6, div(style = "width:session$clientData$output_myImage_2_2_width;
                                                       text-align:center;", 
                                                       imageOutput("myImage2_2")))
                                       ),
                                       fluidRow(
                                         column(6, offset = 3, div(style = "width:session$clientData$output_myImage_2_3_width;
                                                                   text-align:center;", 
                                                                   imageOutput("myImage2_3")))
                                       )
                                   ),
                                   style = "primary"),
                   bsCollapsePanel("Table",  value = 4, # "This panel contains one table.",
                                   fluidPage(
                                     fluidRow(
                                       column(6, offset = 3, DT::dataTableOutput("mytable_bunching"))
                                     )
                                   ),
                                   style = "primary"))
                  ),

          tabPanel("Assembly",  value = 3,
                   bsCollapse(id = "SnakingCollapse", multiple = TRUE, open = c(2,3,4),
                   bsCollapsePanel("Documentation", value = 1, # "This is a panel with just demo image ",
                                   fluidPage( 
                                     fluidRow(
                                       column(12, imageOutput("myImage3", width="100%", height="100%", inline = FALSE))
                                     )
                                   ),
                                   style = "primary"),
                   bsCollapsePanel("Plot",  value = 2, # "This is a panel demo data plot",
                                   fluidPage( 
                                     fluidRow(
                                       column(6, plotOutput("distPlot3_1")),
                                       column(6, plotOutput("distPlot3_2"))
                                     )
                                   ),
                                   style = "primary"),
                   bsCollapsePanel("Assembly Image", value = 3, # "This panel loads the assembly image ",
                                   fluidPage( 
                                     fluidRow(
                                       column(6, offset = 3, imageOutput("myImage3_actual"))
                                     )
                                   ),
                                   style = "primary"),
                   bsCollapsePanel("Table", value = 4, # "This panel contains one table.",
                                   fluidPage(
                                     fluidRow(
                                       column(6, offset = 3, DT::dataTableOutput("mytable_assembly"))
                                     )
                                   ),
                                   style = "primary"))
          ),

          tabPanel("ML Models", value = 4,
                   bsCollapse(id = "mlCollapse", multiple = TRUE, open = c(1),
                   bsCollapsePanel("Linear Regression", value = 1, # "This is a panel with just demo image ",
                                  fluidPage(
                                    box(
                                      selectInput(
                                        "SelectX",
                                        label = "Select variables:",
                                        choices = names(mtcars),
                                        multiple = TRUE,
                                        selected = names(mtcars)
                                      ),
                                      solidHeader = TRUE,
                                      width = "3",
                                      status = "primary",
                                      title = "X variable"
                                    ),
                                    box(
                                      selectInput("SelectY", label = "Select variable to predict:", choices = names(mtcars)),
                                      solidHeader = TRUE,
                                      width = "3",
                                      status = "primary",
                                      title = "Y variable"
                                    )
                                  ),
                                  
                                  fluidPage(  
                                    
                                      tabBox(
                                      id = "tabset1",
                                      height = "1000px",
                                      width = 12,
                                    
                                      tabPanel("Data",
                                              box(withSpinner(DTOutput(
                                                "Data"
                                              )), width = 12)),
                                      tabPanel(
                                        "Data Summary",
                                        box(withSpinner(verbatimTextOutput("Summ")), width = 6),
                                        box(withSpinner(verbatimTextOutput("Summ_old")), width = 6)
                                      ),
                                      

                                      tabPanel("Plots",
                                              box(withSpinner(plotOutput(
                                                "Corr"
                                              )), width = 12)),
                                      #box(withSpinner(verbatimTextOutput("CorrMatrix")), width = 12),
                                      tabPanel(
                                        "Model",
                                        box(
                                          withSpinner(verbatimTextOutput("Model")),
                                          width = 12, #6,
                                          title = "Model Summary"
                                        ),

                                      ),
                                      #textOutput("correlation_accuracy"),
                                      tabPanel(
                                        "Prediction",
                                        box(withSpinner(plotOutput("Prediction")), width = 6, title = "Best Fit Line"),
                                        box(withSpinner(plotOutput("residualPlots")), width = 6, title = "Diagnostic Plots")
                                      )
                                    )
                                  ),
                                  style = "primary"),
                   bsCollapsePanel("Random Forest",  value = 2, # "This is a panel demo data plot",
                                   
                                   style = "primary"),
                   bsCollapsePanel("Support Vector Machine", value = 3,  # "This panel one image and one table",
                                   
                                   style = "primary"))
                   ),

          tabPanel("Browsed Images",  value = 5, imageOutput("browsedImage"))

        )
      )
   )
))


server <- shinyServer(function(input, output, session) {

    pdf_fileName <- "Tool-Manual.pdf"
    pdf_filePath <- "./presentation"

    output$downloadFile <- downloadHandler(
      filename = function() {
        pdf_fileName # default file name use by browser, it could be different
      },
      content = function(file) {
        file.copy(file.path(pdf_filePath, pdf_fileName), file)
      }
    )
  
    cdata <- session$clientData
    
    observeEvent(input$tabs, ({
      updateCollapse(session, "sidebarCollapse", open = input$tabs)
    }))
    
    observeEvent(input$sidebarCollapse, ({
      choice <- input$sidebarCollapse
      updateTabsetPanel(session, "tabs", selected = choice)
    }))
    
    output$clientdataText <- renderText({
      paste(      session$clientData$output_myImage2_1_width,
                  session$clientData$output_myImage2_1_height,
                  session$clientData$output_myImage2_2_width,
                  session$clientData$output_myImage2_2_height,
                  session$clientData$output_myImage2_3_width,
                  session$clientData$output_myImage2_3_height,
                  input$sidebarCollapse ,sep = ' , ')
    })
   
    output$distPlot <- renderPlot({

       if(input$p=='o1'){
         i<-6
         xLabel<- "Snaking Count"
       }
       if(input$p=='o2'){
         i<-7
         xLabel<- "Global Snaking Length (m)"
       }
       if(input$p=='o3'){
         i<-8
         xLabel<- "Maximum Snaking Amplitude (m)"
       }
       x    <- elements[, i] 

      bins <- seq(min(x), max(x), length.out = input$bins + 1)
      
      hist(x,
           main="Histogram for Sanking Data", 
           xlab=xLabel, 
           breaks = bins, 
           col = 'RoyalBlue', 
           border = 'white')
    })

    output$distPlot2_1 <- renderPlot({
      xLabel<- "Maximum bunching amplitude (mm)"
      x    <- Bunching_elems[, 12] 
      bins <- seq(min(x), max(x), length.out = input$bins2 + 1)

      hist(x,
           main="Histogram for Bunching Data", 
           xlab=xLabel, 
           breaks = bins, 
           col = 'RoyalBlue', 
           border = 'white')
    })

    output$distPlot3_1 <- renderPlot({
      xLabel<- "Excessive Length (m)"
      x    <- Bunching_elems[, 9] 
      bins <- seq(min(x), max(x), length.out = input$bins3 + 1)
      
      hist(x,
           main="Histogram for Assembly Data", 
           xlab=xLabel, 
           breaks = bins, 
           col = 'RoyalBlue', 
           border = 'white')
    })
    
    output$distPlot2 <- renderPlot({
     #referring input p in ui.r as input$p
     if(input$q=='i1'){
       j<-2
       xLabel<- "Modulus (E0)- GPa"
     }
     if(input$q=='i2'){
       j<-3
       xLabel<- "Modulus (E1)- GPa"
     }
     if(input$q=='i3'){
       j<-4
       xLabel<- "Friction"
     }
     if(input$q=='i4'){
       j<-5
       xLabel<-"Load (N/mm^2)"
     }

      if(input$p=='o1'){
        elements$out <- elements$count
        yLabel <- "Sanking Count"
      }
      if(input$p=='o2'){
        elements$out <- elements$len
        yLabel <- "Global Sanking Length (m)"
      }
      if(input$p=='o3'){
        elements$out <- elements$amp
        yLabel <- "Maximum Sanking Amplitude (m)"
      }
      

     boxplot(elements$out~elements[, j],
          data = elements,
          main="BoxPlot for Snaking Data", 
          col = "RoyalBlue",
          xlab = xLabel,
          ylab = yLabel,
          boxwex = 0.3,
          notch = T,
          frame=F)
    })    

    output$distPlot2_2 <- renderPlot({
     if(input$q2=='i2_1'){
       j<-2
       xLabel<- "Modulus of Elasticity – Inner – Warp (MPa)"
     }
     if(input$q2=='i2_2'){
       j<-3
       xLabel<- "Modulus of Elasticity – Inner – Weft (MPa)"
     }
     if(input$q2=='i2_3'){
       j<-5
       xLabel<- "Modulus of Elasticity – Outer – Warp (MPa)"
     }
     if(input$q2=='i2_4'){
       j<-6
       xLabel<-"Modulus of Elasticity – Outer – Weft (MPa)"
     }
     if(input$q2=='i2_5'){
       j<-4
       xLabel<-"Shear Modulus - Inner (MPa)"
     }
     if(input$q2=='i2_6'){
       j<-7
       xLabel<-"Shear Modulus - Outer (MPa)"
     }
     if(input$q2=='i2_7'){
       j<-8
       xLabel<-"Friction Coefficient"
     }
     if(input$q2=='i2_8'){
       j<-9
       xLabel<-"Axial Load (water pressure drop) (N/mm^2)"
     }
     if(input$q2=='i2_9'){
       j<-10
       xLabel<-"Pre-Strain (%)"
     }

    Bunching_elems$out <- Bunching_elems$Bunching..mm.
    yLabel <- "Maximum bunching amplitude (mm)"

     boxplot(Bunching_elems$out~Bunching_elems[, j],
          data = Bunching_elems,
          main="BoxPlot for Bunching Data",
          col = "RoyalBlue",
          xlab = xLabel,
          ylab = yLabel,
          boxwex = 0.3,
          notch = T,
          frame=F)
    })
    
    output$distPlot3_2 <- renderPlot({
     if(input$q3=='i3_1'){
       j<-2
       xLabel<- "Modulus of Elasticity – Inner – Warp (GPa)"
     }
     if(input$q3=='i3_2'){
       j<-3
       xLabel<- "Modulus of Elasticity – Inner – Weft (GPa)"
     }
     if(input$q3=='i3_3'){
       j<-5
       xLabel<- "Modulus of Elasticity – Outer – Warp (GPa)"
     }
     if(input$q3=='i3_4'){
       j<-6
       xLabel<-"Modulus of Elasticity – Outer – Weft (GPa)"
     }
     if(input$q3=='i3_5'){
       j<-4
       xLabel<-"Shear Modulus - Inner (MPa)"
     }
     if(input$q3=='i3_6'){
       j<-7
       xLabel<-"Shear Modulus - Outer (MPa)"
     }
     if(input$q3=='i3_7'){
       j<-8
       xLabel<-"Friction Coefficient"
     }

    Assembly_elems$out <- Assembly_elems$el
    yLabel <- "Excessive Length (m)"

     boxplot(Assembly_elems$out~Assembly_elems[, j],
          data = Bunching_elems,
          main="BoxPlot for Assembly Data",
          col = "RoyalBlue",
          xlab = xLabel,
          ylab = yLabel,
          boxwex = 0.3,
          notch = T,
          frame=F)
    })
    
    output$myImage1 <- renderImage({
     # width  <- session$clientData$output_myImage1_width
     # height <- session$clientData$output_myImage1_height

     filename <- normalizePath(file.path('./images',
                                         paste('image', input$n1, '.jpg', sep='')))
     

     list(src = filename,
          width = "100%",
          alt = paste("Image number:", input$n))
    }, deleteFile = FALSE)
    
    output$myImage1_actual <- renderImage({
     
     capture <- subset(elements, (e1 == input$n1_1 & e2 == input$n1_2 & load == input$n1_3 & fc == input$n1_4), select=c(image))
     
     filename <- normalizePath(file.path('./Snaking/Pictures', capture[1,1]))

     list(src = filename,
          width = "100%",
          #height = "100%",
          alt = paste("Image name:",
                      capture[1,1],
                      'could not be found', sep =' '))
     
    }, deleteFile = FALSE)

    output$myImage3_actual <- renderImage({

     capture <- subset(Assembly_elems, (mei1 == input$n3_1 & mei2 == input$n3_2 & meo1 == input$n3_3 & meo2 == input$n3_4 & smi == input$n3_5 & smo == input$n3_6 & 
                                        fc == input$n3_7), select=c(SI))
     image_name <- paste(capture[1,1], "_Image.jpg", sep="")
     filename <- normalizePath(file.path('./Assembly/Images', image_name))
     
     list(src = filename,
          width = "100%",
          #height = "100%",
          alt = paste("Image name:",
                      image_name,
                      'could not be found', sep =' '))
     
    }, deleteFile = FALSE)
    
    output$mytable_snaking = DT::renderDataTable({
     count_out <- subset(elements, (e1 == input$n1_1 & e2 == input$n1_2 & load == input$n1_3 & fc == input$n1_4), select=c(count))
     len_out <- subset(elements, (e1 == input$n1_1 & e2 == input$n1_2 & load == input$n1_3 & fc == input$n1_4), select=c(len))
     amp_out <- subset(elements, (e1 == input$n1_1 & e2 == input$n1_2 & load == input$n1_3 & fc == input$n1_4), select=c(amp))
     my_data <- data.frame(
       Variables =c("Modulus of elasticity - inner (GPa)", "Modulus of elasticity - outer (GPa)", "Axial Load (N/mm^2)", "Friction Coefficient", "Snaking (count)", "Global Snaking Length (m)", "Maximum Snaking Amplitude (m)"),
       Values = c(input$n1_1, input$n1_2, input$n1_3, input$n1_4, count_out[1,1], len_out[1,1], amp_out[1,1])
     )
     
     datatable(my_data, options = list(dom = 't')) %>% formatStyle(
       'Variables',
       target = 'row',
       backgroundColor = styleEqual(c("Snaking (count)", "Global Snaking Length (m)", "Maximum Snaking Amplitude (m)"), c('lightblue', 'lightblue', 'lightblue'))
     )
    })

    datasetInput <- reactive({
        switch(input$dataset,
              "Sanking" = elements,
              "Bunching" = Bunching_elems,
              "Assembly" = Assembly_elems)
      })

    output$downloadData <- downloadHandler(
      filename = function() {
        paste(input$dataset, ".csv", sep = "")
      },
      content = function(file) {
        write.csv(datasetInput(), file, row.names = FALSE)
      }
    )

    output$mytable_assembly = DT::renderDataTable({
     el_out <- subset(Assembly_elems, (mei1 == input$n3_1 & mei2 == input$n3_2 & meo1 == input$n3_3 & meo2 == input$n3_4 & smi == input$n3_5 & smo == input$n3_6 & 
                                        fc == input$n3_7), select=c(el))
     my_data <- data.frame(
       Variables =c("Modulus of elasticity – inner – warp (GPa)", "Modulus of elasticity – inner – weft (GPa)", "Modulus of elasticity – outer – warp (GPa)", "Modulus of elasticity – outer – weft (GPa)", "Shear modulus - inner (MPa)", "Shear modulus - outer (MPa)", "Friction coefficient", "Excessive Length (m)"),
       Values = c(input$n3_1, input$n3_2, input$n3_3, input$n3_4, input$n3_5, input$n3_6, input$n3_7, el_out[1,1])
     )
     
     datatable(my_data, options = list(dom = 't')) %>% formatStyle(
       'Variables',
       target = 'row',
       backgroundColor = styleEqual(c("Excessive Length (m)"), c('lightblue'))
     )
    })
    
    output$mytable_bunching = DT::renderDataTable({
     bunching_out <- subset(Bunching_elems, (MoE.Inner.Warp == input$n2_1 & MoE.Inner.Weft == input$n2_2 & MoE.Outer.Warp == input$n2_3 & MoE.Outer.Weft == input$n2_4 & SM.Inner == input$n2_5 & SM.Outer == input$n2_6 & 
                                        Friction.Coefficient == input$n2_7 & Axial.Load == input$n2_8 & Pre.strain == input$n2_9), select=c(Bunching..mm.))
     my_data <- data.frame(
       Variables =c("Modulus of Elasticity – Inner – Warp (MPa)", "Modulus of Elasticity – Inner – Weft (MPa)", "Modulus of Elasticity – Outer – Warp (MPa)", "Modulus of Elasticity – Outer – Weft (MPa)", "Shear Modulus - Inner (MPa)", "Shear Modulus - Outer (MPa)", "Friction Coefficient", "Axial Load (water pressure drop) (N/mm^2)", "Pre-strain (%)", "Maximum bunching amplitude (mm)"),
       Values = c(input$n2_1, input$n2_2, input$n2_3, input$n2_4, input$n2_5, input$n2_6, input$n2_7, input$n2_8, input$n2_9, bunching_out[1,1])
     )
     
     datatable(my_data, options = list(dom = 't')) %>% formatStyle(
       'Variables',
       target = 'row',
       backgroundColor = styleEqual(c("Maximum bunching amplitude (mm)"), c('lightblue'))
     )
    })
    
    
    output$myImage2 <- renderImage({
     # width  <- session$clientData$output_myImage2_width
     # height <- session$clientData$output_myImage2_height
     
     # When input$n is 3, filename is ./images/image3.png
     filename <- normalizePath(file.path('./images',
                                         paste('image', input$n2, '.jpg', sep='')))
     
     # Return a list containing the filename and alt text
     list(src = filename,
          width = "100%",
          # height = height,
          alt = paste("Image number", input$n2))
     
    }, deleteFile = FALSE)
    
    output$myImage2_1 <- renderImage({
     #width  <- session$clientData$output_myImage3_actual_width
     #height <- session$clientData$output_myImage3_actual_height

     capture <- subset(Bunching_elems, (MoE.Inner.Warp == input$n2_1 & MoE.Inner.Weft == input$n2_2 & MoE.Outer.Warp == input$n2_3 & MoE.Outer.Weft == input$n2_4 & SM.Inner == input$n2_5 & SM.Outer == input$n2_6 & 
                                        Friction.Coefficient == input$n2_7 & Axial.Load == input$n2_8 & Pre.strain == input$n2_9), select=c(sample))
     image_name <- paste(capture[1,1], "_SideView_1.jpg", sep="")
     filename <- normalizePath(file.path('./Bunching/Images', image_name))
     
     # Return a list containing the filename and alt text
     list(src = filename,
          width = "100%",
          #height = "100%",
          alt = paste("Image name:",
                      image_name,
                      'could not be found', sep =' '))
     
    }, deleteFile = FALSE)
    
    
    output$myImage2_2 <- renderImage({

     capture <- subset(Bunching_elems, (MoE.Inner.Warp == input$n2_1 & MoE.Inner.Weft == input$n2_2 & MoE.Outer.Warp == input$n2_3 & MoE.Outer.Weft == input$n2_4 & SM.Inner == input$n2_5 & SM.Outer == input$n2_6 & 
                                        Friction.Coefficient == input$n2_7 & Axial.Load == input$n2_8 & Pre.strain == input$n2_9), select=c(sample))
     image_name <- paste(capture[1,1], "_SideView_2.jpg", sep="")
     filename <- normalizePath(file.path('./Bunching/Images', image_name))
     
     # Return a list containing the filename and alt text
     list(src = filename,
          width = "100%",
          #height = "100%",
          alt = paste("Image name:",
                      image_name,
                      'could not be found', sep =' '))
     
    }, deleteFile = FALSE)
    
    output$myImage2_3 <- renderImage({

     capture <- subset(Bunching_elems, (MoE.Inner.Warp == input$n2_1 & MoE.Inner.Weft == input$n2_2 & MoE.Outer.Warp == input$n2_3 & MoE.Outer.Weft == input$n2_4 & SM.Inner == input$n2_5 & SM.Outer == input$n2_6 & 
                                        Friction.Coefficient == input$n2_7 & Axial.Load == input$n2_8 & Pre.strain == input$n2_9), select=c(sample))
     image_name <- paste(capture[1,1], "_FrontView.jpg", sep="")
     filename <- normalizePath(file.path('./Bunching/Images', image_name))
     
     # Return a list containing the filename and alt text
     list(src = filename,
          width = "100%",
          #height = "100%",
          alt = paste("Image name:",
                      image_name,
                      'could not be found', sep =' '))
     
    }, deleteFile = FALSE)
    
    
    output$myImage3 <- renderImage({
      # width  <- session$clientData$output_myImage3_width
      # height <- session$clientData$output_myImage3_height
     
     filename <- normalizePath(file.path('./images',
                                         paste('image', input$n3, '.jpg', sep='')))
     
     # Return a list containing the filename and alt text
     list(src = filename,
          width = "100%",
          # height = height,
          alt = paste("Image number", input$n))
     
    }, deleteFile = FALSE)
    
    
    output$browsedImage <- renderImage({
     
     inFile <- input$file1
     capture <- subset(elements, (e1 == input$n1_1 & e2 == input$n1_2 & load == input$n1_3 & fc == input$n1_4), select=c(image))
     filename <- normalizePath(file.path('./Snaking/Pictures', capture[1,1]))
     
     if(is.null(inFile))
       inFile$datapath <- filename
     
     # Return a list containing the filename and alt text
     list(src = inFile$datapath,
          alt = paste("Random Image"))
     
    }, deleteFile = FALSE)
    
    output$myImageStat <- renderImage({
     width  <- session$clientData$output_myImageStat_width
     height <- session$clientData$output_myImageStat_height
     
     # When input$n is 3, filename is ./images/image3.png
     filename <- normalizePath(file.path('./images/stat_analysis',
                                         paste('bunching.png')))
     
     # Return a list containing the filename and alt text
     list(src = filename,
          width = width,
          height = height,
          alt = paste("Image name: bunching.png Not generated"))
     
    }, deleteFile = FALSE)
    
    output$selected_var <- renderText({ 
     input$file1$datapath
    })

    session$onSessionEnded(function() {
        stopApp()
    })

##--------------------------------------------------------------------------------------------
##-------------------------Dynamic report generation-------------------------------------------
  output$report <- downloadHandler(
    # For PDF output, change this to "report.pdf"
    filename = paste("report", sep = ".", "html"),
    content = function(file) {
      # Copy the report file to a temporary directory before processing it, in
      # case we don't have write permissions to the current working dir (which
      # can happen when deployed).
      tempReport <- file.path(tempdir(), "report.Rmd")
      file.copy("report.Rmd", tempReport, overwrite = TRUE)
      
      rmarkdown::render(tempReport,
                        output_file = file,
                        
                        envir = new.env(parent = globalenv()))
    }
  )
  
  InputDataset <- reactive({
    mtcars
  })
  
  
  InputDataset_model <- reactive({
    if (is.null(input$SelectX)) {
      dt <- mtcars
    }
    else{
      dt <- mtcars[, c(input$SelectX)]
    }
    
  })
  
  observe({
    lstname <- names(InputDataset())
    updateSelectInput(session = session,
                      inputId = "SelectY",
                      choices = lstname)
  })
  
  splitSlider <- reactive({
    input$Slider1 / 100
  })

  output$Summ <-
    renderPrint(
      stargazer(
        InputDataset(),
        type = "text",
        title = "Descriptive statistics",
        digits = 1,
        out = "table1.txt"
      )
    )

  output$Summ_old <- renderPrint(summary(InputDataset()))
  output$structure <- renderPrint(str(InputDataset()))
  
  set.seed(100)  # setting seed to reproduce results of random sampling
  trainingRowIndex <-
    reactive({
      sample(1:nrow(InputDataset_model()),
             splitSlider() * nrow(InputDataset_model()))
    })# row indices for training data
  
  trainingData <- reactive({
    tmptraindt <- InputDataset_model()
    tmptraindt[trainingRowIndex(), ]
  })
  
  testData <- reactive({
    tmptestdt <- InputDataset_model()
    tmptestdt[-trainingRowIndex(),]
  })
  
  
  
  output$cntTrain <-
    renderText(paste("Train Data:", NROW(trainingData()), "records"))
  output$cntTest <-
    renderText(paste("Test Data:", NROW(testData()), "records"))
  
  output$Data <- renderDT(InputDataset())
  
  
  cormat <- reactive({
    round(cor(InputDataset()), 1)
  })

  output$Corr <-
    renderPlot(corrplot(
      cormat(),
      type = "lower",
      order = "hclust",
      method = "number"
    ))
  
  correlationMatrix <- reactive({
    cor(InputDataset())
  })
  output$CorrMatrix <-
    renderPrint(round(as.data.frame(correlationMatrix())), 4)
  
  
  
  #Code section for Linear Regression-----------------------------------------------------------------------------
  
  f <- reactive({
    as.formula(paste(input$SelectY, "~."))
  })
  
 
  Linear_Model <- reactive({
    lm(f(), data = trainingData())
  })
  
  output$Model <- renderPrint(summary(Linear_Model()))
  output$Model_new <-
    renderPrint(
      stargazer(
        Linear_Model(),
        type = "text",
        title = "Model Results",
        digits = 1,
        out = "table1.txt"
      )
    )
  
  Importance <- reactive({
    varImp(Linear_Model(), scale = FALSE)
  })
  
  tmpImp <- reactive({
    #varImp(Linear_Model())
    imp <- as.data.frame(varImp(Linear_Model()))
    imp <- data.frame(overall = imp$Overall,
                      names   = rownames(imp))
    imp[order(imp$overall, decreasing = T),]
    
  })
  
  output$ImpVar <- renderPrint(tmpImp())
  
  price_predict <- reactive({
    predict(Linear_Model(), testData())
  })
  
  tmp <- reactive({
    tmp1 <- testData()
    tmp1[, c(input$SelectY)]
  })
  
  
  actuals_preds <-
    reactive({
      data.frame(cbind(actuals = tmp(), predicted = price_predict()))
    })
  
  Fit <-
    reactive({
      (
        plot(
          actuals_preds()$actuals,
          actuals_preds()$predicted,
          pch = 16,
          cex = 1.3,
          col = "blue",
          main = "Best Fit Line",
          xlab = "Actual",
          ylab = "Predicted"
        )
      )
    })
  
  output$Prediction <- renderPlot(Fit())
  
  output$residualPlots <- renderPlot({
    par(mfrow = c(2, 2)) # Change the panel layout to 2 x 2
    plot(Linear_Model())
    par(mfrow = c(1, 1)) # Change back to 1 x 1
    
  })
  
  output$digest <- renderExplorer({
    
    explorer(data = dd$data, demo = F)
    #codebook(mtcars) 
  })

})

# Run the application 
shinyApp(ui = ui, server = server)