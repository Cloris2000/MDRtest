library(shiny)
library(shinythemes)

ui <- fluidPage(theme = shinytheme("cerulean"),
  # App title
  titlePanel("MDRClassifier: A tool for analyzing and classifying multi-drug
              resistance"),
    navbarPage("Let's get started",
      tabPanel(icon("home"),

        # Sidebar panel for inputs
        sidebarPanel(

          selectInput(inputId="sampleType",
                      label="Sample Type",
                      choices=c("Single Isolate",
                                "Whole Isolates",
                                "Single Isolate from RSI",
                                "Whole Isolates from RSI",
                                "PCA Analysis")),

          # Single isolate graph
          conditionalPanel(condition="input.sampleType == 'Single Isolate'",
                           textInput(inputId="sampleID",
                                       label="Sample ID",
                                       value="GFBCEDDN_01248"),
                           textInput(inputId="antibioticColName",
                                     label="Column name of antimicrobial agents",
                                     value="Antimicrobial.Agent")),
          # Whole isolates graph
          conditionalPanel(condition="input.sampleType == 'Whole Isolates'",
                           textInput(inputId="antibioticColName",
                                     label="Column name of antimicrobial agents",
                                     value="Antimicrobial.Agent")),

          # Single isolate from RSI
          conditionalPanel(condition="input.sampleType == 'Single Isolate from RSI'",
                           textInput(inputId="sampleRSI",
                                     label="Sample ID",
                                     value="PA1387"),
                           selectInput(inputId="criteria",
                                       label="Choose criteria for MDR classification",
                                       choices=c("ECDC Criteria", "Re-defined Criteria for Incomplete Tests")),
                           textInput(inputId="totalCat",
                                     label="Total number of antimicrobial categories for this species",
                                     value=8)),

          # Whole isolates from RSI
          conditionalPanel(condition="input.sampleType == 'Whole Isolates from RSI'",
                           selectInput(inputId="criteria",
                                       label="Choose criteria for MDR classification",
                                       choices=c("ECDC Criteria", "Re-defined Criteria for Incomplete Tests")),
                           textInput(inputId="totalCat",
                                     label="Total number of antimicrobial categories for this species",
                                     value=8)),
          # PCA Analysis
          conditionalPanel(condition="input.sampleType == 'PCA Analysis'",
                           selectInput(inputId="plotType",
                                       label="Choose the type for PCA plot",
                                       choices=c("individual", "variables", "predict")),
                           textInput(inputId="newDataColName",
                                     label="Enter the column names of new data for prediction",
                                     value="Specimen number,Age,Isolate number"),
                           textInput(inputId="newData",
                                     label="Enter new data for prediction",
                                     value="1367,30,1367"),
                           ),
          ),


        # Main panel for displaying outputs
        mainPanel(
          plotOutput('plot')
        )
      ),
    tabPanel("Help",
             fluidRow(column(width=2),
                      column(
                        h4(p("Analysis Guide",style="color:black;text-align:center")),
                        width=8,style="background-color:lavender;border-radius: 10px")),
             br(),
             fluidRow(column(width=2,icon("hand-point-right","fa-5x"),align="center"),
                      column(width=10,
                        p("There are five tab in for analysis multi-drug resistance in this shiny app.",style="color:black;text-align:justify"),
                        br(),
                        p("Single Isolate: The user should input Sample_ID of a specific isolate and the column name of antimicrobial agents in the data provided.
                          The output is the classification of this specific isolate and the barplot of the distribution of MDR categories in this data.",style="color:black;text-align:justify"),
                        br(),
                        p("Whole Isolte: The user should input the column name of antimicrobial agents in data provided. The output shows the distribution of MDR categories in this data.",style="color:black;text-align:justify"),
                        br(),
                        p("Single Isolate from RSI: The user should input the Sample_ID of a specific isolate, choose the criteria for classification (if not all antimicrobial agents are tested, use re-defined criteria),
                          enter the total number of antimicrobial categories in ECDC criteria for this species.",
                          style="color:black;text-align:justify"),
                        br(),
                        p("Whole Isolates from RSI: The user should choose the criteria for classification (if not all antimicrobial agents are tested, use re-defined criteria) and
                          enter the total number of antimicrobial categories in ECDC criteria for this species.",style="color:black;text-align:justify"),
                        br(),
                        p("PCA Analysis: The user should choose the type of plot of the output. For individual PCA plot,
                         using individual tab. For visualizing relationship between variables, using
                         variables tab. For predict new data and its relationship with old data, using
                         predict tab.",style="color:black;text-align:justify"),
                        ),
             br(),)


    )
    )
)

server <- function(input, output) {
  MDRdata <- reactive({
    data("DrugResistance")
    as.data.frame(MDRClassifier::DrugResistance[1:100,])
  })
  RSIdata <- reactive({
    data("RSI_table")
    as.data.frame(MDRClassifier::RSI_table)
  })
  pcadata <- reactive({
    data("pca_data")
    as.data.frame(MDRClassifier::pca_data[c(1:17), c(2,9,16)])
  })
  output$plot <- renderPlot({
    if(input$sampleType == 'Single Isolate') {
      category <- MDRClassifier::classifyMDR(MDRdata(),input$sampleID,input$antibioticColName)
      df <- MDRClassifier::classifyAllMDR(MDRdata(),input$antibioticColName)
      p <- MDRClassifier::MDRPlot(df)
      mtext(paste(input$sampleID,category, sep=' '), side = 3, line = 0, outer = FALSE)
     }
    else if(input$sampleType == 'Whole Isolates') {
      df <- MDRClassifier::classifyAllMDR(MDRdata(),input$antibioticColName)
      p <- MDRClassifier::MDRPlot(df)
      }
    else if(input$sampleType == 'Single Isolate from RSI') {
      category <- MDRClassifier::classifyMDRfromRSI(RSIdata(), input$sampleRSI, as.numeric(input$totalCat))
      df <- MDRClassifier::classifyAllMDRfromRSI(RSIdata(),as.numeric(input$totalCat))
      p <- MDRClassifier::MDRPlot(df)
      mtext(paste(input$sampleRSI, category, sep=" "), side = 3, line = 0, outer = FALSE)
    }
    else if(input$sampleType == 'Whole Isolates from RSI'){
      df <- MDRClassifier::classifyAllMDRfromRSI(RSIdata(),as.numeric(input$totalCat))
      p <- MDRClassifier::MDRPlot(df)
    }
    else if(input$sampleType == 'PCA Analysis'){
      if(input$plotType == 'individual'){
        p <- MDRClassifier::plotPCA(pcadata(), new_data=NULL,'individual')
      }
      else if(input$plotType == 'variables'){
        p <- MDRClassifier::plotPCA(pcadata(), new_data=NULL,'variables')
      }
      else if(input$plotType == 'predict'){
        new_data_col <- strsplit(input$newDataColName, split=",")[[1]]
        new_data <- strsplit(input$newData, split=",")[[1]]
        col_lst <- c()
        data_lst <- c()
        for (i in 1:length(new_data_col)){
          col_lst <- c(col_lst, new_data_col[i])
          data_lst <- c(data_lst, as.numeric(new_data[i]))
         }
        df <- t(as.matrix(data_lst))
        colnames(df) <- col_lst
        p <- MDRClassifier::plotPCA(pcadata(), df, 'predict')
      }
    }
    print(p)
  })
}

shinyApp(ui, server)
