shinyUI(fluidPage(
  
  # Application title
  titlePanel("Assignment 3-Yan Li"),
  checkboxInput(inputId = "Parallel", label = "Use parallel processing", value = TRUE),
  tabsetPanel(
    tabPanel("Data",
             verbatimTextOutput("DataSummary"),
             fluidRow(
               column(width = 4,
                      sliderInput("Multiplier", "IQR multiplier", min = 0, max = 10, step = 0.1, value = 1.5)
               ),
               column(width = 3,
                      checkboxInput(inputId = "Normalise", label = "Standardise chart", value = TRUE)
               )
             ),
             plotOutput("BoxPlots"),
             plotOutput("Missing"),
             plotOutput("Corr"),
             DT::dataTableOutput("Table")
    ),
    tabPanel("Split",
             sliderInput("Split", "Train proportion", min = 0, max = 1, value = 0.8),
             verbatimTextOutput("SplitSummary")
    ),
    tabPanel("NULL Model",
             fluidRow(
               column(width = 4, selectizeInput(inputId = "NullPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = "knnimpute")),
               column(width = 1, actionButton(inputId = "NullGo", label = "Train", icon = icon("play")))
             ),
             verbatimTextOutput("NullModelSummary2")
    ),
    tabPanel("GLMnet Model",
             verbatimTextOutput("GlmnetModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "GlmnetPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy"))),
               column(width = 1, actionButton(inputId = "GlmnetGo", label = "Train", icon = icon("play")))
             ),
             tags$h5("Best tuning parameters:"),
             tableOutput("GlmnetModelSummary1"),
             hr(),
             plotOutput("GlmnetModelPlots"),
             verbatimTextOutput("GlmnetModelSummary2")
    ),
    tabPanel("PLS Model",
             verbatimTextOutput("PlsModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "PlsPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute","dummy", 'center', 'scale'))),
               column(width = 1, actionButton(inputId = "PlsGo", label = "Train", icon = icon("play")))
             ),
             tags$h5("Best tuning parameters:"),
             tableOutput("PlsModelSummary1"),
             hr(),
             plotOutput("PlsModelPlots"),
             verbatimTextOutput("PlsModelSummary2")
    ),
    tabPanel("Rpart Model",
             verbatimTextOutput("RpartModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "RpartPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c('knnimpute', 'center', 'scale'))),
               column(width = 1, actionButton(inputId = "RpartGo", label = "Train", icon = icon("play")))
             ),
             tags$h5("Best tuning parameters:"),
             tableOutput("RpartModelSummary1"),
             hr(),
             plotOutput("RpartModelPlots"),
             plotOutput("RpartModelTree"),
             verbatimTextOutput("RpartModelSummary2")
             
    ),
    tabPanel("Lasso Model",
             verbatimTextOutput("lassoModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "lassoPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute", "dummy"))),
               column(width = 1, actionButton(inputId = "lassoGo", label = "Train", icon = icon("play")))
             ),
             tags$h5("Best tuning parameters:"),
             tableOutput("lassoModelSummary1"),
             hr(),
             plotOutput("lassoModelPlots"),
             verbatimTextOutput("lassoModelSummary2")
             
    ),
    tabPanel("Ridge Model",
             verbatimTextOutput("ridgeModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "ridgePreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute", "dummy"))),
               column(width = 1, actionButton(inputId = "ridgeGo", label = "Train", icon = icon("play")))
             ),
             tags$h5("Best tuning parameters:"),
             tableOutput("ridgeModelSummary1"),
             hr(),
             plotOutput("ridgeModelPlots"),
             verbatimTextOutput("ridgeModelSummary2")
             
    ),
    tabPanel("SvmLinear3 Model",
             verbatimTextOutput("svmLinear3ModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "svmLinear3Preprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute", "dummy"))),
               column(width = 1, actionButton(inputId = "svmLinear3Go", label = "Train", icon = icon("play")))
             ),
             tags$h5("Best tuning parameters:"),
             tableOutput("svmLinear3ModelSummary1"),
             hr(),
             plotOutput("svmLinear3ModelPlots"),
             verbatimTextOutput("svmLinear3ModelSummary2")
             
    ),
    tabPanel("Qrf Model",
             verbatimTextOutput("qrfModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "qrfPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute", "dummy"))),
               column(width = 1, actionButton(inputId = "qrfGo", label = "Train", icon = icon("play")))
             ),
             tags$h5("Best tuning parameters:"),
             tableOutput("qrfModelSummary1"),
             hr(),
             plotOutput("qrfModelPlots"),
             verbatimTextOutput("qrfModelSummary2")
             
    ),
    tabPanel("Glmboost Model",
             verbatimTextOutput("glmboostModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "glmboostPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c())),
               column(width = 1, actionButton(inputId = "glmboostGo", label = "Train", icon = icon("play")))
             ),
             tags$h5("Best tuning parameters:"),
             tableOutput("glmboostModelSummary1"),
             hr(),
             plotOutput("glmboostModelPlots"),
             verbatimTextOutput("glmboostModelSummary2")
             
    ),
    tabPanel("Cubist Model",
             verbatimTextOutput("cubistModelSummary0"),
             fluidRow(
               column(width = 4, selectizeInput(inputId = "cubistPreprocess", label = "Pre-processing", choices = ppchoices,  multiple = TRUE, selected = c("knnimpute", "dummy"))),
               column(width = 1, actionButton(inputId = "cubistGo", label = "Train", icon = icon("play")))
             ),
             tags$h5("Best tuning parameters:"),
             tableOutput("cubistModelSummary1"),
             hr(),
             plotOutput("cubistModelPlots"),
             verbatimTextOutput("cubistModelSummary2")
             
    ),
    tabPanel("Model Selection",
             tags$h5("Cross validation results:"),
             checkboxInput("Notch", "Show notch", value = FALSE),
             checkboxInput("NullNormalise", "Normalise", value = TRUE),
             plotOutput("SelectionBoxPlot"),
             radioButtons("Choice", "Model choice", choices = c(""), inline = TRUE )
    ),
    tabPanel("Performance",
             htmlOutput("Title"),
             verbatimTextOutput("TestSummary"),
             plotOutput("TestPlot")
    )
  )
))
