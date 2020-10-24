shinyUI(
  fluidPage(
    theme = shinytheme("simplex"),
    useShinyjs(),
    titlePanel(h3("COVID 19 Analysis Dashboard", align = "center")),
    add_busy_bar(color = "Red"),

    tabsetPanel(
      tabPanel(
        strong("Overview"),

        tabsetPanel(
          tabPanel(
            strong("Summary"),
            verbatimTextOutput(outputId = "Summary")
          ),

          tabPanel(
            strong("Raw Data"),
            column(
              10,
              h4("Search & Sort the Dataset", align = "Right")
            ),
            column(10, dataTableOutput(outputId = "Raw_data", height = "100%"))
          )
        )
      ),

      tabPanel(
        strong("Exploratory Data Analysis"),

        tabsetPanel(
          tabPanel(
            strong("Mosaic"),
            selectizeInput(inputId = "VariablesA", label = "Choose Data:", choices = choicesA, multiple = TRUE, selected = choicesA[2:1]),
            plotOutput(outputId = "Mosaic", width = "100%", height = "700px")
          ),
          
          tabPanel(strong("Rising Value Plot"),
                   sidebarLayout(
                     sidebarPanel(
                       checkboxInput(inputId = "standardise_risingplot", label = "Normalized Range", value = TRUE),
                       br(),
                       checkboxGroupInput("variable", "Variables to show:",
                                          c("Median Age" = "AGEMEDIAN","Age < 25" = "AGE25PROP","Age > 55" = "AGE55PROP",
                                            "Population Density" = "POPDENSITY","GDP  in 2019" = "GDP2019",
                                            "Infant Mortality Rate " = "INFANTMORT","Doctors Per 10K People" = "DOC10",
                                            "Vaccination Rate" = "VAXRATE","Heachcare Cost Per Person" = "HEALTHCARE_COST"
                                          )
                       ),
                       width = 2
                     ),
                     mainPanel(
                       plotOutput("RisingValue", width = "100%", height = "600px"),
                       width = 10
                     )
                   )
          ),

          tabPanel(
            strong("Missing Values"),
            plotOutput(outputId = "Values", width = "100%", height = "700px")
          ),

          tabPanel(
            strong("Missing Pattern"),
            plotOutput(outputId = "Pattern", width = "100%", height = "500px")
          ),

          tabPanel(
            strong("Decision Tree"),
            plotOutput(outputId = "Missingness", width = "100%", height = "300px")
          ),
          
          tabPanel(
            strong("Correlation"),
            plotOutput(outputId = "Corrgram", width = "100%", height = "500px")
          ),
          
          tabPanel(
            strong("Outliers"),
            plotOutput(outputId = "Boxplot", width = "100%", height = "700px"),
            checkboxInput(inputId = "standardise_boxplot", label = "Normalized Range", value = TRUE),
            checkboxInput(inputId = "outliers", label = "Show outliers", value = TRUE),
            sliderInput(inputId = "range", label = "IQR Multiplier", min = 0, max = 5, step = 0.1, value = 1.5),
            hr()
          )
        )
      ),
      
      tabPanel(
        strong("Prediction of Deathrate"),
        
        tabsetPanel(
          tabPanel(
            strong("NZV Predictors"),
            column(10, dataTableOutput(outputId = "nzv_data", height = "100%"))
          ),
          
          tabPanel(
            strong("GLMNET Train"),
            tags$h4("Best Tuning Parameters:"),
            tableOutput("BestTune"),
            hr(),
            plotOutput("GlmModelPlot"),
            verbatimTextOutput("GlmModelSummary")     
          ),

          tabPanel(
            strong("GLMNET Test"),
            plotOutput("GlmTestPlot"),
            verbatimTextOutput("GlmTestSummary")
          ),
          
          tabPanel(
            strong("Cook's Distance"),
            plotOutput("CookDistance")
          )
        )
      )
    )
  )
)
