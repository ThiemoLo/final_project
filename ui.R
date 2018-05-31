#install.packages("shiny")

library("shiny")
library("plotly")
library("DT")

my_ui <- fluidPage (
  titlePanel(strong("United States Mortality Rates Report")),
  br(),
  sidebarLayout(
    sidebarPanel(
      h3("Control Panel"),
      selectInput("state_select",
                  label = p("Select State"),
                  choices = state.name, selected = "Alabama"
      ),
      sliderInput("year",
                  label = "Year",
                  min = 1980, max = 2014, value = 1980
      ),
      selectInput("abuse_type",
                  label = "Mortality Type",
                  choices = c("Alcohol use disorders", "Drug use disorders", "Interpersonal violence", "Self-harm"),
                  selected = "Alcohol use disorders",
                  multiple = FALSE
      )
    ),
    
    mainPanel(
      h3("Findings Panel"),
      tabsetPanel(type = "tabs",
                  tabPanel(
                    "Generational Data", h3("As this data set contains the lives of those in different generations is there any
                                            major difference between the mortality rates and types for each generation? What could
                                            some of the historical events or factors that could have contributed to this correlations?"),
                      br(),
                      textOutput("graph_a"),
                      plotOutput("question_one_plot_a"),
                      textOutput("table_a"),
                      br(), DTOutput("question_one_table_a"),
                      textOutput("graph_b"),
                      br(), plotOutput("question_one_plot_b")
                  ),
                  tabPanel("Ripple Effects",
                           h3("Are there high concentration regions of certain mortality types and do these 
                               concentrated areas have noticeable ripple effects in surrounding states? In 
                               other words, does the mortality rate of a single state affect those of its 
                               neighboring states? What are the possible factors contributing to the 
                               results."),
                           br(),
                           h3("Visualizations"),
                           br(),
                           plotlyOutput("choropleth_map", height = 500),
                           DTOutput("mortality_rate_table1"),
                           br(),
                           tableOutput("influence"),
                           textOutput("variable_explained"),
                           h3("Observations and Analysis"),
                           tags$head(tags$style("#variable_explained{font-size: 16px;
                                                           font-style: italic;}")),
                           h4("Alcohol"),
                           textOutput("alcohol_analysis"),
                           br(),
                           tags$head(tags$style("#alcohol_analysis{font-size: 18px;}")),
                           h4("Drug"),
                           textOutput("drug_analysis"),
                           br(),
                           tags$head(tags$style("#drug_analysis{font-size: 18px;}")),
                           h4("Self-harm"),
                           textOutput("self_harm_analysis"),
                           br(),
                           tags$head(tags$style("#self_harm_analysis{font-size: 18px;}")),
                           h4("Interpersonal Violence"),
                           textOutput("interpersonal_violence_analysis"),
                           br(),
                           tags$head(tags$style("#interpersonal_violence_analysis{font-size: 18px;}")),
                           h3("Conclusion"),
                           textOutput("conclusion"),
                           tags$head(tags$style("#conclusion{font-size: 18px;}")),
                           h3("Reference"),
                           htmlOutput("reference"))
                  ),
                  tabPanel(
                    "Question 3", br()
                  ),
                  tabPanel(
                    "Question 4", br()
                  )
                           )  
      )
    )
  
shinyUI(my_ui)