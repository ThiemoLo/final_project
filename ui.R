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
                    h3("Visualizations"),
                    textOutput("graph_a"),
                    br(),
                    plotOutput("question_one_plot_a"),
                    br(),
                    textOutput("table_a"),
                    br(),
                    DTOutput("question_one_table_a"),
                    br(),
                    textOutput("graph_b"),
                    br(),
                    plotOutput("question_one_plot_b"),
                    br(),
                    h3("Observations and Analysis"),
                    textOutput("analysis_one"),
                    br(),
                    h3("Conclusion"),
                    textOutput("conclusion_one"),
                    br(),
                    h3("Reference"),
                    htmlOutput("reference_one")
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
                           htmlOutput("reference")
                  ),
                  tabPanel( "Economic Prosperity and Mortality",
                    h3("Do mortality rate and economic prosperity have an effect on each other? 
                       The two seem unrelated, but are they actually similar or do they have a 
                       relationship to each other? Correlation between socio-economics and how 
                       wealth affects overall mortality rates for populations (are wealthier 
                       populations less prone to death overall/to certain types?)."),
                    br(),
                    plotOutput("question_three_table_a")
                  ),
                  tabPanel("Outside-the-Box Analysis: Self-Harm and Health Bills",
                    h3("Over the past 20 years, with increases in mental illness/disorder awareness
                        and treatment, is there a decrease in self-harm mortality? How many health
                        bills have been passed in a given year and how much on average has each bill
                        changed the rate? Given some bills need time to ramp up, what is this overall
                        change based on current overall number of bills?"),
                    br(),
                    h3("Self-Harm Mortality Rates"),
                    br(),
                    p("Let's review the average self-harm mortality rates in the United States every 5 years:"),
                    br(),
                    plotOutput("self_harm_review_line"),
                    DTOutput("self_harm_review_avgs"),
                    em(textOutput("self_harm_review_explain")),
                    br(),
                    h3("Health Bill Counts and Changes in Mortality Rate"),
                    br(),
                    p("Now let's see the correlation between health bill counts and change in mortality rates:"),
                    br(),
                    DTOutput("mental_health_bills"),
                    em(textOutput("mental_health_bills_explain")),
                    br(),
                    h3("Observations and Analysis"),
                    textOutput("mental_health_bills_analysis"),
                    br(),
                    h3("Conclusion"),
                    textOutput("conclusion_four"),
                    br(),
                    h3("Reference"),
                    htmlOutput("reference_four")
                )
            )
        )
    )
)

shinyUI(my_ui)