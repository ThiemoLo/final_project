#install.packages("shiny")

library("shiny")

library("plotly")

my_ui <- fluidPage (
  pageWithSidebar(
    headerPanel(""),
    
    sidebarPanel(
      sliderInput(
        "year",
        
        "Select year",
        
        min = 1980,
       
        max = 2014,
        
        value = 1980
      ),
    
      selectInput(
        "abuse_type",
        
        "Select the data for type(s) of abuse to be seen",
        
        choices = c("Alcohol", "Drug", "Self-harm", "Interpersonal Violence"),
        
        selected = "Alcohol",
        
        multiple = FALSE
      ),
      
      radioButtons(
        "q2_table_sort",
        
        "Select which value you would like the table to be sorted by(for question 2 only)",
        
        choices = c("State", "Rate", "Change"),
        
        selected = "State",
        
        inline = FALSE
      )
    ),
    
    mainPanel(
      tabsetPanel(type = "tabs",
                  tabPanel("Question 2", h1("Are there high concentration regions of certain mortality types and do these 
                                            concentrated areas have noticeable ripple effects in surrounding states? In 
                                            other words, does the mortality rate of a single state affect those of its 
                                            neighboring states? And what are the possible factors contributing to the 
                                            results"),
                           h3("Visualizations"),
                           plotlyOutput("choropleth_map", height = 500),
                           splitLayout(tableOutput("mortality_rate_table1"), 
                                       tableOutput("mortality_rate_table2"),
                                       tableOutput("mortality_rate_table3"), 
                                       tableOutput("mortality_rate_table4")),
                           tableOutput("influence"),
                           textOutput("variable_explained"),
                           h3("Observations and Analysis"),
                           tags$head(tags$style("#variable_explained{font-size: 16px;
                                                           font-style: italic;}")),
                           textOutput("alcohol_analysis"),
                           tags$head(tags$style("#alcohol_analysis{font-size: 18px;}")),
                           textOutput("drug_analysis"),
                           tags$head(tags$style("#drug_analysis{font-size: 18px;}")),
                           textOutput("self_harm_analysis"),
                           tags$head(tags$style("#self_harm_analysis{font-size: 18px;}")),
                           textOutput("interpersonal_violence_analysis"),
                           tags$head(tags$style("#interpersonal_violence_analysis{font-size: 18px;}"))))  
      )
    )
  )


shinyUI(my_ui)