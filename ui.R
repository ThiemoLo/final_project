library(shiny)
library(dplyr)
library(tidyr)
library(ggplot2)


# Creates UI layout and elements
my_ui <- fluidPage(
  titlePanel(strong("United States Mortality Rates Report")),
  br(),
  sidebarLayout(
    sidebarPanel(
      h3("Control Panel"),
      selectInput("state_select",
        label = p("Select State"),
        choices = state.name, selected = "Alabama"
      ),
      sliderInput("year_slider",
        label = "Year",
        min = 1980, max = 2014, value = 1980
      ),
      selectInput("type_slider",
        label = "Mortality Type",
        choices = c("Alcohol use disorders", "Drug use disorders", "Interpersonal violence", "Self-harm"),
          selected = "Alcohol use disorders"
      )
    ),
    mainPanel(
      h3("Findings Panel"),
      tabsetPanel(
        type = "tabs",
        tabPanel(
          "Question 1", br(), plotOutput("question_one_plot_a"), br(), plotOutput("question_one_plot_b")
        ),
        tabPanel(
          "Question 2", br()
        ),
        tabPanel(
          "Question 3", br(), plotOutput("question_three_plot_a")
        ),
        tabPanel(
          "Question 4", br()
        )
      )
    )
  )
)

shinyUI(my_ui)