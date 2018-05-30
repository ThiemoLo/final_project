library(shiny)
library(dplyr)
library(DT)
library(ggplot2)



my_server <- function(input, output) {
  
### QUESTION 1
  generation_silent <- (1925:1945)
  generation_baby_boomer <- (1946:1964)
  generation_x <- (1965:1979)
  generation_y <- (1980:1994)
  generation_z <- (1995:2012)
  output$question_one_plot_a <- renderPlot({
  ## Pulling in correct csv file based on state selected
    fixed_state <- gsub(" ", "_", input$state_select)
    state_data <- read.csv(paste0("./data/IHME_USA_COUNTY_USE_INJ_MORTALITY_1980_2014_", fixed_state, "_Y2018M03D13.CSV"))
    ## Find birth year and generational bucket
    state_data <- state_data %>%
      mutate(birth_year = year_id - age_id) %>%
      mutate(generation =
               case_when(
                 birth_year %in% generation_silent ~ "Silent",
                 birth_year %in% generation_baby_boomer ~ "Baby Boomer",
                 birth_year %in% generation_x ~ "X",
                 birth_year %in% generation_y ~ "Y",
                 birth_year %in% generation_z ~ "Z"
               ))
    
    ## For overall statistics
    silent <- state_data %>%
      filter(generation == "Silent") %>% 
      group_by(cause_name) %>%
      summarize(
        silent_lower_bound = mean(lower),
        silent_upper_bound = mean(upper)
      )
    baby <- state_data %>%
      filter(generation == "Baby Boomer") %>%
      group_by(cause_name) %>%
      summarize(
        baby_lower_bound = mean(lower),
        baby_upper_bound = mean(upper)
      )
    x <- state_data %>%
      filter(generation == "X") %>%
      group_by(cause_name) %>%
      summarize(
        x_lower_bound = mean(lower),
        x_upper_bound = mean(upper)
      )
    y <- state_data %>%
      filter(generation == "Y") %>%
      group_by(cause_name) %>%
      summarize(
        y_lower_bound = mean(lower),
        y_upper_bound = mean(upper)
      )
    z <- state_data %>%
      filter(generation == "Z") %>%
      group_by(cause_name) %>%
      summarize(
        z_lower_bound = mean(lower),
        z_interval_upper_bound = mean(upper)
      )
    
    # Combining different generations
    state_combined <- right_join(silent, baby, by = "cause_name")
    state_combined <- left_join(state_combined, x, by = "cause_name")
    state_combined <- left_join(state_combined, y, by = "cause_name")
    state_combined <- left_join(state_combined, z, by = "cause_name")
    
    # Turns data into long format for easier graphing
    state_combined_long <- gather(state_combined, key = data_type, value = mortality_percentage, silent_lower_bound,
                                  silent_upper_bound, baby_lower_bound, baby_upper_bound, x_lower_bound, x_upper_bound,
                                  y_lower_bound, y_upper_bound, z_lower_bound, z_interval_upper_bound)
    state_combined_long <- state_combined_long %>%
      mutate(generation =  gsub("_.*","", state_combined_long$data_type))
    
  # Question 1 graph a
    a <- ggplot(data = state_combined_long) +
      geom_point(mapping = aes(x = cause_name, y = mortality_percentage, color = generation)) +
      facet_grid(. ~ generation) +
      labs(
        title = paste0("Average Mortality Rates By Generation Between 1980 and 2014 for ", input$state_select),
        x = "Mortality Type",
        y = "Mortality Percentage (%)"
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    a
  })
  
  output$question_one_table_a <- renderTable({
    fixed_state <- gsub(" ", "_", input$state_select)
    state_data <- read.csv(paste0("./data/IHME_USA_COUNTY_USE_INJ_MORTALITY_1980_2014_", fixed_state, "_Y2018M03D13.CSV"))
    ## For specific year and to pull from for inline analysis
    state_data_year <- state_data %>%
      filter(year_id == input$year_slider)
    
    silent_year <- state_data_year %>%
      filter(generation == "Silent") %>% 
      group_by(cause_name) %>%
      summarize(
        silent_lower_bound = mean(lower),
        silent_upper_bound = mean(upper)
      )
    baby_year <- state_data_year %>%
      filter(generation == "Baby Boomer") %>%
      group_by(cause_name) %>%
      summarize(
        baby_lower_bound = mean(lower),
        baby_upper_bound = mean(upper)
      )
    x_year <- state_data_year %>%
      filter(generation == "X") %>%
      group_by(cause_name) %>%
      summarize(
        x_lower_bound = mean(lower),
        x_upper_bound = mean(upper)
      )
    y_year <- state_data_year %>%
      filter(generation == "Y") %>%
      group_by(cause_name) %>%
      summarize(
        y_lower_bound = mean(lower),
        y_upper_bound = mean(upper)
      )
    z_year <- state_data_year %>%
      filter(generation == "Z") %>%
      group_by(cause_name) %>%
      summarize(
        z_lower_bound = mean(lower),
        z_interval_upper_bound = mean(upper)
      )
    
    # Combining different generations
    state_combined_year <- right_join(silent_year, baby_year, by = "cause_name")
    state_combined_year <- left_join(state_combined_year, x_year, by = "cause_name")
    state_combined_year <- left_join(state_combined_year, y_year, by = "cause_name")
    state_combined_year <- left_join(state_combined_year, z_year, by = "cause_name")
    
    state_combined_year_long <- gather(state_combined_year, key = data_type, value = mortality_percentage, silent_lower_bound,
                                       silent_upper_bound, baby_lower_bound, baby_upper_bound, x_lower_bound, x_upper_bound,
                                       y_lower_bound, y_upper_bound, z_lower_bound, z_interval_upper_bound)
    
    ## Difference from year to overall average (to pull for inline analysis)
    state_combined_long_delta <- state_combined_long %>%
      mutate(overall_average = state_combined_year_long$mortality_percentage) %>%
      mutate(delta = mortality_percentage - overall_average)
  })
  
  output$question_one_plot_b <- renderPlot({
    ## Pulling in correct csv file based on state selected
    fixed_state <- gsub(" ", "_", input$state_select)
    state_data <- read.csv(paste0("./data/IHME_USA_COUNTY_USE_INJ_MORTALITY_1980_2014_", fixed_state, "_Y2018M03D13.CSV"))
    ## Find birth year and generational bucket
    state_data <- state_data %>%
      mutate(birth_year = year_id - age_id) %>%
      mutate(generation =
               case_when(
                 birth_year %in% generation_silent ~ "Silent",
                 birth_year %in% generation_baby_boomer ~ "Baby Boomer",
                 birth_year %in% generation_x ~ "X",
                 birth_year %in% generation_y ~ "Y",
                 birth_year %in% generation_z ~ "Z"
               ))
    ## Year over year changes for specific type
    type <- input$type_slider
    
    silent_change <- state_data %>%
      filter(generation == "Silent") %>%
      filter(cause_name == type) %>%
      group_by(year_id) %>%
      summarize(
        x_lower_bound = mean(lower),
        x_upper_bound = mean(upper)
      )
    baby_change <- state_data %>%
      filter(generation == "Baby Boomer") %>%
      filter(cause_name == type) %>%
      group_by(year_id) %>%
      summarize(
        x_lower_bound = mean(lower),
        x_upper_bound = mean(upper)
      )
    x_change <- state_data %>%
      filter(generation == "X") %>%
      filter(cause_name == type) %>%
      group_by(year_id) %>%
      summarize(
        x_lower_bound = mean(lower),
        x_upper_bound = mean(upper)
      )
    y_change <- state_data %>%
      filter(generation == "Y") %>%
      filter(cause_name == type) %>%
      group_by(year_id) %>%
      summarize(
        x_lower_bound = mean(lower),
        x_upper_bound = mean(upper)
      )
    z_change <- state_data %>%
      filter(generation == "Z") %>%
      filter(cause_name == type) %>%
      group_by(year_id) %>%
      summarize(
        x_lower_bound = mean(lower),
        x_upper_bound = mean(upper)
      )
    
  # Question 1 graph b
    b <- ggplot(silent_change, aes(x = year_id, y = x_lower_bound)) +
      geom_line() +
      geom_line(data = baby_change, color = "red") +
      geom_line(data = x_change, color = "blue") +
      geom_line(data = y_change, color = "green") +
      geom_line(data = z_change, color = "purple") +
      labs(
        title = paste0("Trend in ", input$type_slider, " for Different Generations for ", input$state_select),
        x = "Year",
        y = "Mortality Rate Percentage (Lower Bound)"
      )
    b
  })


### END OF QUESTION 1

### QUESTION 2


### END OF QUESTION 2

### QUESTION 3


### END OF QUESTION 3

### QUESTION 4


### END OF QUESTION 4

}

shinyServer(my_server)