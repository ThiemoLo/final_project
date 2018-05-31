#Prepares the packages needed
# install.packages("splitstackshape")

library("dplyr")
library("plotly")
library("ggplot2")
library("splitstackshape")
library("tidyr")
library("DT")

## QUESTION 1 DATA FORMATTING
generation_silent <- (1925:1945)
generation_baby_boomer <- (1946:1964)
generation_x <- (1965:1979)
generation_y <- (1980:1994)
generation_z <- (1995:2012)

## QUESTION 2 DATA FORMATTING

#Reads in the data files needed
drug_mortality <-
  read.csv("./data/drug_mortality_rate_data_y1980_y2014.csv", 
           stringsAsFactors = FALSE)
alcohol_mortality <-
  read.csv("./data/alcohol_mortality_rate_data_y1980_y2014.csv", 
           stringsAsFactors = FALSE)
interpersonal_violence_mortality <-
  read.csv("./data/interpersonal_violence_mortality_rate_data_y1980_y2014.csv",
           stringsAsFactors = FALSE)
self_harm_mortality <-
  read.csv("./data/self_harm_mortality_rate_data_y1980_y2014.csv", 
           stringsAsFactors = FALSE)
neighbor_states <-
  read.csv("./data/neighbors-states.csv", stringsAsFactors = FALSE) %>%
  filter(StateCode != "HI" & StateCode != "AK" & StateCode != "DC")

#Renamse the columns for easier data wrangling 
columns_renamed <- c("location", "fips", "y1980", "y1985", "y1990", "y1995", 
                     "y2000", "y2005", "y2010", "y2014", "change")
colnames(drug_mortality) <- columns_renamed
colnames(alcohol_mortality) <- columns_renamed
colnames(interpersonal_violence_mortality) <- columns_renamed
colnames(self_harm_mortality) <- columns_renamed

#Selects rows and columns of interest
drug_mortality <-
  filter(drug_mortality, location %in% state.name)
alcohol_mortality <-
  filter(alcohol_mortality, location %in% state.name)
interpersonal_violence_mortality <-
  filter(interpersonal_violence_mortality, location %in% state.name)
self_harm_mortality <-
  filter(self_harm_mortality, location %in% state.name)

#Renders states' full names into states' codes
drug_mortality$code <-
  state.abb[match(drug_mortality$location, state.name)]
alcohol_mortality$code <- 
  state.abb[match(alcohol_mortality$location, state.name)]
interpersonal_violence_mortality$code <- 
  state.abb[match(interpersonal_violence_mortality$location, state.name)]
self_harm_mortality$code <-
  state.abb[match(self_harm_mortality$location, state.name)]

#Reformats the data from the tables so that they can be used to render tables
#and maps.
truncate_string <- function(to_cut) {
  start <-
    regexpr(' ', to_cut)
  to_cut <-
    substr(to_cut, 1, start - 1)
  as.numeric(as.character(to_cut))
}

## END OF QUESTION 2 DATA FORMATTING

## CREATES SERVER
my_server <- function(input, output) {
  
## QUESTION 1 SERVER
  output$question_one_plot_a <- renderPlot({
    fixed_state <- gsub(" ", "_", input$state_select)
    state_data <- read.csv(paste0("./data/IHME_USA_COUNTY_USE_INJ_MORTALITY_1980_2014_", fixed_state, "_Y2018M03D13.CSV"))
    ## Find birth year and generational bucket
    state_data <- state_data %>%
      mutate(birth_year = year_id - 21) %>%
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
    
    output$question_one_table_a <- renderDT({
      state_combined
    })
    
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
    type <- input$abuse_type
    
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
        title = paste0("Trend in ", input$abuse_type, " for Different Generations for ", input$state_select),
        x = "Year",
        y = "Mortality Rate Percentage (Lower Bound)"
      )
    b
  })
  
## END OF QUESTION 1 SERVER
  
## QUESTION 2 SERVER
  year_rounding <- reactive({
    if (input$year == 2014 | input$year == 2013) {
      final_year <- 2014
    } else if (input$year %% 5 != 0) {
      if (input$year %% 5 >= 1 & input$year %% 5 < 3) {
        final_year <- input$year - input$year %% 5
      } else {
        final_year <- input$year + (5 - input$year %% 5)
      }
    } else {
      final_year <- input$year
    }
    final_year
  })
  
  #Creates a table that will be used to render the map visualizations. 
  map_to_render <- reactive({
    input_year <- year_rounding()
    which_year <- paste("y", input_year, sep = "")
    which_year_sym <- rlang::sym(which_year)
    if (input$abuse_type == "Alcohol use disorders") {
      output_table <- alcohol_mortality
    } else if (input$abuse_type == "Drug use disorders") {
      output_table <- drug_mortality
    } else if (input$abuse_type == "Self-harm") {
      output_table <- self_harm_mortality
    } else {
      output_table <- interpersonal_violence_mortality
    }
    output_table[[paste("y", input_year, sep = "")]] <-
      truncate_string(output_table[[paste("y", input_year, sep = "")]])
    output_table <-
      select(output_table, location, code, !!which_year_sym)
  })
  state_influence <- reactive({
    neighbor_states_stats <-
      left_join(neighbor_states, map_to_render(), 
                by = c("StateCode" = "code")) %>%
      left_join(map_to_render(), by = c("NeighborStateCode" = "code"))
    colnames(neighbor_states_stats)[c(4, 6)] <- c("master_state_stat", 
                                                  "neighbor_state_stat")
    average_delta <-
      group_by(neighbor_states_stats, StateCode) %>%
      summarize(average = round(sum(neighbor_state_stat - master_state_stat) / n(), 3))
    combined_table <-
      left_join(map_to_render(), average_delta, by = c("code" = "StateCode"))
    colnames(combined_table) <- c("state_name", "state_code", "state_stat", 
                                  "state_influence")
    combined_table <- arrange(combined_table, state_influence)
  })
  output$choropleth_map <- renderPlotly({
    input_year <- year_rounding()
    boundaries <-
      list(color = toRGB("white"), width = 2)
    geography <- list(
      scope = 'usa',
      projection = list(type = 'albers usa'),
      showlakes = FALSE,
      lakecolor = toRGB('white')
    )
    title_style <- list(
      size = 30
    )
    margin_spec <- list(
      t = 60,
      b = 20,
      l = 100,
      r = 20,
      pad = 4
    )
    
    #Drug Mortality Choropleth Map
    plot_geo(map_to_render(), locationmode = 'USA-states') %>%
      add_trace (
        z = map_to_render()[[paste("y", input_year, sep = "")]],
        
        locations = ~code,
        
        color = map_to_render()[[paste("y", input_year, sep = "")]], 
        
        text = paste(map_to_render()$location, "had a rate of", 
                     map_to_render()[[paste("y", input_year, sep = "")]]),
        
        colors = 'Blues'
      ) %>%
      colorbar(title = "Mortality Rate(percentage)") %>%
      layout(
        geo = geography,
        title = paste("US", input$abuse_type, "Mortality Rate in", input_year),
        titlefont = title_style,
        width = 1000,
        height = 500,
        margin = margin_spec
     )
  })  
  output$mortality_rate_table1 <- renderDT({
    output <- state_influence() %>%
      select(state_name, state_stat, state_influence)
    colnames(output) <- c("State", "Rate", "Change")
    output
  })
  output$variable_explained <- renderText({
    "Due to the fact that the data table can only increment its year by 5 each 
     time, if any selected years were to be outside of it, they will be rounded 
     to the nearest available options. In the above table, the variable Change 
     was calculated based on the average of the differences between the state's 
     mortality rate and those of all its neighboring states. Due to geographical
     natures, the states of Alaska and Hawaii will be excluded from the analysis."
  })  
  output$alcohol_analysis <- renderText({
    "According to the choropleth map, In the span of more tha 30 years between 
     1980 and 2014, the majority of the United States did not have any
     significantly high alcohol-related mortality rates, with the excpetions of
     outliers Alaska and New Mexico. The only state worth noticing is New
     Mexico. One of the potential causes for its high drinking rate might be 
     due to the earlier Alcohol Prohibition that happened in 1917. People that 
     were against prohibition would find illegal means to acquire alcohol. A 
     decrease in frequency of drinking lead to a increase in ammount per time, 
     thus increases the risk of alcohol mortality due to overdose. This habit 
     was likely preserved by the residents' future generations and built a 
     foundation for today's heavy drinkings in New Mexico. However, despite its 
     high mortality rate, the surrounding states seem unaffected. It is possible
     that the state-wise alcohol prohibition happened between 1920 and 1933 was 
     somehow ineffective in suppressing the heavy drinking practice in New 
     Mexico comparing to other states."
  })  
  output$drug_analysis <- renderText({
    "As far as drug is concerned, it seems that from 1980 to 1995, the West
     side states seem to have had a more severe drug-related mortality rate
     comparing to the east, with a few leading states such as California, 
     Nevada and New Mexico, which implies that they might be the \"sources\" of
     drug mortality distribution of other states to the west. However, starting
     from 2000 to 2010, the distribution becamse a lot more even across all 
     states in the US. Although there were a few outliers such as New Mexico
     and West Virginia."
  })
  output$self_harm_analysis <- renderText({
    "Self-harm mortality rate seemed to have always been west-side heavy
     throughout 1980-2014. There had been a clear division in the self-harm 
     mortality rate between the west and the east. A study in 2015 suggested
     a potential correlation between altitude and depression state. It was 
     found that people living in higher altitude were more likely to experience
     hypoxia, a deficiency of amount of oxygen reaching the tissues, which was
     found to be a contributing factor to increased depression. California is
     also noticable for consistent low suicide rate despite being on the west
     coast, and this is likely due to its climate nature. California is known
     for having a mediterranean-like weather throughout a year with dry summer
     and mild, wet winter. A study in 2004 found that good weather could
     potentially lead to a better mood for an individual."
  })
  output$interpersonal_violence_analysis <- renderText({
    "Similar to self-harm, interpersonal violence seemed to be extremely
     one-sided: The south had had a consistently higher interpersonal violence
     mortality rate than the north. "
  })
}

## END OF QUESTION 2 SERVER 

shinyServer(my_server)