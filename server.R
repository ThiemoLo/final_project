#Prepares the packages needed
#install.packages("splitstackshape")

library("dplyr")

library("plotly")

library("ggplot2")

library("splitstackshape")


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


#Creates server
my_server <- function(input, output) {
  
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
    
    if (input$abuse_type == "Alcohol") {
      output_table <- alcohol_mortality
    } else if (input$abuse_type == "Drug") {
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
      summarize(average = sum(neighbor_state_stat - master_state_stat) / n())
    
    combined_table <-
      left_join(map_to_render(), average_delta, by = c("code" = "StateCode"))
    
    colnames(combined_table) <- c("state_name", "state_code", "state_stat", 
                                  "state_influence")
    
    if (input$q2_table_sort == "State") {
      combined_table <- arrange(combined_table, state_name)
    } else if (input$q2_table_sort == "Rate") {
      combined_table <- arrange(combined_table, state_stat)
    } else {
      combined_table <- arrange(combined_table, state_influence)
    }
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
  
  output$mortality_rate_table1 <- renderTable({
    output <- state_influence() %>%
      select(state_name, state_stat, state_influence)
    
    colnames(output) <- c("State", "Rate", "Change")
    
    output[c(1:15),]
  })

  output$mortality_rate_table2 <- renderTable({
    output <- state_influence() %>%
      select(state_name, state_stat, state_influence)
    
    colnames(output) <- c("State", "Rate", "Change")
    
    output[c(16:30),]
  })  
  
  output$mortality_rate_table3 <- renderTable({
    output <- state_influence() %>%
      select(state_name, state_stat, state_influence)
    
    colnames(output) <- c("State", "Rate", "Change")
    
    output[c(31:45),]
  }) 
  
  output$mortality_rate_table4 <- renderTable({
    output <- state_influence() %>%
      select(state_name, state_stat, state_influence)
    
    colnames(output) <- c("State", "Rate", "Change")
    
    output[c(46:50),]
  })

  output$variable_explained <- renderText({
    "Due to the fact that the data table can only increment its year by 5 each 
     time, if any selected years were to be outside of it, they will be rounded 
     to the nearest available options. In the above table, the variable Change 
     was calculated based on the average of the differences between the state's 
     mortality rate and those of all its neighboring states. Due to geographical
     locations, the states of Alaska and Hawaii will be excluded from the 
     analysis."
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
     mortality rate than the north. This was likely due to the higher crime
     rates in the south of United States froom 1976 to 2000"
  })

  output$conclusion <- renderText({
    "In conclusion, whether or not there were states that served as sources
     that directly influence the mortality rates of their surrounding states
     depends on the type of morality discussed. As for alcohol, due to the 
     similarity between each state's legal status and people's accessibility 
     to alcohol, all of the states share pretty much the same statistics with
     a few outliers like New Mexico and Alaska, so no significant ripple
     effect. Drugs on the other hand, initially had a few states with high
     mortality rates, and eventually spread out evenly for all states. Over the
     span of 30 years, smugglers were likely able to come up with means to 
     transport their products to expand their markets, giving people from more
     states access to drugs. Therefore a ripple effect might be present for
     drugs. Self-harm and interpersonal violence shared a similar property:
     a confounding variable created a ripple-effect-lookalike. All of the
     states with higher self-harm rates tend to have higher altitude, which is
     found to be a factor contributing to depression; All of the states with
     higher interpersonal violence rates tend to have a higher crime rates.
     So for the last two types it really is just a conincidence that adjacent
     states happen to share a similarity in their characteristics."
  }) 
  
  output$reference <- renderUI({
    reference1 <-
      "Hanson, D. J. (2017, March 01). Prohibition in New Mexico was Welcomed, 
       then Rejected. Retrieved from 
       https://www.alcoholproblemsandsolutions.org/prohibition-in-new-mexico/"
    
    reference2 <-
      "Gallup, Inc. (2002, July 09). Decades of Drug Use: The '80s and '90s. 
       Retrieved from 
       http://news.gallup.com/poll/6352/decades-drug-use-80s-90s.aspx" 
    
    reference3 <-
      "History of Ecstasy (MDMA). (n.d.). Retrieved from 
       https://www.narconon.org/drug-information/ecstasy-history.html"
    
    reference4 <-
      "Travel Tips & Information. (n.d.). Retrieved from 
       http://www.visitcalifornia.com/feature/travel-tips-information"
    
    reference5 <-
      "Holmes, L. (2017, December 07). This Is Your Brain On Spring. 
       Retrieved from 
       https://www.huffingtonpost.com/2015/04/22/warm-weather-mood_n_7056636.html"
    
    reference6 <-
      "United States Substance Use Disorders and Intentional Injuries Mortality Rates 
       by County 1980-2014. (1970, January 01). Retrieved from 
       http://ghdx.healthdata.org/record/united-states-substance-use-disorders-and-intentional-injuries-mortality-rates-county-1980"
    
    reference7 <-
      "Murder Rates Nationally and By State. (n.d.). Retrieved from 
       https://deathpenaltyinfo.org/murder-rates-nationally-and-state"
    
    reference8 <-
      "Best, A. (2015, May 12). Thin Air Might Increase Depression in Mountain 
       States. Retrieved May 30, 2018, from 
       https://www.livescience.com/50813-low-oxygen-increase-depression.html"
    
    HTML(paste(reference1, reference2, reference3, reference4, reference5,
               reference6, reference7, reference8, sep = '<br/>'))
  })
}

shinyServer(my_server)