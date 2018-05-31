#Prepares the packages needed
#install.packages("splitstackshape")
#install.packages("plotly")

library("dplyr")
library("plotly")
library("ggplot2")
library("splitstackshape")
library("tidyr")
library("DT")

source("analysis4.R") # QUESTION 4 DATA

## QUESTION 1 DATA FORMATTING
generation_baby_boomer <- (1946:1964)
generation_x <- (1965:1979)
generation_y <- (1980:1994)

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

## QUESTION 3 DATA FORMATTING

prosperity_data <- read.csv(file = "data/per_capita_income_per_county.csv", stringsAsFactors = FALSE)
colnames(prosperity_data)[2] <- "location_name"
prosperity_data$location_name <- paste(prosperity_data$location_name, "County")

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
      mutate(birth_year = year_id - 27) %>%
      mutate(generation =
               case_when(
                 birth_year %in% generation_baby_boomer ~ "Baby Boomer",
                 birth_year %in% generation_x ~ "X",
                 birth_year %in% generation_y ~ "Y"
               ))
    ## For overall statistics
    baby <- state_data %>%
      filter(generation == "Baby Boomer") %>%
      group_by(cause_name) %>%
      summarize(
        baby_lower_bound = round(mean(lower), 3),
        baby_upper_bound = round(mean(upper), 3)
      )
    x <- state_data %>%
      filter(generation == "X") %>%
      group_by(cause_name) %>%
      summarize(
        x_lower_bound = round(mean(lower), 3),
        x_upper_bound = round(mean(upper), 3)
      )
    y <- state_data %>%
      filter(generation == "Y") %>%
      group_by(cause_name) %>%
      summarize(
        y_lower_bound = round(mean(lower), 3),
        y_upper_bound = round(mean(upper), 3)
      )
    
    # Combining different generations
    state_combined <- left_join(baby, x, by = "cause_name")
    state_combined <- left_join(state_combined, y, by = "cause_name")

    
    output$question_one_table_a <- renderDT({
      state_combined
    })
    
    # Turns data into long format for easier graphing
    state_combined_long <- gather(state_combined, key = data_type, value = mortality_percentage,
                                  baby_lower_bound, baby_upper_bound, x_lower_bound, x_upper_bound,
                                  y_lower_bound, y_upper_bound)
    state_combined_long <- state_combined_long %>%
      mutate(generation =  gsub("_.*","", state_combined_long$data_type))
    
    # Question 1 graph a
    a <- ggplot(data = state_combined_long) +
      geom_point(mapping = aes(x = cause_name, y = mortality_percentage, color = generation)) +
      facet_grid(. ~ generation) +
      labs(
        title = paste0("Average Mortality Rates By Generation Between 1980 and 2014 for ", input$state_select),
        x = "Mortality Type",
        y = "Mortality Percentage (%)",
        color = "Generations"
      ) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))
    a
  })
  output$question_one_plot_b <- renderPlot({
    ## Pulling in correct csv file based on state selected
    fixed_state <- gsub(" ", "_", input$state_select)
    state_data <- read.csv(paste0("./data/IHME_USA_COUNTY_USE_INJ_MORTALITY_1980_2014_", fixed_state, "_Y2018M03D13.CSV"))
    ## Find birth year and generational bucket
    fixed_state <- gsub(" ", "_", input$state_select)
    state_data <- read.csv(paste0("./data/IHME_USA_COUNTY_USE_INJ_MORTALITY_1980_2014_", fixed_state, "_Y2018M03D13.CSV"))
    ## Find birth year and generational bucket
    state_data <- state_data %>%
      mutate(birth_year = year_id - 27) %>%
      mutate(generation =
               case_when(
                 birth_year %in% generation_baby_boomer ~ "Baby Boomer",
                 birth_year %in% generation_x ~ "X",
                 birth_year %in% generation_y ~ "Y"
               ))
    ## Year changes for specific type
    type <- input$abuse_type
    baby_change <- state_data %>%
      filter(generation == "Baby Boomer") %>%
      filter(cause_name == type) %>%
      group_by(year_id) %>%
      summarize(
        lower_bound = mean(lower),
        upper_bound = mean(upper)
      )
    x_change <- state_data %>%
      filter(generation == "X") %>%
      filter(cause_name == type) %>%
      group_by(year_id) %>%
      summarize(
        lower_bound = mean(lower),
        upper_bound = mean(upper)
      )
    y_change <- state_data %>%
      filter(generation == "Y") %>%
      filter(cause_name == type) %>%
      group_by(year_id) %>%
      summarize(
        lower_bound = mean(lower),
        upper_bound = mean(upper)
      )
    
    # Question 1 graph b
    b <- ggplot(baby_change, aes(x = year_id, y = lower_bound)) +
      geom_line(color = "red") +
      geom_line(data = x_change, color = "green") +
      geom_line(data = y_change, color = "blue") +
      labs(
        title = paste0("Trend in ", input$abuse_type, " for Different Generations for ", input$state_select),
        x = "Year",
        y = "Mortality Rate Percentage (Lower Bound)"
      )
    b
  })
  output$graph_a <- renderText({
    "This graph shows the mortality type on the x-axis and mortality percentage on the y-axis for a
    specific state. The data shown is the average upper and lower 95% uncertainty levels for each mortality
    type. This will show key differences in which mortality types were the worst for each generation (baby
    boomers, generation x, and generation y. With age standardization of the data around the age of 27 this
    age was used to determine which generation the year's data fell into. This is why for the years 1980-
    2014 only three generations are shown while two more generations existed in this time (the silent
    generation and generation z). You can filter by mortality type"
  })
  output$table_a <- renderText({
    "This table shows the data from the above graph is a more concrete fashion."
  })
  output$graph_b <- renderText({
    "This graph shows year on the x-axis between 1980 and 2014 and has mortality percentage,
    It helps show the overall trend. The colors of the lines represent in which generational
    period the year was in and matches the key of the first graph. You can filter by state
    and mortality type"
  })
  output$analysis_one <- renderText({
    "From filtering through the different states and mortality type two main trends appear.
    The first is that alcohol abuse and interpersonal violence have been decreasing overall
    between the time period. Each generation (baby boomer, generation x, and generation y)
    has decreased the mortality of these types more than the previous generation. This
    could be due to overall decreases in alcohol consumption/taste by newer generations. 
    It could also be due to the increased popularity of marijuana, which is a substitute.
    We see the opposite second trend with drug abuse and self-harm. While both of these
    mortality types have been decreasing during the baby boomer and generationx eras, at
    either the end of generation x or the beginning of generation y these mortality rates
    have increased. Even though the United States government enacted heavy anti-drug
    legislation in the 1970's through 1990's (enacted by the baby boomer generation and
    lived through by x and y), this has not slowed down the epidemic facing most states
    in this country. We can also see a spike in some cases in generation y which could
    be seen as partly due to the opioid crisis we still currently face. The reasons for
    self-harm could be attributed to the rise of the internet age for generation x and
    especially y. Social media and easier online communication has attributed to a rise
    in cyberbullying with the potential consequence of suicide."
  })
  output$conclusion_one <- renderText({
    "We can see from this data that different generations have fought with different
    vices, however we as a country have been overall able to combat them once they 
    have reached crisis level. It seems now that generation x and y will have to
    overcome the rise in drug abuse and self harm."
  })
  output$reference_one <- renderUI({
    reference_1 <-
      "https://www.npr.org/templates/story/story.php?storyId=9252490"
    reference_2 <- 
      "https://www.drugabuse.gov/drugs-abuse/opioids/opioid-overdose-crisis"
    reference_3 <- "https://www.washingtonpost.com/news/wonk/wp/2017/12/01/
      medical-marijuana-took-a-bite-out-of-alcohol-sales-recreational-pot-could
      -take-an-even-bigger-one/?noredirect=on&utm_term=.947b8b7dcd37"
    reference_4 <- "https://www.cbsnews.com/news/suicide-youth-teens-whats-behind-rise/"
    HTML(paste(reference_1, reference_2, reference_3, reference_4, sep = '<br/>'))
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


## END OF QUESTION 2 SERVER

## QUESTION 3 PROSPERITY
  
  output$question_three_table_a <- renderPlot({
    fixed_state <- gsub(" ", "_", input$state_select)
    state_data <- read.csv(paste0("./data/IHME_USA_COUNTY_USE_INJ_MORTALITY_1980_2014_", fixed_state, "_Y2018M03D13.CSV"), 
                           stringsAsFactors = FALSE)
    final_data <- left_join(state_data, prosperity_data, by = "location_name")
    final_data <- filter(final_data, year_id == input$year_slider, sex == "Both",
                         cause_name == input$type_slider, State == input$state_select)

    #Graph of mx mortality rate as median family income rises
    p <- ggplot(data = final_data) +
      geom_point(mapping = aes(x = Median.family.income, y = mx)) +
      labs(
        title = paste0("Average Mortality Rates By Median Family Income for ", input$state_select, 
                       " in ", input$year),
        theme(axis.text.x = element_text(face="bold", color="#993333", 
                                         size=10, angle=45)),
        x = "Median Family Income ($)",
        y = paste0("Mortality Rate by ", input$abuse_type) 
      ) 
    p
  })
  
  output$money_alcohol_analysis <- renderText({
    "Two of the highest value median family incomes at $85,885 and $83,558
    in Sublette and Teton County respectively corresponded with the lowest
    average mortality rates at 1.5 and 1.47. There are a lot of factors that
    drive individuals to Alcohol abuse such as: peer pressure, a way to deal
    with stress, financial worries, etc.  The data seems to suggest that the
    higher the median family income in an area, the lower the mortality rate
    from alcohol abuse. In the top 5 counties in terms of median family income,
    only one county had its mortality rate slightly above 3 at (3.35), the
    rest of which were below that point. When looking at the bottom 5 counties
    by median family income, all had a mortality rate greater than 2, one
    going as high as 7.7. Granted the mortality rate based on alcohol abuse
    can be severely skewed on outliers in a population (an example being
    where there is a particularly large homeless population). This still
    indicates populations with overall less financial worries also suffer
    less from alcohol abuse. "
  })
  
  output$money_self_harm_analysis <- renderText({
    "Unlike Alcohol abuse, self-harm mortality rates have a much more even
    distribution, regardless of the median family income. The mortality rate
    from self-harm fluctuates from 15 to 25, from the lowest income county, to
    the highest with the occasional outliers. The highest earning median family
    income at $85,885 corresponds with an average 20 mortality rate, while the
    lowest median family income at $51,882 corresponds to a low 15.1 mortality
    rate. Reasons why people resort to self-harm are: pressures, loss, and
    feelings of inadequacy. This is not as closely tied to financial security,
    because people coming from high income families can also feel the pressure
    of needing to live up to their parents. In these cases, financial security
    does not act as a helping factor but just as an additional stressor. "
  })
  
  output$money_drug_analysis <- renderText({
    "Just like with Self-Harm there does not seem to be a close-tied relationship
    between drug abuse and median family income. Many people who eventually become
    addicted to drugs, begin through recreational use. Recreational use of drugs
    is not tied to any socioeconomic status, although individuals coming from higher
    income families can have more disposable cash to use on drugs. The mortality
    rate from drug use disorders generally fluctuates between 2 and 3.5. 4 of the
    top 6 median family income counties have a mortality rate under 1.5. 2 of the
    bottom 5 median family income counties have a mortality rate less than 2.
    Outside general fluctuations the mortality rate from drug use seems to be
    consistent across socioeconomic classes. "
  })
  
  output$money_interpersonal_analysis <- renderText({
    "Interpersonal violence is defined to be physical, sexual, emotional, or
    psychological actions or threats of actions to intimidate, frighten, or 
    terrorize another individual. Looking at the sample data, the top 6 median
    family income counties, 5 of which have a mortality rates from interpersonal
    violence at less than 3. While the highest mortality rates come in from the
    bottom 5 median family income counties at 5 and 8.8. It is worth noting that
    the rest of the data generally fluctuates at a mortality rate from 2 to 4. 
    Without focusing too much on the outliers, socioeconomic status does not seem
    to have a drastic effect on lowering mortality rates from interpersonal violence. "
  })
  
  output$reference_three <- renderUI({
    ref_1 <-
      "http://alcoholrehab.com/drug-addiction/reasons-for-substance-abuse/"
    ref_2 <- 
      "https://www.mind.org.uk/information-support/types-of-mental-health-problems/self-harm/why-people-self-harm/"
    ref_3 <- "https://www.projectknow.com/research/drug-abuse-causes/"
    ref_4 <- "https://safe.unc.edu/learn-more/prohibited-behaviors/interpersonal-violence/"
    HTML(paste(ref_1, ref_2, ref_3, ref_4, sep = '<br/>'))
  })
  
## END OF QUESTION 3
  
## QUESTIOn 4 SERVER
  output$self_harm_review_line <- renderPlot({
    avg_self_harm_line_graph
  })
  
  output$self_harm_review_avgs <- renderDT({
    colnames(avg_self_harm_data) <- c("Year", "Average Rate")
    avg_self_harm_data
  })
  
  output$self_harm_review_explain <- renderText({
    "The plot represents the average self-harm mortality rate in the United
    States every 5 years, detailed as a line graph to show the trend. The
    x-axis is the year number and the y-axis is the mortality rate number.
    The table shows the average self-harm mortality rates every 5 years
    from 1980 - 2014."
  })
  
  output$mental_health_bills <- renderDT({
    mental_health_data
  })
  
  output$mental_health_bills_explain <- renderText({
    paste0("The table shows the number of health bills, specifically mental health
      bills, that were enacted during each 5-year interval from 1980-2014.
      Along with that, the table shows the change in self-harm mortality rate
      during each interval as well as the average change per bill. The total number
      of mental health bills passed between 1980-2014 is ",
      sum(mental_health_data$`Health Bill Count`), " and the overall average rate
      change per bill is ", overall_avg_rate_change, "."
    )
  })
  
  output$mental_health_bills_analysis <- renderText({
    "From looking back at the average self-harm mortality rates (plot and table) every
    5 years from 1980-2014, it seems like the average rate is within the range of 14-16,
    which isn't even big. By looking at the graph, we can see a clear drop in mortality
    rate between 1995 and 2000, where the mortality rate dropped by 1 whole unit. A
    hypothesis to be made here is that there was an increase in mental illness/disorder
    awareness & treatment. To find out, we observe the number of health bills, specifically
    mental health bills, that have been passed during given 5-year intervals (this data was
    obtainable via GovTrack). We see that, according to the table with bill counts and rate changes,
    between 1995 and 1999, about 169 health bills were enacted. With the range of health bill counts
    between 25 and 189, 169 is very close to the max count at any given 5-year interval.
    Also, the 1995-1999 interval has the largest absolute value of self-harm mortality
    rate change and average rate change per bill. Also, because bills sometimes need
    time to ramp up, the health bill count in the 5-year interval 1990-1994 may also
    have helped the decrease in self-harm mortality rate between years 1995 and 2000.\n\n
    What's very peculiar is that the 5-year interval 2000-2004 had the max number of
    health bills passed, but the mortality rate started to increase greatly. Regardless,
    it also marked the point when the health bill count started to decrease, which could be
    the reason why the self-harm mortality rate started to increase at that time til 2014."
  })
  
  output$conclusion_four <- renderText({
    "From year 1980 to 2000, there has been a decrease in self-harm mortality,
    especially between years 1995 and 2000. Also, at this time, the number of mental health bills
    passed also started to increase, which may be one of the reasons why the mortality rate declined.
    However, between years 2000 and 2005, the mortality rate started to increase greatly and since 2000,
    it has increased. The number of health bills enacted in 5-year intervals does decrease, so we can see
    the same indirect relationship between health bill count and self-harm mortality rate.\n\n
    An article by Rockett, Ian (2016) details an experiment that was done which hinted that there may have
    been a resurgance of substance abuse, substance use disorders, and suicide in the early 21st century. This
    may be why, despite a large number of health bills being enacted between 2000-2005, the self-harm mortality
    rate started to increase dramatically. A link to the article is in the resources below. Climate change,
    altitude, and specific regions could also be reasons to the increase (see \"Ripple Effects\" tab)."
  })
  
  output$reference_four <- renderUI({
    reference_1 <-
      "https://www.govtrack.us/congress/bills/subjects/mental_health/6176"
    reference_2 <- 
      "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC5482223/"
    reference_3 <- "http://ghdx.healthdata.org/record/
      united-states-substance-use-disorders-and-intentional-injuries-mortality-rates-county-1980"
    HTML(paste(reference_1, reference_2, reference_3, sep = '<br/>'))
  })
}

shinyServer(my_server)