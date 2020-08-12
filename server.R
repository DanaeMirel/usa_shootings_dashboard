
#---------------------------------------------#
#---# Mass Shootings in the United States #---#
#---------------------------------------------#

library("shinydashboard")
library("dplyr")
library("lubridate")
library("leaflet")
library("magrittr")
library("forcats")
library("ggplot2")
library("tidyr")
library("shinyWidgets")
library("DT")
library("viridis")
library("plotly")
library("stringr")

#---# original data #---#
shootings <- read.csv('mass-shootingsupdate.csv')

#---# clean the data #---#
mass_shootings <- shootings %>%  
  mutate(date = mdy(date)
         , site = fct_recode(location.1
                            , "Workplace" = "\nWorkplace"
                            , "Other" = "Other\n")
         , race = gsub(" ", "", race, fixed = TRUE) # eliminate blank spaces 
         , race = fct_recode(race
                            , "unclear" = "-"
                            , "Black" = "black"
                            , "White" = "white")
         , state = word(location, -1)
         , mental_health_issues = gsub(" ", "", prior_signs_mental_health_issues, fixed = TRUE) # eliminate blank spaces 
         , mental_health_issues = fct_recode(mental_health_issues
                                            , "Unclear" = "-"
                                            , "Yes" ="yes" 
                                            , "TBD" = "Unknown")
         , legal_weapons = word(weapons_obtained_legally, 1)
         , legal_weapons =  fct_recode(legal_weapons 
                                      , "Yes" = "\nYes"
                                      , "Unknown" = "-"
                                      , "Unknown" = "TBD"
                                      , "Unknown" = "Kelley")
         , gender = fct_recode(gender
                              , "M" = "Male" 
                              , "F" = "Female"
                              , "Other" = "-"
                              , "M & F" = "Male & Female")
         , age_of_shooter = as.numeric(age_of_shooter)
         , age_of_shooter = ifelse(age_of_shooter < 10, age_of_shooter + 10, age_of_shooter )
         , weapon_type = fct_recode(weapon_type
                                   , "handgun" = "Handgun" 
                                   , "shotgun" = "Shotgun" 
                                   , "rifle" = "Rifle")
         , handgun = ifelse(str_detect(weapon_type, "handgun"), 1, 0)
         , rifle = ifelse(str_detect(weapon_type, "rifle"), 1, 0)
         , revolver = ifelse(str_detect(weapon_type, "revolver"), 1, 0)
         , shotgun = ifelse(str_detect(weapon_type, "shotgun"), 1, 0)) %>% 
  select('date','location', 'state', 'site', 'fatalities', 'injured', 'total_victims'
         , 'handgun', 'rifle', 'revolver', 'shotgun', 'weapon_type', 'age_of_shooter'
         , 'mental_health_issues', 'legal_weapons', 'race', 'gender', 'latitude', 'longitude', 'summary')

#---# about the data #---#
text_about <- "The FBI and leading criminologists defined a mass shooting as 
               a single attack in a public place in which four or more victims were killed. 
               
               This data was compiled by Mother Jones, nonprofit founded in 1976. 
               Originally covering cases from 1982-2012, this database has since been 
               expanded numerous times to remain current."

text_learn_more <- "Visit https://www.motherjones.com/politics/2012/12/mass-shootings-mother-jones-full-data/ 
                    to download the whole dataset and get aditional information"

theme_mass_shoot <- function() {
  
  theme_bw() + 
    theme(legend.position = 'none'
      , text = element_text(family = "Bookman", color = "gray25")
      , plot.subtitle = element_text(size = 14)
      , plot.caption = element_text(color = "gray30")
      , plot.margin = unit(c(1, 1, 1, 1), units = "mm"))
  
}

shinyServer(
  
  function(input, output){
    
  #---# about #---#
  observeEvent(input$show_about,{showModal(modalDialog(text_about, title = 'About'))})
  
  #---# global filter #---#
  rval_mass_shootings <- reactive({
    
    # Filter mass_shootings on number of fatalities and date range.
    mass_shootings_fil <- mass_shootings %>%
      filter(between(date, input$date_range[1], input$date_range[2]) & fatalities >= input$nb_fatalities)
    
  })
  
  #-----------------#
  #---# MAP TAB #---#
  #-----------------#
  
  #---# total number of shootings #---#
  output$summary_inf <- renderText({
    
    paste('There have been'
          , nrow(rval_mass_shootings())
          , 'mass shootings in the United States between'
          , year(input$date_range[1]), 'and', year(input$date_range[2])
          , 'involving at least'
          , input$nb_fatalities
          , 'fatalities. This means that in total there have been : ')
  })
  
  #---# sum of the victims #---#
  output$victimsBox <- renderValueBox({
    valueBox( sum(rval_mass_shootings()$total_victims), "Victims", icon = icon("ambulance"), color = "purple")
  })

  #---# sum of fatalities  #---#  
  output$FatalitiesBox  <- renderValueBox({
    valueBox(sum(rval_mass_shootings()$fatalities), "Fatalities", icon = icon("ribbon"), color = "purple")
  })
  
  #---# sum of injured people #---#
  output$InjuredBox <- renderValueBox({
    valueBox(sum(rval_mass_shootings()$injured), "Injured", icon = icon("users"), color = "purple")
  })
  
  #---# map of locations #---#
  output$map <- leaflet::renderLeaflet({
    
    rval_mass_shootings() %>%
      leaflet(options = leafletOptions(minZoom = 2, maxZoom = 10)) %>% 
      addTiles() %>%
      setView( -98.58, 39.82, zoom = 4.25) %>% 
      addTiles() %>% 
      addCircleMarkers(popup  = ~ summary  
                       , radius = ~ fatalities
                       , fillColor = 'red'
                       , color = 'red'
                       , weight = 1)
  })
  
  #----------------------#
  #---# LOCATION TAB #---#
  #----------------------#
  
  output$shooting_location <- renderPlotly({
    
    rval_mass_shootings() %>%
      group_by(site) %>%
      summarise(n = n()) %>%
      mutate(freq = round((n/sum(n))*100, 2)) %>% 
      arrange(desc(freq)) %>%
      mutate(site = fct_reorder(site, freq)) %>%
      ggplot(aes(x=site, y=freq, fill=site)) +
      geom_bar(stat="identity") +
      scale_fill_viridis(option = "B", discrete = TRUE) +
      theme_mass_shoot() + 
      coord_flip() +
      labs(x=' ', y ='%', caption = 'Mother Jones - Mass Shootings Database, 1982 - 2020')
    
  })
  
  output$shooting_state <- renderPlotly({
      
    rval_mass_shootings() %>%
      group_by(state) %>%
      summarise(n = n()) %>%
      mutate(freq = round((n/sum(n))*100, 2)) %>% 
      arrange(desc(freq)) %>%
      mutate(state = fct_reorder(state, freq)) %>%
      top_n(10, freq) %>%      
      ggplot(aes(x = state, y = freq, fill = state)) +
      geom_bar(stat = 'identity') +
      scale_fill_viridis(option = "B", discrete = TRUE) +
      coord_flip() +
      labs(x=' ', y ='%', caption = 'Mother Jones - Mass Shootings Database, 1982 - 2020') +
      theme_mass_shoot()
    
  })
  
  output$shooting_by_year <- renderPlotly({
    
    rval_mass_shootings() %>% 
      group_by(year(date)) %>% 
      summarise(n=n()) %>% 
      set_colnames(c('year', 'n')) %>% 
      ggplot(aes(x=year,y=n)) +
      geom_segment( aes(x=year, xend=year, y=0, yend=n), color="black") +
      geom_point( color="red", size=1.5) +
      ylim(0,13) +
      labs(x ='year', y ='Number of shootings by year', caption = 'Mother Jones - Mass Shootings Database, 1982 - 2020') +
      theme(axis.text.x = element_text(angle=90)) +
      theme_bw()  
    
  })
  
  #---------------------#
  #---# PROFILE TAB #---#
  #---------------------#
  
  #---# mental state #---#
  
  mental_health_summ <- reactive({
    
    rval_mass_shootings() %>%
      group_by(mental_health_issues) %>%
      summarise(n = n()) %>%
      mutate(freq = round((n/sum(n))*100, 2)) %>% 
      arrange(desc(freq)) %>%
      mutate(mental_health_issues = fct_reorder(mental_health_issues, freq))
    
  })
  
  output$mental_health <- renderPlotly({
      
    mental_health_summ() %>%
      ggplot(aes(x=mental_health_issues, y=freq, fill=mental_health_issues)) +
      geom_col() +
      scale_fill_viridis(option = "B", discrete = TRUE) +
      coord_flip() +
      labs(x = " ", y = "%") + 
      theme_mass_shoot()
    
  })
  
  output$mental_health_info <- renderValueBox({ 
    valueBox( paste(mental_health_summ()[1,]$freq, '%')
              , strong("of the attackers were mentally ill.")
              , icon = icon("brain", lib = 'font-awesome'), color = "purple") 
    })
  
  #---# race of the shooter #---# 
  
  race_summ <- reactive({
    
    rval_mass_shootings() %>%
      group_by(race) %>%
      summarise(n = n()) %>%
      mutate(freq = round((n/sum(n))*100, 2)) %>% 
      arrange(desc(freq)) %>%
      mutate(race = fct_reorder(race, freq)) 
    
  })
  
  output$race_shooters <- renderPlotly({
    
    race_summ() %>%
      ggplot(aes(x=race, y=freq, fill=race)) +
      geom_col() +
      scale_fill_viridis(option = "B", discrete = TRUE) +
      coord_flip() + 
      labs(x="", y="%") + 
      theme_mass_shoot()

  })

  output$race_shooters_info <- renderValueBox({ 
    valueBox( paste(race_summ()[1,]$freq, '%')
              , strong("of the attackers were white male.")
              , icon = icon("mars"), color = "teal") 
    })
  
  #---# age of the shooter #---#   
  
  output$age_by_race <- renderPlot({
    
    rval_mass_shootings() %>% 
      ggplot(aes(x=age_of_shooter, fill=race)) +
      geom_histogram() +
      scale_fill_viridis(option = "B", discrete = TRUE) +
      theme_bw() +
      labs(x="Age of the shooter", y="counts")

  })
  
  output$median_age_info <- renderValueBox({ 
    valueBox( paste(median(rval_mass_shootings()$age_of_shooter))
              , strong('is the median age of the shooters.')
              , icon = icon("child"), color = "purple") 
    })
  
  #--------------------#
  #---# WEAPON TAB #---#
  #--------------------#
  
  #---# legal weapon #---#
  
  ind_legal_weapon <- reactive({
    
    rval_mass_shootings() %>%
      group_by(legal_weapons) %>%
      summarise(n = n()) %>%
      mutate(freq = round((n/sum(n))*100, 2)) %>% 
      arrange(desc(freq)) %>%
      mutate(legal_weapons = fct_reorder(legal_weapons, freq))
    
  })
  
  output$legal_weapon <- renderPlotly({

    ind_legal_weapon() %>% 
      ggplot(aes(x=legal_weapons, y=freq, fill=legal_weapons)) +
      geom_col() +
      scale_fill_viridis(option = "B", discrete = TRUE) +
      coord_flip() +
      labs(x="", y="%") +
      theme_mass_shoot()

  })
  
  output$legal_weapon_info <- renderValueBox({
    valueBox( paste(ind_legal_weapon()[1,]$freq, '%')
              , strong("of the weapons were obteined legally.")
              , icon = icon("balance-scale"), color = "purple") 
  })
  
  #---# type weapon #---#  
  
  weapons_subset <- reactive({
    
    rval_mass_shootings() %>% 
      select(handgun, rifle, revolver, shotgun) %>% 
      summarise(handgun = sum(handgun)
                , rifle = sum(rifle)
                , revolver = sum(revolver)
                , shotgun = sum(shotgun)) %>% 
      pivot_longer(everything())  %>% 
      mutate(freq = round((value/nrow(rval_mass_shootings()))*100, 2)) %>% 
      arrange(desc(freq)) %>% 
      mutate(weapon = fct_reorder(name, freq))
    
  })
  
  output$type_weapon  <- renderPlotly({
    
    weapons_subset() %>% 
      ggplot(aes(x=weapon, y=freq, fill=weapon)) +
      geom_col()  +
      scale_fill_viridis(option = "B", discrete = TRUE) +
      coord_flip() +
      labs(x=" ", y="%") +
      theme_mass_shoot()
    
  })
  
  output$type_weapon_info <- renderValueBox({
    valueBox( paste(weapons_subset()[1,]$freq, '%')
              , strong("of the attacks were committed at least with a handgun.")
              , icon = icon("hand-paper"), color = "teal") 
  })
  
  #---# number of weapons #---#  
  
  numb_type_weap <- reactive({
    
    rval_mass_shootings() %>%
      select(weapon_type, handgun, rifle, revolver, shotgun) %>%
      mutate(num_type_gun = handgun + rifle + revolver + shotgun ) %>%
      group_by(num_type_gun) %>%
      summarise(n = n()) %>%
      mutate(freq = round((n/sum(n))*100, 2)) %>%
      arrange(desc(freq)) %>%
      mutate(num_type_gun = as.factor(num_type_gun)
             , num_type_gun = fct_reorder(num_type_gun, freq)) %>%
      filter(num_type_gun !=0) 
    
  })
  
  output$several_types <- renderPlotly({
    
    numb_type_weap() %>% 
      ggplot(aes(x=num_type_gun, y=freq, fill=num_type_gun)) +
      geom_col() +
      scale_fill_viridis(option = "B", discrete = TRUE) +
      coord_flip() +
      xlab(" ") +
      ylab("%") +
      theme_mass_shoot()
    
  })
  
  output$several_types_info <- renderValueBox({
    valueBox( paste(sum(numb_type_weap()[numb_type_weap()$num_type_gun!=1,]$freq), '%')
              , strong("of the shotters possed more than one type of gun.")
              , icon = icon("bomb"), color = "purple") 
  })
  
  #------------------#
  #---# DATA TAB #---#
  #------------------#
  
  output$table <-  DT::renderDT({
  
    datatable(
      rval_mass_shootings() %>%  
                select(date, state, site, fatalities, injured, weapon_type
                       , age_of_shooter,mental_health_issues, legal_weapons
                       , race, gender)
              , class = 'cell-border stripe'
              , rownames = FALSE
              , caption = htmltools::tags$caption(style = 'caption-side: bottom; text-align: center;'
              , 'Table 1: ', htmltools::em('Information filtered by number of fatalities and date'))
    )
    
  })
  
  observeEvent(input$learn_more
               , {showModal(modalDialog(text_learn_more, title = 'About the data')) })
  
  output$download_data <- downloadHandler(
    filename <- "mass_shootings.csv"
    , content <- function(file){
      rval_mass_shootings() %>% 
        select(date, state, site, fatalities
               , injured, weapon_type, age_of_shooter
               , mental_health_issues, legal_weapons, race, gender)
    }
  )
})
