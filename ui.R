
library(shinydashboard)
library(dplyr)
library(lubridate)
library(leaflet)
library(magrittr)
library(forcats)
library(ggplot2)
library(tidyr)
library(shinyWidgets)
library(DT)
library(viridis)
library(plotly)
library(stringr)

# original data 
shootings <- read.csv('mass-shootingsupdate.csv')


# clean the data 
mass_shootings <- shootings %>%  
  mutate(date = mdy(date)
         ,site = fct_recode(location.1,
                            "Workplace" = "\nWorkplace",
                            "Other" = "Other\n")
         ,race = gsub(" ", "", race, fixed = TRUE) # eliminate blank spaces 
         ,race = fct_recode(race,
                            "unclear" = "-",
                            "Black" = "black", 
                            "White" = "white")
         ,state = word(location, -1)
         ,mental_health_issues = gsub(" ", "", prior_signs_mental_health_issues, fixed = TRUE) # eliminate blank spaces 
         ,mental_health_issues = fct_recode(mental_health_issues,
                                            "Unclear" = "-",
                                            "Yes" ="yes", 
                                            "TBD" = "Unknown")
         ,legal_weapons = word(weapons_obtained_legally, 1)
         ,legal_weapons =  fct_recode(legal_weapons, 
                                      "Yes" = "\nYes",
                                      "Unknown" = "-",
                                      "Unknown" = "TBD",
                                      "Unknown" = "Kelley")
         ,gender = fct_recode(gender, 
                              "M" = "Male", 
                              "F" = "Female",
                              "Other" = "-",
                              "M & F" = "Male & Female")
         ,age_of_shooter = as.numeric(age_of_shooter)
         ,age_of_shooter = ifelse(age_of_shooter < 10, age_of_shooter + 10, age_of_shooter )
         ,weapon_type = fct_recode(weapon_type, 
                                   "handgun" = "Handgun", 
                                   "shotgun" = "Shotgun", 
                                   "rifle" = "Rifle")
         ,handgun = ifelse(str_detect(weapon_type, "handgun"), 1, 0)
         ,rifle = ifelse(str_detect(weapon_type, "rifle"), 1, 0)
         ,revolver = ifelse(str_detect(weapon_type, "revolver"), 1, 0)
         ,shotgun = ifelse(str_detect(weapon_type, "shotgun"), 1, 0)
  ) %>% 
  select('date','location', 'state', 'site', 'fatalities', 'injured', 'total_victims', 
         'handgun', 'rifle', 'revolver', 'shotgun', 'weapon_type', 'age_of_shooter',
         'mental_health_issues', 'legal_weapons', 'race', 'gender', 'latitude', 'longitude', 'summary')

#----------------#
#---# HEADER #---#
#----------------#
header <- dashboardHeader(title ="Mass Shootings in the USA", titleWidth = 350)

#-----------------#
#---# SIDEBAR #---#
#-----------------#
sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Geographic location"
             , tabName = "map"
             , icon = icon("globe-americas")
    ),
    menuItem("Location"
             , tabName = "location"
             , icon = icon("map-pin")
    ),
    menuItem("Perpetrators's profile"
             , tabName = "profile"
             , icon = icon("user-alt")
    ),
    menuItem("Type of weapon"
             , tabName = "weapon"
             , icon = icon("unlock-alt")
    ),
    menuItem("Data"
             , tabName = "data"
             , icon = icon("table")
    ),
    sliderInput(inputId = 'nb_fatalities'
                , label = 'Minimum Fatalities'
                , min = min(mass_shootings$fatalities)
                , max = max(mass_shootings$fatalities)
                , value = min(mass_shootings$fatalities)
                , step = 1
    ),
    dateRangeInput(inputId = 'date_range'
                   , label = 'Select Date'
                   , start = min(mass_shootings$date)
                   , end = max(mass_shootings$date)
    ),
    br(),
    actionButton(inputId = 'show_about'
                 , label = 'About'
    )
  )
)

#--------------#
#---# BODY #---#
#--------------#
body <- dashboardBody(
  tabItems(
    #-----------------#
    #---# MAP TAB #---#
    #-----------------#
    tabItem(tabName = "map"
      ,textOutput("summary_inf")
      ,fluidRow(valueBoxOutput("victimsBox")
              , valueBoxOutput("FatalitiesBox")
              , valueBoxOutput("InjuredBox")
              )
      ,leaflet::leafletOutput('map',width = "100%", height = 600)
      ,tags$head(tags$style("#summary_inf{color: black;
                                 font-size: 22px;
                                 font-style: bold;
                                 }"
      )
     )
    ),
    #----------------------#
    #---# LOCATION TAB #---#
    #----------------------#
    tabItem(tabName = "location",
      fluidRow(box(em("The most  common locations of public shooting are retail 
                          establishments such as restaurants and stores.")
                  , br()
                  , strong("shooting location.")
                  , plotlyOutput('shooting_location', height = 300))
             , box(strong("The 10 most affected states from suffering attacks.")
                   , plotlyOutput('shooting_state', height = 300))
      ), 
      fluidRow(box(width = 12
                  , strong("The number of attacks has risen dramatically, 
                           and many of the deadliest shootings have occurred within the past few years.")
                  , plotlyOutput('shooting_by_year', height = 300)))
    ), 
    #---------------------#
    #---# PROFILE TAB #---#
    #---------------------#
    tabItem(tabName = "profile"
      ,fluidRow(
        column(width = 12
        , box(strong("Race of the shooters."), plotlyOutput('race_shooters', height = 300))
        , box(strong("Age by race of the shooters."), plotOutput('age_by_race', height = 300))
        ),
        column(width = 12       
        , box(em('Some of these mass shooters were known to have violent tendencies or criminal pasts.')
              , br()
              , strong("Did the shooter had mental health issues?")
              , plotlyOutput('mental_health', height = 300)
        )
        , valueBoxOutput("mental_health_info", width = 6)
        , valueBoxOutput("race_shooters_info", width = 6)
        , valueBoxOutput("median_age_info", width = 6))
      )  
    ),
    #--------------------#
    #---# WEAPON TAB #---#
    #--------------------#
    tabItem(tabName = "weapon"
    , fluidRow(
        column(width = 12
        , box(strong("Did weapons were obtained legally?")
             , plotlyOutput('legal_weapon', height = 300))
        , box(strong("Type of weapon ")
             , plotlyOutput('type_weapon', height = 300))
      ),
        column(width = 12
        , box(em('Shooters often carried more than one weapon.')
              , br()
              , strong("How many types of weapons did the shooter possed?")
              , plotlyOutput('several_types', height = 300))
        , valueBoxOutput("legal_weapon_info", width = 6)
        , valueBoxOutput("type_weapon_info", width = 6)
        , valueBoxOutput("several_types_info", width = 6))
      )
    ),
    #------------------#
    #---# DATA TAB #---#
    #------------------#
    tabItem(tabName = "data"
      , downloadButton(outputId = "download_data", label = "Download")
      , br()
      , br()
      , DT::DTOutput("table")
      , actionButton(inputId = 'learn_more', label = 'Learn more')
      , tags$style(" #download_data {
      /* Change the background color of the download button to orange. */
      background: orange;}"
      )
    )
  )
)

#-----------------#
#---# UI call #---#
#-----------------#

ui <- dashboardPage(skin= 'purple'
                  , header = header
                  , sidebar=sidebar
                  , body = body)
