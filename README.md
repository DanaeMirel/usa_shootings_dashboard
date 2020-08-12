# Mass Shootings in the USA Shiny Dashboard 

This dashboard provides descriptive analysis about mass shooting in the United States since 1982. 
The data comes from the 
[Mother Jones’ Investigation](https://www.motherjones.com/politics/2012/12/mass-shootings-mother-jones-full-data/) 
about mass shootings in the USA.

It's possible to obtain summary statistics filtering the data by date and number of fatalities involved in each attack. 
The dashboard is organized in five tabs that are described below.

## Tab 1 : Geographic Location 

This tab provides a general insight about the location and number of victims regarding mass shootings having taken place between the selected dates. 
The map has been build using the [`leaet` package](http://rstudio.github.io/leaflet/).

![view](images\geographic_loc.png)

## Tab 2 : Location 

Here we show the most common public spaces in where the shootings take place, been the most frequents retail establishments such as restaurants and stores. 
We also include a graphic with the most affected states and the number of shootings by year according to the applied filters. 
The interactivity of the graphs has been added by using the [`plotly` package](https://plot.ly/r/). 

### Tab 3 : Shooter' Profile 

This tab shows a brief descriptive analysis of the demographic variables of the attackers, such as the race, the age, and whether they suffer a mental ill or not. 

### Tab 4 : type of weapon 

A description of the number of weapons, the type of gun and whether it were obtained legally or not is provided in this tab.

### Tab 5 : Data 
A table showing the row data is included in this tab. 
The interactivity of the table has been added using [`DT` package](http://rstudio.github.io/DT/) 


It's possible to download the data used in these analysis in the [Mother Jones](https://www.motherjones.com/politics/2012/12/mass-shootings-mother-jones-full-data/) web site. 

Visit [shiny io](https://danaemirelmartinez.shinyapps.io/usa_shootings_dashboard/) to see the action!



