library("tidyverse",warn.conflicts = FALSE)
library("plotly",warn.conflicts = FALSE)
library("rjson",warn.conflicts = FALSE)
library(plotly)
library(usdata)


#source data
df <- read_csv("../source/incarceration-trends-master/incarceration_trends.csv",show_col_types = FALSE)
View(df)
#find the total population of each year  
total_population <- select(df, year, total_jail_pop, total_prison_pop) %>% 
  group_by(year)%>%
    summarise(
      total_population = round(sum(total_jail_pop, na.rm = TRUE) +
                                 sum(total_prison_pop, na.rm = TRUE))
)
total_population2 <- total_population %>% slice(-c(48,49))

view(total_population2)

max_pop <- max(total_population2)
min_pop <- min(total_population2)
max_year <- max(total_population2$year)
min_year <- min(total_population2$year)
change <- (max_pop-min_pop)/(max_year-min_year)
View(change)
#create line chart of the total population increased by year 
line_plot<- plot_ly(
  data = total_population2,     
  x = ~year, 
  y = ~total_population, 
  type = "scatter", 
  mode = "lines" 
) %>% layout(
  title = "Jail and prison population increased by year",
  xaxis = list(title = 'Year'), 
  yaxis = list(title = 'Population') 
)

line_plot

blackwhite_population <- select(df, year, black_prison_pop, black_jail_pop,white_prison_pop,white_jail_pop )%>%
  group_by(year)%>%
    summarise(black_pop_total = sum(black_prison_pop,na.rm = TRUE) + sum(black_jail_pop,na.rm = TRUE),
              sum_white_pop = sum(white_prison_pop,na.rm = TRUE) + sum(white_jail_pop,na.rm = TRUE))
change_black_pop <- (247546.-3171.0)/(2018-1970)
change_white_pop <- (346061.8-1666.0)/(2018-1970)


race_population <- select(df, year, aapi_prison_pop, black_prison_pop, latinx_prison_pop,
                          native_prison_pop, white_prison_pop, aapi_jail_pop, black_jail_pop,
                          latinx_jail_pop, native_jail_pop, white_jail_pop) %>% 
  group_by(year)%>%
  summarise(sum_aapi_pop = sum(aapi_prison_pop,na.rm = TRUE) + sum(aapi_jail_pop,na.rm = TRUE) ,
            sum_black_pop = sum(black_prison_pop,na.rm = TRUE) + sum(black_jail_pop,na.rm = TRUE),
            sum_latinx_pop = sum(latinx_prison_pop,na.rm = TRUE) + sum(latinx_jail_pop,na.rm = TRUE),
            sum_native_pop = sum(native_prison_pop,na.rm = TRUE) + sum(native_jail_pop,na.rm = TRUE),
            sum_white_pop = sum(black_prison_pop,na.rm = TRUE) + sum(black_jail_pop,na.rm = TRUE),
            Total_jail_pop = sum(sum_aapi_pop ,sum_black_pop , sum_latinx_pop , sum_native_pop ,sum_white_pop)
  )
race_population2 <- race_population %>% slice(-c(1:15, 48:49))

View(race_population2)
#create the stacked bar chart 
Population_Count <- year
year <- c(race_population2$year)
aapi <- c(race_population2$sum_aapi_pop)
black <- c(race_population2$sum_black_pop)
latinx <- c(race_population2$sum_latinx_pop)
native <- c(race_population2$sum_native_pop)
white <- c(race_population2$sum_white_pop)

data <- data.frame(race_population2,year, aapi, black, latinx, native, white)

fig <- plot_ly(data, x = ~race_population2, y = ~aapi, type = 'bar', name = 'aapi')
fig <- fig %>% add_trace(y = ~black, name = 'Black')
fig <- fig %>% add_trace(y = ~latinx, name = 'Latinx')
fig <- fig %>% add_trace(y = ~native, name = 'Native')
fig <- fig %>% add_trace(y = ~white, name = 'White')
fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')

fig

total_pop <- select(df, year, aapi_pop_15to64, black_pop_15to64, latinx_pop_15to64, native_pop_15to64,
                    white_pop_15to64,) %>% 
  group_by(year)%>%
  summarise(total_aapi_pop = sum(aapi_pop_15to64),
            total_black_pop = sum(black_pop_15to64),
            total_latinx_pop = sum(latinx_pop_15to64),
            total_native_pop = sum(native_pop_15to64),
            total_white_pop = sum(white_pop_15to64)
  )%>% slice(-c(1:20))

View(total_pop)

#sort out urban and the different area code 
state_map_2016 <- filter(df, year == 2016, na.rm = TRUE)
View(state_map_2016)
state_map <- select(state_map_2016, state, aapi_prison_pop, black_prison_pop, latinx_prison_pop,
                         native_prison_pop, white_prison_pop, aapi_jail_pop, black_jail_pop,
                         latinx_jail_pop, native_jail_pop, white_jail_pop) %>%
  
  group_by(state)%>%
  summarise(
            sum_aapi_pop = sum(aapi_prison_pop,na.rm = TRUE) + sum(aapi_jail_pop,na.rm = TRUE) ,
            sum_black_pop = sum(black_prison_pop,na.rm = TRUE) + sum(latinx_jail_pop,na.rm = TRUE),
            sum_latinx_pop = sum(latinx_prison_pop,na.rm = TRUE) + sum(black_jail_pop,na.rm = TRUE),
            sum_native_pop = sum(native_prison_pop,na.rm = TRUE) + sum(native_jail_pop,na.rm = TRUE),
            sum_white_pop = sum(black_prison_pop,na.rm = TRUE) + sum(black_jail_pop,na.rm = TRUE),
            total_pop = sum(sum_aapi_pop ,sum_black_pop , sum_latinx_pop , sum_native_pop ,sum_white_pop)
          )
  
View(state_map)
#write.csv(state_map,'state_map.csv')
total_pop_states <- sum(state_map$total_pop)
black_total_2016 <- sum(state_map$sum_black_pop)
white_total_2016 <- sum(state_map$sum_white_pop)

black_ratio<- black_total_2016/28461107
white_ratio<- white_total_2016/130697535


df2 <- state_map
df2$hover <- with(df2, paste(state,"Aapi", sum_aapi_pop, "Black",sum_black_pop,"Latinx",sum_latinx_pop, 
                            "Native",sum_native_pop, "White",sum_white_pop))
View(df2)
# give state boundaries a white border
l <- list(color = toRGB("white"), width = 2)
# specify some map projection/options
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)

fig2 <- plot_geo(df2,locationmode = 'USA-states')
fig2 <- fig2 %>% add_trace(
  z = ~total_pop, text = ~hover, locations = ~state,
  color = ~total_pop, colors = 'Blues'
)
fig2 <- fig2 %>% colorbar(title = "population count")
fig2 <- fig2 %>% layout(
  title = 'Us jail population and location count',
  geo = g
)

fig2

