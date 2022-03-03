library("tidyverse",warn.conflicts = FALSE)
library("plotly",warn.conflicts = FALSE)
library("rjson",warn.conflicts = FALSE)
library(plotly)
library(rjson)

json <- fromJSON(file="https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json")
#source data
code <- read_csv("../source/incarceration-trends-master/incarceration_trends.csv",show_col_types = FALSE)
View(code)
df <- read_csv("../source/incarceration-trends-master/incarceration_trends.csv",show_col_types = FALSE)
#find the total population of each year  
total_population <- select(df, year, total_jail_pop, total_prison_pop) %>% 
  group_by(year)%>%
    summarise(
      total_population = round(sum(total_jail_pop, na.rm = TRUE) +
                                 sum(total_prison_pop, na.rm = TRUE))
)
total_population2 <- total_population %>% slice(-c(48,49))

view(total_population2)
 
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

race_population <- select(df, year, aapi_prison_pop, black_prison_pop, latinx_prison_pop,
                          native_prison_pop, white_prison_pop, aapi_jail_pop, black_jail_pop,
                          latinx_jail_pop, native_jail_pop, white_jail_pop) %>% 
  group_by(year)%>%
  summarise(sum_aapi_pop = sum(aapi_prison_pop,na.rm = TRUE) + sum(aapi_jail_pop,na.rm = TRUE) ,
            sum_black_pop = sum(black_prison_pop,na.rm = TRUE) + sum(latinx_jail_pop,na.rm = TRUE),
            sum_latinx_pop = sum(latinx_prison_pop,na.rm = TRUE) + sum(black_jail_pop,na.rm = TRUE),
            sum_native_pop = sum(native_prison_pop,na.rm = TRUE) + sum(native_jail_pop,na.rm = TRUE),
            sum_white_pop = sum(black_prison_pop,na.rm = TRUE) + sum(black_jail_pop,na.rm = TRUE) 
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

fig <- plot_ly(data, x = ~Population_Count, y = ~aapi, type = 'bar', name = 'aapi')
fig <- fig %>% add_trace(y = ~black, name = 'Black')
fig <- fig %>% add_trace(y = ~latinx, name = 'Latinx')
fig <- fig %>% add_trace(y = ~native, name = 'Native')
fig <- fig %>% add_trace(y = ~white, name = 'White')
fig <- fig %>% layout(yaxis = list(title = 'Count'), barmode = 'stack')

fig

#sort out urban and the different area code 
county_map <- select(df,county_name, aapi_prison_pop, black_prison_pop, latinx_prison_pop,
                         native_prison_pop, white_prison_pop, aapi_jail_pop, black_jail_pop,
                         latinx_jail_pop, native_jail_pop, white_jail_pop) %>%
  group_by(county_name)%>%
  summarise(
            sum_aapi_pop = sum(aapi_prison_pop,na.rm = TRUE) + sum(aapi_jail_pop,na.rm = TRUE) ,
            sum_black_pop = sum(black_prison_pop,na.rm = TRUE) + sum(latinx_jail_pop,na.rm = TRUE),
            sum_latinx_pop = sum(latinx_prison_pop,na.rm = TRUE) + sum(black_jail_pop,na.rm = TRUE),
            sum_native_pop = sum(native_prison_pop,na.rm = TRUE) + sum(native_jail_pop,na.rm = TRUE),
            sum_white_pop = sum(black_prison_pop,na.rm = TRUE) + sum(black_jail_pop,na.rm = TRUE),
            total_pop = sum_aapi_pop + sum_black_pop + sum_latinx_pop + sum_native_pop + sum_white_pop
  )
  

View(county_map)
write.csv(county_map,'county_map.cvs')

url <- 'https://raw.githubusercontent.com/plotly/datasets/master/geojson-counties-fips.json'
counties <- rjson::fromJSON(file=url)
url2<- 'https://github.com/info-201a-wi22/a3-AZAHAZA/blob/main/source/urbanicity_map.cvs'
data <- read.csv(url2, colClasses=c(county_name="character"))
g <- list(
  scope = 'usa',
  projection = list(type = 'albers usa'),
  showlakes = TRUE,
  lakecolor = toRGB('white')
)
fig2 <- plot_ly()
fig2 <- fig2 %>% add_trace(
  type="choropleth",
  geojson=counties,
  locations=data$county_name,
  z=data$total_pop,
  colorscale="Viridis",
  zmin=0,
  zmax=2535942,
  marker=list(line=list(
    width=0)
  )
)
fig2 <- fig2 %>% colorbar(title = "US jail population")
fig2 <- fig2 %>% layout(
  title = "US location vs. jail population"
)

fig2 <- fig2 %>% layout(
  geo = g
)

fig2

