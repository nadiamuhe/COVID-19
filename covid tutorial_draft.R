# COVID-19 Tutorial Draft
#------------------------


# Import data: Johns Hopkins Github data
confirmed <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv")
str(confirmed) # Check latest date at the end of data
deaths <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv")
recovered <- read.csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_recovered_global.csv")
str(recovered)
# Check all raw have same dimensions


# Convert each data set from wide to long AND sort by Country/Province/Date
library(tidyr)
library(dplyr)
confirmed <- confirmed %>% gather(key="date", value="confirmed", -c(Country.Region, Province.State, Lat, Long)) %>% arrange(Country.Region, Province.State, date)
str(confirmed)
deaths <- deaths %>% gather(key="date", value="deaths", -c(Country.Region, Province.State, Lat, Long)) %>% arrange(Country.Region, Province.State, date)
recovered <- recovered %>% gather(key="date", value="recovered", -c(Country.Region, Province.State, Lat, Long)) %>% arrange(Country.Region, Province.State, date)


# Final data: combine all three
covid <- merge(confirmed, deaths) %>% merge(recovered) 
# Fix date variable and convert from character to date
str(covid) # check date character
covid$date <- covid$date %>% sub("X", "", .) %>% as.Date("%m.%d.%y") 
str(covid) # check date Date

# Summary of covid data
summary(covid) # Check latest date



# Aggregate to country level
country <- covid %>% group_by(Country.Region, date) %>% summarize(confirmed=sum(confirmed), cumconfirmed=cumsum(confirmed), deaths=sum(deaths), recovered=sum(recovered)) 
country <- country %>% group_by(Country.Region) %>% mutate(days = date - first(date) + 1)
# Extract specific country: Italy
italy <- country %>% filter(Country.Region=="Italy")
# Aggregate at world level 
world <- country %>% group_by(date) %>% summarize(confirmed=sum(confirmed), cumconfirmed=cumsum(confirmed), deaths=sum(deaths), recovered=sum(recovered)) %>% mutate(days = date - first(date) + 1)
str(world)


# Summary Statistics
summary(country)
by(country$confirmed, country$Country.Region, summary)
by(country$deaths, country$Country.Region, summary)
by(country$recovered, country$Country.Region, summary)


# Visualization
## Barchart of new cases over time
library(ggplot2)
# World confirmed
ggplot(world, aes(x=date, y=confirmed)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Global Confirmed Cases", x= "Date", y= "Daily confirmed cased") +
  theme(plot.title = element_text(hjust = 0.5))

# France confirmed
ggplot(italy, aes(x=date, y=confirmed)) + geom_bar(stat="identity", width=0.1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Cases in Italy", x= "Date", y= "Daily confirmed cased") +
  theme(plot.title = element_text(hjust = 0.5))

# World confirmed, deaths and recovered
str(world)
world %>% gather("Type", "Cases", -c(date, cumconfirmed, days)) %>% 
ggplot(aes(x=date, y=Cases, colour=Type)) + geom_bar(stat="identity", width=0.2, fill="white") +
  theme_classic() +
  labs(title = "Covid-19 Global New Confirmed Cases", x= "Date", y= "Daily confirmed cased") +
  theme(plot.title = element_text(hjust = 0.5))
  



## Line graph of cumulative cases
# World confirmed
ggplot(world, aes(x=days, y=confirmed)) + geom_line() +
  theme_classic() +
  labs(title = "Covid-19 Global Confirmed Cases", x= "Days", y= "Daily confirmed cased") +
  theme(plot.title = element_text(hjust = 0.5))
# Ignore warning

# World confirmed: show log10 scale instead
ggplot(world, aes(x=days, y=confirmed)) + geom_line() +
  theme_classic() +
  labs(title = "Covid-19 Global Confirmed Cases", x= "Days", y= "Daily confirmed cased  (log scale)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(trans="log10")

# World confirmed, deaths and recovered in log scale
str(world)
world %>% gather("Type", "Cases", -c(date, cumconfirmed, days)) %>% 
ggplot(aes(x=days, y=Cases, colour=Type)) + geom_line() +
  theme_classic() +
  labs(title = "Covid-19 Global Confirmed Cases", x= "Days", y= "Daily confirmed cased  (log scale)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(trans="log10")


# Line graph by country for select countries in log scale
countryselection <- country %>% filter(Country.Region==c("US", "Italy", "China", "France", "United Kingdom", "Germany"))
ggplot(countryselection, aes(x=days, y=confirmed, colour=Country.Region)) + geom_line(size=1) +
  theme_classic() +
  labs(title = "Covid-19 Confirmed Cases by Country", x= "Days", y= "Daily confirmed cases  (log scale)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(trans="log10")



# Matrix of line graph for select countries in log scale
str(countryselection)
countryselection %>% gather("Type", "Cases", -c(date, cumconfirmed, days, Country.Region)) %>%
ggplot(aes(x=days, y=Cases, colour=Country.Region)) + geom_line(size=1) +
  theme_classic() +
  labs(title = "Covid-19 Cases by Country", x= "Days", y= "Daily cases (log scale)") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_y_continuous(trans="log10") +
  facet_grid(rows=vars(Type))


## Map 
countrylatest <- country %>% group_by(Country.Region) %>% summarize(cumconfirmed=sum(confirmed), cumdeaths=sum(deaths), cumrecovered=sum(recovered))



# Model







