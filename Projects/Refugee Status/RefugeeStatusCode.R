install.packages("rworldmap")
install.packages("ggmap")
install.packages("mapproj")
install.packages("countrycode")
library(tidyverse)
library(countrycode)  
library(lubridate)
library(readr)

refugees_raw <- read_csv("refugee_status.csv", na = c("-", "X", "D"))
non_countries <- c("Africa", "Asia", "Europe", "North America", "Oceania",
                   "South America", "Unknown", "Other", "Total")
refugees_clean <- refugees_raw %>%
  # Make this column name easier to work with
  rename(origin_country = `Continent/Country of Nationality`) %>%
  # Get rid of non-countries
  filter(!(origin_country %in% non_countries)) %>%
  # Convert country names to ISO3 codes
  mutate(iso3 = countrycode(origin_country, "country.name", "iso3c",
                            custom_match = c("Korea, North" = "PRK"))) %>%
  # Convert ISO3 codes to country names, regions, and continents
  mutate(origin_country = countrycode(iso3, "iso3c", "country.name"),
         origin_region = countrycode(iso3, "iso3c", "region"),
         origin_continent = countrycode(iso3, "iso3c", "continent")) %>%
  # Make this data tidy
  gather(year, number, -origin_country, -iso3, -origin_region, -origin_continent) %>%
  # Make sure the year column is numeric + make an actual date column for years
  mutate(year = as.numeric(year),
         year_date = ymd(paste0(year, "-01-01")))

refugees_countries_cumulative <- refugees_clean %>%
  arrange(year_date) %>%
  group_by(origin_country) %>%
  mutate(cumulative_total = cumsum(number))

refugees_continents <- refugees_clean %>%
  group_by(origin_continent, year_date) %>%
  summarize(total = sum(number, na.rm = TRUE))

refugees_continents_cumulative <- refugees_clean %>%
  group_by(origin_continent, year_date) %>%
  summarize(total = sum(number, na.rm = TRUE)) %>%
  arrange(year_date) %>%
  group_by(origin_continent) %>%
  mutate(cumulative_total = cumsum(total))


#World Map Code
install.packages("rworldmap")
install.packages("ggmap")
install.packages("mapproj")
library(rworldmap)
library(ggmap)
library(grid)

Installpackage 'mapproj'

# Get the world map
worldMap <- getMap()

# Member States of the European Union
europeanUnion <- c("Argentina", "Bolivia", "Brazil",
                   "Chile", "Colombia", "Ecuador",
                   "Guyana", "Paraguay", "Peru",
                   "Suriname", "Uruguay", "Venezuela")
# Select only the index of states member of the E.U.
indEU <- which(worldMap$NAME%in%europeanUnion)

europeCoords <- lapply(indEU, function(i){
  df <- data.frame(worldMap@polygons[[i]]@Polygons[[1]]@coords)
  df$region =as.character(worldMap$NAME[i])
  colnames(df) <- list("long", "lat", "region")
  return(df)
})

europeCoords <- do.call("rbind", europeCoords)
# Add some data for each member
value <- sample(x = seq(0,3,by = 0.1), size = length(europeanUnion),
                replace = TRUE)
europeanUnionTable <- data.frame(country = europeanUnion, value = value)
europeCoords$value <- europeanUnionTable$value[match(europeCoords$region,europeanUnionTable$country)]

# Plot the map
P <- ggplot() + geom_polygon(data = europeCoords, aes(x = long, y = lat, group = region, fill = value),
                             colour = "black", size = 0.1) +
  coord_map(xlim = c(-100, 100),  ylim = c(-100, 100))

P <- P + scale_fill_gradient(name = "Growth Rate", low = "#FF0000FF", high = "#FFFF00FF", na.value = "grey50")


P <- P + theme(#panel.grid.minor = element_line(colour = NA), panel.grid.minor = element_line(colour = NA),
  #panel.background = element_rect(fill = NA, colour = NA),
  axis.text.x = element_blank(),
  axis.text.y = element_blank(), axis.ticks.x = element_blank(),
  axis.ticks.y = element_blank(), axis.title = element_blank(),
  #rect = element_blank(),
  plot.margin = unit(0 * c(-1.5, -1.5, -1.5, -1.5), "lines"))
P


b<- ggplot(refugees_continents_cumulative,
           aes(x=year, y=total, color=origin_continent))+
  geom_point() + 
  geom_line() +
  ylab("amount granted refugee status") +
  scale_x_discrete(limits=c(2006,2007,2008,2009,2010,2011,2012,2013,2014,2015))+
  ggtitle("Total Amount of People Granted Refugee Status from 2006-2015") +
  guides(col=guide_legend("Continent"))+
  theme_minimal()

b + scale_color_manual(values=c("#0099FF","#FF6600","#9900CC","#FF00CC"))



refugees_america <- refugees_clean %>%
  filter(origin_continent == "Americas")

ggplot(refugees_america,
       aes(x=year, y=number, color=origin_country))+
  geom_point() + 
  geom_line()

