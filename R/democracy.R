library(leaflet)
library(DT)
library(sp)
library(rnaturalearth)
library(tidyverse)
library(ggplot2)
library(plotly)

# import datasets
democracy_number <- read.csv('./data/number-democracies-age-bmr.csv')
democracy_age <- read.csv('./data/age-of-democracy-bmr.csv')

liberal_democracy_number <- read.csv('./data/number-liberal-democracies-age.csv')
liberal_democracy_age <- read.csv('./data/age-of-liberal-democracy-row.csv')

electoral_democracy_number <- read.csv('./data/number-electoral-democracies-age.csv')
electoral_democracy_age <- read.csv('./data/age-of-electoral-democracy.csv')

# rename columns
colnames(democracy_number)[c(4,5,6,7,8,9)] <- c('non-democracies',
                                                '1-18 years',
                                                '19-30 years',
                                                '31-60 years',
                                                '61-90 years',
                                                '91 years +')
colnames(liberal_democracy_number)[c(4,5,6,7,8,9,10,11)] <- c('close autocracies',
                                                              'electoral autocracies',
                                                              'close democracies',
                                                              '1-18 years',
                                                              '19-30 years',
                                                              '31-60 years',
                                                              '61-90 years',
                                                              '91 years +')
colnames(electoral_democracy_number)[c(4,5,6,7,8,9,10)] <- c('91 years +',
                                                             '61-90 years',
                                                             '31-60 years',
                                                             '19-30 years',
                                                             '1-18 years',
                                                             'electoral autocracies',
                                                             'close autocracies')

colnames(democracy_age)[4] <- 'age'
colnames(liberal_democracy_age)[4] <- 'age'
colnames(electoral_democracy_age)[4] <- 'age'

#democracy maps

democracy_age %>% filter(Year==2020) -> map
ne_countries <- ne_countries(scale = "medium", returnclass = "sf")
map_sp <- merge(ne_countries, map, by.x = "iso_a3", by.y = "Code", all.x = TRUE)
map_sp <- map_sp[, c("name","iso_a3", "geometry", "age")]
map_sp$age[map_sp$age == "no data"] <- -2
map_sp$age[map_sp$age == "non-democracy"] <- -1
map_sp$age[is.na(map_sp$age)] <- -2
map_sp$age <- as.numeric(map_sp$age)

map_sp <- sf::st_transform(map_sp, "+proj=longlat +datum=WGS84")

leaflet(map_sp) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~colorBin(c('#F2EFE9','red','#9ECAE1','#4292C6','#2171B5','#1E69AF','#084594'), domain = c(-2,-1,0,18,30,60,90,221), bins = c(-2,-1,0,18,30,60,90,221))(map_sp$age),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~ paste0(name, ": ", ifelse(
      age == -2, 'no data', ifelse(age == -1, 'non-democracy', age)
    ))
  ) %>%
  addLegend(
    position = "bottomright",
    title = "Age",
    colors = c('#F2EFE9','red','#9ECAE1','#4292C6','#2171B5','#1E69AF','#084594'),
    labels = c("No data", "Non-democracy", "0-18 years", "18-30 years", "30-60 years", "60-90 years", "90-221 years"),
    values = ~age,
    opacity = 1
  )

# liberal democracy
liberal_democracy_age %>% filter(Year==2020) -> map
ne_countries <- ne_countries(scale = "medium", returnclass = "sf")
map_sp <- merge(ne_countries, map, by.x = "iso_a3", by.y = "Code", all.x = TRUE)
map_sp <- map_sp[, c("name","iso_a3", "geometry", "age")]
map_sp$age[map_sp$age == "no data"] <- -4
map_sp$age[map_sp$age == "electoral democracy"] <- -1
map_sp$age[map_sp$age == "electoral autocracy"] <- -2
map_sp$age[map_sp$age == "closed autocracy"] <- -3
map_sp$age[is.na(map_sp$age)] <- -4
map_sp$age <- as.numeric(map_sp$age)

map_sp <- sf::st_transform(map_sp, "+proj=longlat +datum=WGS84")

leaflet(map_sp) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~colorBin(c('#F2EFE9','red','#FF5C5D','#EDF8B1','#9ECAE1','#4292C6','#2171B5','#1E69AF','#084594'), domain = c(-4,-3,-2,-1,0,18,30,60,90,221), bins = c(-4,-3,-2,-1,0,18,30,60,90,221))(map_sp$age),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~ paste0(name, ": ", ifelse(age==-4,'No data',ifelse(age==-3,'Closed autocracy',ifelse(age==-2,'Electoral autocracy',ifelse(age==-1,'Electoral democracy',age)))))
  ) %>%
  addLegend(
    position = "bottomright",
    title = "Age",
    colors = c('#F2EFE9','red','#FF5C5D','#EDF8B1','#9ECAE1','#4292C6','#2171B5','#1E69AF','#084594'),
    labels = c("No data","Closed autocracy", "Electoral autocracy","Electoral democracy", "0-18 years", "18-30 years", "30-60 years", "60-90 years", "90-221 years"),
    values = ~age,
    opacity = 1
  )

# electoral democracies
electoral_democracy_age %>% filter(Year==2020) -> map
ne_countries <- ne_countries(scale = "medium", returnclass = "sf")
map_sp <- merge(ne_countries, map, by.x = "iso_a3", by.y = "Code", all.x = TRUE)
map_sp <- map_sp[, c("name","iso_a3", "geometry", "age")]
map_sp$age[map_sp$age == "no data"] <- -3
map_sp$age[map_sp$age == "electoral autocracy"] <- -1
map_sp$age[map_sp$age == "closed autocracy"] <- -2
map_sp$age[is.na(map_sp$age)] <- -3
map_sp$age <- as.numeric(map_sp$age)

map_sp <- sf::st_transform(map_sp, "+proj=longlat +datum=WGS84")

leaflet(map_sp) %>%
  addTiles() %>%
  addPolygons(
    fillColor = ~colorBin(c('#F2EFE9','red','#FF5C5D','#9ECAE1','#4292C6','#2171B5','#1E69AF','#084594'), domain = c(-3,-2,-1,0,18,30,60,90,174), bins = c(-3,-2,-1,0,18,30,60,90,174))(map_sp$age),
    weight = 2,
    opacity = 1,
    color = "white",
    dashArray = "3",
    fillOpacity = 0.7,
    highlight = highlightOptions(
      weight = 5,
      color = "#666",
      dashArray = "",
      fillOpacity = 0.7,
      bringToFront = TRUE
    ),
    label = ~ paste0(name, ": ", ifelse(age==-3,'No data',ifelse(age==-2,'Closed autocracy',ifelse(age==-1,'Electoral democracy',age))))
  ) %>%
  addLegend(
    position = "bottomright",
    title = "Age",
    colors = c('#F2EFE9','red','#FF5C5D','#9ECAE1','#4292C6','#2171B5','#1E69AF','#084594'),
    labels = c("No data","Closed autocracy", "Electoral autocracy", "0-18 years", "18-30 years", "30-60 years", "60-90 years", "90-174 years"),
    values = ~age,
    opacity = 1
  )


# democracy number
democracy_number %>% filter(Entity=='Africa', Year==2020) -> selected_data
selected_data <- selected_data[4:9]
df_gathered <- gather(selected_data, key = "variable", value = "value")

ggplot(df_gathered, aes(x = variable, y = value)) +
  geom_bar(stat = "identity") +
  labs(x = NULL, y = "Values")

# liberal number

liberal_democracy_number %>% filter(Entity=='Africa', Year==2020) -> selected_data
selected_data <- selected_data[4:11]
df_gathered <- gather(selected_data, key = "variable", value = "value")

ggplot(df_gathered, aes(x = variable, y = value)) +
  geom_bar(stat = "identity") +
  labs(x = NULL, y = "Values")

# electoral number

electoral_democracy_number %>% filter(Entity=='Africa', Year==2022) -> selected_data
selected_data <- selected_data[4:10]
selected_data <- selected_data[, rev(names(selected_data))]
df_gathered <- gather(selected_data, key = "variable", value = "value")

gg <- ggplot(df_gathered, aes(x = variable, y = value)) +
  geom_bar(stat = "identity") +
  labs(x = NULL, y = "Values")

ggplotly(gg)

# democracy number - summary
democracy_number %>% filter(Entity=='Africa', Year==2020) -> selected_data
selected_data <- selected_data[4:9]

headers <- c('Non-democracies','Democracies')
nondem <- selected_data$`non-democracies`
dem <- apply(selected_data[-1],1,sum)

dem_data <- data.frame(
  headers,
  values = c(nondem,dem)
)

ggplot(dem_data, aes(x="", y=values, fill=headers)) +
  geom_bar(stat="identity", width=1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(fill="Political System") +
  ggtitle("Democracies") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c('#9ECAE1','#FF5C5D'))

# liberal number - summary

liberal_democracy_number %>% filter(Entity=='Africa', Year==2020) -> selected_data
selected_data <- selected_data[4:11]

headers <- c('Non-democracies','Democracies')
nondem <- apply(selected_data[1:2],1,sum)
dem <- apply(selected_data[3:8],1,sum)

dem_data <- data.frame(
  headers,
  values = c(nondem,dem)
)

ggplot(dem_data, aes(x="", y=values, fill=headers)) +
  geom_bar(stat="identity", width=1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(fill="Political System") +
  ggtitle("Democracies") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c('#9ECAE1','#FF5C5D'))

# electoral number - summary

electoral_democracy_number %>% filter(Entity=='Africa', Year==2022) -> selected_data
selected_data <- selected_data[4:10]
selected_data <- selected_data[, rev(names(selected_data))]

headers <- c('Non-democracies','Democracies')
nondem <- apply(selected_data[1:2],1,sum)
dem <- apply(selected_data[3:7],1,sum)

dem_data <- data.frame(
  headers,
  values = c(nondem,dem)
)

ggplot(dem_data, aes(x="", y=values, fill=headers)) +
  geom_bar(stat="identity", width=1) +
  coord_polar(theta = "y") +
  theme_void() +
  labs(fill="Political System") +
  ggtitle("Democracies") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values = c('#9ECAE1','#FF5C5D'))

# democracy number summary years
democracy_number %>% filter(Entity=='orld') -> selected_data
selected_data <- selected_data[3:9]
selected_data$democracies <- apply(selected_data[3:7],1,sum)

ggplot() + 
  geom_line(aes(x = selected_data$Year, y = selected_data$democracies, color = "democracies")) +
  geom_line(aes(x = selected_data$Year, y = selected_data$`non-democracies`, color = "non democracies")) +
  labs(x="Years", y="Countries") +
  ggtitle("Democracies") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(name="Political System", 
                     values=c("democracies"="#9ECAE1", "non democracies"="#FF5C5D"))

# liberal democracy number summary years
liberal_democracy_number %>% filter(Entity=='World') -> selected_data
selected_data <- selected_data[3:11]
selected_data$`non-democracies` <- apply(selected_data[2:3],1,sum)
selected_data$democracies <- apply(selected_data[4:9],1,sum)

ggplot() + 
  geom_line(aes(x = selected_data$Year, y = selected_data$democracies, color = "democracies")) +
  geom_line(aes(x = selected_data$Year, y = selected_data$`non-democracies`, color = "non democracies")) +
  labs(x="Years", y="Countries") +
  ggtitle("Democracies") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(name="Political System", 
                     values=c("democracies"="#9ECAE1", "non democracies"="#FF5C5D"))

# electoral democracy number summary years
electoral_democracy_number %>% filter(Entity=='Africa') -> selected_data
selected_data <- selected_data[3:10]
selected_data <- selected_data[, rev(names(selected_data))]

selected_data$`non-democracies` <- apply(selected_data[1:2],1,sum)
selected_data$democracies <- apply(selected_data[3:7],1,sum)

ggplot() + 
  geom_line(aes(x = selected_data$Year, y = selected_data$democracies, color = "democracies")) +
  geom_line(aes(x = selected_data$Year, y = selected_data$`non-democracies`, color = "non democracies")) +
  labs(x="Years", y="Countries") +
  ggtitle("Democracies") +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_color_manual(name="Political System", 
                     values=c("democracies"="#9ECAE1", "non democracies"="#FF5C5D"))
