library("readr")
library("dplyr")
library("plotly")
library("ggplot2")
library("htmlwidgets")

df <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2014_world_gdp_with_codes.csv')
df2<- read.csv("https://gist.githubusercontent.com/tadast/8827699/raw/7255fdfbf292c592b75cf5f7a19c16ea59735f74/countries_codes_and_coordinates.csv")
df3 <- left_join(df,df2,by = c("COUNTRY" = "Country" ))
# light grey boundaries
l <- list(color = toRGB("grey"), width = 0.5)

# specify map projection/options
g <- list(
  showframe = FALSE,
  showcoastlines = FALSE,
  projection = list(
    type = 'orthographic',
    rotation = list(
      lon = -100,
      lat = 40,
      roll = 0
    ))
)

p <- plot_geo(df3) %>%
  add_trace(
    z = ~GDP..BILLIONS.*1000, colors = 'Reds',
    text = ~COUNTRY, locations = ~CODE, marker = list(line = l)
  ) %>%
  add_segments(
    x = ~Longitude..average., xend = 25,
    y = ~Latitude..average., yend = 25,
    alpha = 0.3, color= toRGB("gray60"),size = ~log10(GDP..BILLIONS.), hoverinfo = "none"
  )%>%
  colorbar(title = 'GDP Millions US$', tickprefix = '$') %>%
  layout(
    title = '2014 Global GDP<br>Source:<a href="https://www.cia.gov/library/publications/the-world-factbook/fields/2195.html">CIA World Factbook</a>',
    geo = g
  )
p
