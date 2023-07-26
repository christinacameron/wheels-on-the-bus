# Loading packages
library(tidyverse) 
library(sf) 
library(tmap)

# Preparing for spatial visualisation
schools_data <- schools_data %>%
filter(!grepl("OneSchool", Org_Name),      # filter out OneSchool Global
       !grepl("Correspondence", Org_Type)) # and Te Aho o Te Kura Pounamu
tmap_mode("view") # set to interactive mode
dbreaks <- c(1:10) # breaks to be used for deprivation and decile
ebreaks <- seq(0, 100, 10) # breaks to be used for European student % in map 1

# 1: European student % vs deprivation
tm_shape(sa2_dep_census_sf) +
  tm_polygons(col = "Median_Dep", # colour by deprivation
              palette = "Reds", # colour palette
              breaks = dbreaks, # breaks
              border.alpha = 0.5, # border transparency
              textNA = "Data not available", # NA in legend
              title = "Deprivation score", # legend title
              id = "SA22018_name", # hover text
              popup.vars = c("SA2 code" = "SA22018_V1", # popup text
                             "Deprivation score" = "Median_Dep")) +
  tm_shape(schools_data) +
  tm_dots(col = "European_perc", # colour by European student %
          palette = "Greys", # colour palette
          scale = 2, # scale of dots
          breaks = ebreaks, # breaks
          showNA = F, # hide NA in legend
          title = "European students (%)", # legend title
          id = "Org_Name", # hover text
          popup.vars = c("European students (%)" = "European_perc"), 
          popup.format = list(digits = 2)) + # popup text and format
  tm_view(symbol.size.fixed = T) + # fixed dot size
  tm_scale_bar(position = c("left", "bottom")) + # scale bar
  tm_layout(title = "White flight from schools in deprived areas") # main title
tmap_last() %>% # retrieve map
  tmap_save("Map1.html") # save map

# 2: European student % vs decile
tm_shape(sa2_dep_census_sf) +
  tm_polygons(alpha = 0, # completely transparent polygons
              border.alpha = 0.5, # border transparency
              id = "SA22018_name", # hover text
              popup.vars = c("SA2 code" = "SA22018_V1")) + # popup text
  tm_shape(schools_data) +
  tm_bubbles(size = "European_perc", # size by European student %
             col = "Decile", # colour by decile
             palette = "RdYlGn", # colour palette
             alpha = 0.5, # transparency of bubbles
             border.col = "black", # colour of borders
             breaks = dbreaks, # breaks
             style = "cat", # break style
             title.col = "Decile", # legend title
             id = "Org_Name", # hover text
             popup.vars = c("European students (%)" = "European_perc",
                            "Decile" = "Decile"), # popup text
             popup.format = list(digits = 2)) +   # and format
  tm_view(symbol.size.fixed = T) + # fixed bubble size
  tm_scale_bar(position = c("left", "bottom")) + # scale bar
  tm_layout(title = "White flight from low-decile schools") # main title
tmap_last() %>% # retrieve map
  tmap_save("Map2.html") # save map

# 3: Māori student % vs authority
tm_shape(sa2_dep_census_sf) +
  tm_polygons(alpha = 0, # completely transparent polygons
              border.alpha = 0.5, # border transparency
              id = "SA22018_name", # hover text
              popup.vars = c("SA2 code" = "SA22018_V1")) + # popup text
  tm_shape(schools_data) +
  tm_bubbles(size = "Māori_perc", # size by Māori student %
             col = "Authority", # colour by authority
             drop.levels = T, # remove unused categories
             labels = c("Private", "State", "State-integrated"), # fix labels
             palette = "Accent", # colour palette
             alpha = 0.5, # transparency of bubbles
             border.col = "black", # border colour
             title.col = "Authority", # legend title
             id = "Org_Name", # hover text 
             popup.vars = c("Māori students (%)" = "Māori_perc"), # popup text
             popup.format = list(digits = 2)) +                   # and format
  tm_view(symbol.size.fixed = T) + # fixed bubble size
  tm_scale_bar(position = c("left", "bottom")) + # scale bar
  tm_layout(title = "Educational segregation by authority") # main title
tmap_last() %>% # retrieve map
  tmap_save("Map3.html") # save map

# 4: Māori student % vs co-educational status
tm_shape(sa2_dep_census_sf) +
  tm_polygons(alpha = 0, # completely transparent polygons
              border.alpha = 0.5, # border transparency
              id = "SA22018_name", # hover text
              popup.vars = c("SA2 code" = "SA22018_V1")) + # popup text
  tm_shape(schools_data) +
  tm_bubbles(size = "Māori_perc", # size by Māori student %
             col = "CoEd_Status", # colour by co-ed status
             drop.levels = T, # remove unused categories
             labels = c("Co-educational", # fix labels
                        "Boys' school", "Girls' school"),
             palette = "-Pastel1", # colour palette
             alpha = 0.5, # transparency of bubbles
             border.col = "black", # border colour
             title.col = "Co-educational status", # legend title
             id = "Org_Name", # hover text
             popup.vars = c("Māori students (%)" = "Māori_perc"), # popup text
             popup.format = list(digits = 2)) +                   # and format
  tm_view(symbol.size.fixed = T) + # fixed bubble size
  tm_scale_bar(position = c("left", "bottom")) + # scale bar
  tm_layout(title = "Educational segregation by co-educational status") # main title
tmap_last() %>% # retrieve map
  tmap_save("Map4.html") # save map