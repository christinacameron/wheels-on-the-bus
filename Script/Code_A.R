# Loading packages
library(tidyverse) 
library(sf) 
library(GGally) 

# Importing and cleaning SA2 data
sa22018 <- st_read("Data/SA2-2018-clipped-generalised-SHP")
sa22018 <- sa22018 %>% # filter to North Island
  filter(SA22018_V1 < "300000") %>%
  st_transform(3857) # convert CRS to Web Mercator

# Importing and cleaning schools data
schools <- read_csv("Data/schooldirectory-25-09-2021-083022.csv")
schools <- schools %>% # keep only relevant columns
  select(c(-School_Id, -Telephone, -Fax, -Email, -Contact1_Name, 
           -Regional_Council, -Local_Office_Name, -Education_Region, 
           -General_Electorate, -Māori_Electorate, -Area_Unit, -Ward, 
           -Col_Id, -Col_Name, -Roll_Date, -School_Donations), 
         -Territorial_Authority, -(contains("Add"))) %>%
   drop_na(Latitude) %>%   # remove entries without geometry
  mutate(across(Definition:Isolation_Index, # change "Not calculated" etc. to 0
                ~ifelse(. %in% c("Not calculated",
                                 "Not applicable", 
                                 "Not Applicable"), 0, .))) %>%
  mutate(Decile = ifelse(Decile == 99, 10, Decile)) %>% # change decile 99 to 10
  mutate(CoEd_Status = ifelse(CoEd_Status %in% "Senior Co-ed; Junior Boys", 
                               "Co-Educational", # change one school to fit in
                              CoEd_Status)) %>%
  mutate(European_perc = round(European/Total*100,2)) %>% # add percentage
  mutate(Māori_perc = round(Māori/Total*100,2)) %>%       # columns for race
  mutate(Isolation_Index = # make isolation numeric
           as.numeric(Isolation_Index)) %>% 
  filter(!grepl("Proposed", Org_Name), # filter out proposed schools,
        !grepl("10", Org_Type),        # and those ending at Year 10
         grepl("Secondary|Composite|Parent|Correspondence", 
               Org_Type)) # narrow down to NCEA-age schools

# Converting schools to sf
schools_sf <- schools %>%
  st_as_sf(coords = c("Longitude", "Latitude"), 
           crs = "+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0") %>%
  st_transform(3857) %>% # convert CRS to Web Mercator
  st_filter(sa22018) # filter to North Island

# Importing and cleaning census data
census <- st_read("Data/census-individual-part-2-SHP")
census <- census %>% # keep only relevant columns
  select(c(SA12018_V1, Qual_None = C18_Highes, Qual_L1 = C18_High_1, 
           Qual_L2 = C18_High_2, Qual_L3 = C18_High_3, Qual_Total = C18_High14, 
           geometry)) %>%
  mutate(across(Qual_None:Qual_L3, # change -999-suppressed data to 0
              ~ifelse(. == -999, 0, .))) %>%
  filter(SA12018_V1 < 7999901)  # remove data with empty geometries

# Importing and cleaning deprivation data
nzdep2018 <- read_delim("https://www.otago.ac.nz/wellington/otago730418.txt", 
                        delim = "\t")
nzdep2018 <- na.omit(nzdep2018) # remove NAs

# Joining nzdep2018 to census
colnames(nzdep2018)[1]<-"SA12018_V1" # change column names to match
colnames(nzdep2018)[5]<-"SA22018_V1" 
nzdep2018 <- nzdep2018 %>% # change to character for joins
  mutate(SA12018_V1 = as.character(SA12018_V1)) %>%
  mutate(SA22018_V1 = as.character(SA22018_V1))
dep_census <- left_join(nzdep2018, census, # join by SA1
                        by = c("SA12018_V1" = "SA12018_V1"))

# Joining dep_census to sa22018
sa2_dep_census <- dep_census %>%
  select(c(NZDep2018, # select relevant variables
           URPopnSA1_2018:Qual_Total)) %>% 
  group_by(SA22018_V1, SA22018_name) %>% # group by SA2
  summarise(NZDep2018, # find SA2 median dep
            Median_Dep = median(NZDep2018, na.rm = T), 
            URPopnSA1_2018, # sum SA2 population
            SA2_Pop = sum(URPopnSA1_2018, na.rm = T), 
            across(Qual_None:Qual_Total, # sum qualification count
                   ~sum(., na.rm = T))) %>%
  mutate(Qual_None_perc = round(Qual_None/Qual_Total*100,2), # add percentage
         Qual_L1_perc = round(Qual_L1/Qual_Total*100,2),     # columns for
         Qual_L2_perc = round(Qual_L2/Qual_Total*100,2),     # qualification
         Qual_L3_perc = round(Qual_L3/Qual_Total*100,2)) 
sa2_dep_census <- right_join(sa2_dep_census, sa22018, # join to sa22018
                            by = c("SA22018_V1" = "SA22018_V1")) %>%
  select(c(-NZDep2018, -URPopnSA1_2018, -SA22018__1, -LAND_AREA_, -AREA_SQ_KM, 
           -Shape_Leng)) %>% # remove irrelevant variables 
  group_by(SA22018_V1, SA22018_name) %>% # group by SA2
  distinct(SA22018_V1, .keep_all = T) # remove duplicates
  
# Converting sa2_dep_census to sf
sa2_dep_census_sf <- sa2_dep_census %>%
st_as_sf() # convert to sf

# Joining schools_sf to sa2_dep_census_sf
schools_data <- st_join(schools_sf, sa2_dep_census_sf) 

# Preparing data for ESDA
allcorr <- schools_data %>% 
  st_drop_geometry() %>% # remove geometry
  select(Deprivation = Median_Dep, # narrowing down variable selection to most
         Decile = Decile,          # essential, otherwise plots are too crowded
         `No Qual %` = Qual_None_perc,
         `L3 Qual %` = Qual_L3_perc,
         `European %` = European_perc,
         `Māori %` = Māori_perc,
         Isolation = Isolation_Index,
         Authority = Authority,
         `Co-ed Status` = CoEd_Status) %>%   
  na.omit # remove NAs

# Correlation matrix of numeric variables
numvar <- ggpairs(allcorr, 
                  lower = list(continuous = "smooth"), # add regression lines
                  aes(alpha = 0.1)) + # make points transparent
  theme_light() # theme
plots = list() 
for (i in 1:7) {plots <- c(plots, lapply(1:7, # determine which plots to use
                                         function(j) getPlot(numvar, 
                                                             i = i, j = j)))}  
ggmatrix(plots,    # create matrix
         nrow = 7, # with 7 rows
         ncol = 7, # and 7 columns
         xAxisLabels = numvar$xAxisLabels[1:7], # specify x-axis labels
         yAxisLabels = numvar$yAxisLabels[1:7]) + # and y-axis labels
  ggtitle("Numeric variables analysis", # add title and subtitle
          "Sources: 2018 Deprivation Index, 2018 Census, Ministry of Education School Directory")
ggsave(filename = "numeric_ESDA.jpg", # save
       width = 8.5, height = 8, units = "in")

# Authority box plots
authvar <- ggpairs(allcorr, 
                   aes(col = Authority, alpha = 0.5)) + # colour by Authority
  theme_light() # theme
plots = list() 
for (i in 1:7){plots <- c(plots, lapply(8, # determine which plots to use
                                        function(j) getPlot(authvar, 
                                                            i = i, j = j)))}  
ggmatrix(plots,    # create matrix
         nrow = 7, # with 7 rows
         ncol= 1,  # and 1 column
         xAxisLabels = authvar$xAxisLabels[8],   # specify x-axis labels
         yAxisLabels = authvar$yAxisLabels[1:7]) + # and y-axis labels
  ggtitle("Categorical variables analysis: Authority", # add title and subtitle
          "Sources: 2018 Deprivation Index, 2018 Census, Ministry of Education School Directory") 
ggsave(filename = "authority_ESDA.jpg", # save
       width = 7, height = 8, units = "in")

# CoEd box plots
coedvar <- ggpairs(allcorr, 
                   aes(col = `Co-ed Status`, alpha = 0.5)) + # colour by CoEd_Status
  theme_light() # theme
plots = list() 
for (i in 1:7){plots <- c(plots, lapply(9, # determine which plots to use 
                                        function(j) getPlot(coedvar, 
                                                            i = i, j = j)))}  
ggmatrix(plots,    # create matrix
         nrow = 7, # with 7 rows
         ncol= 1,  # and 1 column
         xAxisLabels = coedvar$xAxisLabels[9],   # specify x-axis labels
         yAxisLabels = coedvar$yAxisLabels[1:7]) + # and y-axis labels 
  ggtitle("Categorical variables analysis: Co-educational status", # add title and subtitle
          "Sources: 2018 Deprivation Index, 2018 Census, Ministry of Education School Directory") 
ggsave(filename = "coed_ESDA.jpg", # save
       width = 7, height = 8, units = "in")