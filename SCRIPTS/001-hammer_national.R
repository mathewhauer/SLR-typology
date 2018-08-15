######################################## Prepare Workspace ########################################

#################### Clean Up Workspace ####################
rm(list = ls()) # Remove Previous Workspace
gc(reset = TRUE) # Garbage Collection

#################### R Workspace Options ####################
options(scipen = 12) # Scientific Notation
options(digits = 6) # Specify Digits
options(java.parameters = "-Xmx1000m") # Increase Java Heap Size

######################################## Functions, Libraries, & Parallel Computing ########################################

#################### Functions ####################

########## Install and/or Load Packages ##########
packages <- function(x){
  
  x <- deparse(substitute(x))
  installed_packages <- as.character(installed.packages()[,1])
  
  if (length(intersect(x, installed_packages)) == 0){
    install.packages(pkgs = x, dependencies = TRUE, repos = "https://urldefense.proofpoint.com/v2/url?u=http-3A__cran.r-2Dproject.org&d=DwIGAg&c=HPMtquzZjKY31rtkyGRFnQ&r=4dsMboKrDZCTWM4zk0ZTRw&m=iqWkH-VJmPddnymetSfNU3I5qe7iOfF68uIgLRhqtO0&s=BdE60D4UVYh3A_px1EkMJt2xK_7xkk0CT6SVfE-rS50&e=")
  }
  
  library(x, character.only = TRUE)
  rm(installed_packages) # Remove From Workspace
}

########## Specify Number of Digits (Forward) ##########
numb_digits_F <- function(x,y){
  numb_digits_F <- do.call("paste0", list(paste0(rep(0, y - nchar(x)), collapse = ""), x))
  numb_digits_F <- ifelse(nchar(x) < y, numb_digits_F, x)
}

########## Remove Double Space ##########
numb_spaces <- function(x) gsub("[[:space:]]{2,}", " ", x)

#################### Libraries ####################
packages(data.table) # Data Management/Manipulation
packages(doParallel) # Parallel Computing
packages(foreach) # Parallel Computing
packages(openxlsx) # Microsoft Excel Files
packages(plyr) # Data Management/Manipulation
packages(readxl) # Microsoft Excel Files
packages(reshape2) # Data Management/Manipulation
packages(stringi) # Character/String Editor
packages(stringr) # Character/String Editor
packages(zoo) # Time Series
packages(tidyverse) # Tidyverse
rm(packages) # Remove From Workspace

#################### Parallel Computing ####################

########## Establish Parallel Computing Cluster ##########
# detectCores() # Determine Number of Cores
clusters <- makeCluster(detectCores() - 1) # Create Cluster with Specified Number of Cores
registerDoParallel(clusters) # Register Cluster

########## Parallel Computing Details ##########
getDoParWorkers() # Determine Number of Utilized Clusters
getDoParName() #  Name of the Currently Registered Parallel Computing Backend
getDoParVersion() #  Version of the Currently Registered Parallel Computing Backend
################################################

historic <- read.xlsx("DATA/historicaldat.xlsx", sheet =1) %>%
  mutate_all(funs(replace(., is.na(.), 0)))


download.file("https://www2.census.gov/geo/docs/reference/codes/files/national_county.txt", "DATA/national_county.txt")
fips <- read_csv("DATA/national_county.txt", col_names=FALSE) %>%
  filter(X2<=56) # getting rid of PR, Guam, etc.

### Getting the Decennial Data ###
dat_decen <- foreach(i = 1:nrow(fips), .combine = rbind, .errorhandling = "stop", .packages = c("data.table", "doParallel", "foreach", "reshape2", "stringi", "stringr", "zoo", "tidycensus", "tidyverse")) %dopar% {  
  decennial <- get_decennial(geography = "block group", variables = c("H00010001", "P0010001", "P0430001"), 
                             state = paste0(fips$X1[i]), county = paste0(fips$X3[i])) %>%
    spread(variable, value) %>% # Turning the data from tall/skinny to short/fat
    mutate(pphu = ((P0010001-P0430001) / H00010001), # estimating the PPHU
           FIPS = substr(GEOID, 1, 5)) # setting the FIPS code
  
}

### Setting the variables to get from the ACS ###
var <- c("B25034_001","B25034_002", "B25034_003", "B25034_004", "B25034_005", "B25034_006",
         "B25034_007", "B25034_008", "B25034_009", "B25034_010") 

### Getting the ACS data
dat_acs <- foreach(i = 1:nrow(fips), .combine = rbind, .errorhandling = "stop", .packages = c("data.table", "doParallel", "foreach", "reshape2", "stringi", "stringr", "zoo", "tidycensus")) %dopar% {  
  dat_acs <- get_acs(geography = "block group", variables = var, state = paste0(fips$X1[i]), county = paste0(fips$X3[i]), year = 2012)
}  

dat_acs <- dat_acs %>%
  mutate(year = case_when( # renaming variables
    variable == "B25034_001" ~  "TOTALa",
    variable == "B25034_003" ~  "a2009",
    variable == "B25034_004" ~  "a1999",
    variable == "B25034_005" ~  "a1989",
    variable == "B25034_006" ~  "a1979",
    variable == "B25034_007" ~  "a1969",
    variable == "B25034_008" ~  "a1959",
    variable == "B25034_009" ~  "a1949",
    variable =="B25034_010" ~  "a1939", 
    TRUE ~ "a2019"
    ),
    FIPS = substr(GEOID, 1, 5)) %>% # Creating a FIPS code from the first 5 digits of the GEOID
  dplyr::select(GEOID, FIPS, estimate,moe, year)

moes <- dat_acs %>% # separating out the margins of error
  mutate(upper = estimate+moe) %>% # creating an upper bound for the error
  dplyr::select(GEOID, FIPS, year, estimate=upper) %>%
  spread(year, estimate)
# # moes[moes<0] <- 0
# moes <- moes %>%
#   filter(!year == "TOTALa") %>% # getting rid of the
#   group_by(GEOID, FIPS) %>%
#   mutate(adj_upp = upper) %>%
#   dplyr::select(-estimate, -upper)
# moes <- moes %>%
#   dplyr::select(GEOID, FIPS, year, estimate=adj_upp ) %>%
#   spread(year, estimate)

dat_acs <- dat_acs %>%
  dplyr::select(-moe) %>%
  spread(year, estimate)  ## Going from tall/skinny to short/fat

data_sum <-  group_by(dat_acs, FIPS) %>% # grouping the data by FIPS code
  summarise(t1939 = sum(a1939),
            t1949 = sum(a1949),
            t1959 = sum(a1959),
            t1969 = sum(a1969),
            t1979 = sum(a1979),
            t1989 = sum(a1989),
            t1999 = sum(a1999),
            t2009 = sum(a2009)) %>% # summing the total number of housing units in each decade
  left_join(historic) %>% # Joining the summed ACS with the historic Census data
  mutate(adj_1940 = h1940 / t1939,
         adj_1950 = h1950 / (t1939 + t1949),
         adj_1960 = h1960 / (t1939 + t1949 + t1959),
         adj_1970 = h1970 / (t1939 + t1949 + t1959  + t1969),
         adj_1980 = h1980 / (t1939 + t1949 + t1959  + t1969 + t1979),
         adj_1990 = h1990 / (t1939 + t1949 + t1959  + t1969 + t1979 + t1989),
         adj_2000 = h2000 / (t1939 + t1949 + t1959  + t1969 + t1979 + t1989 + t1999),
         adj_2010 = h2010 / (t1939 + t1949 + t1959  + t1969 + t1979 + t1989 + t1999 + t2009)) # Creates adjustment factors for each decade


joined <- full_join(dat_acs, data_sum) %>% # Joining the unsummed ACS data with the summed ACS housing units
  full_join(., dat_decen) %>% #adding in the decennial data
  mutate(hat_1940 = (h1940 / t1939) * a1939, # This block of code creates the housing units from the Hammer Method
         hat_1950 = (h1950 / (t1939 + t1949)) * (a1939 + a1949),
         hat_1960 = (h1960 / (t1939 + t1949 + t1959)) * (a1939 + a1949 + a1959),
         hat_1970 = (h1970 / (t1939 + t1949 + t1959 + t1969))  * (a1939 + a1949 + a1959 + a1969),
         hat_1980 = (h1980 / (t1939 + t1949 + t1959 + t1969 + t1979)) * (a1939 + a1949 + a1959 + a1969 + a1979),
         hat_1990 = (h1990 / (t1939 + t1949 + t1959 + t1969 + t1979 + t1989)) * (a1939 + a1949 + a1959 + a1969 + a1979 + a1989),
         hat_2000 = (h2000 / (t1939 + t1949 + t1959 + t1969 + t1979 + t1989 + t1999)) * (a1939 + a1949 + a1959 + a1969 + a1979 + a1989 + a1999),
         hat_2010 = (h2010 / (t1939 + t1949 + t1959 + t1969 + t1979 + t1989 + t1999 + t2009)) * (a1939 + a1949 + a1959 + a1969 + a1979 + a1989 + a1999 + a2009),
         pphu1940 = p1940/h1940, # estimating the PPHU from each historical time period
         pphu1950 = p1950/h1950,
         pphu1960 = p1960/h1960,
         pphu1970 = p1970/h1970,
         pphu1980 = p1980/h1980,
         pphu1990 = p1990/h1990,
         pphu2000 = p2000/h2000,
         pphu2010 = p2010/h2010,
         pphurat = pphu / pphu2010, # creating the PPHU adjustment factor
         pop1940_1 = hat_1940 * pphu1940*pphurat,
         pop1950_1 = hat_1950 * pphu1950*pphurat,
         pop1960_1 = hat_1960 * pphu1960*pphurat,
         pop1970_1 = hat_1970 * pphu1970*pphurat,
         pop1980_1 = hat_1980 * pphu1980*pphurat,
         pop1990_1 = hat_1990 * pphu1990*pphurat,
         pop2000_1 = hat_2000 * pphu2000*pphurat
  ) # this creates the first rake of population.

fin <- group_by(joined, FIPS) %>%
  summarize(pop1940_1s = sum(pop1940_1, na.rm = TRUE),
            pop1950_1s = sum(pop1950_1, na.rm = TRUE),
            pop1960_1s = sum(pop1960_1, na.rm = TRUE),
            pop1970_1s = sum(pop1970_1, na.rm = TRUE),
            pop1980_1s = sum(pop1980_1, na.rm = TRUE),
            pop1990_1s = sum(pop1990_1, na.rm = TRUE),
            pop2000_1s = sum(pop2000_1, na.rm = TRUE)) %>%
  full_join(., joined) %>%
  mutate(adj_1940a =  p1940 /pop1940_1s,
         adj_1950a = p1950/pop1950_1s,
         adj_1960a =  p1960/pop1960_1s,
         adj_1970a = p1970/pop1970_1s,
         adj_1980a =  p1980/pop1980_1s,
         adj_1990a = p1990/pop1990_1s,
         adj_2000a =  p2000/pop2000_1s,
         p1940fin = adj_1940a * pop1940_1,
         p1950fin = adj_1950a * pop1950_1,
         p1960fin = adj_1960a * pop1960_1,
         p1970fin = adj_1970a * pop1970_1,
         p1980fin = adj_1980a * pop1980_1,
         p1990fin = adj_1990a * pop1990_1,
         p2000fin = adj_2000a * pop2000_1,
         p2010fin = P0010001
  ) %>%
  dplyr::select(GEOID, FIPS, p1940fin:p2010fin)


data_sum <-  group_by(moes, FIPS) %>% # grouping the data by FIPS code
  summarise(t1939 = sum(a1939),
            t1949 = sum(a1949),
            t1959 = sum(a1959),
            t1969 = sum(a1969),
            t1979 = sum(a1979),
            t1989 = sum(a1989),
            t1999 = sum(a1999),
            t2009 = sum(a2009)) %>% # summing the total number of housing units in each decade
  left_join(historic) %>% # Joining the summed ACS with the historic Census data
  mutate(adj_1940 = h1940 / t1939,
         adj_1950 = h1950 / (t1939 + t1949),
         adj_1960 = h1960 / (t1939 + t1949 + t1959),
         adj_1970 = h1970 / (t1939 + t1949 + t1959  + t1969),
         adj_1980 = h1980 / (t1939 + t1949 + t1959  + t1969 + t1979),
         adj_1990 = h1990 / (t1939 + t1949 + t1959  + t1969 + t1979 + t1989),
         adj_2000 = h2000 / (t1939 + t1949 + t1959  + t1969 + t1979 + t1989 + t1999),
         adj_2010 = h2010 / (t1939 + t1949 + t1959  + t1969 + t1979 + t1989 + t1999 + t2009)) # Creates adjustment factors for each decade


joined <- full_join(moes, data_sum) %>% # Joining the unsummed ACS data with the summed ACS housing units
  full_join(., dat_decen) %>% #adding in the decennial data
  mutate(hat_1940 = (h1940 / t1939) * a1939, # This block of code creates the housing units from the Hammer Method
         hat_1950 = (h1950 / (t1939 + t1949)) * (a1939 + a1949),
         hat_1960 = (h1960 / (t1939 + t1949 + t1959)) * (a1939 + a1949 + a1959),
         hat_1970 = (h1970 / (t1939 + t1949 + t1959 + t1969))  * (a1939 + a1949 + a1959 + a1969),
         hat_1980 = (h1980 / (t1939 + t1949 + t1959 + t1969 + t1979)) * (a1939 + a1949 + a1959 + a1969 + a1979),
         hat_1990 = (h1990 / (t1939 + t1949 + t1959 + t1969 + t1979 + t1989)) * (a1939 + a1949 + a1959 + a1969 + a1979 + a1989),
         hat_2000 = (h2000 / (t1939 + t1949 + t1959 + t1969 + t1979 + t1989 + t1999)) * (a1939 + a1949 + a1959 + a1969 + a1979 + a1989 + a1999),
         hat_2010 = (h2010 / (t1939 + t1949 + t1959 + t1969 + t1979 + t1989 + t1999 + t2009)) * (a1939 + a1949 + a1959 + a1969 + a1979 + a1989 + a1999 + a2009),
         pphu1940 = p1940/h1940, # estimating the PPHU from each historical time period
         pphu1950 = p1950/h1950,
         pphu1960 = p1960/h1960,
         pphu1970 = p1970/h1970,
         pphu1980 = p1980/h1980,
         pphu1990 = p1990/h1990,
         pphu2000 = p2000/h2000,
         pphu2010 = p2010/h2010,
         pphurat = pphu / pphu2010, # creating the PPHU adjustment factor
         pop1940_1 = hat_1940 * pphu1940*pphurat,
         pop1950_1 = hat_1950 * pphu1950*pphurat,
         pop1960_1 = hat_1960 * pphu1960*pphurat,
         pop1970_1 = hat_1970 * pphu1970*pphurat,
         pop1980_1 = hat_1980 * pphu1980*pphurat,
         pop1990_1 = hat_1990 * pphu1990*pphurat,
         pop2000_1 = hat_2000 * pphu2000*pphurat
  ) # this creates the first rake of population.

upper <- group_by(joined, FIPS) %>%
  summarize(pop1940_1s = sum(pop1940_1, na.rm = TRUE),
            pop1950_1s = sum(pop1950_1, na.rm = TRUE),
            pop1960_1s = sum(pop1960_1, na.rm = TRUE),
            pop1970_1s = sum(pop1970_1, na.rm = TRUE),
            pop1980_1s = sum(pop1980_1, na.rm = TRUE),
            pop1990_1s = sum(pop1990_1, na.rm = TRUE),
            pop2000_1s = sum(pop2000_1, na.rm = TRUE)) %>%
  full_join(., joined) %>%
  mutate(adj_1940a =  p1940 /pop1940_1s,
         adj_1950a = p1950/pop1950_1s,
         adj_1960a =  p1960/pop1960_1s,
         adj_1970a = p1970/pop1970_1s,
         adj_1980a =  p1980/pop1980_1s,
         adj_1990a = p1990/pop1990_1s,
         adj_2000a =  p2000/pop2000_1s,
         p1940fin = adj_1940a * pop1940_1,
         p1950fin = adj_1950a * pop1950_1,
         p1960fin = adj_1960a * pop1960_1,
         p1970fin = adj_1970a * pop1970_1,
         p1980fin = adj_1980a * pop1980_1,
         p1990fin = adj_1990a * pop1990_1,
         p2000fin = adj_2000a * pop2000_1,
         p2010fin = P0010001
  ) %>%
  dplyr::select(GEOID, FIPS, p1940fin:p2010fin)



write_csv(fin, "DATA-PROCESSED/hammer_populations.csv")
write_csv(moes, "DATA-PROCESSED/hammer_populations_upperest.csv")