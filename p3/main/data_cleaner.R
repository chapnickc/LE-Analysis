library(zoo) # using the na.locf() function
# setwd('~/Google_Drive/Saint Louis University/Spring 2016 Courses/Statistics/Project_2')

# load in the data
data <- read.csv(file = '2015-World-Data - Sheet1.csv', stringsAsFactors = FALSE)


#------------------------------------------
#   Relabel the columns appropriately
#------------------------------------------
labels <- c('',
            'LifeExp_Both',
            'LifeExp_Male',
            'LifeExp_Female',
            'Maternal_Deaths_1990',
            'Maternal_Deaths_2013',
            'Percent_HIV-AIDS_Male',
            'Percent_HIV-AIDS_Female',
            'Secondary_School_Male',
            'Secondary_School_Female',
            'Tertiary_School_Gender_Parity',
            'Gender_Ratio_Labor_Force',
            'Female_Share_of_Nonagricultural_Wage_Earners',
            'Female_Share_of_Parliament_Members',
            'Population_mid2015_mill',
            'Births_per_100k_Population',
            'Deaths_per_100k_Population',
            'Net_migration_rate_per_100k',
            'Pop_mid2030_mill',
            'Pop_mid2050_mill',
            'Infant_mortality_rate',
            'Total_fertility_rate',
            'Percent_of_pop_under_15',
            'Percent_of_pop_over_65',            
            'GNI_per_capita_2014',
            'Percent_urban',
            'Population_per_Square_kilom_of_arable_land',
            'Percent_of_married_women_using_all_contraception',
            'Percent_of_married_women_using_modern_contraception'
)
colnames(data) <- labels

data[data == 'Ñ'] <- NA # replace all the Ñ values with NA

#---------------------------------------------
#         Fix the Bangldesh data
#---------------------------------------------

# grab the index of Banladesh
bangladesh_row <- which(data[,1] == 'Bangladesh')

cols <- seq(13, 3, -1)
for (col in cols){
  data[bangladesh_row,col] <- data[bangladesh_row,col-1]
}

data[bangladesh_row,'LifeExp_Both'] <- 71
data[bangladesh_row,'Female_Share_of_Parliament_Members'] <- 20
data[bangladesh_row, 'Tertiary_School_Gender_Parity'] <- 0.72


#---------------------------------------------------------
#       Creating a "Region" Variable (categorical) 
#---------------------------------------------------------

# Find the unique region names
major_rows = c()
for (row in 1:nrow(data)){

  # get the element from the row and check if it's in all caps
  string = data[row, 1]
  all_caps = string == toupper(string)

    if (all_caps){
    # store the row number
    major_rows = c(major_rows, row)
  }
}

# make a new column called region and place the 
# the regions found in the previous step into the 
# appropriate rows
data[major_rows, 'REGION'] <- data[major_rows, 1]

# rearrange the columns so region is in the second column
data <- data[, c(1,30,2:29)]

# carry the last observation forward for all NA values
# This effectively labels each country with the correct region
data[,2] = c(rep(NA,4), na.locf(as.character(data[,2])))

# convert the 1st column to a character rather than a factor
data[,1] <- as.character(data[,1])

# fix some country names
data[19,1] <- 'Cote d’Ivoire'
data[88,1] <- 'Curacao'


#========================================================
#   Removing the summary stats for each region/continent
#========================================================
# grab all the region names from the second column 
temp_data <- unique(data[,2])

# create a data frame (exculding the NA values)
temp_df <- data.frame(REGION = temp_data[-1])

# loop through each region in the temporary data frame
# and find the first row it occurs in the main data frame 
rm_rows <- c()
for (element in temp_df[,'REGION']){
  rm_row <- which(data[,'REGION'] == element)[1]
  rm_rows <- c(rm_rows, rm_row)
}

# need to get the Asia (Excl. China) as the algorithm did not 
# grab that row 
rm_rows <- c(rm_rows, 116, 1:4)

#=========================================================================================
# Pull out the summary data for each region and continent, and save in a second data frame 
# and remove those rows from the original data frame
#=========================================================================================
region_df <- data[rm_rows, ]
data <- data[-rm_rows, ]

#======================================================================================
# Pull out the summary data for each continent from the data frame made in the previous
# step, and save in a third data frame and remove those rows from the region data frame
#======================================================================================
rownames(region_df) <- 1:nrow(region_df)
cont_rows <- c(1, 8, 13, 14, 21, 27:32)

continent_df <- region_df[cont_rows, ]
region_df <- region_df[-cont_rows, ]

#======================================================================
#             Get the columns which have <0.1 in the data
#======================================================================
leq_pt1_cols <- c()
for (col in 3:ncol(data)){
  column = as.vector(data[,col])
  leq_pt1 = any(column == '<0.1', na.rm = TRUE)
  if (leq_pt1){
  leq_pt1_cols <- c(leq_pt1_cols, col)
  }
}

# make a vector of column numbers in order to convert the columns to numeric
n <- 3:ncol(data)
n <- n[-(leq_pt1_cols -2)]

# convert the appropriate columns to numeric
for (col in n){
  data[,col] <- as.numeric(as.vector(data[,col]))
}

# convert the categorical columns
source('convert_levels.R')
#data$`Maternal_Deaths_2013`<- convert_levels(data$`Maternal_Deaths_2013`,c(0, 0.09,20,40,90,400,1100))
data$`Percent_HIV-AIDS_Male` <- convert_levels(data$`Percent_HIV-AIDS_Male`,c(0,0.09, 0.15, 0.25, 0.5,1,10))
data$`Percent_HIV-AIDS_Female` <- convert_levels(data$`Percent_HIV-AIDS_Female`,c(0, 0.099, 0.25, 0.5, 1, 5, 50))

#==================================================================================
# Create a categorical variable that encodes the continent that each country is in.
#==================================================================================

# make a continent column
data[, 'CONTINENT'] <- rep(NA, nrow(data))

# put it next to the region column
data <- data[,c(1,2,31,3:30)]

# rename the rows
rownames(data) <- 1:nrow(data)

# fill in the continents
cont_rows <- c(1,58,85,98,149,194)
continents <- c('AFRICA', 'NORTH AMERICA', 'SOUTH AMERICA', 'ASIA', 'EUROPE', 'AUSTRALIA')
for (i in 1:length(continents)){
  row <- cont_rows[i]
  cont <- continents[i]
  
  data[row, 'CONTINENT'] <- cont
}

# fill in the rest of the data frame by carrying the last 
# observation forward
data[,'CONTINENT'] <- na.locf(data[,'CONTINENT'])


#===============================================================
#       Convert the Region and Continent data to factors
#============================================================
data$REGION <- as.factor(data$REGION)
data$CONTINENT <- as.factor(data$CONTINENT)

# remove the region column for the continent data frame
continent_df <- continent_df[,-2]

#=======================================================
# Cleaning up the region data frame and adding continents
#=======================================================

colnames(region_df)[1:2] <- c('REGION', 'CONTINENT')
region_df[,'CONTINENT'] <- rep(NA, nrow(region_df))

rownames(region_df) <- 1:nrow(region_df)

cont_rows <- c(1, 7, 11, 17)
continents <- c('AFRICA', 'NORTH AMERICA', 'ASIA', 'EUROPE')
for (i in 1:length(continents)){
  row <- cont_rows[i]
  cont <- continents[i]
  
  region_df[row, 'CONTINENT'] <- cont
}

region_df[,'CONTINENT'] <- na.locf(region_df[,'CONTINENT'])

#===========================================================
#    Trim whitespace off of the countries to get the data
#    in a standard format
#==========================================================
colnames(data)[1] <- 'COUNTRY'

data$COUNTRY <- trimws(data$COUNTRY, which = 'both')
data$COUNTRY <- tolower(data$COUNTRY)


#==============================================================
#                Write the modified data to a file
#==============================================================
write.csv(data, file = '2015_World_Data_MODIFIED.csv', row.names = FALSE)
write.csv(region_df, file = '2015_Region_Data.csv', row.names = FALSE)
write.csv(continent_df, file = '2015_Continent_Data.csv', row.names = FALSE)


#-----------------------------------------------------------
#                   Garbage Collection
#-----------------------------------------------------------
keep_objs <- c('data', 'region_df', 'continent_df')

# get the indicies
keep_ix <- c()
for (element in keep_objs){
  ix <- which(ls() == element)
  keep_ix <- c(keep_ix, ix)
}

# get all the objects we don't want
rm_objs <- ls()[-keep_ix]

# remove them
rm(list = rm_objs)
rm(rm_objs)

