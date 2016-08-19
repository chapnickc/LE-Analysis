#===================================================================
# Add the number of internet users in each country to the data frame
#==================================================================

data['INET_USRS_2014'] <- rep(NA, nrow(data))

dat <- read.csv(file = 'internet_users.csv', skip = 4)
dat$Country.Name <- tolower(dat$Country.Name)
dat$Country.Name <- trimws(dat$Country.Name)

for (row in 1:nrow(dat)){
  c = dat$Country.Name[row]
  
  match <- any(data$COUNTRY == c)
  
  if (match){
    ix <- which(data$COUNTRY == c)
    data$INET_USRS_2014[ix] <- dat$X2014[row]
  }
}


data['IMPROVED_H20_SRC_2015'] <- rep(NA, nrow(data))
dat <- read.csv(file = 'improved_water.csv', skip = 4)
dat$Country.Name <- tolower(dat$Country.Name)
dat$Country.Name <- trimws(dat$Country.Name)

for (row in 1:nrow(dat)){
  c = dat$Country.Name[row]
  
  match <- any(data$COUNTRY == c)
  
  if (match){
    ix <- which(data$COUNTRY == c)
    data$IMPROVED_H20_SRC_2015[ix] <- dat$X2015[row]
  }
}

#=============================================================
#             Removing Unneded Objects from Memory
#=============================================================
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
