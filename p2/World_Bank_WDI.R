library(WDI)

new_cache = WDIcache()
#df <- WDIsearch(cache = new_cache)
#df <- as.data.frame(df)

#dat <- WDI(indicator = 'EN.ATM.CO2E.PP.GD', end = 2011)
#WDIsearch()
#WDIcache()


#dat <- WDI(indicator = 'BN.GSR.FCTY.CD.ZS')

# get the internet users per 100 people
dat <- WDI(indicator = 'IT.NET.USER.P2', start = 2011)

for (row in 1:nrow(dat)){
  c = dat$country[row]
  c = tolower(c)
  c = trimws(c, which = 'both')
  match <- any(data$COUNTRY == c)
  if (match){
    ix <- which(data$COUNTRY == c)
    data$WBI[ix] <- dat$IT.NET.USER.P2[row]
  }
}




#################################

dat <- read.csv(file = 'secure_internet_servers.csv', skip = 4)
dat$Country.Name <- tolower(dat$Country.Name)
dat$Country.Name <- trimws(dat$Country.Name)

for (row in 1:nrow(dat)){
  c = dat$Country.Name[row]
  
  match <- any(data$COUNTRY == c)
  
  if (match){
    ix <- which(data$COUNTRY == c)
    data$WBI[ix] <- dat$X2014[row]
  }
}

# dat <- read.csv(file = 'air_pollution.csv', skip = 4)
# dat$Country.Name <- tolower(dat$Country.Name)
# dat$Country.Name <- trimws(dat$Country.Name)
# 
# for (row in 1:nrow(dat)){
#   c = dat$Country.Name[row]
#   
#   match <- any(data$COUNTRY == c)
#   
#   if (match){
#     ix <- which(data$COUNTRY == c)
#     data$WBI2[ix] <- dat$X2013[row]
#   }
# }

dat <- read.csv(file = 'internet_users.csv', skip = 4)
dat$Country.Name <- tolower(dat$Country.Name)
dat$Country.Name <- trimws(dat$Country.Name)

for (row in 1:nrow(dat)){
  c = dat$Country.Name[row]
  
  match <- any(data$COUNTRY == c)
  
  if (match){
    ix <- which(data$COUNTRY == c)
    data$WBI3[ix] <- dat$X2014[row]
  }
}

dat <- read.csv(file = 'improved_water.csv', skip = 4)
dat$Country.Name <- tolower(dat$Country.Name)
dat$Country.Name <- trimws(dat$Country.Name)

for (row in 1:nrow(dat)){
  c = dat$Country.Name[row]
  
  match <- any(data$COUNTRY == c)
  
  if (match){
    ix <- which(data$COUNTRY == c)
    data$WBI4[ix] <- dat$X2015[row]
  }
}

#################################