#setwd('')
library(WDI, quietly = TRUE)

new_cache = WDIcache()
WDIsearch(cache = new_cache)

# GNI per Capita is in $US dollars. Maybe grab previous year?

indicators = c(
  'NY.GNP.PCAP.CD', # GNI in current us dollars
  'SP.DYN.CDRT.IN' # death rate
            )

# --------------------
#  Also Considered
# --------------------
#'SP.DYN.IMRT.IN', # infant mortality rate.  infant deaths per 1,000 live births
#'SP.POP.65UP.TO.ZS', # percent of population over 65
#'SP.DYN.CONU.ZS', # percent of married women using all contraception 
#'SH.HIV.1524.FE.ZS', # hiv female 
#'SH.HIV.1524.MA.ZS' # hiv male


# this first line is your predictor variable
pred_df <- WDI(indicator = 'SP.DYN.LE00.MA.IN', start = 2011)[,c(2:3)]

for (indi in indicators){
  dat <- WDI(indicator = indi, start = 2011)
  dat <- dat[,3]
  dat <- as.numeric(dat)
  
  pred_df <- cbind(pred_df, dat)
}
  
  


colnames(pred_df)[3:ncol(pred_df)] <- indicators

#want the indicies of gni and death rate
ix_1 = which(colnames(data) == "GNI_per_capita_2014")
ix_2 = which(colnames(data) == "Deaths_per_100k_Population")

# indicies of columns
# country, your indicator, other indicators (in the same order as indicators vector)

# 5 is index of male life exp
var_names = colnames(data)[c(1,5,  ix_1, ix_2)]
colnames(pred_df) <- var_names

new_fit = lm(LifeExp_Male ~ 
                GNI_per_capita_2014 + 
                Deaths_per_100k_Population, data = data)


# use your model instead of new_fit
prediction_pre <- predict(new_fit, newdata = pred_df, interval = 'pre')
prediction_c <- predict(new_fit, newdata = pred_df, interval = 'c')


# sort the data frame
prediction_c <- prediction_c[order(prediction_c[, 1]), ]
prediction_pre <- prediction_pre[order(prediction_pre[, 1]), ]

#new <- as.data.frame(new)
head(new)
x <- prediction_pre[, 1]
y <- pred_df$LifeExp_Male

plot(y ~ x)
matlines(x = x, y=  prediction_pre, col = c(1, 4, 4))
matlines(x = x, y = prediction_c, col = c(1, 2,2), lty = c(1,1,1))
