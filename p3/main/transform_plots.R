# This will generate pop up windows for each graph

plot(LifeExp_Male ~ I(log(Percent_of_pop_over_65)), data = data)
plot(LifeExp_Male ~ I(log(GNI_per_capita_2014)), data = data)
plot(LifeExp_Male ~ I(log(Maternal_Deaths_1990)), data = data)
plot(LifeExp_Male ~ I(log(Maternal_Deaths_2013)), data = data)
plot(LifeExp_Male ~ Percent_of_pop_under_15, data = data)
