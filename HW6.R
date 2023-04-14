library(tidyverse)

#load data in
data = read_csv("/RDS-2016-0005/Data/TS3_Raw_tree_data.csv")

#*****Question 1: Sample sizes by state****
#I checked to see if the city column was all characters so that I new if I could do text processing.
#Then I tested to see what statement would be true for the column. Once I found one that worked, I attached
#it to the table. I also relocated the State column so that it was in a place that made sense
class(data$City)
all(str_detect(data$City, "[:alnum:]+[:punct:]+ [:alnum:]+$"))
data[,c("City", "State")] =str_match(data$City, "([:alnum:]+)[:punct:]+ ([:alnum:]+$)")[,2:3]
data = data %>% relocate(State, .after=City)

#a table to see how many records are in each state
as.data.frame(table(data$State))


#*****Question 2: Cities in NC/SC*****
#Since different trees grow differently in different parts of the country, the city 
#wants you to only use data from North and South Carolina. Filter the dataset to only 
#these states, and use that filtered dataset for all remaining questions.
states = c('NC','SC')
filtered = filter(data, State %in% states)


#What cities did they collect data from in North and South Carolina? [1 point]
unique(filtered$City)

#*****Question 3: Genera and species*****
#I checked to see if the city column was all characters so that I new if I could do text processing.
#Then I tested to see what statement would be true for the column. Once I found one that worked, I attached
#it to the table. I also relocated the State column so that it was in a place that made sense
class(filtered$ScientificName)
all(str_detect(filtered$ScientificName, "^([[:alnum:]]+)[[:space:]]+[[:alnum:]]*"))
filtered[, c("Genus")] <- str_match(filtered$ScientificName, "^([[:alnum:]]+)[[:space:]]+[[:alnum:]]*")[, 2]
#What genus of trees has the largest crown diameter in North and South Carolina? [2 points]

# Group by genus and compute mean crown diameter
crown_mean <- filtered %>% group_by(Genus) %>% summarise(mean_diameter = mean(`AvgCdia (m)`))

crown_mean = crown_mean[order(crown_mean$mean_diameter, decreasing=TRUE),]
#Quercus (13.623163)

#*****Extra credit*****

#***Tree age***
#Older trees, of course, have larger crowns. Are there differences in the average age of the different genera of trees in the dataset? Might this explain the results of the previous question? [1 point]
age = filtered %>% select(Genus,Age)
age_mean <- age %>% group_by(Genus) %>% summarise(mean_age= mean(`Age`))
age_mean = age_mean[order(age_mean$mean_age, decreasing=TRUE),]
#Recommend a genera that produces a large crown quickly (among trees in North and South Carolina). You can use any analytical methods you want (group by, plots, linear regression, etc.). Document the process by which you chose this type of tree. [2 points]
age_dia = merge(x=crown_mean, y=age_mean, by="Genus", all.x=TRUE)

#adds a ratio for age_dia by dividing mean_diameter by mean_age
age_dia$ratio = age_dia$mean_diameter/age_dia$mean_age

# a scatteplot that includes mean_diameter and mean_age with labels of the genus. 
ggplot(age_dia, aes(x = mean_diameter, y = mean_age, label = Genus, color = Genus)) +
  geom_point() +
  labs(x = "Mean Diameter", y = "Mean Age", color = "Genus") +
  geom_text()
  #***Species***
#So far, all of the analysis has focused on genera. Refine your regular expression to also extract the species as well, as a separate column. Within each genus of tree in North and South Carolina, how many species are recorded in the dataset? [2 points]
# Extract genus and species names from ScientificName column
extracred = filtered
str_match(extracred$ScientificName, "^([[:alnum:]]+)[[:space:]]['x']?[[:space:]]?([[:alnum:]]*)")
extracred[, c("Genus", "Species")] <- str_match(extracred$ScientificName, "^([[:alnum:]]+)[[:space:]]['x']?[[:space:]]?([[:alnum:]]*)")[, 2:3]
extracred = select(extracred,'Genus', 'Species')

#a table to see how many records are for each species
as.data.frame(table(extracred$Species))
