
#install.packages("arules")                       # install and load library arules
library(arules)
#install.packages("arulesViz")                    # install and load library arulesViz
library(arulesViz)


#7)	Coerce the hotelSurvey data frame into a sparse transactions matrix using:

hotelSurveyX <- as(hotelSurvey_clean,"transactions")    # convert the hotelSurvery_clean into transaction matrix format 

#8)	Use the inspect( ), itemFrequency( ), and itemFrequencyPlot( ) commands to explore the contents of hotelSurveyX

#inspection <- inspect(hotelSurveyX)                     # use inspect method to inspect the resultant transaction matrix.
itemFrequency(hotelSurveyX, weighted = FALSE)           # item frequency fucntion is called on the hotelsurveryX transaction matrix to counts the number of distinct occurrences of items or itemsets (elements) in a collection of sequences
frequencyPlot <- itemFrequencyPlot(items(hotelSurveyX), las=2)           # the plot is then generated for the itemfrequency using itemfrequencyPlot function which generates a bar plot for each attribute and frequency

