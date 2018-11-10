
#Part C: Use arules to discover patterns

hotelSurveyX <- as(hotelSurvey_clean,"transactions")
#9)	Run the apriori command to try and predict happy customers (as defined by their overall satisfaction being high – above 7).

#install.packages("arules")                       # install and load library arules
library(arules)
#install.packages("arulesViz")                    # install and load library arulesViz
library(arulesViz)

ruleset <- apriori(hotelSurveyX, parameter=list(support=0.1 , confidence =0.5),appearance=list(default="lhs", rhs=("Hotel_customer_satisfaction=High")))

#10)	Once you have a reasonable number of rules, use inspect( ) to view the ruleset.
summary(ruleset)
inspect(ruleset)
inspect(head(sort(ruleset, by="lift"),3))

plot(ruleset)

#11) If you had to provide two rules to the hotel owner (in terms of what helps drive high overall customer satisfaction, what would those two rules be?  Use a block comment to explain your answer

# the 2 rules that can be recommended to the hotel owner would be 

rules <- apriori(hotelSurveyX, parameter = list(supp = 0.001, conf = 0.80), appearance=list(default="lhs", rhs=("Hotel_customer_satisfaction=High")))
inspect(rules[1:10])

  #1. Hotel Friendliness to be maintained above Average - Since the rule set with support being highest with confidence
  #2. Hotel Friendly = high + Hotel SIze => AVERAGE - Best supports more customer satisfaction
  #2. Hotel Friendly = HIGH + Guest Age with older people being more satisfied,
  #3. Hotel Clean being the 3rd rule with greater customer satisfaction as cleanliness is better.
