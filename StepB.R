
#1)	Ensure hotelSurvey is a dataframe, and look at the structure via the str() command
str(hotelSurvey)                                                          # str command is used to get the structure of the variable. In this case it displays the structure as a data frame of factors and integers


cust_sat_range <- range(hotelSurvey$overallCustSat)                       # Get range of each attribute in the data frame columns using range function
hotelsize_range <- range(hotelSurvey$hotelSize)
checkInSat_range <- range(hotelSurvey$checkInSat)
hotelClean_range <- range(hotelSurvey$hotelClean)
hotelFriendly_range <- range(hotelSurvey$hotelFriendly)
guestAge_range <- range(hotelSurvey$guestAge)
lengthOfStay_range <- range(hotelSurvey$lengthOfStay)
whenBookedTrip_range <- range(hotelSurvey$whenBookedTrip)


#2)	Map each numeric attribute to a category  - Since we want to create rules, we should convert the attributes that have a numeric range into buckets (ex. low or high)

gender <- as.factor(hotelSurvey$gender)                                   # type safing it so that state is taken as factors for conversion
hotelSurvey$gender <- unclass(gender)                                     # unclass fucntion is used to convert the factor categorical variables into numerical nomianal variables. 


# Function for mapping numeric attributes.
Bucket_range_1_to_10 <- function(vec)                                     # Function Bucket_range1to10 is defined to convert numerical range values between 1-10 into buckets of 2 possible values High and Low
{ 
  Buckets1<- replicate(length(vec), "Average")                            # Replicate is used to re-evaluate the expression for every call or replication here average
  Buckets1[vec> 7] <- "High"                                              # all values above 7 are classified HIGH
  Buckets1[vec< 7] <- "Low"                                               # all values below 7 are classified LOW
  return(Buckets1)                                                        # return value
}


Buckets2 <- function(var1, df1) {                                         # Function Bucket_range_above_10 is defined to convert numerical range values above 10 into buckets of 3 possible values High Average and Low
  colname_each <- df1[[var1]]                                             # get the column names from the paramneter that is passed inorder to fetch corresponding column vector from data frame
  q <- quantile(colname_each, c(0.4, 0.6))                                # compute the quantiles using quantile fucntion on the vector that is identified and defined,
  vBuckets <- replicate(length(colname_each), "Average")                  # Replicate is used to re-evaluate the expression for every call or replication here average
  vBuckets[colname_each <= q[1]] <- "Low"                                 # All values below first quantile are considered and assigned LOW
  vBuckets[colname_each > q[2]] <- "High"                                 # All values above second quantile are considered HIGH
  buckets <- vBuckets
}

Hotel_customer_satisfaction <- Bucket_range_1_to_10(hotelSurvey$overallCustSat)    # Compute corresponding buckets for data frame attributes based on range computed in step 1
Hotel_clean <- Bucket_range_1_to_10(hotelSurvey$hotelClean)                        # Compute corresponding buckets for data frame attributes based on range computed in step 1
Hotel_friendly <- Bucket_range_1_to_10(hotelSurvey$hotelFriendly)                  # Compute corresponding buckets for data frame attributes based on range computed in step 1
Hotel_length_of_stay <- Bucket_range_1_to_10(hotelSurvey$lengthOfStay)             # Compute corresponding buckets for data frame attributes based on range computed in step 1
Hotel_checkin_sat <- Bucket_range_1_to_10(hotelSurvey$checkInSat)                  # Compute corresponding buckets for data frame attributes based on range computed in step 1

colname <- c(colnames(hotelSurvey))                                       # get colnames of data frame hotelsurvery using function colnames
colnames_range_grater_than_10 <- colname[c(2, 10, 8)]                     # get the columnname of attributes whose range is greater than 10 and assign it to a variable.

bucket_transform <- sapply(colnames_range_grater_than_10, Buckets2, hotelSurvey)  # use sapply fucntion to compute the fucntion Bucket_greater_than_10 to compute Bucket Values for these columns with range < 10

hotelSurvey_clean <- data.frame(Hotel_customer_satisfaction, Hotel_clean, Hotel_friendly, Hotel_length_of_stay, Hotel_checkin_sat, bucket_transform)          # data.frame function is used to get a resultant combined data frame of the vectors derived in previous step.



#3)	Count the people in each category of for the age and friendliness attributes
#4)	Express the results of problem 3 as percentages by sending the results of the table( ) command into the prop.table( ) command

frequencytable_overalSatisfaction <- table(hotelSurvey_clean$Hotel_customer_satisfaction)    # table fucntion is used to get the frequency distribution of the bucketed attributes customer satisfaction _ HIGH LOW AVERAGE
frequencytable_overalSatisfaction
frequencytable_overalSatisfaction_perc <- prop.table(frequencytable_overalSatisfaction)      # prop.table function is used to get the proportion distribution or the percentage distribution of HIGH LOW AVERAGE values in the column attribute 

frequencytable_age <- table(hotelSurvey_clean$guestAge)                                      # table fucntion is used to get the frequency distribution of the bucketed attributes guest_age _ HIGH LOW AVERAGE
frequencytable_age
frequencytable_age_perc <- prop.table(frequencytable_age)                                    # prop.table function is used to get the proportion distribution or the percentage distribution of HIGH LOW AVERAGE values in the column attribute 


frequencytable_friendliness <- table(hotelSurvey_clean$Hotel_friendly)                       # table fucntion is used to get the frequency distribution of the bucketed attributes friendliness _ HIGH LOW AVERAGE
frequencytable_age
frequencytable_friendliness
frequencytable_friendliness_perc <- prop.table(frequencytable_friendliness)                  # prop.table function is used to get the proportion distribution or the percentage distribution of HIGH LOW AVERAGE values in the column attribute 



#5)	Show a “contingency table” of percentages for the age and the overall satisfaction variables together. Write a block comment about what you see.

#Contingency tables provide a way to display the frequencies and relative frequencies of observations, which are classified according to two categorical variables.
#library(gmodels)                                                                                       # Library gmodels can be used to generate contingency tables or cross tables.
#with(hotelSurvey_clean, CrossTable(frequencytable_overalSatisfaction_perc, frequencytable_age_perc))   # Cross table fucntion is called with hotel survey data and the 2 matrix vectors on which a cross table frequesncy distribution is required,

#library(descr)                                                                                         # Library descr can also be used to generate cross tables.
CrossTable(hotelSurvey_clean$Hotel_customer_satisfaction, hotelSurvey_clean$guestAge, prop.c = FALSE, prop.chisq = FALSE, prop.t = FALSE)   # cross table fucntion is called with parameters like data , 2 vectors, 
                                                                                                       # prop.r = row proportions will be included.
                                                                                                       # prop.c = column proportions will be included.
                                                                                                       # prop.chisq = chi-square contribution of each cell will be included.


