
#Part A: Load and condition the data  

#1.	The data is available on blackboard (hotelSurveySherison), as a JSON file.

library(jsonlite)                                                          # json lite library is loaded to perform json operations.
data <- data.frame(fromJSON("hotelSurveySherison.json"))                           # the data from json file is loaded using fromJSON function and converted to data frame using data.frame fucntion. 


#2.	Use the str command to make sure you can see the following attributes

str(data)                                                                   # str - represents structure whcih gives us the structure of the data including data types of the column variables in the data frame.

