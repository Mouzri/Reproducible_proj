# loading the data firstly
setwd(dir = "C:/Users/21260/Desktop/R learning")
unzip("repdata_data_activity.zip")
##defining a function to coerce characters
setClass('mydate')
setAs('character','mydate', function(from){as.Date(from)})
raw_data <- read.csv("activity.csv",header = TRUE,stringsAsFactors = FALSE,
                     na.strings = "NA",colClasses = c("integer","mydate","integer"))
