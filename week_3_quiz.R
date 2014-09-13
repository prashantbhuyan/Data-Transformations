# Prashant B. Bhuyan
# is607 Data Acquisition and Management Week 3 Quiz Solutions

##### Problem 1 #####

# Solution:

vec <- c(1:10)
calcMean <- function(vec){
  print(mean(vec))
}

# Results: 
# Test Case 1: 
# > vec <- c(1:10)
# > calcMean(vec)
# [1] 5.5
# Check: (1+10)/2.0 = 5.5
#
# Test Case 2: 
# > vec <- c(55:93)
# > calcMean(vec)
# [1] 74
# Check: (55+93)/2.0 = 74

##### Problem 2 #####

# Solution:
vec <- c(1,2,3,4,NA,5,NA,6,7,9,10,NA,NA)
calcMean <- function(vec){
  print(mean(vec, na.rm = TRUE))
}

# Results:
# > vec
# [1]  1  2  3  4 NA  5 NA  6  7  8  9 10 NA NA
#
# > calcMean(vec)
# [1] 5.5

##### Problem 3 #####

# *Note for Problem 3 I've included 2 ways (Solution A is recursive, Solution B uses 
# a package 'Pracma') to compute gcd because I wasn't sure if I could just use the 
# same function as I used in Problem 3 for Problem 4.  

# Solution A:
findGCD <- function(p,q){
  ifelse(q == 0, p, findGCD(q, p%%q))
}

# Results: 
#
# Test Case 1: 
# > findGCD(0,1)
# [1] 1
#
# Test Case 2: 
# > findGCD(5,1)
# [1] 1
#
# Test Case 3:
# > findGCD(12,4)
# [1] 4
#
# Test Case 4: 
# > findGCD(108,91)
# [1] 1
# 
# Test Case 5: 
# > findGCD(1022488,24)
# [1] 8

# Solution B:

install.packages("pracma")

# trying URL 'http://cran.rstudio.com/bin/macosx/contrib/3.0/pracma_1.7.0.tgz'
# Content type 'application/x-gzip' length 983852 bytes (960 Kb)
# opened URL
# ==================================================
  # downloaded 960 Kb


# The downloaded binary packages are in
# /var/folders/d4/t6d5sr5s1jq4k848yz88_9km0000gn/T//RtmpiJVQL9/downloaded_packages

library(pracma)

# Attaching package: ‘pracma’

findGCD2 <- function(a,b){
  print(gcd(a,b))
}

# Results:

# Test Case 1: 
# > findGCD2(12,4)
# [1] 4
#
# Test Case 2: 
# > findGCD2(1,0)
# [1] 1
#
# Test Case 3: 
# > findGCD2(584,64)
# [1] 8
#
# Test Case 4: 
# > findGCD2(9,18)
# [1] 9
#
# Test Case 5: 
# > findGCD2(100,99)
# [1] 1


##### Problem 4 #####

# Solution A:
findGCD <- function(p,q){
  ifelse(q == 0, p, findGCD(q, p%%q))
}

# Results: 
#
# Test Case 1: 
# > findGCD(0,1)
# [1] 1
#
# Test Case 2: 
# > findGCD(5,1)
# [1] 1
#
# Test Case 3:
# > findGCD(12,4)
# [1] 4
#
# Test Case 4: 
# > findGCD(108,91)
# [1] 1
# 
# Test Case 5: 
# > findGCD(1022488,24)
# [1] 8

##### Problem 5 #####

# Solution:
computeFunc <- function(x,y){
  print(((x^2)*y)+(2*x*y)-(x*(y^2)))
}

# Results:
#
# Test 1: 
# > computeFunc(2,10)
# [1] -120
# Check: 
# (((2^2)*10)+(2*2*10)-(2*(10^2)))
# [1] -120
#
# Test 2: 
# > computeFunc(1,1)
# [1] 2
# Check: (((1^2)*1)+(2*1*1)-(1*(1^2)))
# [1] 2
#
# Test 3: 
# > computeFunc(5,5)
# [1] 50
# Check: (((5^2)*5)+(2*5*5)-(5*(5^2)))
# [1] 50

##### Problem 6 #####

# Solution:
#
# There are 27 obs. of 8 variables in the resulting data frame merged by ModelNumber.  
#
# This is what I expected because the same model number can have different
# colors, mileages and prices.  In other words the relationship between model number
# and the types of colors, miles and prices of cars of the same model number (released in a particular year)
# is a one to many relationship.  For example, there are 6 unique (in terms of color, miles etc) Toyota Camry's 
# that belong to the model number 1091 from the year 2010.

file1 <- read.table("~/Desktop/week-3-price-data.csv", sep = ",", header = TRUE)
file2 <- read.table("~/Desktop/week-3-make-model-data.csv", sep = ",", header = TRUE)

file1df <- data.frame(file1)
file2df <- data.frame(file2)

mergedFiles <- merge(file1df, file2df, by = "ModelNumber")


# Results:
# > head(mergedFiles)
# ModelNumber ID  Color Mileage Price   Make Model Year
# 1        1091  1   Blue   36281 12400 Toyota Camry 2010
# 2        1091  6    Red   61130  9900 Toyota Camry 2010
# 3        1091 17 Silver   43017 11700 Toyota Camry 2010
# 4        1091 10   Blue   56095 10400 Toyota Camry 2010
# 5        1091 24   Blue   31204 12900 Toyota Camry 2010
# 6        1091 18   Blue   53126 10700 Toyota Camry 2010

# > tail(mergedFiles)
# ModelNumber ID Color Mileage Price Make    Model Year
# 22        2310 23 Black   53942 10600 Ford Explorer 2010
# 23        2310 28 White   37107 12300 Ford Explorer 2010
# 24        2312 11 Black   47647 11200 Ford Explorer 2011
# 25        2312 15   Red   42685 11700 Ford Explorer 2011
# 26        2312 27   Red   30479 13000 Ford Explorer 2011
# 27        2312 25   Red   52674 10700 Ford Explorer 2011

##### Problem 7 #####

# Solution: 

merged2 <- merge(filedf, file2df)


# Results:
# > head(merged2)
# ModelNumber ID  Color Mileage Price   Make Model Year
# 1        1091  1   Blue   36281 12400 Toyota Camry 2010
# 2        1091  6    Red   61130  9900 Toyota Camry 2010
# 3        1091 17 Silver   43017 11700 Toyota Camry 2010
# 4        1091 10   Blue   56095 10400 Toyota Camry 2010
# 5        1091 24   Blue   31204 12900 Toyota Camry 2010
# 6        1091 18   Blue   53126 10700 Toyota Camry 2010

# > tail(merged2)
# ModelNumber ID Color Mileage Price Make    Model Year
# 22        2310 23 Black   53942 10600 Ford Explorer 2010
# 23        2310 28 White   37107 12300 Ford Explorer 2010
# 24        2312 11 Black   47647 11200 Ford Explorer 2011
# 25        2312 15   Red   42685 11700 Ford Explorer 2011
# 26        2312 27   Red   30479 13000 Ford Explorer 2011
# 27        2312 25   Red   52674 10700 Ford Explorer 2011

##### Problem 8 #####

# Solution:
  
newData <- subset(merged2, merged2$Year == "2010")


# Results: 
# newData
# ModelNumber ID  Color Mileage Price   Make    Model Year
# 1         1091  1   Blue   36281 12400 Toyota    Camry 2010
# 2         1091  6    Red   61130  9900 Toyota    Camry 2010
# 3         1091 17 Silver   43017 11700 Toyota    Camry 2010
# 4          1091 10   Blue   56095 10400 Toyota    Camry 2010
# 5         1091 24   Blue   31204 12900 Toyota    Camry 2010
# 6         1091 18   Blue   53126 10700 Toyota    Camry 2010
# 14        1254  7    Red   68400  9200 Toyota  Corolla 2010
# 15        1254  4  White   63624  9600 Toyota  Corolla 2010
# 16        1254 26  Green   34716 12500 Toyota  Corolla 2010
# 18        2111 19  Black   42945 11700   Ford    Focus 2010
# 19        2111 16  White   36216 12400   Ford    Focus 2010
# 21        2310 22 Silver   57672 10200   Ford Explorer 2010
# 22        2310 23  Black   53942 10600   Ford Explorer 2010
# 23        2310 28  White   37107 12300   Ford Explorer 2010


##### Problem 9 #####

# Solution

redMoreThan10k <- subset(merged2, merged2$Color == "Red" & merged2$Price > 10000)



# Results:
# redMoreThan10k
# ModelNumber ID Color Mileage Price   Make    Model Year
# 7         1142  3   Red   45827 11400 Toyota    Camry 2011
# 25        2312 15   Red   42685 11700   Ford Explorer 2011
# 26        2312 27   Red   30479 13000   Ford Explorer 2011
# 27        2312 25   Red   52674 10700   Ford Explorer 2011

##### Problem 10 #####

# Solution

redMoreThan10k$ModelNumber <- readMoreThan10k$Color <- NULL


# Results:
# redMoreThan10k
# ID Mileage Price   Make    Model Year
# 7   3   45827 11400 Toyota    Camry 2011
# 25 15   42685 11700   Ford Explorer 2011
# 26 27   30479 13000   Ford Explorer 2011
# 27 25   52674 10700   Ford Explorer 2011

##### Problem 11 #####

# Solution


# Input Characer Vector 

charVec <- as.character(1:100)

# > charVec
# [1] "1"   "2"   "3"   "4"   "5"   "6"   "7"   "8"   "9"   "10"  "11"  "12" 
# [13] "13"  "14"  "15"  "16"  "17"  "18"  "19"  "20"  "21"  "22"  "23"  "24" 
# [25] "25"  "26"  "27"  "28"  "29"  "30"  "31"  "32"  "33"  "34"  "35"  "36" 
# [37] "37"  "38"  "39"  "40"  "41"  "42"  "43"  "44"  "45"  "46"  "47"  "48" 
# [49] "49"  "50"  "51"  "52"  "53"  "54"  "55"  "56"  "57"  "58"  "59"  "60" 
# [61] "61"  "62"  "63"  "64"  "65"  "66"  "67"  "68"  "69"  "70"  "71"  "72" 
# [73] "73"  "74"  "75"  "76"  "77"  "78"  "79"  "80"  "81"  "82"  "83"  "84" 
# [85] "85"  "86"  "87"  "88"  "89"  "90"  "91"  "92"  "93"  "94"  "95"  "96" 
# [97] "97"  "98"  "99"  "100"

charToNumFunc <- function(charVec){
  c <- as.character(charVec)
  n <- as.numeric(c)
  print("Converted Char to Num Vector: ")
  print(n)
  print("Number of Elements per Char in Original Char Vector: ")
  print(nchar(c))
}


# Results: 
# charToNumFunc(charVec)
# [1] "Converted Char to Num Vector: "
# [1]   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17  18
# [19]  19  20  21  22  23  24  25  26  27  28  29  30  31  32  33  34  35  36
# [37]  37  38  39  40  41  42  43  44  45  46  47  48  49  50  51  52  53  54
# [55]  55  56  57  58  59  60  61  62  63  64  65  66  67  68  69  70  71  72
# [73]  73  74  75  76  77  78  79  80  81  82  83  84  85  86  87  88  89  90
# [91]  91  92  93  94  95  96  97  98  99 100
# [1] "Number of Elements per Char in Original Char Vector: "
# [1] 1 1 1 1 1 1 1 1 1 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
# [37] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2
# [73] 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 2 3

##### Problem 12 #####

# Solution

chVec1 <- c('a','c','e','g','i','k')
chVec2 <- c('b','d','f','h','j','l')
shorterCharVec <- c('a','b')
concatCharVecs <- function(chVec1, chVec2){
  if(length(chVec1) == length(chVec2)){
    paste(chVec1, chVec2, collapse = ' ')
  }
  else{
    print("Sorry, input char vector lengths do not match . . . ")
  }
}

# Results: 

concatCharVecs(chVec1, chVec2)
# [1] "a b c d e f g h i j k l"

concatCharVecs(chVec1, shorterCharVec)
# [1] "Sorry, input char vector lengths do not match . . . "

##### Problem 13 #####

# Solution

start <- 0
count <- 0
retSubVowelSubStr <- function(inputCharVec){
  for(k in inputCharVec){
    if(k == "a" | k == "e" | k == "i" | k == "o" | k == "u"){
      count <- count + 1
      if(count == 1){
        start <- which(inputCharVec == k)
        for(i in start:(start+2)){
          print(inputCharVec[i])
        }
      }
    }
  }
}

# Results:
#
# Test Case 1: 
# inputCharVec <- c('b','d','f','e','4','q','10')
# > retSubVowelSubStr(inputCharVec)
# > retSubVowelSubStr(inputCharVec)
# [1] "e"
# [1] "4"
# [1] "q"
#
# Test Case 2: 
# > inputCharVec <- c('q','r','z','p','b','n','14','1231','a','d')
# > retSubVowelSubStr(inputCharVec)
[1] "a"
[1] "d"
[1] NA
#
# Test Case 3: 
# > inputCharVec <- c('12312','a','1231','53432')
# > retSubVowelSubStr(inputCharVec)
# [1] "a"
# [1] "1231"
# [1] "53432"
#
# Test Case 4: 
# > inputCharVec <- c('z','d','2','q','r')
# > retSubVowelSubStr(inputCharVec)
# > 
#
# Test Case 5: 
# > inputCharVec <- c('z','e','4','gggg')
# [1] "e"
# [1] "4"
# [1] "gggg"

##### Problem 14 #####

# Solution:

# Dates By Month:
datesByJan <- as.numeric(rep(1,31))
datesByFeb <- as.numeric(rep(2,31))
datesByMar <- as.numeric(rep(3,31))
datesByApr <- as.numeric(rep(4,31))
datesByMay <- as.numeric(rep(5,31))
datesByJun <- as.numeric(rep(6,31))
datesByJuly <- as.numeric(rep(7,31))
datesByAug <- as.numeric(rep(8,31))
datesBySep <- as.numeric(rep(9,31))
datesByOct <- as.numeric(rep(11,31))
datesBySep <- as.numeric(rep(9,31))
datesByOct <- as.numeric(rep(10,31))
datesByNov <- as.numeric(rep(11,31))
datesByDec <- as.numeric(rep(12,31))

datesByMonth<-as.vector(cbind(datesByJan,datesByFeb,datesByMar,datesByApr,datesByMay,datesByJun,datesByJuly,datesByAug,datesBySep,datesByOct,datesByNov,datesByDec))

# Dates By Day: 
datesByDay<-as.numeric(rep(1:31,12))

# Dates By Year (One Year): 
datesByYear<-as.numeric(c(1:372))

# Dates Data Frame:
datesDF<-data.frame(datesByMonth, datesByDay, datesByYear)

# Results: 
# Jan 1 of This Year (denoted by 1) to Jan 6 of This Year
# > head(datesDF)
# datesByMonth datesByDay datesByYear
# 1            1          1           1
# 2            1          2           1
# 3            1          3           1
# 4            1          4           1
# 5            1          5           1
# 6            1          6           1
#
# Dec 26 of This Year (denoted by 1) to Dec 31 of This Year
# > tail(datesDF)
# datesByMonth datesByDay datesByYear
# 367           12         26           1
# 368           12         27           1
# 369           12         28           1
# 370           12         29           1
# 371           12         30           1
# 372           12         31           1
#
# Adding Fourth Date Column 

sumVec <- c(1:length(datesDF$datesByDay))
c2<-as.Date(sumVec, origin = "2013-12-25")
head(c2)
"2013-12-25" "2013-12-26" "2013-12-27" "2013-12-28" "2013-12-29" "2013-12-30"
tail(c2)
[1] "2014-12-26" "2014-12-27" "2014-12-28" "2014-12-29" "2014-12-30" "2014-12-31"
length(C2)
# [1] 372
# Adding to data frame
#
# **Note**: I didn't account for shorter months- I assumed all months had 31 days and thats why the values in c2
# are slightly off at the start/head of the data frame below.  Dates catch up by the end/tail because I tried to 
# account for shorter months.  I'm trying to figure out a fix for this but currently do not have one.
datesDFAddedCol <- data.frame(datesDF$datesByMonth, datesDF$datesByDay, datesDF$DatesByYear, c2)
head(datesDFAddedCol)
# datesByMonth datesByDay datesByYear         c2
# 1            1          1           1 2013-12-25
# 2            1          2           1 2013-12-26
# 3            1          3           1 2013-12-27
# 4            1          4           1 2013-12-28
# 5            1          5           1 2013-12-29
# 6            1          6           1 2013-12-30
tail(datesDFAddedCol)
# datesByMonth datesByDay datesByYear         c2
# 367           12         26           1 2014-12-26
# 368           12         27           1 2014-12-27
# 369           12         28           1 2014-12-28
# 370           12         29           1 2014-12-29
# 371           12         30           1 2014-12-30
# 372           12         31           1 2014-12-31

##### Problem 15 #####

# Solution

strDate <- '12-01-2014'
# Function to convert a date given in string format to a date object with the format month-day-year.
convDate <- function(strDate){
  as.Date(strDate, "%m-%d-%Y")
}

# Results: 
#
# Test Case 1: 
strDate <- '12-01-2014'
convDate(strDate)
# [1] "2014-12-01"
#
# Test Case 2: 
strDate <- '1-31-2014'
convDate(strDate)
# [1] "2014-01-31"
#
# Test Case  3: 
strDate <- '6-30-2013'
convDate(strDate)
# [1] "2013-06-30"
#
# ***Note***: There are only 30 days in June so the returned value is NA.
strDate <- '6-31-2001'
convDate(strDate)
# [1] NA

##### Problem 16 #####

# Solution
# We'll use the function convDate from Problem 15 above. 

dateObj <- convDate(strDate)
extrDate <- function(dateObj){
  extractedMonth <- format(dateObj, "%B")
  print(extractedMonth)
}

# Results:
# Test Case 1: 
strDate <- '12-25-2014'
dateObj <- convDate(strDate)
extrDate(dateObj)
# [1] "December"
#
# Test Case 2: 
strDate <- '06-30-2014'
dateObj <- convDate(strDate)
extrDate(dateObj)
# [1] "June"
#
# Test Case 3: 
strDate <- '05-04-1978'
dateObj <- convDate(strDate)
extrDate(dateObj)
[1] "May"

##### Problem 17 #####

# Solution:
dateRange <- as.Date(c("2005-01-01", "2014-12-31"))
dateSequenceOverRange <- seq(dateRange[1], dateRange[2], by = 1)

# Results: 
head(z)
# [1] "2005-01-01" "2005-01-02" "2005-01-03" "2005-01-04" "2005-01-05" "2005-01-06"
tail(z)
# [1] "2014-12-26" "2014-12-27" "2014-12-28" "2014-12-29" "2014-12-30" "2014-12-31"



