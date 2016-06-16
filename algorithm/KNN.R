library(XLConnect)
library(XLConnectJars)
library(readxl)

###################################
##Initialize
df <- read_excel("ParsedData.xlsx")
######################################################################################
##Assigning to Variables
findingTimeDifference <- function(x)
{
  options(scipen = 999) #remove scientific notation
  x["Time Difference"] <- NA
  firstSeen <- x[1:nrow(x),c('First Seen')]
  lastSeen <- x[1:nrow(x),c('Last Seen')]
  firstSeenData <- strptime(firstSeen, format = "%c",  tz = "CET")
  lastSeenData <- strptime(lastSeen, format = "%c",  tz = "CET")
  timeDifference <- difftime(lastSeenData, firstSeenData, units = "mins")
  timeDifference <- round(timeDifference, digits = 3)
  x[1:nrow(x), "Time Difference"] <- timeDifference
  
  i = 1
  while ( i <= nrow(x))
  {
    if((x[i,"Time Difference"] < 2.000) | (x[i,"Time Difference"] > 25.00))
    {
      x[i, "Time Difference"] <- -1
    }
      i <- i + 1
  }
  
  return (x[1:nrow(x),"Time Difference"])
}
df[1:nrow(df), "Time Difference"] <- findingTimeDifference(df)
########################################################
## creating phone list
#phone <- read_excel("phone.xlsx")
########################################################
##categorizing as vehicle or pedestrian
findingVP <- function(x)
{
  x["V or P"] <- NA
  i <- 1
  while (i <= nrow(x))
  {
    if((x[i, "Time Difference"] >= 2.000)  & x[i,"Time Difference"] < 15.000) # greater than 3 min to 25 min, pedestrian
    {
      x[i, "V or P"] <- "Vehicle"
      i <- i + 1
    }
    else if (x[i, "Time Difference"] >= 15.000 & x[i, "Time Difference"] <= 25.000)
    {
      x[i, "V or P"] <- "Pedestrian"
      i <- i + 1
    }
    else
    {
      x[i, "V or P"] <- "Other"
      i <- i + 1
    }
  }
  return (x[1:nrow(x), "V or P"])
}
df[1:nrow(df), "V or P"] <- findingVP(df)
########################################################
## functions to find the mean of vehicle and pedestrian
findingMeanVehicle <- function(x)
{
  temp <- matrix(NA, nrow(x), 1)
  i <- 1 #pointer to temp table
  j <- 1 #pointer to x, datafile
  while( j <= nrow(x) )
  {
    if( is.na(x[j, "V or P"]) | x[j, "V or P"] == "Pedestrian")
    {
      j <- j + 1  
    }
    else
    {
      temp[i, 1] <- x[j, "Time Difference"]
      i <- i + 1
      j <- j + 1
    }
  }
  return (colMeans(temp, na.rm = TRUE))
} 
meanVehicle <- findingMeanVehicle(df)
meanVehicle <- round(meanVehicle, digits = 3)
#######
findingMeanPedestrian <- function(x)
{
  temp <- matrix(NA, nrow(x), 1)
  i <- 1 #pointer to temp table
  j <- 1 #pointer to x, datafile
  while( j <= nrow(x) )
  {
    if( is.na(x[j, "V or P"]) | x[j, "V or P"] == "Vehicle")
    {
      j <- j + 1  
    }
    else
    {
      temp[i, 1] <- x[j, "Time Difference"]
      i <- i + 1
      j <- j + 1
    }
  }
  return (colMeans(temp, na.rm = TRUE))
}
meanPedestrian <- findingMeanPedestrian(df)
meanPedestrian <- round(meanPedestrian, digits = 3)
###########################################################################
#library(Hmisc)
#library(car)
#library(plyr)
##Category Identifying 
findingCategory <- function(x, y, z)
{
  x["Category"] <- NA
  updatedY = y + 2 # for fair
  updatedZ = z + 2
  updatedYY = y + 4 # for congestion
  updatedZZ = z + 4
  
  i <- 1
  while (i <= nrow(x))
  {
    if((x[i, "V or P"]) == "Other")
    {
      x[i, "Category"] <- "Other"
      i <- i + 1
    }
    else if (x[i, "V or P"] == "Vehicle")
    {
      if( (x[i,"Time Difference"] <= updatedY) && (x[i,"Time Difference"] > 0) ) #when cars are <= average
      {
        x[i,"Category"] <- "Good"
        i <- i + 1
      }
      else if ( (x[i, "Time Difference"] >= updatedYY) )                 #when cars are >= average + 2secs
      {
        x[i, "Category"] <- "Congested"
        i <- i + 1
      }
      else                                                            #when cars are between avg and avg+2
      {
        x[i, "Category"] <- "Fair"
        i <- i + 1
      }
    }
    else
    {
      if( (x[i,"Time Difference"] <= updatedZ) && (x[i,"Time Difference"] > 0) ) #when pedestrians are less than average
      {
        x[i,"Category"] <- "Good"
        i <- i + 1
      }
      else if ( (x[i, "Time Difference"] >= updatedZZ))                   #when pedestrians are greater than average
      {
        x[i, "Category"] <- "Congested"
        i <- i + 1
      }
      else
      {
        x[i, "Category"] <- "Fair"
        i <- i + 1
      }
    }
  }
  return (x[1:nrow(x),"Category"])
}
df[1:nrow(df), "Category"] <- findingCategory(df, meanVehicle, meanPedestrian)
#############################################################################################
## KNN
table(df$Category)
head(df)
set.seed(9850)
gp <- runif(nrow(df))
df2 <- df[order(gp), ]
head(df2)

summary(df2[,c(5)])
df_n <- as.data.frame(df2[,c(5)])
str(df_n)
summary(df_n)

df_train <- df_n[1:300, ,drop = FALSE]
df_test <- df_n[301:360, , drop = FALSE]
df_train_target <- df2[1:300, 7]
df_test_target <- df2[301:360, 7]
require(class)
sqrt(360)
m1 <- knn(train = df_train, test = df_test, cl = df_train_target, k = 19)
table(df_test_target, m1)



