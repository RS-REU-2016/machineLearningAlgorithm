library(XLConnect)
library(XLConnectJars)
library(readxl)

###################################
##Example
df <- read_excel("dataCopy.xlsx")
dtm <- strptime(c("Mon Mar 28 13:02.00 2016"), format = "%a %b %d %M:%OS %Y",  tz = "CET")
dtm1 <- strptime(c("Mon Mar 28 23:55.04 2016"), format = "%a %b %d %M:%OS %Y",  tz = "CET")
difftime(dtm1[1], dtm[1])
######################################################################################
##Assigning to Variables
firstSeen <- df[1:292,c('First Seen')]
lastSeen <- df[1:292,c('Last Seen')]
firstSeenData <- strptime(firstSeen, format = " %a %b %d %H:%M:%S %Y",  tz = "CET")
lastSeenData <- strptime(lastSeen, format = " %a %b %d %H:%M:%S %Y",  tz = "CET")
df[1:292, "Time Difference"] <- timeDifference <- difftime(lastSeenData, firstSeenData, units = "mins")
########################################################
## functions to find the mean of vehicle and pedestrian
findingMeanVehicle <- function(x)
{
  temp <- matrix(NA, 292, 1)
  i <- 1 #pointer to temp table
  j <- 1 #pointer to x, datafile
  while( j <= 292 )
  {
    if( x[j, "Company"] == "Unknown" ) ## calculating mean for vehicle
    {
      if( x[j, "Time Difference"] != 0 )
      {
        temp[i, 1] <- x[j, "Time Difference"]
        i <- i + 1
        j <- j + 1
      }
      else
      {
        j <- j + 1
      }
    }
    else
    {
      j <- j + 1
    }
  }
  return (colMeans(temp, na.rm = TRUE))
} 
meanVehicle <- findingMeanVehicle(df)
#######
findingMeanPedestrian <- function(x)
{
  temp <- matrix(NA, 292, 1)
  i <- 1 #pointer to temp table
  j <- 1 #pointer to x, datafile
  while( j <= 292 )
  {
    if( x[j, "Company"] != "Unknown" ) ## calculating mean for vehicle
    {
      if( x[j, "Time Difference"] != 0 )
      {
        temp[i, 1] <- x[j, "Time Difference"]
        i <- i + 1
        j <- j + 1
      }
      else
      {
        j <- j + 1
      }
    }
    else
    {
      j <- j + 1
    }
  }
  return (colMeans(temp, na.rm = TRUE))
}
meanPedestrian <- findingMeanPedestrian(df)
###########################################################################
library(Hmisc)
library(car)
library(plyr)
##Category Identifying 
df["Category"] <- NA

category <- function(x, y, z)
{
  updatedY = y + 2 # for fair
  updatedZ = z + 2
  updatedYY = y + 4 # for congestion
  updatedZZ = z + 4
  for(i in 1:292)
  {
    if(x[i,"Company"] == "Unknown")
    {
      if( (x[i,"Time Difference"] <= updatedY) && (x[i,"Time Difference"] > 0) ) #when cars are <= average
      {
        x[i,"Category"] <- "Good"
      }
      else if ( (x[i, "Time Difference"] >= updatedYY) )                 #when cars are >= average + 2secs
      {
        x[i, "Category"] <- "Congested"
      }
      else                                                            #when cars are between avg and avg+2
      {
        x[i, "Category"] <- "Fair"
      }
    }
    else
    {
      if( (x[i,"Time Difference"] <= updatedZ) && (x[i,"Time Difference"] > 0) ) #when pedestrians are less than average
      {
        x[i,"Category"] <- "Good"
      }
      else if ( (x[i, "Time Difference"] >= updatedZZ))                   #when pedestrians are greater than average
      {
        x[i, "Category"] <- "Congested"
      }
      else
      {
        x[i, "Category"] <- "Fair"
      }
    }
  }
  return (x[1:292,"Category"])
}
df[1:292, "Category"] <- category(df, meanVehicle, meanPedestrian)
#############################################################################################

















