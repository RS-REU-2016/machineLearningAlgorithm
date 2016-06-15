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
  
#  i = 1
#  while ( i <= nrow(x))
#  {
#    if((x[i,"Time Difference"] < 1.000) | (x[i,"Time Difference"] > 30.00))
#    {
#      x[i, "Time Difference"] <- NA
#    }
#    i <- i + 1
#  }
  
  return (x[1:nrow(x),"Time Difference"])
}
df[1:nrow(df), "Time Difference"] <- findingTimeDifference(df)
########################################################
## creating phone list
phone <- read_excel("phone.xlsx")
########################################################
##categorizing as vehicle or pedestrian
findingVP <- function(x)
{
  x["V or P"] <- NA
  i <- 1
  while (i < nrow(x))
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
      x[i, "V or P"] <- NA
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
  for(i in 1:nrow(x))
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
df[1:nrow(df), "Category"] <- category(df, meanVehicle, meanPedestrian)
#############################################################################################

















