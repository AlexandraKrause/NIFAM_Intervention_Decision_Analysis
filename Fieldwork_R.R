
library(readxl)
library(tidyverse)
library(ggplot2)
library("readxl")

library(sp)
library(raster)
library(gstat)
library(randomForest)
library(quantregForest)
library(plotKML)
library(scales)
library(ranger)
library("geosphere") 
library(dplyr)


fieldwork1 <- read_csv2("Corrected-Step1-field-work.csv")
fieldwork1

#fieldwork1 <- data.frame(fieldwork1[,1]) 
#for (column in c(2:ncol(fieldwork1)))
  
#fieldwork1 %>% 
#  dplyr::select(Group,Person,Longitude,Latitude)


fieldwork1 %>% dplyr::select(c(1,2,4,5))

#Filtered <-  fieldwork1 %>% 
#  filter(fieldwork1 == "starfish")

#So, the first step is to differentiate between NAs
#Fieldwork1 <- fieldwork1 %>% 
#  mutate_all(~replace(., . == "NA", NA))

#das klappt noch nicht:
#Summary of the data without NA
#n_method <- na.omit(Fieldwork1)


Brown = fieldwork1 %>%
  dplyr::filter(Person =="brown")
Brown.a=Brown$Longitude
Brown.a

Brown.b= Brown$Latitude
Brown.b

#rbind vectors into a matrix
mat<-rbind(Brown.a,Brown.b)
#calculate manhattan distance between each vector in the matrix
#note to myself: think again about how to best use the workplace
#and home address in this manhattan distance.
#maybe ask gis specialsts back in germany
#i assume i cannot use euclidean distance here, but rather anhattan distcance.
#Clariify in germany!
#as far as i get it, it calculates the overall distance between all
#points in a grid: buffers already? Euclidean distance should give straight
#lines (distances point a and b), manhattan is using a grid
#manhattan: sum of absolute diffrence between vectors
#maybe do it again in python and use "cityblock" function for geolocations
#think: also include start points f strret when poeple only give ca. locations?

result<-dist(mat,method="manhattan")
result

#The manhattan distance between the two vectors is 220.93820 city blocks
#bit high.....check result
#The Manhattan distance between two vectors, A and B, is calculated as:
#Σ|ai – bi|
#where i is the ith element in each vector.

#The Manhattan distance between two points is the length of the shortest
#path that connects them, measured in city blocks.
#This distance is a common measure of distances in urban settings.

#this distance can be compared between all groups of grandmothers, grandfathers,
#young mothers, young fathers, teens, maids
#then, use buffers in gis to see the os to say "circle measuremnets" per group
###############################################################
#Eucledean distance for bufferoing
######################

euclidean <- function(a, b) sqrt(sum((a - b)^2))
result2<-euclidean(Brown.a, Brown.b)
#so, i would need to insert utm coordinates, no lat and long for eucledean distance
#rifht now i have no time to change the table
# The Euclidean distance between the two vectors turns out to be (here wrong: 129.1806
#the max travel distance of person "brown" in hanoi should be 129 no unit
#-> i need to find better,more precise coordinates for the adresses...
#google earth would be good
#this result is not correct, since i took them from googl maps

#Euclidean buffers—another kind
#of buffer—measure distance on a two-dimensional Cartesian plane.
#With Euclidean buffers, straight-line distances are calculated between
#two points on a plane. Euclidean buffers appear as perfect circles
#when drawn on a projected flat map, while geodesic buffers appear
#as perfect circles only on a globe. On two-dimensional maps,
#geodesic buffers often appear as irregular ellipses.

#i should best do two: one from workplace to food, and ne from home to food
#in the future!

##################################################################
#Food types

fieldwork1 <- read_csv2("Corrected-Step1-field-work.csv")
fieldwork1

#fieldwork1 <- data.frame(fieldwork1[,1]) 
#for (column in c(2:ncol(fieldwork1)))

#fieldwork1 %>% 
#  dplyr::select(Group,Person,Longitude,Latitude)


fieldwork1 %>% dplyr::select(c(2,8,9,10,11,12))
Brown = fieldwork1 %>%
  dplyr::filter(Person =="brown")

#So, the first step is to differentiate between NAs
fieldwork1 <- fieldwork1 %>% 
  mutate_all(~replace(., . == "NA", NA))


#Summary of the data without NA
n_method <- na.omit(fieldwork1)

Together <- fieldwork1 %>% 
  gather(key = "TopicPart", value = "Topics", c(Food-type1, Food-type2,
                                                Food-type3,Food-type4,
                                                Food-type5))






