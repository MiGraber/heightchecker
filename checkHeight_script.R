# Script for simple function that checks the difference in height from the sex-
# specific mean for each of the students in the given dataframe
# Date: 24.10.2017
# Author: Jann Goschenhofer

library(dplyr)


# Initial Dataset
age = c(19, 22, 21, 23, 22, 20, 28, 25)
weight = c(50, 75, 80, 56, 75, 58, 65, 82)
height = c(1.66, 1.78, 1.90, 1.72, 1.83, 1.68, 1.70, 1.85)
sex = c("F", "M", "M", "F", "M", "F", "F", "M")

# Arrange data in Dataframe
students = data.frame(cbind(age, weight, height, sex))
students = transform(students, age = as.numeric(as.character(age)))
students = transform(students, height = as.numeric(as.character(height)))
students = transform(students, weight = as.numeric(as.character(weight)))

# Adding the coresponding names
students$name = c("Maria", "Franz", "Peter", "Lisa", "Hans", "Eva", "Mia", "Karl")

# Input: row of dataframe, female mean height, male mean height
# output: dataframe with name and difference to mean
check_height_2 <- function(person, fmeanHeight,mmeanHeight){
  #initialise resuting df
  result.frame <- data.frame(name=NA,differ=NA)
  
  fmeanHeight <- as.numeric(fmeanHeight)
  mmeanHeight <- as.numeric(mmeanHeight)
  
  #Sex specific mean comparsion
  if(person["sex"]=="F"){
    result.frame$name = person["name"]
    result.frame$differ = (as.numeric(person["height"]) - fmeanHeight)*100
    result.frame
  }else{
    result.frame$name = person["name"]
    result.frame$differ = (as.numeric(person["height"]) - mmeanHeight)*100
    result.frame
  }
}




# Function to compare the student heights with their mean
# Input is a dataframe; output is a dataframe
checkHeight3 = function(students.input = students){
  
  # Calculate the male mean height
  male.mean = students.input %>%
    filter(sex == "M") %>%
    summarise(mean = mean(height))
  # Calculate the female mean height
  female.mean = students.input %>%
    filter(sex == "F") %>%
    summarise(mean = mean(height))
  
  # Apply returns the data as list
  listedData <- apply(students.input, MARGIN = 1, FUN=check_height_2, mmeanHeight=male.mean, fmeanHeight=female.mean)
  # Unlisting and reorganizing the data into a dataframe
  result.frame <- data.frame(matrix(unlist(listedData), ncol = 2, byrow = T))
  colnames(result.frame) = c("name", "difference")
  return(result.frame)
}

checkHeight3(students.input = students)

