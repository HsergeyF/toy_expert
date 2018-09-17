library(readr)
library(stringr)
library(dplyr)
library(tidyr)
library(tidytext)
library(ggplot2)
toys = read_csv("D:\\amazon_co-ecommerce_sample.csv")


toys$price = str_replace_all(toys$price, "??", "")

toys$number_available_in_stock = str_replace_all(toys$number_available_in_stock, "new", "")
toys$number_available_in_stock = str_replace_all(toys$number_available_in_stock, "used", "")

toys$average_review_rating= str_replace_all(toys$average_review_rating, "out of 5 stars", "")

age <- "(age:+[0-9]{1,2})"
toys$recommended_age = str_extract(toys$product_information, age)
toys$recommended_age = str_replace_all(toys$recommended_age, "age:", "")
toys$recommended_age = str_replace_all(toys$recommended_age, "36", "3")




toys$uniq_id = as.factor(toys$uniq_id)
toys$number_of_reviews = as.factor(toys$number_of_reviews)
toys$number_of_answered_questions = as.factor(toys$number_of_answered_questions)
toys$manufacturer = as.factor(toys$manufacturer)
toys$price = as.numeric(toys$price)
toys$number_available_in_stock = as.factor(toys$number_available_in_stock)
toys$average_review_rating = as.factor(toys$average_review_rating)
toys$recommended_age = as.numeric(toys$recommended_age)





toys$amazon_category_and_sub_category = as.factor(toys$amazon_category_and_sub_category)
toys = separate(toys, col = amazon_category_and_sub_category, into = c("category", "sub_category", "sub_category1", "sub_category2", "sub_category3"), sep =">")

toys$customer_reviews = tolower(toys$customer_reviews)

toys$boy =0
toys$girl =0

uniqe_category = unique(toys$sub_category1)
girls = c(3,6,9,10,13,14,15,17,27,40,48,61,67,89,96,98,109,110,112,114,115,116)
boys = c(1,2,5,16,20,23,24,33,34,35,36,37,38,39,41,42,43,44,49,64,65,68,69,70,71,72,82,83,97,102,105,106,107,108)
both = c(7,8,11,12,18,19,21,22,25,26,28,29,30,31,32,45,46,47,50,51,52,53,54,55,56,57,58,59,60,62,63,66,73,74,75,76,77,78,79,80,81,84,85,86,87,88,90,91,92,93,94,95,99,100,101,104,105,111,113)
#========GILRS=============
for (j in 1:10000){
  
  for(i in girls){


  if(is.na(toys$sub_category1[j])){
    toys$girl[j] = 0
  }
else{
  if(toys$girl[j] == 1){break();}
    if (toys$sub_category1[j] == uniqe_category[i])
     {
       toys$girl[j] = 1  
     }
     else{
    toys$girl[j] = 0
     }
    }
} 
  }
#========BOYS=============

for (j in 1:10000){
  
  for(i in boys){
    
    if(is.na(toys$sub_category1[j])){
      toys$boy[j] = 1
    }
    else{
      if(toys$boy[j] == 1){break();}
      if (toys$sub_category1[j] == uniqe_category[i])
      {
        toys$boy[j] = 1  
      }
      else{
        toys$boy[j] = 0
      }
    }
  } 
}


#========BOTH=============

for (j in 1:10000){
  
  for(i in both){
    
    if(is.na(toys$sub_category1[j])){
      toys$boy[j] = 1
      toys$girl[j] = 0 
    }
    else{
      if(toys$boy[j] == 1&&toys$girl[j] == 1 )
        {break();}
      if (toys$sub_category1[j] == uniqe_category[i])
      {
        toys$boy[j] = 1
        toys$girl[j] = 1  
      }
    
    }
  } 
}

#========ADDITIONAL=============
for (j in 1:10000){
  
  if(toys$sub_category[i] == "Barbie "|| toys$sub_category[i] == "Accessories ")
  {toys$girl[i] =1}
}

write.csv(toys, "sranyi_ds.csv")

