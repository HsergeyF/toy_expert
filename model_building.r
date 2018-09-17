amazon_co_ecommerce_sample <- readr::read_csv("final_ds.csv")

library(stringr)

library(recommenderlab)

library(dplyr)

library(tidyr)

#Загрузим данные и уберем из них лишнее:

a=amazon_co_ecommerce_sample

a$price = tolower(a$price)

a$price = str_replace_all(a$price, "??", "")

a$average_review_rating = str_replace_all(a$average_review_rating, " out of 5 stars", "")

a$number_available_in_stock = str_replace_all(a$number_available_in_stock, "new", "")

a$number_available_in_stock = str_replace_all(a$number_available_in_stock, "used", "")


### Кластеризация

Одна из рекомендательных систем будет строиться на похожести игрушек по цене, рейтингу и количеству оставленных отзывов. 

Отберем нужные переменные:
  
  ```{r}
consumerChoice = "878048c41f3c249badb3704e160b4c6e"
class<-select(a, price, average_review_rating, uniq_id, recommended_age)
class <- class[1:100,]

rownames(class) <- class$uniq_id
class <- dplyr::select(class, -uniq_id)

library(lsa)
#cosine(class)
#cosine(t(data[1:3,])) #транспонируем данные, так как предыдущая функция работает со столбцами (находит схожесть столбцов)
class$price <- as.numeric(class$price)
class$average_review_rating <- as.numeric(class$average_review_rating)

#dist = cosine(t(class))
class[is.na(class)] = 0 #решаем вопрос с NA, которые могли возникнуть из-за первоначального пропуска данных
dist = cosine(t(class))

finalrecommend = tail(sort(dist[consumerChoice,]))
finalrecommend = finalrecommend[-1]
rec_id = rownames(as.data.frame(finalrecommend))
rec_id
saveRDS(class, "class.rds")
saveRDS(dist, "dist.rds")
#############
Customer_choice= "0bdc3b566d4fe151f0339047c2eba36a"
a=amazon_co_ecommerce_sample

a$price = tolower(a$price)

a$price = str_replace_all(a$price, "??", "")

a$average_review_rating = str_replace_all(a$average_review_rating, " out of 5 stars", "")

a$number_available_in_stock = str_replace_all(a$number_available_in_stock, "new", "")

a$number_available_in_stock = str_replace_all(a$number_available_in_stock, "used", "")
modelX<-dplyr::select(a, price, average_review_rating, uniq_id, manufacturer, number_of_reviews)

modelX$manufacturer=as.factor(modelX$manufacturer)
str(modelX$manufacturer) 
labels(modelX$manufacturer)

modelX$manufacturer = as.numeric(modelX$manufacturer)
rownames(modelX) <- modelX$uniq_id
modelX <- dplyr::select(modelX, -uniq_id)

library(lsa)

modelX$price <- as.numeric(modelX$price)
modelX$average_review_rating <- as.numeric(modelX$average_review_rating)
modelX$number_of_reviews <- as.numeric(modelX$number_of_reviews)

modelX[is.na(modelX)] = 0 #решаем вопрос с NA, которые могли возникнуть из-за первоначального пропуска данных
dist2= cosine(t(modelX))

Customer_choice = "0bdc3b566d4fe151f0339047c2eba36a"

finalrecommend_X = tail(sort(dist2[Customer_choice,]))
finalrecommend_X = finalrecommend_X[-1]
rec_id_x = rownames(as.data.frame(finalrecommend_X))
rec_id_x
toys_names_x <- a$product_name[match(rec_id_x, a$uniq_id)]
toys_names_x
modelX$girl = class$girl
modelX$boy = class$boy
modelX$recommended_age = class$recommended_age
saveRDS(modelX,"model.rds")
saveRDS(dist2,"dist2.rds")
