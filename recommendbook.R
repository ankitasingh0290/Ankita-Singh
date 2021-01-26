library("recommenderlab")
library(caTools)


book_rate_data <- read.csv("book.csv")
book_rate_data<-data.frame(book_rate_data[,-1])
View(book_rate_data)

str(book_rate_data)


#rating distribution
hist(book_rate_data$Book.Rating)

book_rate_data_matrix <-as(book_rate_data, 'realRatingMatrix')

#Popularity based 

book_recomm_model_2 <- Recommender(book_rate_data_matrix, method="POPULAR")

#Predictions for two users 
recommended_items_12 <- predict(book_recomm_model_2, book_rate_data_matrix[1167], n=5)
as(recommended_items_12, "list")
#######RESULT#########

#1] "In the Beauty of the Lilies" "Black House"                
#[3] "White Oleander : A Novel"    "The Magician's Tale"        
#[5] "Nowle's Passing: A Novel"   


