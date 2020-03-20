################################
# Create edx set, validation set
################################

# Note: this process could take a couple of minutes

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data
set.seed(1, sample.kind="Rounding")
# if using R 3.5 or earlier, use `set.seed(1)` instead
test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)
edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)

rm(dl, ratings, movies, test_index, temp, movielens, removed)

#### List of questions for the test
#Structure of edx set :
dim(edx)
#other answer for number of rows and columns
nrow(edx)
ncol(edx)
#number of films having a 3.0 rating (using nrow or tally)
edx%>%filter(rating==3)%>%nrow()
#How many users and movies rated ?
edx%>%
  summarize(n_users=n_distinct(userId),n_movies=n_distinct(movieId))
#number of different movies
length(unique(edx$movieId))
#simpler :
n_distinct(edx$movieId)
#number of different users
n_distinct(edx$userId)
#number of films by genre
edx%>%filter(genres %like% "Thriller")%>%nrow()
edx %>% separate_rows(genres, sep = "\\|") %>%
  group_by(genres) %>%
  summarize(count = n()) %>%
  arrange(desc(count))
#number of ratings by film, decreasing
edx%>%
  group_by(movieId,title)%>%
  summarize(count=n())%>%
  arrange(desc(count))
#number of films, rating decreasing
edx%>%
  group_by(rating)%>%
  summarise(count=n())%>%
  arrange(desc(count))
edx %>%
  group_by(rating) %>%
  summarize(count = n()) %>%
  ggplot(aes(x = rating, y = count)) +
  geom_line()


#Data cleaning
head(edx)
#It seems like data is organised by userId and movieId. 
#The timestamp is not very useful in that format, title includes title and year and genres includes multiple genres separated by |
#let's clear all that !
library(lubridate)
library(tidyr)
library(tidyverse)
library(stringr)
library(purrr)
library(readr)
#Let's test on a few entries how to split title and year, and obtaining a useful date
test<-edx[1:5,]
test
test_2<-test %>%
  mutate("title"=str_match(test$title,"(.*) \\((.*)\\)")[,2], "year"=as.numeric(str_match(test$title,"(.*) \\((.*)\\)")[,3])) %>%
  mutate (date = as_datetime(timestamp))
test_2
class(test_2$year)
# Now let's do this on the whole set
edx_mod<-edx %>%
  mutate("title"=str_match(edx$title,"(.*) \\((.*)\\)")[,2], "year"=as.numeric(str_match(edx$title,"(.*) \\((.*)\\)")[,3])) 
#just checking
tail(edx_mod)

#Data exploration
#Let's observe the influence of year on the movie ratings 
edx_mod %>% group_by(movieId) %>%
  summarize(n = n(), year = as.character(first(year))) %>%
  qplot(year, n, data = ., geom = "boxplot") +
  coord_trans(y = "sqrt") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#We can see the number of ratings improving in the 90's, and decreasing afterwards.So year seems to have a strong impact.
#What about the most rated movies ?
edx_mod %>% 
  filter(year >= 1995) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2020 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  top_n(25, rate) %>%
  arrange(desc(rate)) 


edx_mod %>% 
  filter(year >= 1993) %>%
  group_by(movieId) %>%
  summarize(n = n(), years = 2018 - first(year),
            title = title[1],
            rating = mean(rating)) %>%
  mutate(rate = n/years) %>%
  ggplot(aes(rate, rating)) +
  geom_point() +
  geom_smooth()

#the more often the movie is rated, the higher its average is.

#Modifying the timestamp into date
edx_mod2 <- mutate(edx_mod, date = as_datetime(timestamp))
edx_mod2 <- edx_mod2%>%mutate(date = round_date(date, unit = "week"))
head(edx_mod2)
#influence of date over rating
edx_mod2 %>%
  group_by(date) %>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()



#We could predict the mean rating for any movie, if every movie had the same rating
mu_hat<-mean(edx_mod$rating)
mu_hat

#Building an RMSE function
RMSE<-function(true_ratings,predicted_ratings){
  sqrt(mean((true_ratings-predicted_ratings)^2))
}
#Simplest model : predict the mean of ratings for unrated movie
simple_rmse<-RMSE(edx_mod$rating,mu_hat)
simple_rmse
#as we get something around 1, we are 1 point away from the actual rating when we are predicting
#it is more likely that the highest rated movies will get higher ranking.
movie_avgs<-edx_mod%>%
  group_by(movieId)%>%
  summarize(b_i=mean(rating-mu_hat))
qplot(b_i, data = movie_avgs, bins = 10, color = I("black"))

predicted_ratings<-mu_hat+edx_mod %>%
  left_join(movie_avgs,by='movieId') %>%
  pull(b_i)
RMSE(predicted_ratings,edx_mod$rating)

#user effect
#Establishing that the user has an effect on rating
edx_mod2%>% 
  group_by(userId) %>% 
  summarize(b_u = mean(rating)) %>% 
  filter(n()>=100) %>%
  ggplot(aes(b_u)) + 
  geom_histogram(bins = 30, color = "black")
#Modeling the user effect
user_avgs <- edx_mod2 %>% 
  left_join(movie_avgs, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu_hat - b_i))
#Predicting based on mean, common rating and user effect
predicted_ratings <- edx_mod2 %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu_hat + b_i + b_u) %>%
  pull(pred)
RMSE(predicted_ratings, edx_mod$rating)

#RMSE(predicted_ratings,validation$rating)

#adding a date effect ? make a plot of the date effect, then model it
edx_mod2%>%
  mutate(date=round_date(date,unit='week'))%>%
  group_by(date)%>%
  summarize(rating = mean(rating)) %>%
  ggplot(aes(date, rating)) +
  geom_point() +
  geom_smooth()
#just a smooth function to model time effect :

time_effect <- edx_mod2 %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(date=round_date(date,unit='week'))%>%
  group_by(date)%>%
  summarize(b_t = smooth(mean(rating - mu_hat - b_i - b_u)))
edx_mod2<-edx_mod2%>%mutate(date=round_date(date,unit='week'))

#Predicting based on mean, common rating, user effect and date effect

predicted_ratings <- edx_mod2 %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(time_effect, by='date')%>%
  mutate(pred = mu_hat + b_i + b_u + b_t) %>%
  pull(pred)

RMSE(predicted_ratings, edx_mod2$rating)


#adding a genre effect ? make a plot of the genre effect, then model it
edx_mod2%>% group_by(genres) %>%
  summarize(n = n(), avg = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(n >= 1000) %>% 
  mutate(genres = reorder(genres, avg)) %>%
  ggplot(aes(x = genres, y = avg, ymin = avg - 2*se, ymax = avg + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
#modeling
genre<-edx_mod2%>%
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(time_effect,by='date') %>%
  group_by(genres)%>%
  summarize(b_g = mean(rating - mu_hat - b_i - b_u - b_t))
#predicting
predicted_ratings <- edx_mod2 %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(time_effect, by='date')%>%
  left_join(genre, by='genres') %>%
  mutate(pred = mu_hat + b_i + b_u + b_t + b_g) %>%
  pull(pred)
#computing rmse
RMSE(predicted_ratings, edx_mod2$rating)

# RMSE on validation set
validation_2<-validation %>%
  mutate("title"=str_match(validation$title,"(.*) \\((.*)\\)")[,2], "year"=as.numeric(str_match(validation$title,"(.*) \\((.*)\\)")[,3])) 
validation_2 <-validation_2 %>%
  mutate(date = as_datetime(timestamp)) %>% 
  mutate(date = round_date(date, unit = "week"))
predicted_ratings <- validation_2 %>% 
  left_join(movie_avgs, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  left_join(time_effect, by='date')%>%
  left_join(genre, by='genres') %>%
  mutate(pred = mu_hat + b_i + b_u + b_t + b_g) %>%
  pull(pred)

RMSE(predicted_ratings, validation_2$rating)
#penalty term (lambda)
#PCA to identify correlation 
