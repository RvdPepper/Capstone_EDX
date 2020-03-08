## Create edx set, validation set

# Note: this process could take a couple of minutes
if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")
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
# set.seed(1, sample.kind="Rounding")
set.seed(1)
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
```
# Data Exploration (Before Data Cleaning)

# The first ten lines of the dataset:
  
edx[1:10,]

# summary of the statistics of the dataset:
  
summary(edx)

#The number of rows

nrow(edx)

#The number of columns

ncol(edx)

#How many different movies are in the edx dataset?

Titles <- edx %>% select(movieId) %>% distinct()
nrow(Titles)

#How many different users are in the edx dataset

Users <- edx %>% select(userId) %>% distinct()
nrow(Users)`|
  
#Which movie has the greatest number of ratings?
number_ratings <- edx %>% group_by(title) %>% summarise(number = n()) %>% arrange(desc(number))
number_ratings[1:10,]

## Data Cleaning

#Modify the year as a column in the edx & validation datasets
edx <- edx %>% mutate(year = as.numeric(str_sub(edx$title,-5,-2)))
validation <- validation %>% mutate(year = as.numeric(str_sub(validation$title,-5,-2)))

## Data Exploration (After Data Cleaning)

### How are the ratings distributed (histogram)? 
edx %>% qplot(rating, geom ="histogram", bins = 10, data = ., color = I("black"))

### How are the ratings distributed over the years (histogram)? 
edx %>% qplot(year, geom ="histogram", bins = 10, data = ., color = I("black"))

## Modelling Approach

#RMSE function
RMSE <- function(true_ratings, predicted_ratings){sqrt(mean((true_ratings - predicted_ratings)^2))}

### Building the recommendation system

#Simple Model
mu_hat <- mean(edx$rating)

#How well does this model
naive_rmse <- RMSE(mu_hat, validation$rating)
naive_rmse

#table that stores the result of the different model
rmse_results <- data.frame(Method = "Just the average", RMSE = naive_rmse)
rmse_results %>% knitr::kable()

#Second step, movie effect Model 
mu <- mean(edx$rating)
movie_avgs <- edx %>% group_by(movieId) %>% summarize(b_i = mean(rating-mu))

#The histogram shows the deviation of a movie rating from the average rating. 
movie_avgs %>% qplot(b_i, geom ="histogram", bins = 10, data = ., color = I("black"))

#Updated RMSE table
predicted_ratings <- mu + validation %>% left_join(movie_avgs, by='movieId') %>% .$b_i
model_1_rmse <- RMSE(validation$rating, predicted_ratings)
rmse_results <- bind_rows(rmse_results, data.frame(Method="Movie Effect Model", RMSE = model_1_rmse))
rmse_results %>% knitr::kable()

#Third step, movie effect and user effect Model.

#The histogram shows the variability of a movie rating of a user.
edx %>% group_by(userId) %>% summarize(b_u = mean(rating)) %>% filter(n()>=100) %>% ggplot(aes(b_u)) + geom_histogram(bins = 30, color = "black")

#Updated RMSE table
user_avgs <- edx %>% left_join(movie_avgs, by='movieId') %>% group_by(userId) %>% summarize(b_u = mean(rating - mu - b_i))
predicted_ratings <- validation %>% left_join(movie_avgs, by='movieId') %>% left_join(user_avgs, by='userId') %>% mutate(pred = mu + b_i + b_u) %>% .$pred
model_2_rmse <- RMSE(validation$rating, predicted_ratings)
rmse_results <- bind_rows(rmse_results, data.frame(Method="Movie + User Effects Model", RMSE = model_2_rmse ))
rmse_results %>% knitr::kable()

#Fourth step, regularize the movie and user effect.

#Top 5 Best predicted movies:
movie_titles <- edx %>% select(movieId, title) %>% distinct()
edx %>% count(movieId) %>% left_join(movie_avgs) %>% left_join(movie_titles, by="movieId") %>% arrange(desc(b_i)) %>% select(title, b_i,n) %>% slice(1:5) %>% knitr::kable()

#Top 5 Worst predicted movies:
edx %>% count(movieId) %>% left_join(movie_avgs) %>% left_join(movie_titles, by="movieId") %>% arrange(b_i) %>% select(title, b_i,n) %>% slice(1:5) %>% knitr::kable()
    
#First we have to pick the optimal tuning parameter lambda. 
lambdas <- seq(0, 10, 0.25)
rmses <- sapply(lambdas, function(l){
mu <- mean(edx$rating)
  
  b_i <- edx %>% group_by(movieId) %>% summarize(b_i = sum(rating-mu)/(n()+l))
  b_u <- edx %>% left_join(b_i, by="movieId") %>% group_by(userId) %>% summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  
predicted_ratings <- validation %>% left_join(b_i, by = "movieId") %>% left_join(b_u, by = "userId") %>% mutate(pred = mu + b_i + b_u) %>% pull(pred)
  
return(RMSE(predicted_ratings, validation$rating))})

qplot(lambdas, rmses)

#The optimal lambda is:
lambda <- lambdas[which.min(rmses)]
lambda

#Updated RMSE table
rmse_results <- bind_rows(rmse_results, data.frame(Method="Regularized Movie + User Effects Model", RMSE = min(rmses)))
rmse_results %>% knitr::kable()

## Results Section
rmse_results %>% knitr::kable()
