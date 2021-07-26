### title: "Movielens_Project_Capstone 1"
#### author: "Paolo Sebastianelli" 

## Project Overview: 

# The purpose of this project is to create a movie recommendation ML model
# using the data set extracted from MovieLens Dataset (hereafter MLD) provided 
# in the last course of final course in HarvardX's multi-part 
# **Data Science Professional Certificate** series.
# Since the entire MLD contains millions of ratings,
# a subset will be used to show the results obtained by the models implementation.

###################################
########## GENERATING DATA SETS ###
###################################

### CHECKING IF THE PACKAGES ARE INSTALLED #############
if(!require(DataExplorer)) install.packages("DataExplorer", 
                                            repos = "http://cran.us.r-project.org")
if(!require(rayshader)) install.packages("DataExplorer", 
                                         repos = "http://cran.us.r-project.org")
if(!require(rayrender)) install.packages("DataExplorer", 
                                         repos = "http://cran.us.r-project.org")
if(!require(tidyverse)) install.packages("tidyverse", 
                                         repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", 
                                     repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", 
                                          repos = "http://cran.us.r-project.org")
if(!require(Metrics)) install.packages("Metrics", 
                                       repos = "http://cran.us.r-project.org")
if(!require(dplyr)) install.packages("dplyr", 
                                     repos = "http://cran.us.r-project.org")
if(!require(ggplot2)) install.packages("ggplot2", 
                                       repos = "http://cran.us.r-project.org")
if(!require(lubridate)) install.packages("lubridate", 
                                         repos = "http://cran.us.r-project.org")
if(!require(recosystem)) install.packages("recosystem", 
                                         repos = "http://cran.us.r-project.org")
if(!require(plot3D)) install.packages("plot3D", 
                                          repos = "http://cran.us.r-project.org")

##### LOADING THE LIBRARIES ###############

library(tidyverse)
library(caret)
library(data.table)
library(Metrics)
library(dplyr)
library(ggplot2)
library(DataExplorer)
library(lubridate)
library(rayshader)
library(rayrender)
library(tidyr)
library(recosystem)
library(plot3D)

###################### CODE FROM EDX COURSE ############################
########################################################################
## This is the code provided in HarvardX PH125.9x Data Science: Capstone
########################################################################
# Create edx set, validation set (final hold-out test set)
########################################################################

# The entire MovieLens 10M dataset can be found at:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

dl <- tempfile()

### downloading the zip file ml-10m.zip from files.grouplens.org
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

### USING R version 4.0.1

movies <- as.data.frame(movies) %>% 
          mutate(movieId = as.numeric(movieId),
                 title  = as.character(title),
                 genres = as.character(genres))

##Joining the tables specifying the column to use to match: movieId
movielens <- left_join(ratings, movies, by = "movieId")

### Setting a seed 
set.seed(1, sample.kind="Rounding")

# We want a Validation set that has to be 10% of MovieLens data
### Using createDataPartition from caret package 

test_index <- createDataPartition(y = movielens$rating, 
                                  times = 1, 
                                  p = 0.1, 
                                  list = FALSE)

edx <- movielens[-test_index,]   
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also 
# in edx set using semi_join function

validation <- temp %>%
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)  #### edx contains again all the rows

rm(dl, ratings, movies, test_index, temp, movielens, removed)  ### removing objects

##### As final result of this code chunks you should see in your environment 
##### window only two sets: edx and validation data sets.

###########################################################
###################   DATA ANALYSIS  ###################### 
###########################################################

#############################
### Edx data set structure ##
#############################

str(edx)

### At this point: 9000055 obs. of  6 variables

#############################
### DataExplorer functions ##
#############################

### Plotting the structure
DataExplorer::plot_str(edx) ### 6 variables displayed

### Making an introductory plot
DataExplorer::plot_intro(edx)   #### no missing values 

##########################################
#### Modifying edx and validation sets ###
##########################################

#### Before to continue the analysis I modified _edx_ and _validation_ sets 
#### for further investigation, adding other five columns 
#### The five columns are: date, date_week, time with format "%H", 
#### year of the rating rat_year with format "%Y" and movie_year as 
#### year of movie release.

#### EDX
edx <- mutate(edx, date = as_datetime(timestamp))
edx <- mutate(edx, date_week = round_date(date, unit = "week"))
edx <- mutate(edx, time = format(date, format = "%H"))
edx <- mutate(edx, rat_year = format(date, format = "%Y"))

####Extracting the year from title
movie_year <- as.numeric(str_sub(edx$title,-5,-2))  
edx <- mutate(edx, movie_year = movie_year)

### VALIDATION
validation <- mutate(validation, date = as_datetime(timestamp))
validation <- mutate(validation, date_week = round_date(date, unit = "week"))
validation <- mutate(validation, time = format(date, format = "%H"))
validation <- mutate(validation, rat_year = format(date, format = "%Y"))

####Extracting the year from title
movie_year_v <- as.numeric(str_sub(validation$title,-5,-2))
validation <- mutate(validation, movie_year = movie_year_v)

### Check with 
edx %>% as_tibble()
validation %>% tibble()

### Note: Edx and Validation have the same columns
###       The Data Analysis will be performed only 
###       on edx set. 

################################
##### EDX DATA SET STUDY #######
################################

#### Fast summary 

edx %>%
  summarize(n_genres = n_distinct(genres),
            n_movies = n_distinct(movieId),
            n_users = n_distinct(userId),
            n_ratings = length(rating),
            n_years = n_distinct(rat_year),
            n_movie_years = n_distinct(movie_year))

## Summary output:
## n_genres n_movies n_users n_ratings n_years n_movie_years
##     797    10677   69878   9000055      15            94

### RATINGS DISTRIBUTION (Best number of bins for visualization = 10)

plot(table(edx$rating), 
     main="Edx Data Set - Rating distribution", 
     xlab="Rating values", ylab="# of occurences")
### Adding a dashed red line to identify the mean value position
abline(v = mean(edx$rating), col="red", lwd=3, lty=2) 

### The most frequent rating values are between 3 and 4 
### and the mean value is 3.512

###########################
###### MOVIE EFFECTS ######
###########################

#### Number of ratings per each movie 
plot(table(edx$movieId), main="Edx Data Set - Number of ratings per movie",
     xlab="Movie Id", ylab="# of occurences")
### NOTE: DIFFERENT MOVIES SHOW DIFFERENT NUMBERS OF RATING.
### In various cases the difference is wide. 

### Storing number of ratings and Rating Mean Values (hereafter RMV)
### per each movie
pippo <- edx %>%
  group_by(movieId) %>%
  summarize(count = n(), RMV = mean(rating))

### Plotting userId vs RMV
ggplot(pippo) +
  geom_point(aes(movieId,RMV)) +
  geom_hline(yintercept=4.5, color = "red", size=1)+
  geom_hline(yintercept=1.5,color = "blue", size=1)
### Note: Red and Blue lines define an "average" zone.

### 3D Visualization
scatter3D(pippo$movieId, pippo$RMV, pippo$count, 
          phi = 10, theta = -50, col = "blue",
          pch = 5, cex = 0.2, margin = 0.1, 
          xlab = "movieId", ylab = "RMV", zlab ="count")
### Note: The most liked movies (higher RMV) are the most rated
###       (Top up left corner)
###       Some movie with extreme RMV (5 or 1) have been rated few times

###########################
###### USER EFFECTS #######
###########################

### Storing number of ratings and Rating Mean Values (hereafter RMV)
### per each user
edx_user_RMV <- edx %>%
  group_by(userId) %>%
  summarize(count = n(), edx_user_RMV = mean(rating))%>%
  arrange(desc(edx_user_RMV)) 
edx_user_RMV
#### Note: RMV Descending order. Some user seems to know only the 5 stars rating
####       Others only very low ratings value. 

### Storing number of ratings and Rating Mean Values (hereafter RMV)
### per each user
edx_user_RMV <- edx %>%
  group_by(userId) %>%
  summarize(count = n(), edx_user_RMV = mean(rating))%>%
  arrange(desc(count))
edx_user_RMV
#### Note: RMV Descending count. Some user is very active, others no. 

### Visualizing Number of Ratings per user
plot(table(edx$userId), 
     main="Edx Data Set - Number of ratings per user",
     xlab="User Id",
     ylab="# of occurences")
linea <- edx %>%
  group_by(userId) %>%
  summarize(count = n())
abline(h = mean(linea$count), col="red", lwd=3, lty=2)
abline(v = 59269 , col="blue", lwd=2, lty=2)
abline(h = 6637, col="blue", lwd=2, lty=2)

### Looking for the user with the highest number of rating
linea$userId[which.max(linea$count)]
linea$count[which.max(linea$count)]
### User: 59269   Count: 6616 movies that this user has rated

### Using rayshader package to visualize a 3D plot
pluto <- edx %>%
  group_by(userId) %>%
  summarize(count = n(), RMV = mean(rating))
ggp <- ggplot(pluto) +
  geom_point(aes(userId, RMV, color = count), size=0.9)+
  scale_color_gradient(low="blue", high="red")

#### The plot showed in the pop-up can be rotated to understand better
#### The situation.
plot_gg(ggp, multicore = TRUE, 
        width=5, 
        height=5, 
        zoom = 0.45, 
        phi = 30, 
        theta = 45)

###Taking a snapshot for pdf rendering
render_snapshot()
#### Clearing
render_snapshot(clear = TRUE)

###########################
###### GENRE EFFECTS ######
###########################

# Illustrative String detection
# Trying with some of the genres that are in the dataset. 

### Genres' vector c

genres = c("Drama", 
           "Comedy", 
           "Thriller", 
           "Romance", 
           "Action", 
           "Adventure", 
           "Animation", 
           "Children")

## Building a function that using the names x in genres vector to count
## each genre.

sapply(genres, function(x) {
  sum(str_detect(edx$genres, x))
})

#### results:  Drama    Comedy    Thriller   Romance    Action    Adventure  Animation  Children
############  3909401   3541284   2325349    1712232    2560649   1908692    467220     737851 

### Visualizing the RMV per each genre, after selecting only the genres 
### rated more than 100000 times. 
paperino1 <- edx %>%
  group_by(genres) %>%
  summarize(count = n(), RMV = mean(rating), se = sd(rating)/sqrt(n())) %>%
  filter(count >= 100000) %>% 
  arrange(RMV)

ggplot(paperino1, aes(x = genres, y = RMV, ymin = RMV - 2*se, ymax = RMV + 2*se)) + 
  geom_point() +
  geom_errorbar() + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
### Note: in this subset Drama|War has the best RMV, and Comedy the lowest

### Storing count and RMV per each genre
paperino2 <- edx %>%
  group_by(genres) %>%
  summarize(count = n(), RMV = mean(rating)) %>%
  arrange(desc(RMV))
paperino2
### Note: RMV Descending order. 
###       !! Interesting Animation|IMAX|Sci-Fi has the 
###          highest RMV but was rated very few times (7).
###       This motivates the regularization process.

plot(table(edx$genres), main="Edx Data Set - Number of ratings per genre",
     xlab="Genre description", ylab="# of occurences")

paperino3 <- edx %>%
  group_by(genres) %>%
  summarize(count = n(), RMV = mean(rating))

gg <- ggplot(paperino3) +
  geom_point(aes(genres, RMV, color = count), size=0.9) +
  theme(axis.text.x = element_blank()) +
  scale_color_gradient(low="blue", high="red")

plot_gg(gg, multicore = TRUE, 
                       width=5, 
                       height=5, 
                       zoom = 0.45, 
                       phi = 30, 
                       theta = 45)

render_snapshot()

###########################
###### TIME EFFECTS #######
###########################

### Visualizing RMV for rating date (units: weeks)

edx %>%
  group_by(date_week) %>%
  summarize(RMV = mean(rating)) %>%
  ggplot(aes(date_week, RMV)) +
  geom_point() +
  geom_smooth()
### Note: little effect

### Visualizing RMV for year movie release
edx %>%
  group_by(movie_year) %>%
  summarize(RMV = mean(rating)) %>%
  ggplot(aes(movie_year, RMV)) +
  geom_point() +
  geom_smooth()
### Note: Wide variations. 
### This RMV variation due to year of release 
### will be used for building the ML model


#######################################################################
#################### MovieLens Project ################################
#################### MACHINE LEARNING #################################
#######################################################################


### The final algorithm will be "tested" on a validation set 
### (the final hold-out test set). For this reason the edx data set 
### will be split in other subsets: train_edx_set, test_edx_set.

### The train_edx_set will be split again to avoid overtraining during 
### the Regularization Tuning phase. 

### To compare the various models with each other a Loss Function 
### based on the Root Mean Square Error (RMSE) is adopted: 

RMSE <- function(true_ratings, predicted_ratings){
  sqrt(mean((true_ratings - predicted_ratings)^2))
}

##########################################################
### SPLITTING THE EDX DATA SET INTO TRAIN AND TEST SETS ##
##########################################################
options(digits=10)

set.seed(1, sample.kind="Rounding")
test_index <- createDataPartition( y = edx$rating, 
                                   times = 1, 
                                   p = 0.1,   #### 10% for the test data set
                                   list = FALSE )
train_edx_set <- edx[-test_index,]
temp <- edx[test_index,]

test_edx_set <- temp %>%
  semi_join(train_edx_set, by = "movieId") %>%
  semi_join(train_edx_set, by = "userId")

# Add rows removed from validation set back into train_edx_set set
removed <- anti_join(temp, test_edx_set)
train_edx_set <- rbind(train_edx_set, removed)  #### train_edx_set contains again all the rows

### Does these data sets contain NA values? Answer: NO
sum(is.na(train_edx_set))  ## 0
sum(is.na(test_edx_set))   ## 0

### Does these data sets contain repeated rows?
### No
sum(duplicated(train_edx_set))  ### 0
sum(duplicated(test_edx_set))   ### 0

##################################################
#### FIRST MODEL: only the average value       ###
##################################################

### Given the total rating distribution, it is assumed that the true rating 
##  is the simple average of the rating values across all the movies and users.

### Mean value
mu <- mean(train_edx_set$rating)  

### Storing and displaying the RMSE of this model
rmse_mean <- RMSE(test_edx_set$rating, mu)
rmse_mean   #### 1.060054

#### DON'T DO THIS !!! 
### fit_lm <- lm(rating ~ as.factor(movieId), data = train_edx_set) 
### a vector of 571.6Gb not allocable

#######################################################################
######################### AVOIDING OVERTRAINING #######################
#######################################################################

#### In order to avoid over-training and performing full cross-validation
#### during the regularization phase, the train_edx_set has been split
#### again in other two sub-sets.

set.seed(2, sample.kind="Rounding")

test_index <- createDataPartition( y = train_edx_set$rating, 
                                   times = 1, 
                                   p = 0.1,   #### 10% for the test data set
                                   list = FALSE )

train_edx_set_for_tuning <- train_edx_set[-test_index,]
temp <- train_edx_set[test_index,]

test_edx_set_for_tuning <- temp %>%
  semi_join(train_edx_set_for_tuning, by = "movieId") %>%
  semi_join(train_edx_set_for_tuning, by = "userId")


# Add rows removed from validation set back into train_edx_set_for_tuning, set
removed <- anti_join(temp, test_edx_set_for_tuning)
train_edx_set_for_tuning <- rbind(train_edx_set_for_tuning, removed)  

##############################################
#### INCLUDING MOVIE EFFECT ##################
##############################################

#### Modeling: Y_{u,i} = mu + b_{i] + epsilon_{u,i}
#### Y_{u,i}: rating value for user u and movie i
#### b_{i}: deviations from mu for movie i
#### epsilon_{u,i}: errors

#### Visualizing an example of b_{i} distribution
movie_avg <- train_edx_set %>%
  group_by(movieId)%>%
  summarize(b_i = mean(rating - mu))

movie_avg %>% qplot(b_i, geom ="histogram", 
                    bins = 20, data = ., 
                    color = I("black"))


###############################
##### MOVIE  ##################
#### Without Regularization ###

## "pr" means predicted ratings
pr_movie <- mu + test_edx_set %>%
  left_join(movie_avg, by ='movieId') %>%
  pull(b_i)   ### these values actually are b_i + mu, which are the Yu,i

# ###Check with this code chunk
# pr_movie1 <- test_edx_set %>%
#   left_join(movie_bias, by ='movieId') %>%
#   mutate(b_i_plus_mu = mu + b_i)

#### Storing RMSE for movie effect
RMSE_movie <- RMSE(test_edx_set$rating, pr_movie)  #### result: 0.942961498
RMSE_movie


##############################################
#### MOVIE EFFECT - REGULARIZATION ###########
##############################################

#### Looking for the best lambda value for the following equation to minimize:
#### sum_{u,i} (( y_{u,i} - mu - b_{i} - b_{u})^2 + lambda*(sum_{i} b_{i}^2+ sum_{u} b_{u}^2)

### Selecting lambda values to test for tuning phase
lambdas <- seq(0, 10, 0.1)

### Building a function for testing selected lambdas

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_edx_set$rating)
  
  b_i <- train_edx_set_for_tuning %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  
  predicted_ratings <-
    test_edx_set_for_tuning %>%
    left_join(b_i, by = "movieId") %>%
    mutate(pred = mu + b_i) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings, test_edx_set_for_tuning$rating))
})

### Visualizing the Tuning process

qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)]  
lambda #### Best lambda 2.3

#### Using the best lambda to train and predict }

movie_reg_avgs <- train_edx_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda), n_i = n())

predicted_ratings_reg <- test_edx_set %>%
  left_join(movie_reg_avgs, by ='movieId') %>%
  mutate(pred = mu + b_i) %>%
  pull(pred)  

mean(is.na(predicted_ratings_reg))  ### no NA values

RMSE_reg_mov <- RMSE(predicted_ratings_reg, test_edx_set$rating)
RMSE_reg_mov  #### 0.9429391752

#######################################
##### MOVIE + USER EFFECT #############
#### Without Regularization ###########

user_avgs <- train_edx_set %>%
  left_join(movie_avg, by='movieId') %>%
  group_by(userId) %>%
  summarize(b_u = mean(rating - mu - b_i))

predict_user_eff <- test_edx_set %>%
  left_join(movie_avg, by='movieId') %>%
  left_join(user_avgs, by='userId') %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

mean(is.na(predict_user_eff))  ### no NA values

RMSE_user <- RMSE(predict_user_eff, test_edx_set$rating)
RMSE_user ### 0.8646842949

##################################################
#### REGULARIZATION MOVIE + USER EFFECT ##########
##################################################

lambdas <- seq(0, 10, 0.1)

rmses <- sapply(lambdas, function(l){
  
  mu <- mean(train_edx_set$rating)
  
  b_i <- train_edx_set_for_tuning%>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_edx_set_for_tuning%>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  predicted_ratings_movieuser_reg <-
    test_edx_set_for_tuning %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    mutate(pred = mu + b_i + b_u) %>%
    pull(pred)

  return(RMSE(predicted_ratings_movieuser_reg, test_edx_set_for_tuning$rating))
})

qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)]  
lambda  ### best lambda: 5

#### Using the best lambda to train and predict 
b_i <- train_edx_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))
b_u <- train_edx_set %>%
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))

predicted_ratings_movieuser_reg <-
  test_edx_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  mutate(pred = mu + b_i + b_u) %>%
  pull(pred)

sum(is.na(predicted_ratings_movieuser_reg))

RMSE_reg_mov_user <- RMSE(predicted_ratings_movieuser_reg, test_edx_set$rating)

RMSE_reg_mov_user  ### 0.8641361793


#########################################################
##### REGULARIZED MOVIE + User + TIME Effects ###########
#########################################################

### Year movie release has been taken into account to perform this step
lambdas <- seq(0, 10, 0.1)

rmses <- sapply(lambdas, function(l){
  mu <- mean(train_edx_set$rating)
  b_i <- train_edx_set %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_edx_set_for_tuning %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  b_t <- train_edx_set_for_tuning %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(movie_year)%>%
    summarize(b_t = sum(rating - b_i - b_u - mu)/(n()+l))
  
  predicted_ratings_movieusertime_reg <-
    test_edx_set_for_tuning %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_t, by = "movie_year") %>%
    mutate(pred = mu + b_i + b_u + b_t) %>%
    pull(pred)
  
  return(RMSE(predicted_ratings_movieusertime_reg, test_edx_set_for_tuning$rating))
})

qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)]  
lambda ### best lambda: 4.4

b_i <- train_edx_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))
b_u <- train_edx_set %>%
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))
b_t <- train_edx_set %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(movie_year)%>%
  summarize(b_t = sum(rating - b_i - b_u - mu)/(n()+lambda))

predicted_ratings_movieusertime_reg <-
  test_edx_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_t, by = "movie_year") %>%
  mutate(pred = mu + b_i + b_u + b_t) %>%
  pull(pred)

RMSE_reg_m_u_t <- RMSE(predicted_ratings_movieusertime_reg, test_edx_set$rating)
RMSE_reg_m_u_t  #### 0.8638384784

#################################################################
##### REGULARIZED MOVIE + User + TIME + Genre Effects ###########
#################################################################

lambdas <- seq(0, 10, 0.1)
rmses <- sapply(lambdas, function(l){
  mu <- mean(train_edx_set$rating)
  b_i <- train_edx_set_for_tuning %>%
    group_by(movieId) %>%
    summarize(b_i = sum(rating - mu)/(n()+l))
  b_u <- train_edx_set_for_tuning %>%
    left_join(b_i, by="movieId") %>%
    group_by(userId) %>%
    summarize(b_u = sum(rating - b_i - mu)/(n()+l))
  b_t <- train_edx_set_for_tuning %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    group_by(movie_year)%>%
    summarize(b_t = sum(rating - b_i - b_u - mu)/(n()+l))
  b_g <- train_edx_set_for_tuning %>%
    left_join(b_i, by="movieId") %>%
    left_join(b_u, by="userId") %>%
    left_join(b_t, by="movie_year") %>%
    group_by(genres)%>%
    summarize(b_g = sum(rating - b_i - b_u - b_t - mu)/(n()+l))
  predicted_ratings_movieusertimegenre_reg <-
    test_edx_set_for_tuning %>%
    left_join(b_i, by = "movieId") %>%
    left_join(b_u, by = "userId") %>%
    left_join(b_t, by = "movie_year") %>%
    left_join(b_g, by = "genres") %>%
    mutate(pred = mu + b_i + b_u + b_t + b_g) %>%
    pull(pred)
  return(RMSE(predicted_ratings_movieusertimegenre_reg, test_edx_set_for_tuning$rating))
})

qplot(lambdas, rmses)
lambda <- lambdas[which.min(rmses)]  
lambda  #### best lambda: 

b_i <- train_edx_set %>%
  group_by(movieId) %>%
  summarize(b_i = sum(rating - mu)/(n()+lambda))
b_u <- train_edx_set %>%
  left_join(b_i, by="movieId") %>%
  group_by(userId) %>%
  summarize(b_u = sum(rating - b_i - mu)/(n()+lambda))
b_t <- train_edx_set %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  group_by(movie_year)%>%
  summarize(b_t = sum(rating - b_i - b_u - mu)/(n()+lambda))
b_g <- train_edx_set %>%
  left_join(b_i, by="movieId") %>%
  left_join(b_u, by="userId") %>%
  left_join(b_t, by="movie_year") %>%
  group_by(genres)%>%
  summarize(b_g = sum(rating - b_i - b_u - b_t - mu)/(n()+lambda))

predicted_ratings_movieusertimegenre_reg <-
  test_edx_set %>%
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_t, by = "movie_year") %>%
  left_join(b_g, by = "genres") %>%
  mutate(pred = mu + b_i + b_u + b_t + b_g) %>%
  pull(pred)

RMSE_i_u_t_g <- RMSE(predicted_ratings_movieusertimegenre_reg, test_edx_set$rating)
RMSE_i_u_t_g  ### 0.8636102103

###################################################################
#################          VALIDATION          ####################
################# FINAL DSexp MODELS (M+U+T+G) ####################
###################################################################

predicted_ratings_movieusertimegenre_reg_validation <-
  validation %>%  #### <<<<<
  left_join(b_i, by = "movieId") %>%
  left_join(b_u, by = "userId") %>%
  left_join(b_t, by = "movie_year") %>%
  left_join(b_g, by = "genres") %>%
  mutate(pred = mu + b_i + b_u + b_t + b_g) %>%
  pull(pred)

RMSE_validation <- RMSE(predicted_ratings_movieusertimegenre_reg_validation, validation$rating)
RMSE_validation  #### 0.8646581736

###########################################################
############### MATRIX FACTORIZATION ######################
###########################################################

# Rating matrix $R$ for the _edx_ data set
# is characterized by 10677(titles)*69878(users) = 7.46087406e+8 entries. 
# In the "movie space" defined by the different titles each users 
# has his/her own preferences according the movie's features (genres,actors etc.)
# that define a specific user pattern. What is useful here is to find the users 
# that show similar patterns or groups of movies having similar rating patterns. 
# To solve this the $R$ (M x N dimensioned) matrix can  be _factorized_ in 
# two smaller rectangular matrix $U$ ( M x _s_ dimensioned) and $I$
# ( _s_ x N dimensioned), the _factors_, that contain 
# information about the aforementioned "similarities". The letter _s_ is what is 
# called _number of latent factors_ (or features), that for the movies, 
# for example, could be genre, actor etc. 

## Strictly speaking here will be used a Matrix Factorization method with 
## Stochastic Gradient Descent (SGD) to optimize the learned parameters during 
## the tuning phase. 

### Loading library recosystem
library(recosystem)

#### Instructions from the package website have been followed:
#### https://github.com/yixuan/recosystem

##################################################################
############## WORKING ON VALIDATION DATA SET ####################
##################################################################

##########  DATA FORMAT  #########################################

## From: https://github.com/yixuan/recosystem:

## The data file for training set needs to be 
## arranged in sparse matrix triplet form, i.e. 
## each line in the file contains three numbers: user_index, item_index, rating

### The same train data set used for the final model will be used:

edxf <- train_edx_set %>% select(userId, movieId, rating)

## Testing data file is similar to training data, 
## but since the ratings in testing data are usually UNKWON, 
## the rating entry in testing data file can be omitted, 
## or can be replaced by any placeholder such as 0 or ?.

### Selecting only userId and movie Id from validation
### To compare the performance with the final model previously selected
validf <- validation %>%  select(userId, movieId)

### Ratings values from validation
ratings_val <- validation %>%  select(userId,moviedId,rating)

## Note: From version 0.4 recosystem supports two special 
## types of matrix factorization: the binary matrix factorization (BMF), 
## and the one-class matrix factorization (OCMF). BMF requires ratings to take
## value from {-1, 1}, and OCMF requires all the ratings to be positive.

## The rating provided here are all positive. 

edxf   <- as.matrix(edxf)
validf <- as.matrix(validf)
write.table(edxf, file = "train.txt", sep = " ", row.names = FALSE, 
            col.names = FALSE)
write.table(validf, file = "validationset.txt", sep = " ", 
            row.names = FALSE, col.names = FALSE)

set.seed(1)

# From the recosystem manual:
# "The following function data_file() is used to specify the source of data in 
# the recommender system. It is intended to provide the input argument of 
# functions such as $tune(), $train(), and $predict()".

training_dataset <- data_file("train.txt")
validation_dataset <- data_file("validationset.txt")

## From recosystem manual: "Reco() returns an object of class "RecoSys"
## equipped with methods $train(), $tune(), $output() and $predict(), 
## which describe the typical process of building and tuning model, 
## exporting factorization matrices, and predicting results."

r=Reco()

## r$tune ### From recosystem manual: This method is a member 
## function of class "RecoSys" that uses cross validation 
## to tune the model parameters

opts = r$tune(training_dataset, 
              opts = list(dim = c(10, 20, 30), ### dim, number of latent factors
              lrate = c(0.1,0.2), ##the learning rate, which can be thought of as 
                                  ## the step size in gradient descent.
              costp_l1 = 0, ## L1 regularization cost for user factors
              costq_l1 = 0, ## L1 regularization cost for item factors
              nthread = 1,  ## number of threads for parallel computing
              niter = 10))  ## number of iterations

### L2 regularization cost left as 0.1

stored_prediction = tempfile()

### From the manual:  Function train() trains a recommender model. It will read
### from a training data source and create a model file at the specified location. 
### The model file contains necessary information for prediction.

r$train(training_dataset, 
        opts = c(opts$min, 
                 nthread = 1, 
                 niter = 20))

### From the manual: "Predicts unknown entries in the rating matrix.
### Prior to calling this method, model needs to be trained using member 
### function $train()

r$predict(validation_dataset, out_file(stored_prediction))
print(r)
### Storing the values from validation set
validation_ratings <- read.table("validationset.txt", header = FALSE, sep = " ")$V3

### Scanning the temporary file containing the predictions
predicted_ratings <- scan(stored_prediction)
predicted_ratings

### Calculating the RMSE using the ratings from validation set
rmse_of_model_mf <- RMSE(validation$rating, predicted_ratings)
rmse_of_model_mf

#####################################################################
#################### RMSEs - Summary #################################
#####################################################################
options(pillar.sigfig = 8)

####################################
### RMSE RESULTS on TEST DATA SET 
####################################

RMSE_table <- tibble(method ='Algorithm - Average', RMSE = 1.060054) %>%
              add_row(method='Algorithm - Movie effect', RMSE = 0.942961498)%>%
              add_row(method='Regularized: Movie', RMSE = 0.9429391752)%>%
              add_row(method='Movie + User effects', RMSE = 0.8646842949) %>% ### < 0.86490 !!
              add_row(method='Regularized: Movie + User', RMSE = 0.8641361793)%>% ### < 0.86490 !!
              add_row(method='Regularized: Movie + User + Time', RMSE = 0.8638384784)%>% ### < 0.86490 !!
              add_row(method='Regularized: Movie + User + Time + Genre', RMSE = 0.8636102103) ### < 0.86490 !!
RMSE_table

########################################
### RMSE RESULTS on VALIDATION DATA SET 
########################################

RMSE_table_validation <- tibble(method = "Regularized Movie + User + Time + Genre", RMSE = 0.8646581736) %>% ### < 0.86490 !!
                         add_row(method='Matrix Factorization"', RMSE = 0.7861181454)

RMSE_table_validation

