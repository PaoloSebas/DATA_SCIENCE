### 
###
### title: "Movielens_Project"
#### author: "Paolo Sebastianelli"
#### cdate: "17/04/2021"
### output: to determine



## Project Overview: MovieLens

# The purpouse of this project is to create a movie recommendation system (hereafter MVS) using the MovieLens dataset (hereafter MLD). Since the entire MLD contains milions of ratings, a subset will be used to show the results obtained by the MVS application. 

# Generating the Data Set 

# As first step the Data Set will be obtained by using this code:

##########################################################
# Create edx set, validation set (final hold-out test set)
##########################################################

# Note: this process could take a couple of minutes

### CHECKING IF THE PACKAGES ARE INSTALLED #############

if(!require(tidyverse)) install.packages("tidyverse", repos = "http://cran.us.r-project.org")
if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org")
if(!require(data.table)) install.packages("data.table", repos = "http://cran.us.r-project.org")

##### LOADING THE LIBRARIES ###############

library(tidyverse)
library(caret)
library(data.table)

# MovieLens 10M dataset:
# https://grouplens.org/datasets/movielens/10m/
# http://files.grouplens.org/datasets/movielens/ml-10m.zip

##### OBJECTS DESCRIPTION ### 

### dl is vector of character strings generate with the tempfile() function
### readings is a "data.table" "data.frame" with 10000054 obs of 4 variables: userId, movieId, rating, timestamp
### movies is a matrix containing 10681 rows (number of movies) and 3 columns (movieId, title, genres)
### movielens is a "data.table" "data.frame" with 10000054 obs of 6 variables obtaining joining (left) ratings and movies (by movieId) : userId, movieId, rating, timestamp, title, genres
### test_index is a matrix (in this case a vector) containing the indexes resulting from a DataPartition on movielens$ratings

### edx and temp are two dataframes created using test_index. In the first case all the observations corresponding to test_index vector are subtracted from movielens dataset, so edx is composed by 9 milions of observations. For what concerns temp (10%, or 1 milion), all the data concerning test_set vector are inserted in the dataframe. 

### validation is the dataframe will be used for validating the algorithm 


############END OBJECTS DESCRIPTION ####################333



dl <- tempfile()
download.file("http://files.grouplens.org/datasets/movielens/ml-10m.zip", dl)

ratings <- fread(text = gsub("::", "\t", readLines(unzip(dl, "ml-10M100K/ratings.dat"))),
                 col.names = c("userId", "movieId", "rating", "timestamp"))

movies <- str_split_fixed(readLines(unzip(dl, "ml-10M100K/movies.dat")), "\\::", 3)
colnames(movies) <- c("movieId", "title", "genres")

# if using R 3.6 or earlier:
movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(levels(movieId))[movieId],
                                           title = as.character(title),
                                           genres = as.character(genres))
# # if using R 4.0 or later:
# movies <- as.data.frame(movies) %>% mutate(movieId = as.numeric(movieId),
#                                             title = as.character(title),
#                                             genres = as.character(genres))

movielens <- left_join(ratings, movies, by = "movieId")

# Validation set will be 10% of MovieLens data

set.seed(1, sample.kind="Rounding") # if using R 3.5 or earlier, use `set.seed(1)`

test_index <- createDataPartition(y = movielens$rating, times = 1, p = 0.1, list = FALSE)

edx <- movielens[-test_index,]
temp <- movielens[test_index,]

# Make sure userId and movieId in validation set are also in edx set
validation <- temp %>% 
  semi_join(edx, by = "movieId") %>%
  semi_join(edx, by = "userId")

# Add rows removed from validation set back into edx set
removed <- anti_join(temp, validation)
edx <- rbind(edx, removed)  #### edx contains again all the rows 

rm(dl, ratings, movies, test_index, temp, movielens, removed)  ### removing objects 