ratings <- read.csv("D:\\ratings.csv")

head(ratings)

library(reshape2)

MovieMatrx <- as.matrix(dcast(ratings, userId~movieId, value.var = "rating", na.rm=FALSE))[, -1]

MovieRating <- as(MovieMatrx, "realRatingMatrix")


