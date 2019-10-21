### This section of code is in order to justify the feature importance 
### statement in the section "Real data".
day <- read_csv("datasets/day.csv")
day$holiday <- as.factor(day$holiday)
day$workingday <- as.factor(day$workingday)
day <- day[, c(3, 6, 8:10, 12:13, 16)]
day$season <- as.factor(day$season)
day$weathersit <- as.factor(day$weathersit)
day <- as.data.frame(day)
data_set <- make_split(day, 0.85)

rf <- randomForest(cnt ~., data = data_set$train)
fi <- importance(rf)

saveRDS(fi, file = "R-results/LIME/Neighbourhood/real_data_fi.RDS")
