library(dplyr)
library(ggplot2)

train = read.csv("trainingData.csv",header = T, row.names=NULL, sep = ",")
train=replace(train,is.na(train),100)
test = read.csv("validationData.csv",header = T, row.names=NULL, sep = ",")
test=replace(test,is.na(test),100)

NUM_WAPS <- 520

train = train %>% filter(BUILDINGID=="1", FLOOR=='1')
xtrain <- train[,1:NUM_WAPS]
ytrain <- train[,c("BUILDINGID","FLOOR","LONGITUDE","LATITUDE")]

test = test %>% filter(BUILDINGID=="1", FLOOR=='1')
xtest <- test[,1:NUM_WAPS]
ytest <- test[,c("BUILDINGID","FLOOR","LONGITUDE","LATITUDE")]

oneNN <- matrix(0,nrow=nrow(ytest),ncol=1)
threeNN <- matrix(0,nrow=nrow(ytest),ncol=1)
fiveNN <- matrix(0,nrow=nrow(ytest),ncol=1)
colnames(oneNN) <- c("DEVIATION")
colnames(threeNN) <- c("DEVIATION")
colnames(fiveNN) <- c("DEVIATION")
#Original data is in db. Close to zero means strong signal, and 100 means no signal perceived.
#We change the data to be in the range 0,100. 0 means no signal, 100 very strong signal
minimun_in_database <- min(xtrain)
#2:col
xtrain <- apply(xtrain, 2, function(x) ifelse(x == 100, minimun_in_database, x)) + abs(minimun_in_database)
xtest  <- apply(xtest,  2, function(x) ifelse(x == 100, minimun_in_database, x)) + abs(minimun_in_database)

N_TRAIN <- nrow(xtrain)
N_TEST  <- nrow(xtest)

vec <- 0.0
#for each test sample, look for the most similar in the training set
for (i in 1:N_TEST)
{
  most_similar  <- 1
  best_distance <- sqrt( sum( (xtest[i,]-xtrain[1,])^2 ) ) # big number
  vec <- best_distance
  for (j in 2:N_TRAIN)
  {
    # euclidean distance
    d <- sqrt( sum( (xtest[i,]-xtrain[j,])^2 ) )
    if (d <= best_distance)
    {
      best_distance <- d
      most_similar  <- j
    }
    
    #distance vector
    vec <- append(vec, d)
  }
  index_five <- order(vec)[1:5]
  # 1NN
  deviation_one <- sqrt((ytest[i,3]-ytrain[most_similar,3])^2+(ytest[i,4]-ytrain[most_similar,4])^2)
  oneNN[i, "DEVIATION"] <- deviation_one
  # 3NN
  longitude_three <- (ytrain[index_five[1:3],"LONGITUDE"])
  latitude_three <- (ytrain[index_five[1:3],"LATITUDE"])
  longitude_three <- mean(longitude_three)
  latitude_three <- mean(latitude_three)
  deviation_three <- sqrt((ytest[i,3]-longitude_three)^2+(ytest[i,4]-latitude_three)^2)
  threeNN[i, "DEVIATION"] <- deviation_three
  # 5NN
  longitude_five <- (ytrain[index_five,"LONGITUDE"])
  latitude_five <- (ytrain[index_five,"LATITUDE"])
  longitude_five <- mean(longitude_five)
  latitude_five <- mean(latitude_five)
  deviation_five <- sqrt((ytest[i,3]-longitude_five)^2+(ytest[i,4]-latitude_five)^2)
  fiveNN[i, "DEVIATION"] <- deviation_five
}
# Give the chart file a name.
png(file = "1NN.png")
# Create the histogram.
hist(oneNN, breaks =20,freq = T, main="1NN",xlab = "DEVIATION", ylab="frequency",col = "yellow",border = "blue")
# Save the file.
dev.off()
png(file = "3NN.png")
hist(threeNN,  main="3NN",xlab = "DEVIATION", ylab="frequency",col = "yellow",border = "blue")
dev.off()
png(file = "5NN.png")
hist(fiveNN, breaks =20, freq = T, main="5NN",xlab = "DEVIATION", ylab="frequency",col = "yellow",border = "blue")
dev.off()

deviation_one_means <- colMeans(oneNN)
deviation_one_means
deviation_three_means <- colMeans(threeNN)
deviation_three_means
#threeNN[N_TEST+1, "DEVIATION"] <- deviation_three_means
deviation_five_means <- colMeans(fiveNN)
deviation_five_means

#fiveNN[N_TEST+1, "DEVIATION"] <- deviation_five_means
write.table(oneNN,file="1NN.csv", col.names = T,row.names = F,sep = ",")
write.table(threeNN,file="3NN.csv", col.names = T,row.names = F,sep = ",")
write.table(fiveNN,file="5NN.csv", col.names = T,row.names = F,sep = ",")

#錯誤率
(length(oneNN)-length(which(oneNN==0)))/length(oneNN)
(length(threeNN)-length(which(threeNN==0)))/length(threeNN)
(length(fiveNN)-length(which(fiveNN==0)))/length(fiveNN)

#***************plot.csv***************

train = read.csv("trainingData.csv",header = T, row.names=NULL, sep = ",")
train=replace(train,is.na(train),100)
plot_test = read.csv("plot.csv",header = T, row.names=NULL, sep = ",")
plot_test=replace(plot_value,is.na(plot_value),100)

NUM_WAPS <- 520

train = train %>% filter(BUILDINGID=="1", FLOOR=='1')
xtrain <- train[,1:NUM_WAPS]
ytrain <- train[,c("BUILDINGID","FLOOR","LONGITUDE","LATITUDE")]

plot_est <- matrix(0,nrow=3,ncol=2)
colnames(plot_est) <- c("LONGITUDE","LATITUDE")

minimun_in_database <- min(xtrain)
#2:col
xtrain <- apply(xtrain, 2, function(x) ifelse(x == 100, minimun_in_database, x)) + abs(minimun_in_database)
xtest  <- apply(plot_test,  2, function(x) ifelse(x == 100, minimun_in_database, x)) + abs(minimun_in_database)

N_TRAIN <- nrow(xtrain)
N_TEST  <- nrow(xtest)

for (i in 1:N_TEST)
{
  most_similar  <- 1
  best_distance <- sqrt( sum( (xtest[i,]-xtrain[1,])^2 ) ) # big number
  for (j in 2:N_TRAIN)
  {
    # euclidean distance
    d <- sqrt( sum( (xtest[i,]-xtrain[j,])^2 ) )
    if (d <= best_distance)
    {
      best_distance <- d
      most_similar  <- j
    }
  }
  plot_est[i,"LONGITUDE"]  <- ytrain[most_similar,"LONGITUDE"]
  plot_est[i,"LATITUDE"]   <- ytrain[most_similar,"LATITUDE"]
  
  print(i)
}

