#### Multivariate statistics 
#### Practical - week 9
 

setwd("/Users/martinwutke/Documents/Owncloud/shared_Uni/Multivariate Verfahren/Practical")
 
### Ex.1

# part a) 
# Calculate the distance matrix with the squared Euclidean distance as distance measure

# function for computing the squared Euclidean distance

eucl_dist <- function(x_vec,y_vec) { # the vectors need to have the same length
  result_mat <- matrix(nrow = length(x), ncol = length(x))
 
   ## assign the corresponding elements to a specific point  (rows in a data.frame)
  df <- data.frame(x_vec,y_vec)
  
  ## compute the distence between the elements in df
  for (i in 1:length(df[,1])) {
    for (j in 1:length(df[,1])) {
      result_mat[i,j] <- sum((df[i,] - df[j,])^2)
    }
  }
  return(result_mat)
}

# calculate the distance
x <- c(8,5,10,4,13)
y <- c(24,22,25,21,28)

eucl_dist(x,y)

# the clustering is done by hand

## part b)
##

data <- data.frame(x,y)
cov_mat <- cov(data)

biotools::D2.dist(data, cov=cov_mat, inverted = FALSE)






### Ex 2.

## part a)
ramus <- read.table("ramus.dat", header = F) 

ramus # first column seems to contain just the observation number -> drop first column
ramus <- ramus[,-1]

canberra_dist <- function(x_matrix){
  result_mat <- matrix(nrow = length(x_matrix[,1]), ncol = length(x_matrix[,1]) )
  ## compute the distence between the elements using the Canberra metric
  for (i in 1:length(x_matrix[,1])) {
    for (j in 1:length(x_matrix[,1])) {
      result_mat[i,j] <- sum( (abs(x_matrix[i,] - x_matrix[j,])) / (x_matrix[i,] + x_matrix[j,] )       )
    }
  }
  return(result_mat)
}

dist_mat <- canberra_dist(ramus)




### EX. 3

# load dataset
USA_arrests <- datasets::USArrests


## part a)

# compute the distance matrix

  eucl_Dist2 <- function(x_matrix){
    result_mat <- matrix(nrow = length(x_matrix[,1]), ncol = length(x_matrix[,1]) )
    ## compute the distence between the elements using the Canberra metric
    for (i in 1:length(x_matrix[,1])) {
      for (j in 1:length(x_matrix[,1])) {
        result_mat[i,j] <- sqrt(sum( (x_matrix[i,] - x_matrix[j,])^2 ))
      }
    }
    return(result_mat)
}

dist_mat <- eucl_Dist2(USArrests) # results compared to implemented R-function dist() - same results
# View(dist(USA_arrests))

# change the rownames from 1:20 to statenames
dist_mat <- as.data.frame(dist_mat)
rownames(dist_mat) <- rownames(USA_arrests)

## do the clustering by states
# define the distance matrix as.dist
dist_mat <-as.dist(dist_mat)

# use R-function hclust for clustering by states

cluster1 <- hclust(dist_mat, method = "complete")


## part b) create a dendrogram
plot(cluster1)


## part c) cut the dendrogram -> resulting in 3 distinct clusters
abline(h=130, col="red")

# counting the number of states:
# by eye:
# left cluster    -> contains 16 states
# middle cluster  -> contains 14 states
# right cluster   -> contains 20 states

# by analysing the corresponding hclust-output
# we see that the left cluster beginns with Florida and goes to Nevada

cluster1$labels # Florida has the entry 9 - Nevada has the entry 28
cluster1$order
which(cluster1$order == 28) # Nevada is the 16th entry in the ordering vector
# from this it follows that the left cluster has contains 16 states

# middle cluster: from Missoury(= 25) to New Jersey(=30) contains 14 states
which(cluster1$labels=="Missouri")
which(cluster1$labels=="New Jersey")
length(cluster1$order[c(which(cluster1$order==25): which(cluster1$order==30))])

# right cluster: from Ohio(=35) to Vermont(=45) contains 20 states
which(cluster1$labels=="Ohio")
which(cluster1$labels=="Vermont")
length(cluster1$order[c(which(cluster1$order==35): which(cluster1$order==45))])




## part d) scaling the variable to standard deviation 1 
# store scaled variables in data.frame ramus_stan
USArrests_stan <- data.frame(matrix(nrow = length(USArrests[,1]), ncol = length(USArrests[1,])))

# standardize the original varaibles using a for-loop
for (i in 1:length(USArrests[1,])) {
  USArrests_stan[,i] <- (USArrests[,i] - mean(USArrests[,i])) / sqrt(var(USArrests[,i]))
}

USArrests_stan

# control for standard deviation 1
var(USArrests_stan[,1])
var(USArrests_stan[,2])
var(USArrests_stan[,3])
var(USArrests_stan[,4])

## cluster the standarized variables

# compute the distance matrix
dist_USArrests_stan <- eucl_Dist2(USArrests_stan) # results compared to implemented R-function dist() - same results

dist(USArrests_stan)

# change the rownames from 1:20 to statenames
dist_USArrests_stan <- as.data.frame(dist_USArrests_stan)
rownames(dist_USArrests_stan) <- rownames(USA_arrests)

## do the clustering by states
# define the distance matrix as.dist
dist_USArrests_stan <-as.dist(dist_USArrests_stan)

# use R-function hclust for clustering by states

cluster2 <- hclust(dist_USArrests_stan, method = "complete")
plot(cluster2)



## part e)

# noch offen -> genauer nachlesen!!!