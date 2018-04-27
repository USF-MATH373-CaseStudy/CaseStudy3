library(factoextra)

####### Below is for hierarchical clustering ########
df <- cluster::agriculture


euclidean_hc(df)
manhattan_hc(df)
correlation_hc(df)
t(df)
cor(df)
nrow(dist.cor)
dist.cor <- get_dist(df, method = "pearson")
dist.cor_2 <- round(as.matrix(dist.cor)[1:12,1:12],4)
cor(dist.cor_2)

euclidean_hc <- function(df) {
  row <- nrow(df)
  x <- data.frame(matrix(ncol = row, nrow = row))
  for (i in 1:row) {
    for (j in 1:row) {
      x[i, j] <- sqrt(sum((df[i, ] - df[j, ]) ^ 2))
    }
  }
  return (x)
}

manhattan_hc <- function(df) {
  row <- nrow(df)
  x <- data.frame(matrix(ncol = row, nrow = row))
  for (i in 1:row) {
    for (j in 1:row) {
      x[i, j] <- sqrt(sum(abs(df[i, ] - df[j, ])))
    }
  }
  return (x)
}

correlation_hc <- function(df) {
  row <- nrow(df)
  x <- data.frame(matrix(ncol = row, nrow = row))
  for (i in 1:row) {
    for (j in 1:row) {
      x[i, j] <- 1 - cor(t(df[i, ]), t(df[j, ]))
    }
  }
  return (x)
}

####### Above is for hierarchical clustering ########
####### Below is for kmean ##########

df <- cluster::agriculture
threshold = 0.1

euclidean <- function(df,mean_pt,link) {
  df_row <- nrow(df)
  df_means <- nrow(mean_pt)
  clusters <- c()
  for (i in 1:df_row) {
    dis_vec = c()
    for (j in 1:df_means) {
      di <- sqrt(sum((df[i, ] - df[j, ]) ^ 2))
      dis_vec <- c(dis_vec, di)
    }
    if(link == "single")
      dis <- min(dis_vec)
    if(link == "average")
      dis <- median(dis_vec) ## Not work with even number, odd even problem
    if(link == "total")
      dis <- max(dis_vec)
    
    if(link=="average" & df_row%%2==1){
      ## Figure out later
    }
    else{
      cl <- match(dis, dis_vec)
    }
    clusters <- c(clusters, cl)
  }
  print(clusters)
  return (clusters)
}

manhattan <- function(df,mean_pt,link) {
  df_row <- nrow(df)
  df_means <- nrow(mean_pt)
  clusters <- c()
  for (i in 1:df_row) {
    dis_vec = c()
    for (j in 1:df_means) {
      di <- sum(abs(df[i, ] - df[j, ]))
      dis_vec <- c(dis_vec, di)
    }
    print("Vector choice:")
    print(dis_vec)
    if(link == "single")
      dis <- min(dis_vec)
    else if(link == "average") ## Not work with even number again
      dis <- median(dis_vec)
    else if(link == "total") ## Does not choose 0 at all, not choosing itself
      dis <- max(dis_vec)

    cl <- match(dis, dis_vec)
    clusters <- c(clusters, cl)
  }
  print(clusters)
  return (clusters)
}

correlation <- function(df,mean_pt,link) {
  df_row <- nrow(df)
  df_means <- nrow(mean_pt)
  clusters <- c()
  for (i in 1:df_row) {
    dis_vec = c()
    for (j in 1:df_means) {
      di <- 1 - cor(t(df[i, ]),t(df[j, ]))
      print(di)
    }
    clusters <- c(clusters, di)
  }
  return (clusters)
}




data <- df
## k-means
kmeans <- function(data, k, method, link){
  row_data <- nrow(data)
  random_vec <- sample.int(row_data, row_data, replace = F)
  newData <- data.frame(matrix(ncol = 2))
  names(newData) <- names(data)
  for(i in 1:row_data){
    newData <- rbind(newData,data[random_vec[i], ])
  }
  
  newData <- newData[2:(row_data + 1),]
  
  x <- data[, 1]
  y <- data[, 2]
  
  d <- matrix(data = NA, ncol = 0, nrow = 0)
  
  for (i in 1:k)
    d <- c(d, c(x[i],y[i]))
  init <- matrix(d, ncol = 2, byrow = T)
  oldMeans <- init
  ##print(paste("Old Means:",oldMeans))

  if (method == "euclidean")
    cl <- euclidean(data, oldMeans, link)
  else if (method == "manhattan")
    cl <- manhattan(data, oldMeans, link)
  else if (method == "correlation")
    cl <- correlation(data, oldMeans, link)
  print(cl)
  newMeans <- UpdateMeans(data, cl, k)
  print(newMeans)
  thr <- delta(oldMeans,newMeans,method)
  print(thr)
  itr <- 1
  while (thr > threshold)
  {
    if (method == "euclidean")
      cl <- euclidean(data, newMeans,link)
    else if (method == "manhattan")
      cl <- manhattan(data, oldMeans, link)
    else if (method == "correlation")
      cl <- correlation(data, oldMeans, link)
    
    oldMeans <- newMeans
    newMeans <- UpdateMeans(data, cl, k)
    thr <- delta(oldMeans, newMeans, method)
    itr <- itr+1
  }
  
  return (cl)
}

UpdateMeans <- function(m, cl, k){
  means <- c()
  for(c in 1:k)
  {
    # get the point of cluster c
    group <- which(cl == c)
    
    # compute the mean point of all points in cluster c
    mt1 <- mean(m[group, 1])
    mt2 <- mean(m[group, 2])
    vMean <- c(mt1, mt2)
    means <- c(means, vMean)
  }
  means <- matrix(means, ncol = 2, byrow = TRUE)
  return(means)
}

delta <- function(oldMeans, newMeans, method)
{
  a <- newMeans - oldMeans
  if(method == "euclidean")
    di <- max(sqrt(sum((a[1, ] - a[2, ]) ^ 2)))
  else if (method == "manhattan")
    di <- max(abs((a[1, ] - a[2, ]) ^ 2))
  else if (method == "correlation")
    di <- 1 - cor(t(a[1,]), t(a[2,]))
  return (di)
}


## Test k-means
answer <- kmeans(df, 3, "euclidean", "single")
answer2 <- kmeans(df, 5, "manhattan", "single")
answer3 <- kmeans(df, 3, "correlation", "single") ## Not working

answer4 <- kmeans(df, 7, "euclidean", "average")

table(answer)


