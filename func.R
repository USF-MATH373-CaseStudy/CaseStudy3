df <- cluster::agriculture
install.packages("factoextra")
library(factoextra)

euclidean <- function(df) {
  row <- nrow(df)
  x <- data.frame(matrix(ncol = row, nrow = row))
  for (i in 1:row) {
    for (j in 1:row) {
      x[i, j] <- sqrt(sum((df[i, ] - df[j, ]) ^ 2))
    }
  }
  return (x)
}

manhattan <- function(df) {
  row <- nrow(df)
  x <- data.frame(matrix(ncol = row, nrow = row))
  for (i in 1:row) {
    for (j in 1:row) {
      x[i, j] <- sqrt(sum(abs(df[i, ] - df[j, ])))
    }
  }
  return (x)
}

correlation <- function(df) {
  row <- nrow(df)
  x <- data.frame(matrix(ncol = row, nrow = row))
  for (i in 1:row) {
    for (j in 1:row) {
      x[i, j] <- 1 - cor(t(df[i, ]), t(df[j, ]))
    }
  }
  return (x)
}
correlation(df)
t(df)
cor(df)
nrow(dist.cor)
dist.cor <- get_dist(df, method="pearson")
dist.cor_2 <- round(as.matrix(dist.cor)[1:12,1:12],4)
cor(dist.cor_2)
