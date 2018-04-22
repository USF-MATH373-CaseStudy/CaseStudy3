df <- cluster::agriculture

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
