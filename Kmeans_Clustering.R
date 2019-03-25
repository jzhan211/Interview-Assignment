library(mclust)
library(ggplot2)

# Generate Data (last column is the label of clusters)
generate.data <- function(n_rows,n_cols,k,x_mus,x_sds,y_mus,y_sds,prop1){
  comp1 <- sample(seq_len(k), prob=prop1, size=n_cols, replace=TRUE)
  samples1 <- cbind(rnorm(n=n_cols, mean=x_mus[comp1],sd=x_sds[comp1]),
                  rnorm(n=n_cols, mean=y_mus[comp1],sd=y_sds[comp1]))
  proj <- matrix(rnorm(n_rows* n_cols), nrow=n_rows, ncol=2)
  A1 <- samples1 %*% t(proj)  # (100*2)*(2*1000)
  A1 <- A1 + rnorm(n_rows*n_cols)
  A <- cbind(A1,comp1) # last column is the label of clusters.
  return(A)
}

# Perform K-means, and Calculate ARI and within clusters sum of squares (withinss).
get.cluster <- function(A,k){
  n_cols <- dim(A)[2]-1       #1000
  A1 <- A[,-(n_cols+1)]
  comp1 <- A[,(n_cols+1)]
  kmeans.result <- kmeans(A1 , k)
  ARI=adjustedRandIndex(kmeans.result$cluster, comp1)
  withinss=kmeans.result$withinss
  output=list()
  output$ARI <- ARI
  output$withinss <- sum(withinss)
  return(output)
}

# Simulation
n_rows = 1000
n_cols = 100
k=3
x_mus = c(0,5,5)
x_sds = c(1,0.1,1)
y_mus = c(5,5,0)
y_sds = c(1,0.1,1)
prop1 = c(0.3,0.5,0.2)

k.vec = NULL
for (i in 1:100){
  A = generate.data(n_rows,n_cols,k,x_mus,x_sds,y_mus,y_sds,prop1) 
  aa=NULL
  bb=NULL
  for (j in 1:10){
    model <- get.cluster(A,j)
    aa[j] <- model$ARI
    bb[j] <- model$withinss
  }
  index=which(aa==max(aa))
  k.vec[i]=index[which.min(bb[index])]
  
  #k.vec[i]=which.max(aa)
}

ggplot(data.frame(k.vec)) +
  geom_histogram(aes(x = data.frame(k.vec)$k.vec)) +
  labs(x = 'k') +
  theme_light()
