# example 
# data: two Gaussian clouds, projected onto the 2D sphere
# aim: compare the three initialisation methods

source("initialisationFunctions.R")
source("kmeansFunction.R")

generateData <- function(n1, n2, d) {
    x1 <- rnorm(n1, d)
    y1 <- rnorm(n1, 0)
    x2 <- rnorm(n2, 8)
    y2 <- rnorm(n2, 8)
    X <- rbind(cbind(x1, y1), cbind(x2, y2))
    X <- X * sample(c(-1, 1), size=n1+n2, replace=TRUE)
    X / sqrt(rowSums(X^2))
}

testData <- list(
    `gen(40,10,6)`=function() generateData(40, 10, 6),
    `gen(80,20,6)`=function() generateData(80, 20, 6),
    `gen(800,200,6)`=function() generateData(800, 200, 6)
)

testMethod <- function(fn) {
    cat("\n\n*** testing ", deparse(substitute(fn)), "\n\n", sep="")
    times <- numeric(0)
    clusters <- integer(0)
    iter <- numeric(0)
    rmse <- numeric(0)
    for (test.idx in 1:length(testData)) {
        X <- testData[[test.idx]]()
        M <- 100
        t0 <- proc.time()["elapsed"]
        iterTmp <- numeric(0)
        rmseTmp <- numeric(0)
        for (i in 1:M) {
                initial <- fn(X)
                cl <- kmeansClustering(X, K = 2, iter.max=100, 
                                       initial = initial, verbose=FALSE) 
                iterTmp <- c(iterTmp, cl$iter)
                clRMSE <- clusterRMSE(X, cl$clusters)
                rmseTmp <- c(rmseTmp, clRMSE$rmse)
        }
        t1 <- proc.time()["elapsed"]
        times <- c(times, (t1 - t0) / M)
        clusters <- c(clusters, length(unique(cl$clusters)))
        iter <- c(iter, mean(iterTmp))
        rmse <- c(rmse, mean(rmseTmp))
    }
    res <- data.frame(`time [ms]`=1000*times, clusters, iter,
                      rmse, check.names=FALSE)
    row.names(res) <- names(testData)
    print(res)
}

set.seed(1)
testMethod(function(X) forgy(X, K = 2))
testMethod(function(X) randomPartitions(X, K = 2))
testMethod(function(X) plusplus(X, K = 2))
