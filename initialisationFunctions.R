# Three Initialisation Functions for K-means Clustering on the projective space

# Input: 
# - data, X, that lies on p-dim sphere 
# - number of clusters, K
# Output: vector - which cluster each X_i belongs to

# forgy: points picked randomly as cluster center
forgy <- function(X, K) {
        n <- nrow(X)
        whichCenters <- sample(1:n, size = K, replace = FALSE,
                               prob = rep(1, n))
        centers <- lapply(seq_len(K), function(k) {
                                  X[whichCenters[k], ]
                               })
        distance <- lapply(centers, function(c) {
                                   distanceTmp <- 1 - (X %*% c)^2
                                   distanceTmp
                               })
        distance <- do.call(cbind, distance)
        whichCluster <- apply(distance, 1, which.min)
        whichCluster
}

# random partitions: points assigned to random cluster
randomPartitions <- function(X, K) {
        n <- nrow(X)
        i <- 0
        while (TRUE) { # loop to ensure no empty clusters
                i <- i + 1
                if (i == 100) { # stop eternal loop
                        warning("Empty clusters: consider reducing K")
                        break
                }
                whichCluster <- sample(1:K, size = n, replace = TRUE,
                                       prob = rep(1, K))
                # check for empty clusters
                allClusters <- unique(whichCluster)
                if (length(allClusters) == K) break
                #allClusters <- all(sapply(1:K, function(i) i %in% whichCluster))
                #if (allClusters == TRUE) break
        }
        whichCluster
}

# k-means++ algorithm: see https://en.wikipedia.org/wiki/K-means%2B%2B
plusplus <- function(X, K) {
    n <- nrow(X)
    DX <- rep(1, n)                                                                                  
    dist <- matrix(0, nrow=n, ncol=K)
    allSamples <- as.numeric() 
    for (k in 1:K) {
        sampleTmp <- sample(n, size=1, prob=DX)
        allSamples <- c(allSamples, sampleTmp)
        pointTmp <- X[sampleTmp, ]
        dist[, k] <- 1 - (X %*% pointTmp)^2
        DX <- apply(dist[,1:k,drop=FALSE], 1, min)
        # overwrite to stop numerical errors so that DX is always positive
        DX[allSamples] <- 0
        # use following to stop "too few positive probabilities" error
        if (all(DX == 0)) DX[-allSamples] <- rep(1, n - k)
        # as sample() only requires relative probabilites
        DXlog <- log(DX)
        # make max DXlog equal 0
        DXlog <- DXlog - max(DXlog)
        # exp(log) to set max to 1
        DX <- exp(DXlog)
        if(any(is.na(DX))) DX[is.na(DX)] <- 0
        # ensure  no error in sample()
        if (all(DX == 0)) DX[-allSamples] <- rep(1, n - k)
    }
    whichCluster <- apply(dist, 1, which.min)
    whichCluster
}
