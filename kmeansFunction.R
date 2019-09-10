kmeansClustering <- function(X, K, iter.max=100, initial, verbose=TRUE) {
        # `initial' is vector that pairs points to corresponding cluster
        n <- nrow(X)
        c <- initial
        if (missing(K)) K <- max(c)
        j <- 0
        for (i in 1:iter.max) {
                j <- j + 1
                dist <- matrix(0, nrow=n, ncol=K)
                for (k in 1:K) {
                    Y <- X[c == k,, drop=FALSE]
                    # need to change this so that the clust mean changes...
                    # don't want empty clustering
                    if(!(nrow(Y) == 0)) {
                        s <- La.svd(Y, nu=0, nv=1)
                        centre <- s$vt[1,]
                        dist[, k] <- 1 - (X %*% centre)^2
                    }
                }
                cOld <- c
                c <- apply(dist, 1, which.min)
                if (all(cOld == c)) {
                            break
                }
        }
        if(verbose == TRUE) {
                if(j < iter.max) {
                        cat("Converged to ", K, " clusters in ", j, " iterations \n")
                }
                if(j == iter.max) {
                        cat("Maximum iterations of ", iter.max, 
                                " used, consider increasing iter.max \n")
                }
        }
        list(clusters = c, iter = j)
}

# find total sum of squares and
# within cluster sum-of-squares
clusterRMSE <- function(X, c) {
    K <- max(c)
    p <- ncol(X)
    n <- nrow(X)
    # emply list of matrix for splitting into clusters
    clust <- vector(mode = "list", length = K)
    for(i in 1:K) {
        clust[[i]] <- matrix(nrow=length(which(c == i)), ncol = p)
    }
    # split X into clusters
    ticker <- rep(0, K)
    for(i in 1:n) {
        cTmp <- c[i]
        ticker[cTmp] <- ticker[cTmp] + 1
        clust[[cTmp]][ticker[cTmp],] <- X[i,]
    }
    squaredErrorIndividual <- sapply(clust, function(x) {
                        nTmp <- nrow(x)
                        if(nTmp == 0) {
                            SSE <- 0
                        } else {
                            s <- La.svd(x, nu=0, nv=1)
                            SSE <- nTmp - s$d[1]^2
                        }
                        SSE
                    })
    if(any(squaredErrorIndividual < 0)) {
        whichWss <- which(squaredErrorIndividual < 0)
        squaredErrorIndividual[whichWss] <- 0
    }
    squaredError <- sum(squaredErrorIndividual)
    rmse <- sqrt(squaredError / n)
    return(list(rmse = rmse, 
                squaredError = squaredError, 
                squaredErrorEach=squaredErrorIndividual))
}
