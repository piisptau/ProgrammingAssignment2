## makeCacheMatrix and cachesolve functions can be used speed up calculation of inverse matrices
## if the inverse matrix has already been calculated the cached result is used
## 

## makeCacheMatrix function creates a new cache matrix matrix object from matrix parameter, fro example
##
##   m1 <- matrix(c(78,56,45,23),nrow=2,ncol=2)
##   cm <- makeCacheMatrix(m1)
##

makeCacheMatrix <- function(x) {
        if(!is.matrix(x) || length(dim(x)) != 2 || dim(x)[1] != dim(x)[2]) {
                message("argument is not a square matrix")
                return(NULL)
        }
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setsolve<- function(solve) m <<- solve
        getsolve <- function() m
        list(set = set, get = get,
             setsolve = setsolve,
             getsolve = getsolve)
}



## cachesolve function calculates inverse matrix for a cache matrix object parameter
## if the inverse matrix has already been calculated, the cached result is returned
##
## Example:
##
##    m1 <- matrix(c(78,56,45,23),nrow=2,ncol=2)
##    cm <- makeCacheMatrix(m1)
##
##    cachesolve(cm)
##            [,1]        [,2]
## [1,] -0.03168044  0.06198347
## [2,]  0.07713499 -0.10743802
##
##    cachesolve(cm)
## getting cached data
##            [,1]        [,2]
## [1,] -0.03168044  0.06198347
## [2,]  0.07713499 -0.10743802
##

cachesolve <- function(x, ...) {
        m <- x$getsolve()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setsolve(m)
        m
}


