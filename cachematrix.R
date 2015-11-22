## a pair of functions that cache the inverse of a 2x2matrix

## This function creates a special "matrix" object 
##  that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    Inv <- matrix(nrow = nrow(x), ncol = ncol(x))
    set <- function(y) {
        x <<- y
        Inv <<- matrix(nrow = 2, ncol = 2)
    }
    get <- function() x
    setinv <- function(Inverse) Inv <<- Inverse
    getinv <- function() Inv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse
## of the special "matrix" returned by
## makeCacheMatrix above or extracts it from the cache, if inverse matrix
## has already been calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    Inv <- x$getinv()
    Sum <- 0
    for (i in 1:length(Inv))  {
        Sum <- Sum + is.na(Inv[[i]])
    } 
    if (!(Sum == length(Inv))) {
        message("getting cached data")
        return(Inv)
    }
    data <-  x$get()
    Inv <- solve(data,...)
    x$setinv(Inv)
    Inv
    }
