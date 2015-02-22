## This script cachematrix.R creates a special "matrix" with a cached inverse,
## which can significantly save time for multiple times inverse calculation
## for large scale matrix.

## In this function, makeCacheMatrix creates a special "matrix", which is 
## really a list containing a function to
##     1. set the value of the matrix
##     2. get the value of the matrix
##     3. set the value of the inverse
##     4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinv <- function(inv) i <<- inv
        getinv <- function() i
        list(set = set, get = get, 
             setinv = setinv,
             getinv = getinv)
}


## This function calculates the inverse of the special "matrix" created with
## the above function. However, it first checks to see if the inverse has 
## already been calculated. If so, it gets the inverse from the cache and 
## skips the computation. Otherwise, it calculates the inverse of the matrix 
## and sets the value of the inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinv()
        ## if the inverse has been calculated 
        if(!is.null(i)) {
                message("getting cached data")
                return(i) # return the cached inverse
        }
        ## else, calculated the inverse and set to the cache
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
