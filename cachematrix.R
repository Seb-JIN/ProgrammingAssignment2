## The two functions below are used to create an object that stores a
## matrix and cache's its inversion

## This first function creates a list to 
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inversion
## 4. get the value of the inversion

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        set_inverse <- function(inverse) m <<- inverse
        get_inverse <- function() m
        list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)
}
## The following function first checks if the inversion of the matrix has 
## already been calculated. If so, it gets the inversion from the cache 
## and skips the computation. Otherwise, it calculates the inversion of 
## the data and sets the value of the inversion in the cache via the 
## set_inverse function

cacheSolve <- function(x, ...) {
        m <- x$get_inverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$set_inverse(m)
        m
}
