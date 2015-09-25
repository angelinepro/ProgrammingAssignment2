##This function enables caching potentially time-consuming operations.
##It will only perform the calculation if the solution does not
##already exist, and after it performs the calculation, it will
##store the result so that it does not need to be repeated.

## makeCacheMatrix creates a list of functions to be used later, such
## as functions to return the vector in the main function, change
## the vector in the main function, store the value of the calculation,
## and retrieve it.

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y){
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve checks to see if there is a value stored, and if it
## exists, it will return it. Otherwise, it will perform the calculation,
## store it, and return it.

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message ("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    ## Return a matrix that is the inverse of 'x'
}


