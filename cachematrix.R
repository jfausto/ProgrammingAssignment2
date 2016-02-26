## R programming course as part of Data Science Program by Johns Hopkins University
## Assigment week 3. Caching a function that calculates the inverse of a matrix
## Date: Feb 26 2016
## Note: The matrix should be invertible and square to make use of R solve() function

## makeCacheMatrix creates a matrix to cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    ## set the inverse to null
    m <- NULL
    ## set the function
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    ## get the parameter
    get <- function() x
    ## execute the inverse and store in setsolve 
    setsolve <- function(solve) m <<- solve
    ## obtain the inverse when requested
    getsolve<- function() m
    ## return a list
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## This function calculates the inverse of a square matrix (det=!0), checking
## first if it is in thecache 
cacheSolve <- function(x, ...) {
    
    m <- x$getsolve()
    ## Check if this value is in cache
    if(!is.null(m)) {
        #if so, do not re-calculate, display a message and exit function
        message("getting cached data")
        return(m)
    }
    ## Not cached, calculate for the 1st time
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}
