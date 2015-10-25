## Two functions that enable inverses of matricies to be cached.

## makeCacheMatrix() takes a matrix and returns a list containing
## two functions that set or get the matrix and two functions
## that get or set the cached inverse of the matrix.

## cacheSolve takes the special matrix created by makeCacheMatrix and
## either calculates the inverse of it or returns the cached inverse
## if it's already been calculated.

## Example usage:
## m1 <- matrix(c(1,2,3,4), nrow=2, ncol=2)
## cachedM1 <- makeCacheMatrix(m1)
## cacheSolve(cachedM1)
##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

## When it is called again with cachedM1, it prints a message
## that the inverse is cached:

## cacheSolve(cachedM1)
##getting cached inverse
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5



## Create a special matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
  
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
  
    get <- function() {
        x
    }
  
    setinverse <- function(inverse) {
        m <<- inverse
    }
  
    getinverse <- function() {
        m
    }
  
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Calculate the inverse of the special matrix object created by makeCacheMatrix

cacheSolve <- function(x, ...) {
    m <- x$getinverse()
  
    if(!is.null(m)) {
        message("getting cached inverse")
        return(m)
    }
  
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
  
    ## Return a matrix that is the inverse of 'x'
  
    m
}
