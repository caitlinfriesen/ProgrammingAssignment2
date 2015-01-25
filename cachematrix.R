## This document contains two functions: 'makeCacheMatrix' and 'cacheSolve'
## 1) The first function creates a matrix that can cache it's inverse
## 2) The second function computes the inverse of the matrix above if it has not already been calculated
## and retrieves the inverse from the cache if it has

## 1) The function 'makeCacheMatrix' creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y, silent = TRUE) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse<- function(inverse) inv <<-inverse
        getinverse <- function() inv
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## 2) The function 'cacheSolve' computes the inverse of the special "matrix" returned by makeCacheMatrix above
## if the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if (!is.null(inv)) {
                message("getting cached inverse matrix")
                return(inv)
        } else {
                inv <- solve(x$get())
                x$setinverse(inv)
                return(inv)
        }
}
