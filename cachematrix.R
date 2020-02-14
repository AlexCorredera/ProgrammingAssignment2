## This R file contains code for the Programming Assignment 2. 
## The following two functions (1) creates a special "matrix" object that 
## can cache its inverse and (2) returns the inverse of this special "matrix

## First, the makeCacheMatrix function creates a special "matrix" that can 
## cache its inverse (the argument x must be a matrix for the function to work).

makeCacheMatrix <- function(x = matrix()) {
        i = NULL
        setMatrix <- function(y) {
                x <<- y
                i <<- NULL
        }
        getMatrix <- function() x
        setInverseMatrix <- function(InverseMatrix) 
                i <<- InverseMatrix
        getInverseMatrix <- function() i
        list(setMatrix = setMatrix, getMatrix = getMatrix, 
             setInverseMatrix = setInverseMatrix, 
             getInverseMatrix = getInverseMatrix)
}


## Second, the cacheSolve function returns the inverse of the special "matrix'
## object returned by the makeCacheMatrix function, either by retrieving the
## inverse from the cache or by calculating the inverse on the spot.

cacheSolve <- function(x, ...) {
        i <- x$getInverseMatrix()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$getMatrix()
        i <- solve(data, ...)
        x$setInverseMatrix(i)
        i
}