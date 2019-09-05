## Description: This is Programming Assignment #2 for Week #3 of the
## Coursera "R Programming" class.  It implements the makeCacheMatrix(...)
## and cacheSolve(...) functions.
##
## Author: Robert Lin
## Date:   Sep 5, 2019

## makeCacheMatrix <- function(matrix_param = matrix())
##      This function accepts a matrix input.  It also contains the necessary
##      data structures to hold the inverse of the matrix.
##
##      Usage:
##          First, make an invertible matrix:
##              $> m <- matrix(c(4,7,2,6), nrow = 2, ncol = 2)
##
##          There are two ways to input a matrix into makeCacheMatrix:
##          First way: Initialize the matrix in the function's constructor:
##              $> cacheMatrix <- makeCacheMatrix(m)
##
##          Second way: Initialize an empty makeCacheMatrix and set the matrix later:
##              $> cacheMatrix <- makeCacheMatrix()
##              $> cacheMatrix$set(m)
##
##          Here are all of the methods of makeCacheMatrix:
##              $> cacheMatrix$set(matrix)
##              $> cacheMatrix$get()
##              $> cacheMatrix$setinverse(inverted_matrix)
##              $> cacheMatrix$getinverse()

makeCacheMatrix <- function(matrix_param = matrix())
{
    inverse <- NULL
    matrix <- matrix_param
    
    set <- function(matrix_param)
    {
        message("resetting any cached data, if applicable")
        inverse <<- NULL  # Reset the inverse if new matrix is set
        matrix <<- matrix_param
    }
    
    get <- function() matrix
    
    setinverse <- function(inverse_param) inverse <<- inverse_param
    getinverse <- function() inverse
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## cacheSolve <- function(cacheMatrix_param, ...)
##      The "cacheSolve" function accepts a cacheMatrix object as input.
##      If this is the first time it's being run, cacheSolve will compute the inverse of the matrix
##      inside of cacheMatrix, cache the inverse inside of cacheMatrix, and then print out the result.

##      If cacheMatrix's matrix's inverse has already computed before, the inverse will have already been cached.
##      So on subsequent cacheSolve calls, the inverse will then be retrieved from memory.

##      Note: As per instructed, cacheMatrix *assumes* cacheMatrix's matrix is invertible. If it's not,
##      R will produce an error message.  There is no error checking or validation in this implementation.

##      Usage:
##              $> cacheSolve(cacheMatrix)  ## The first time run, you'll see a 
##                                             "computing inverse and caching" message and the matrix's inverse.
##              $> cacheSolve(cacheMatrix)  ## The second time run, you'll see a 
##                                             "getting cached data" message and the matrix's inverse.

cacheSolve <- function(cacheMatrix_param, ...)
{
    inverse <- cacheMatrix_param$getinverse()
    if(!is.null(inverse))
    {
        message("getting cached data")
        return(inverse)
    }
    
    message("computing inverse and caching")
    data <- cacheMatrix_param$get()
    inverse <- solve(data)
    cacheMatrix_param$setinverse(inverse)
    inverse
}
