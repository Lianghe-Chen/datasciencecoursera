## Put comments here that give an overall description of what your
## functions do

## These functions are written to fulfil Coursera Data Science: R Programming Week 3 Peer-Graded Assignment.

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) { ## Defines the argument with default mode of "matrix"
  inv <- NULL                             ## Initialises inv as NULL to hold value of matrix inverse
  set <- function(y) {                    ## Defines the set function to assign new value of matrix
    x <<- y                             ## in parent environment
    inv <<- NULL                        ## If there is a new matrix, reset inv to NULL
  }
  get <- function() x                     ## Defines the get function to return value of the matrix argument
  setinverse <- function(inverse)
    inv <<- inverse                     ## Assigns value of inv in parent environment
  getinverse <- function() inv            ## Gets the value of inv when called
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
  ## Refer to the functions with the $ operator
}

## Write a short comment describing this function

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
## If the inverse has already been calculated (and the matrix has not changed),
##then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()               ## Get the invertible matrix from the makeCacheMatrix function
  if(!is.null(inv)) {                 ## If inverse matrix is not NULL
    message("Getting Cached Invertible Matrix")  ## Show message: Getting Cached Invertible Matrix
    return(inv)                     ## Return the invertible matrix
  }
  ## If value of invertible matrix is NULL then 
  data <- x$get()                     ## Get original Matrix Data
  inv <- solve(data, ...)         ## Use solve function to inverse the matrix
  x$setinverse(inv)               ## Set the invertible matrix
  return(inv)                             ## Return the invertible matrix
}