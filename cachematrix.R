
## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than computing it repeatedly 

## Order of Steps
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse matrix
## 4. get the value of the inverse matrix

## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    ## Initialize the inverse property 
    i <- NULL                          

    ## 1. set the value of the matrix
    set <- function(y) {
      x <<- y
      i <<- NULL
      }

    ## 2. get the value of the matrix
    get <- function() {
      x
      }

    ## 3. set the value of the inverse matrix
    setinverse <- function(inversematrix) {
      i <<- inversematrix
      }

    ## 4. get the value of the inverse matrix
    getinverse <- function() {
      i
      }

    ## Encapsulate to a list
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)

}

## ====================================================================

cacheSolve <- function(x, ...) {

   i <- x$getinverse()

   if(!is.null(i)) {
   message("Retrieve cached data")
   return(i)
   }

   m <- x$get()
   i <- solve(m, ...)

   ## Return a matrix which is inverse of 'x'
   x$setinverse(i)
   i
}

