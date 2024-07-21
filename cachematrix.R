## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## Here, there are two functions 1. makeCacheMatrix 2. cacheSolve
## library(MASS) is used to calculate inverse for non squared and square matrices

> library(MASS)
> makeCacheMatrix <- function(x = matrix()) {
+     inv <- NULL               ## starting inverse as Null
+     set <- function(y) {
+         x <<- y
+         inv <<- NULL
+     }
+     get <- function() x       ## function to obtain matrix x
+     setinv <- function(inverse) inv <<- inverse
+     getinv <- function() {
+         inver <- ginv(x)
+         inver%*%x
+     }
+     list(set = set, get = get,
+          setinv = setinv,
+          getinv = getinv)
+ }
> cacheSolve <- function(x, ...) {
        
+     inv <- x$getinv()       ## function to obtain inverse of the matrix
+     if(!is.null(inv)) {
+         message("getting cached data")
+         return(inv)
+     }
+     data <- x$get()
+     inv <- solve(data, ...)
+     x$setinv(inv)
+     inv
+ }
