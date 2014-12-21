## Functions: 
##
## - makeCacheMatrix() is a function that allows you to create a matrix whose inverse can be cached.
## - cacheSolve() function calculates the inverse of a matrix created through the makeCacheMatrix
## function. 
##
##
## Example of usage: 
##
##  > a <- matrix(c(2,3,6,2), nrow=2, ncol=2)
##  > b <- makeCacheMatrix(a)
##  > b$get()
##      [,1] [,2]
## [1,]    2    6
## [2,]    3    2
##  >
##  > cacheSolve(b)
##             [,1]       [,2]
## [1,] -0.1428571  0.4285714
## [2,]  0.2142857 -0.1428571
## 
## if you call cacheSolve(b) again, it uses the cached value instead of computing the inverse again
##
##  > cacheSolve(b)
##  getting cached data
##             [,1]       [,2]
##  [1,] -0.1428571  0.4285714
##  [2,]  0.2142857 -0.1428571
##  > 
##
## --------------------------------------
##
## makeCacheMatrix is a constructor function that creates a matrix and then provides a set of functions
## that allow you to manipulate its value, as well as cache its inverse.
##
## The list of functions created by makeCacheMatrix are: 
## 1. set:         Sets the value of the matrix 
## 2. get:         Retrieves the value of the matrix
## 3. setInverse:  Stores the inverse of the matrix in the cache
## 4. getInverse:  Returns the inverse value stored in the cache
## 
## The 'i' variable in this function serves as the cache for the inverse which can be accessed 
## by the functions defined by makeCacheMatrix using the '<<-' operator

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  
  set <- function(newValue) {
    x <<- newValue
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## The list returned by makeCacheMatrix() after creating a matrix is passed as an argument to cacheSolve()
##
## The cacheSolve function can then use the getinverse function to check if the 
## inverse has already been calculated. If it has, it returns the cached data. 
##
## However, if the inverse is null, it uses solve() to calculate the inverse and then uses the setinverse
## function to store the calculated inverse in the cache for future use. It also returns the newly  
## calculated inverse. 

cacheSolve <- function(x, ...) {
        
  i <- x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}
