## Put comments here that give an overall description of what your
## functions do
## To simplify the matrix inversion process, we can create an object to 
## store the inverse matrix and cache its inverse. When the matrix 
## inversion is needed, it can be looked up in the cache.

## Write a short comment describing this function
## Creat a special "matirx" object and cache its inversion.
## 1. set the value of matrix
## 2. get the value of matrix
## 3. set the value of matrix inversion 
## 4. get the value of matrix inversion

makeCacheMatrix <- function(x = matrix()) {
  m<- NULL
  set<- function(y){
    x<<- y
    m<<- NULL
  }
  get<- function() x
  setinverse<- function(solve) m<<- solve
  getinverse<- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
  
}


## Write a short comment describing this function
## Calculate the inverse of the special "matrix" created by makeCacheMatrix
## It will first check if the inversion has been caluculated. If it does, it
## can get the inversion from the cache and skips the computation. Otherwise,
## it will calculate the inversion of the matrix and set the value in the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m<- x$getinverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  data<- x$get()
  m<- solve(data, ...)
  x$setinverse(m)
  m
}
