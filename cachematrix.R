########################################################################################
## Matrix inversion is usually a costly computation and there may be some benefit to
## caching the inverse of a matrix rather than compute it repeatedly.                 
## We are going to use 2 functions to create a matrix object that cashes its inverse  
## and computes the matrix object invers either from cache or from scratch.           
########################################################################################



## makeCacheMatrix
#  This function creates a special "matrix" object that can cache its inverse.



makeCacheMatrix <- function(x = matrix())
{
  inv <- NULL
  set <- function(y)                               # The special matrix is assigned to "set"
  {
    x <<- y
    inv <<- NULL
  }
  get <- function() x                              # The value of the matrix is assigned to "get"
  setinverse <- function(inverse) inv <<- inverse  # inverse of the matrix is calculated
  getinverse <- function() inv                     # Inverse of the matrix is stored in "getinverse"
  list(set=set, get=get, 
       setinverse=setinverse, getinverse=getinverse)	 
}



## cacheSolve
#  This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
#  above. If the inverse has already been calculated (and the matrix has not changed), 
#  then the cachesolve should retrieve the inverse from the cache. This function assumes
#  that the matrix is always invertible.


cacheSolve <- function(x, ...) 
{
  inv <- x$getinverse()               # checks cache globally to see if inverse already exists
  if(!is.null(inv))                   # condition is true if it finds an inverse in cache
  {
    message("getting cached data.")   # A message "getting cached data." is displayed
    return(inv)                       # outputs matrix invers and gets out of the function
  }
  data <- x$get()                     # get the matrix in to "data"
  inv <- solve(data)                  # solve is user to calculate the inverse
  x$setinverse(inv)						
  inv                                 # Inverse of the matrix is outputted
}
