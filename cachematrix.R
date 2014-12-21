########################################################################################
## Matrix inversion is usually a costly computation and there may be some benefit to
## caching the inverse of a matrix rather than compute it repeatedly.                 
## We are going to use 2 functions to create a matrix object that cashes its inverse  
## and computes the matrix object invers either from cache or from scratch.           
########################################################################################



## makeCacheMatrix
#  This function creates a special "matrix" object that can cache its inverse.



makeCacheMatrix <- function(A = matrix())
{
  M <- NULL
  set <- function(B)                               # The special matrix is assigned to "set"
  {
    A <<- B
    M <<- NULL
  }
  get <- function() A                              # The value of the matrix is assigned to "get"
  setmatrix<-function(solve) M<<- solve            # inverse of the matrix is calculated
  getmatrix<-function() M                          # Inverse of the matrix is stored in "getinverse"
  list(set=set, get=get, 
       setmatrix=setmatrix, getmatrix=getmatrix)	 
}



## cacheSolve
#  This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
#  above. If the inverse has already been calculated (and the matrix has not changed), 
#  then the cachesolve should retrieve the inverse from the cache. This function assumes
#  that the matrix is always invertible.


cacheSolve <- function(A=matrix(), ...) 
{
  M<-A$getmatrix()                    # checks cache globally to see if inverse already exists
  if(!is.null(M))                     # condition is true if it finds an inverse in cache
  {
    message("getting cached data.")   # A message "getting cached data." is displayed
    return(M)                         # outputs matrix invers and gets out of the function
  }
  matrix <- A$get()                   # get the matrix in to "data"
  M <- solve(matrix, ...)             # solve is user to calculate the inverse
  A$setmatrix(M)					
  M                                   # Inverse of the matrix is outputted
}
