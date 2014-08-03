## Matrix inversion is usually a costly computation and there may be some benefit 
## to caching the inverse of a matrix rather than computing it repeatedly. This is
## implemented in this program.



## makeCacheMatrix function takes in a default matrix data structure and creates
## a special "matrix" object that can cache its inverse.
## cacheSolve function checks if the special matrix object already has its inverse 
## cached. If it does, it returns the cached inverse. If not, it calculates the 
## inverse, caches it and then returns it.

## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It takes a default matrix data structure as its argument and creates a special 
## "matrix" object.


makeCacheMatrix <- function(x = matrix()) {
  #set inverse of matrix to null
  inv <-NULL
  
  #set new value of matrix and set inverse to null
  set<-function(y){
    x<<-y
    inv <<-NULL
  }
  
  #return matrix value
  get <- function() x
  
  #set the new inverse
  setinverse <- function(new_inv) inv <<- new_inv
  
  #get the inverser of hte matrix
  getinverse <- function() inv
  
  #list of 4 functions set/get matrix value getmatrix/setmatrix
  list(set=set, get=get, setinverse =setinverse, getinverse=getinverse)
  
  
}



## cacheSolve computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cacheSolve function would retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  #get inverse
  inv <- x$getinverse()
  #if inverse is not null, return it
  if (!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  #get matrix value
  matrix <- x$get()
  #compute inverse
  inv <- solve(matrix,...)
  #save inverse
  x$setinverse(inv)
  #return inverse
  inv
  
}
