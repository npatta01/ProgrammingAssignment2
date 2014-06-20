## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
  data <- x$get()
  #compute inverse
  inv <- solve(data,...)
  #save inverse
  x$setinverse(inv)
  #return inverse
  inv
  
}
