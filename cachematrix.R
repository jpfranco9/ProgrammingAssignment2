## These functions will cache the inverse of a square matrix. 

## This function will create a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  i<- NULL
  set<- function(y){
    x<<-y
    i<<-NULL
  }
  get <-function() x
  setinverse <- function(inverse) i<<-inverse
  getinverse <- function() i
  list(set=set, get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## Taking the special matrix created in the above, this function will compute the inverse. 
## This function will retrieve the inverse from the cache if it has already been calculated.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i<-x$getinverse()
  if(!is.null(i)) {
    message("getting cached data")
    return(i)
  }
  data<- x$get()
  i <-solve(data, ...)
  x$setinverse(i)
  i
}
