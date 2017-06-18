## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  ##Set the matrix
  set <- function(y){
    x<<-y
    inv <<- NULL
  }
  ##Get the matrix
  get <- function() x
  ##Set Inverse Matrix
  setinverse <-function(inverse) inv<<-inverse
  ##Get the Inverse Matrix
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        ##If the Inverse Matrix from MakeCacheMatrix is NOT null, 
        ##return the invverse from the cache. 
        if(!is.null(inv)){
          message("getting cached Inverse Matrix")
          return(inv)
        }
        ##If the Inverse Matrix from MakeCacheMatrix is null,
        ##computes the Inverse Matrix by solve.
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        ##Return Inverse Matrix
        inv
}
