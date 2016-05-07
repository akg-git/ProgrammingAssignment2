## below code gives option to retrive a matrix inverse from cache
## if system has already calculated it once and if matrix itself has not changed

## steps to test it(example)
## type below commands in R console
##
## b<-makeCacheMatrix()
## b$set(matrix(rnorm(4),2,2))
## b$get()         this will let u see value in your matrix
## b$getinverse    this return null as we have not assigned inverse
## cacheSolve(b)   first usage: it will return inverse of matrix 
##                             and save it to cache
##                 output         : inverse of matrix is printed
##
## cacheSolve(b)   second usage: now cache has inverse value, 
##                 output message : getting inverse from cache
##                 output         : inverse of matrix is also printed

## makeCacheMatrix returns a list of functions that can be used to :
## get : get value of input matrix
## set : set value of matrix 'x' using input matrix
## setinverse : this funvtion sets the inverse of matrix using value passesd as parameter
## getinverse : retrives the inverse of input matrix 'x'

makeCacheMatrix <- function(x = matrix()) {
  
  x.inv<-NULL
  set<-function(y){
    x<<-y
    x.inv <<- NULL
  }
  
  get <- function() x
  setinverse <-function(inv) x.inv <<- inv
  getinverse <-function() x.inv
  
  list (set=set,get=get,setinverse=setinverse,getinverse=getinverse)
  
}


## cacheSolve look for inverse of matrix in cache. 
## if found it returns value from cache
## if cache do not have it then it calculates inverse, 
##    saves it to cache and return the inverse.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  x.inv <-x$getinverse()
  if(!is.null(x.inv))
  {
    message("getting inverse value from cache")
    return(x.inv)
  }
  
  data<-x$get()
  if (det(data)!=0){
    x.inv <- solve(data,...)
    x$setinverse(x.inv)
    x.inv
  }
  else
    message("singular matrix - Inverse not possible")
  
}