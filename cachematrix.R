## makeCacheMatrix creates a special matrix and function which will
##set the value of the matrix
##get the value of the matrix
## Get the inverse of the matrix
## cache the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL #Create blank value if none provided
  set <- function(y) {
    x <<- y #assign value
    m<<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) m <<- solve
  getmatrix <- function() m
  list(set=set, get=get,
       setmatrix=setmatrix,
       getmatrix=getmatrix)
}
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache.
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getmatrix() #the inverse of the matrix
  if(!is.null(m)){ #if it has already been calculated..
    message("getting cached data")
    return(m) #return cached inverse
  }
  #if it hasn't been calculated..then calculate the inverse
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setmatrix(m)
  return(m)
}  




