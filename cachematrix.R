## This R Source program contains two functions which computes and caches the inverse 
## of the matrix.  The functions checks returns the cached inverse of the matrix if exist,
## if not, it computes and caches the inverse of the matrix.
## 
## 
## 
##     This function makes a matrix, with the matrix inverse cache.  It provides a
##     list with includes the functions get, set and set inverse and getinverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set <- function(y) {
    x<<-y
    m<<-NULL
  }
  get <- function() x
  setinverse <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
     setinverse = setinverse,
     getinverse = getinverse)
}


##     A function which makes use of makeCacheMatrix to compute for the inverse of the
##     matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m
}

