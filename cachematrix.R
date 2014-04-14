## Functions to cache results of computations,
##     specifically, this set of functions cache
##     result of matrix inversion
##
##     Written 14 April 2014 as part of Coursera R Programming course
##
##     Skeleton forked from repository rdpeng/ProgrammingAssignment2

## Function builds skeleton of matrix inversion caching
##
##     Input an assumed square, invertible matrix (no checking of input performed
##     Output  list of 4 functions: get/set pair and getinv/setinv pair
##
##    Note side-effect use of '<<-' operator outisde of "normal" scoping practice


makeCacheMatrix <- function(x = matrix()) {
##      manufacture list of functions needed to cache matrix inversion
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(solve) m <<- solve
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## Either performs matrix inversion or returns result of cached solution if
##     inversion has previously been carried out for argument

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getinv()
##    Two cases depending upon m; if m is not NULL then calculation
##          on argument has previously been performed, result is in x$getinv
  if(!is.null(m)) {
    message("Getting cached inversion solution")
    return(m)
  }
##    Otherwise, perform inversion computation on data and indicate through
##      setinv() function that an inverse for argument has been calculated
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  return(m)
}
