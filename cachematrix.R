makeCacheMatrix <- function(x = matrix()) { 
## create a list to store the matrix and related inversed maxrix 
  m <- NULL # assigning local m standing for inversed matrix to NULL
  set <- function(y){
    x <<- y ## <<- is an analog of <-, but it is to assign to global enviroment 
    m <<- NULL ## totally crush m
  }
  get <- function() x # subfunction returning initial matrix
  setsolve <- function(solve) m <<- solve # subfunction to cache the inverse matrix
  getsolve <- function() m # extract the cache
  list(set= set, get = get,
  setsolve = setsolve,
  getsolve = getsolve)
}


cacheSolve <- function(x, ...) {
  m <- x$getsolve()
  if (!is.null(m)) # check if cache available
  {
    message("getting cached data")
    return(m) #exit cacheSolve and return cache
  }
  message("computing the cache")
  data <- x$get() #intermediate variable
  m <- solve(data, ...) 
  x$setsolve(m) 
  m
}
