## The makeCacheMatrix function builds a matrix capable of caching. Once this data
## structure is built, one can use the cacheSolve function with the build caching
## matrix to compute its inverse. 
##
## Re-execution of the cacheSolve function with a previously computed cached 
## matrix will leverage the caching capabilities and won't re-compute, but 
## simply returns the previously computed value. 
## 
## This file follow the Google R Guide for function documentation:
## https://google-styleguide.googlecode.com/svn/trunk/Rguide.xml#comments
##
## Author: Jimmy Royer
## Date: April 25, 2015

makeCacheMatrix <- function(x = matrix()) {
  # Builds a matrix capable of caching capabilities.
  #
  # Args:
  #   x: A regular R matrix.
  #
  # Returns:
  #   A matrix capable of caching. 

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setcache <- function(computed) m <<- computed
  getcache <- function() m
  list(set = set, get = get,
       setcache = setcache,
       getcache = getcache)
}

cacheSolve <- function(x, ...) {
  # Solves the inverse of a matrix and cache it for future invocation.
  #
  # This function only works on a special list structure provided by
  # the makeCacheMatrix function which supports caching.
  #
  # The cached matrix should be an invertible matrix else this function
  # will fail (the weekly assigment states that this should always be 
  # the case).
  #
  # Args:
  #   x: A matrix with caching capabilities, provided by the makeCacheMatrix function.
  #   ...: Further arguments passed to or from other methods
  #
  # Returns:
  #   The inverse matrix of x.
  
  m <- x$getcache()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setcache(m)
  m
}

testCachingTime <- function() {
  # Use a time test to see if we really save time
  # (Thanks to Karl Schultz for this custom test)

  # Replace with any number that will crunch your CPU to your likings
  n <- 1024 
  mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
  matCached <- makeCacheMatrix(mat)
  time1 <- system.time(matSolved1 <- cacheSolve(matCached))
  time2 <- system.time(matSolved2 <- cacheSolve(matCached))
  if (time1["user.self"] < time2["user.self"])
    message("Solve time is less than cache time")
  message(paste0("Elapsed time without cache: ", time1[3]))
  message(paste0("Elapsed time with cache: ", time2[3]))
}
