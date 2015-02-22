#The purpose of these two functions is to cache the inverse of a matrix.

makeCacheMatrix <- function(x = matrix()) {
  #This function creates a special "matrix" object that can cache its inverse.
  m <- NULL
  set <- function(y) {
    x <<- y #creates a global object
    m <<- NULL
  }
  get <- function() x
  setMatrix <- function(solve) m <<- solve #function to calculate inverse
  getMatrix <- function() m #stores the inverse matrix in cache
  list(set = set, get = get,
       setMatrix = setMatrix,
       getMatrix = getMatrix)
}

cacheSolve <- function(x, ...) {
  # This function computes the inverse of the special "matrix" returned by makeCacheMatrix
  m <- x$getMatrix() 
  if(!is.null(m)) { #checks to see if the inverse is already in cache
    message("getting cached data")
    return(m) #if in cache, returns results
  }
  matrix <- x$get() #if not in cache, calls matrix
  m <- solve(matrix, ...) #inverses matrix
  x$setMatrix(m) #sets matrix inverse
  m 
}