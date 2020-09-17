## calling cacheSolve will return the inverse of a matrix.
## The inverse will be computed and saved if its a new matrix,
## otherwise will be called from cache if previously calculated

## Creates a cached matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() {x}
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() {m}
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Solves the inverse of a matrix if not saved
## in cache and saves inverse in cache
## For argument x, pass in makeCacheMatrix(a)
## where a is the matrix to be inverted

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'

  m <- x$getinverse()
  
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  matrix <- x$get()
  m <- solve(matrix)
  x$setinverse(m)
  m
  }
