## These two functions should be used together and aims to demonstrate the use of lexical scope of R to implement a cache processing. 
## The first function "makeCacheMatrix" creates a special matrix that should be passed as parameter to "cacheSolve" function,
## that checks if the value exists in the cache or if it will run the "solve" function.

## "makeCacheMatrix", receive a "inversible matrix" and create a list with the functions used to implement the cache feature

makeCacheMatrix <- function(x = matrix()) {
  mx <- NULL
  set <- function(y) {
    x <<- y
    mx <<- NULL
  }
  get <- function() x
  setsolve <- function(solve) mx <<- solve
  getsolve <- function() mx
  list(set = set, get = get,
       setsolve = setsolve,
       getsolve = getsolve)
  
}

## "cachSolve" receive a special matrix created with "makeCacheMatrix"
## and checks if result of the function "solve" for the object already exists to determine
## whether returns this value or performs the "solve" function to the object and stores the result (cache).

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mx <- x$getsolve()
  if(!is.null(mx)) {
    message("getting cached data")
    return(mx)
  }
  data <- x$get()
  mx <- solve(data, ...)
  x$setsolve(mx)
  mx
}
