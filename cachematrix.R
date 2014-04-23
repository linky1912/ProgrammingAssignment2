## makeCacheMatrix() is a list containing 4 functions: set(), get(), setinverse() and getinverse()
## set() assigns a new matrix value to the variable chosen, get() returns the latest assigned matrix
## setinverse() assigns a value as the matrix's inverse
## getinverse() returns the value of the matrix's inverse, either assigned or solved through cacheSolve()

makeCacheMatrix <- function(x = matrix()) {
     matrix <- NULL
     set <- function(y) {
          x <<- y
          matrix <<- NULL
     }
     get <- function() x
     setinverse <- function(inverse) matrix <<- inverse
     getinverse <- function() matrix
     list(set = set, get = get, setinverse = setinverse, getinverse = getinverse )
}


## cacheSolve() takes the value of the matrix from makeCacheMatrix() and returns the inverse
## if the inverse has been calculated before or set previously using setinverse(), the function returns the cache'd
## value instead
## cacheSolve() assumes matrices passed through it have an inverse, and returns an error when the matrix is not
## solvable

cacheSolve <- function(x, ...) {
     inv <- x$getinverse()
     if (!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- ginv(data,...)
     x$setinverse(inv)
     inv
}

