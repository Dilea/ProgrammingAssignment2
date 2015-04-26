## These functions will return the inverse of a matrix and cache the inversed matrix for future use


## makeCacheMatrix will return a list of functions that reads the matrix (get), changes the matrix based on 
## the input while resetting the inversed matrix stored in i (set),  stores the inversed matrix (setinverse)
## and returns the stored inversed matrix (getinverse)

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
              x <<- y
              i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve returns the inverse of the matrix in makeCacheMatrix by first checking if the inverse has been calculated previously and stored in i,
## or else performing the calculation and storing the inversed matrix in makeCacheMatrix

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)){
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
