## Assignment 2

## getter and setter for matrix

makeCacheMatrix <- function(x = matrix()) {
		inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inv <<- inverse
        getinverse<- function() inv
        matrix(set = set, get = get,
             setinverse = setinverse ,
             getinverse= getinverse)

}


## this will try to get the matrix from cache otherwise it will invert and send it back

cacheSolve <- function(x, ...) {   
		inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv

}
