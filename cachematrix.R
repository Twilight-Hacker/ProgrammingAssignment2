## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
        x.inv <- NULL
        set <- function(y) {
                if(is.matrix(y)){
                        x <<- y
                        x.inv <<- NULL
                }
                else {
                        print("Provided value is not a Matrix")
                }
        }
        get <- function() x
        setInverted <- function(Inverted) x.inv <<- Inverted
        getInverted <- function() x.inv
        list(set = set, get = get,
             setInverted = setInverted,
             getInverted = getInverted)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x.inv <- x$getInverted()
        if(!is.null(x.inv)) {
                message("getting cached data")
                return(x.inv)
        }
        data <- x$get()
        m <- Solve(data, ...)
        x$setInverted(m)
        x.inv

}
