## Put comments here that give an overall description of what your
## functions do

## The following function creates a list object for a matrix that can store its inverted matrix.
## If no matrix is provided during creation, the function defaults to the 2x2 Identity matrix, to be able to use Solve without problems

makeCacheMatrix <- function(x = matrix(c(1,0,0,1),2,2)) { ##create a matrix, if none is given, default to Identity 2x2 Matrix
        x.inv <- NULL ##initialize the inverted as NULL
        set <- function(y) {  ##function to set the matrix
                if(is.matrix(y)){ ##apply value to x, only if the provided object is a matrix (we cannot assume that it is a matrix)
                        x <<- y
                        x.inv <<- NULL ##reset the inverted matrix value to NULL
                }
                else {
                        print("Provided variable is not a Matrix. Nothing set.") ##print a warning message
                }
        }
        get <- function() x ##get the matrix
        setInverted <- function(Inverted) x.inv <<- Inverted ##set the cached value of the inverted matrix to the provided matrix
        getInverted <- function() x.inv ##get the inverted matrix
        list(set = set, get = get,
             setInverted = setInverted,
             getInverted = getInverted) ##return the function list for storing/printing

}


## This fuction calculates and stores the Inverted matrix of the provided matrix. As by the indtructions, 
## we assume that the provided matrix can always be inverted

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        x.inv <- x$getInverted() ##gets the cached value of the inverted matrix
        if(!is.null(x.inv)) { ##if the value is not NULL, then print a message and then return the cached value
                message("getting cached data")
                return(x.inv)
        }
        data <- x$get() ##if it is NULL (the function does not return above) then get the matrix, and
        m <- solve(data, ...) ##calculate the value of the Inverted, then
        x$setInverted(x.inv) ##cache it and
        x.inv ##return it

}
