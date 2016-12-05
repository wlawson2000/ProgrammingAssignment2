## This function returns an object of type "makeCacheMatrix "
## This object has four prototypes , two that set the value of the matrix and get this value
## And two prototypes that set the value of the inverse and get the value of the inverse
## These four prototypes are returned in a list



makeCacheMatrix <- function(x = matrix()) {
        Inv <- NULL
        set <- function(y) {
                x <<- y
                Inv <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) Inv <<- inverse
        getinverse <- function() Inv
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## The following function calculates the inverse of the object "makeCacheMatrx"
## But it first checks to see if the inverse has already been calculated
## If this is the case it gets the inverse from the cache
## Otherwise it calculates the inverse of the data and sets the value of the inverse in the cache 
## Via the setinverse prototype


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        Inv <- x$getinverse()
        if(!is.null(Inv)) {
                message("getting cached matrix")
                return(Inv)
        }
        Mat <- x$get()
        Inv <- solve(Mat, ...)
        x$setinverse(Inv)
        Inv
}