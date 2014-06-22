## This program consists of two functions makeCacheMatrix that stores the matrix inverse object 
## and provides the cached matrix inverse object when asked and cacheSolve function that 
## that creates the inverse of a matrix object if its new or return the inverse object that 
## makeCacheMatrix has stored already

## makeCacheMatrix is a function that creates a matrix object.
## returns a matrix inverse object if it has one already and sets a matrix inverse object 
## It also lists two functions to create a matrix with set method and get the matrix
## created with get method.
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## This functions checks and returns the inverse of the matrix object if there is 
## a inverse of the matrix object stored before (cached) and creates the inverse
## of the matrix object if its not.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
    
}
