## As the inverse of a matrix may take a while to process, the following 
## code firstly calls the makeCacheMatrix function then uses output from 
## this function as the parameter for the second function, cacheSolve.
## The second function assesses if the same matrix was previously inverted and 
## if so, retrieves the cached list, otherwise it inverts the new matrix.

## Error will be returned if matrix cannot be inverted ie singular

## The makeCacheMatrix function creates a special "matrix" object that can cache
## its inverse.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) i <<- solve
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)  
}


## The cacheSolve function computes the inverse of the special "matrix" returned
## by the makeCacheMatrix function above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getinverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
    i
}

My_matrix <- matrix(4:1,2,2) # create a matrix
Output_MCM <- makeCacheMatrix(My_matrix) # Pass matrix to first function
cacheSolve(Output_MCM) # first run so should not show "getting cached data"
cacheSolve(Output_MCM) # second run so shows message "getting cached data"
