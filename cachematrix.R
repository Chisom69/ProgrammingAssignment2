# Creates a special "matrix"
# set the value of the matrix
# get the value of the vector
makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinver <- function(solve) m <<- solve
        getinver <- function() m
        list(set = set, get = get,
             # set the value of the inverse
             setinver = setinver,
             # get the value of the inverse
             getinver = getinver)
}
## Calculates the inverse of the special "matrix" created by above function.
## However, check cache for if previously calculated and skip if true. 
## Otherwise, calculate inverse and 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinver()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinver(m) #sets inverse value in the cache via the setinver function

        m
}
