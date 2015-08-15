## Put comments here that give an overall description of what your
## functions do
## 

# Function to manage the caching of the data ( matrix and results)
# Implement methods to retrieve (get) and to push data to the cache (set)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinverse <- function(solve) m <<- solve
    getinverse <- function() m
    list ( set = set, get = get, 
           setinverse = setinverse, 
           getinverse = getinverse )
}

## Write a short comment describing this function
# This function recieve a Cached Object as a parameter
# Then checks if the matrix inverse has been calculated previously 
# If is not in the cache, the function calculates it and store 
# the results in the cache 

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    }

