## Put comments here that give an overall description of what your
## functions do
## 

# Function to manage the caching of the data ( matrix and results)
# Implement methods to retrieve (get) and to push data to the cache (set)

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    #this function is the method for inserting matrix data in the cache
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    #with this method we'll retrieve the matrix data 
    get <- function() x
    #store the results of the inverse calculation
    setinverse <- function(solve) m <<- solve
    #obtain the results of a previous calculation
    getinverse <- function() m
    #the function return a list with the 4 methods
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
    #First we evaluate if we've cached data for the inverse matrix
    m <- x$getinverse()
    if (!is.null(m)) {
        message("getting cached data")
        #if we have some data we return it 
        return(m)
    }
    # if not ... we obtain the matrix data, calculate the inverse, and store it
    # in the cache
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    # ... and we return the inverse 
    m
    }

