## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
# First, create a list of functions which will allow our cacheSolve function
# to interact with a "special" matrix which carries an additional piece of data
# (the inverse value)

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
# In this function, we test whether or not an inverse has been previously
# calculated / stored. If so, we retrieve that value from the special vector.
# If not, it is calculated and then stored for future use. 
# NOTE: the x argument is the "special" vector created by the above...

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        #calculate the inverse because no value has been cached previously
        inv <- solve(data)
        #set in special vector for future use
        x$setinv(inv)
        inv
}
