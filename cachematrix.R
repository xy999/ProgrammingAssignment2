## Put comments here that give an overall description of what your
## functions do

## The "makeCacheMatrix" function creates a special "vector", which is really a list containing a function to do the following: set the value of the vector; get the value of the vector; set the value of the mean; and get the value of the mean

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function (y) { # sets value of vector
                x <<- y
                m <<- NULL
        }
        get <- function() x # gets value of vector
        setinv <- function(inverse) m <<- inverse # sets inverse of vector
        getinv <- function() m # gets inverse of vector
        list(set = set, get = get, setinv = setinv, getinv = getinv) # creates a list with the preceding elements
}


## Calculates the mean of the special "vector" created with the preceding function. But first, it checks to see if the mean has already been calculated. If so, it gets the mean from the cache and skips the computation. Otherwise, it calculates the mean of the data and sets the value of the mean in the cache via the setmean function.

cacheSolve <- function(x, ...) {
        m <- x$getinv()
        if(!is.null(m)) { # check to see if mean has already been calculated
                message("getting cached data")
                return(m) # return mean from cache
        }
        data <- x$get() 
        m <- solve(data, ...) 
        x$setinv(m)
        return(m) # returns m
}
