## This function will creates a matrix and cache it's inverse matrix 
## It will create an ordinary matrix
## Get the value of the matrix
## Create value for inverse matrix
## Get the value of inverse matrix


## Lets create the matrix

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        # Initiate the matrix with null
        set <- function(y) {
                x <<- y    # Set the value
                m <<- NULL # Clear the cache
        }
        # Define function for the values of the matrix
        get <- function() x
        # Define a function to set inverse matrix value
        setInverse <- function(inverse) m <<- inverse
        # Define function to get the inverse
        getInverse <- function() m
        
        # Return a list with the above four functions
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}



## Now we need to cache and solve to get the inverse

cacheSolve <- function(x, ...) {
        m <- x$getInverse() #  The cached value for the inverse
        if(!is.null(m)) { # Get the non-empty cache
                message("getting cached data")
                return(m)
        }
        # Calculate and get cache and inverse
        data <- x$get()  # Get value of matrix
        m <- solve(data) # Calculate inverse
        x$setInverse(m)  # Cache the result
        m                # Return the inverse
}

