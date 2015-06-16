## These function are designed to aid in the calculation of a matrix inverse.  They
## work together to create a cached version of the candidate matrix 'x' and the 
## calculated inverse 'inv'.

## The first function 'makeCacheMatrix' constructs a list of sub-functions that aid 
## in the cache calculation that takes place in the 'cacheSolve' function below.

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL                                     # resets inverse cache 'inv'
        set <- function(y) {                            # 1:
                x <<- y                                 # puts matrix in cache 'x' and
                inv <<- NULL                            # clears cached inverse 'inv'
        }
        get <- function() x                             # 2: gets cached matrix 'x'
        setInverse <- function(inverse) inv <<- inverse # 3: sets cached inverse 'inv'
        getInverse <- function() inv                    # 4: gets cached inverse 'inv'
        list(set = set, get = get,                      # creates list of sub-functions
             setInverse = setInverse,                   #   1-4 referenced by the
             getInverse = getInverse)                   #   'cacheSolve' function below
}


## The second function 'cacheSolve' works with 'makeCacheMatrix' to solve the inverse
## of the matrix 'x' that was stored in that first function.  The 'cacheSolve' function
## first checks to see if the inverse of the cached matrix has already been calculated
## and stored in the cache called 'inv'.  If so, it returns that cached inverse with a 
## message that it is reporting a cached value.  If no cached inverse exists, it 
## calculates the inverse of the matrix stored in the 'makeCacheMatrix' function, then 
## stores that calculated inverse matrix in the cache, and finally reports the calculated
## inverse matrix to the console.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse()                     # grabs cached inverse value
        if(!is.null(inv)) {                       # checks cached inverse is not null
                message("getting cached data")    # if so, says cached inverse exists
                return(inv)                       # and returns cached inverse
        }
        data <- x$get()                           # otherwise, grabs cached matrix
        inv <- solve(data, ...)                   # and calculates the inverse
        x$setInverse(inv)                         # then stores that inverse in cache
        inv                                       # and returns calculated inverse
}
