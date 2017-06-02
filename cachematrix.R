
## The first function, makeCacheMatrix creates a special "vector", which is really a list containing a function to
# set the value of the matrix
# get the value of the matrix
# set the value of the inverse
# get the value of the inverse
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) m <<- solve
    getsolve <- function() m
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}


## The following function calculates the inverse 
## of the special "matrix" created with the makeCacheMatrix function. 
## However, it first checks to see if the inverse has already been calculated. 
## If so, it gets the inverse from the cache and skips the computation. 
## Otherwise, it calculates the inverse of the data and sets the value of 
## the inverse in the cache via the setsolve function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getsolve()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setsolve(m)
    m
}

## Test case

## Get a random matrix
## set.seed(199)
## m <- matrix(rnorm(1000000), 1000,1000)
## solve(m)

## Use makeCacheMatrix() and cacheSolve(), and get the running time
## mymatrix = makeCacheMatrix()
## mymatrix$set(m)
## system.time({
##     cacheSolve(mymatrix)
## })
## system.time({
##     cacheSolve(mymatrix)
## })
## system.time({
##     cacheSolve(mymatrix)
## })

## Running time results
## user  system elapsed 
## 1.946   0.004   1.954 
## getting cached data
## user  system elapsed 
## 0.002   0.001   0.002 
## getting cached data
## user  system elapsed 
## 0.002   0.000   0.002 
