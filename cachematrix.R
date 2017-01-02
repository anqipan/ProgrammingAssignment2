## Put comments here that give an overall description of what your
## functions do

## Calculate the inverse of a new matrix or get the cached inverse if previously calculated.
## Example:
## my_matrix<-matrix(c(-2,4,1,-3),nrow=2,ncol=2)
## cached_matrix <- makeCacheMatrix(my_matrix)
## cacheSolve(cached_matrix)  ------------- For the first time this function will calculate the inverse.
## cacheSolve(cached_matrix)  ------------- Each time after, the function will directly look up the inverse from the cache.

## Write a short comment describing this function
## makeCacheMatrix creates a special "matrix", which is essentially a list containing four functions:

## 1. set(y), set the value of the matrix to y.
## 2. get(), get the value of the matrix.
## 3. setInverse(i),  set the inverse of the matrix to i.
## 4. getInverse(), get the inverse of the matrix.

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<- inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## Write a short comment describing this function

## The following function calculates the inverse of the special "matrix" created with the above function.
## However, it first checks to see if the inverse has already been calculated. If so, it gets the inverse
## from the cache and skips the computation. Otherwise, it calculates the inverse of the data and sets the
## value of the inverse in the cache via the setInverse function.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    i <- x$getInverse()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setInverse(i)
    i
}
