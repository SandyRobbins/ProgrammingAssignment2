##1.  `makeCacheMatrix`: This function creates a special "matrix" object
##that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## `cacheSolve`: This function computes the inverse of the special
   ## "matrix" returned by `makeCacheMatrix` above. If the inverse has
   ## already been calculated (and the matrix has not changed), then
    ##`cacheSolve` should retrieve the inverse from the cache.


## Return a matrix that is the inverse of 'x'   
cacheSolve <- function(x) {
    m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data)
    x$setinverse(m)
    m
}

## Example usage of the above functions
#exampleMatrix <- matrix(runif(16,1,10),nrow=4,byrow=T)
#print("Example matrix:")
#print(exampleMatrix)
#
#cachedMatrix <- makeCacheMatrix(exampleMatrix)
#inverse <- cacheSolve(cachedMatrix)
#print("(cached) Inverse matrix:")
#print(inverse)
#
#print("Proof this works (below should be identity matrix):")
#print(inverse %*% exampleMatrix)
#
#print("Now we retrive cached matrix again and you should see it gets it from cache rather than calculating it again:")
#print(cacheSolve(cachedMatrix))

#A more efficient way of doing this
#savex <- NULL
#play <- function(x = matrix()) {
#    if (is.null(savex) || !setequal(savex, x)) {
#        print("Updating")
#        y <<- solve(x)
#        savex <<- x
#    }
#    y
#}
