#makeCacheMatrix: This function creates a special "matrix" object 
##that can cache its inverse.

# makeCacheMatrix(mat)$set(mat)
# makeCacheMatrix(mat)$get()
# makeCacheMatrix(mat)$setinverse(solve(mat))


makeCacheMatrix <- function(x = matrix()) {
        #inv = matrix()
        set <- function(y = matrix()) {
                x <<- y
                inv <<- matrix()
        }
        get <- function() x
        setinverse <- function(inverse = matrix()){  
                inv <<- inverse
                print(inv)
        }
        getinverse <- function() inv
        list(set=set, get=get,setinverse=setinverse,getinverse=getinverse)
}

# cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.
# Computing the inverse of a square matrix can be done with the solve 
## function in R. For example, if X is a square invertible matrix, then 
## solve(X) returns its inverse.

# cacheSolve(makeCacheMatrix(mat))

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data)
        x$setinverse(inv)
        inv
}
