## These functions cache the inverse of a matrix.

## It results in a inverse matrix object. 
## Example: pmatrix <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
## cacheSolve(pmatrix)
## pmatrix$getInverse()


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function() {x}
        setInverse <- function(inverse) {inv <<- inverse}
        getInverse <- function() {inv}
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

cacheSolve <- function(x, ...){
        inv <- x$getInverse()
        if(!is.null(inv)){
                message("obtendo a matriz inversa")
                return(inv)
        }
     mat <- x$get()
     inv <- solve (mat, ...)
     x$setInverse(inv)
     inv
}
