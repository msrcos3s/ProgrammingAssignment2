## These functions cache the inverse of a matrix.

## It results in a inverse matrix object. 
## Example: pmatrix <- makeCacheMatrix(matrix(1:4, nrow=2, ncol=2))
## cacheSolve(pmatrix)
## pmatrix$getInverse()


makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL    # setting the value to NULL to be used later
        set <- function(y){     # creating the function for the new matrix. 
                x <<- y
                inv <<- NULL   # clearing cached value
        }
        get <- function() {x}  # retrieves value of x from the environment
        setInverse <- function(inverse) {inv <<- inverse}  # assigns the value of the invervse matrix
        getInverse <- function() {inv}  # retrieves the value of the inverse matrix 
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  # outputs a list of objects that can be used by downstream functions
}

cacheSolve <- function(x, ...){   # defining a function that will populate and retrieve values from makeCacheMatrix
        inv <- x$getInverse()  # retrieves the inverse matrix from makeCacheMatrix
        if(!is.null(inv)){
                message("obtendo a matriz inversa")  # if there is an inverse matrix, returns that matrix
                return(inv)
        }
     mat <- x$get()   # if there is no inverse matrix in makeCacheMatrix, solves for the inverse matrix
     inv <- solve (mat, ...)
     x$setInverse(inv)
     inv  # print inverse matrix
}
