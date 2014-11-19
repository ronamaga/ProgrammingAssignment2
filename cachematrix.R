##      Hello there,

##              Thanks for taking time to evaluate this assignment.

##              All the best!

##      Ronald



## When you create a matrix object with makeCacheMatrix(m), in which 'm' is a
## square matrix with non-zero determinant (i.e. an invertible matrix), and call
## 'cacheSolve(<the recently created matrix object>)' it will return 'm' inverted
## either by calculating it using the solve() function or retrieving cached data
## if the computing was already done. Pretty cool idea!!



## This code creates a matrix object and  functions to i)store the matrix to be
## inverted, ii)read it back, iii)calculate and iv)retrieve an inverted one.

makeCacheMatrix <- function(z = matrix()) {
        i <- NULL
        definematrix <- function(w){
                x <<- w
                i <<- NULL
        }
        getmatrix <- function() z
        defineinverted <- function(inverted) i <<- inverted
        getinverted <- function() i
        list(definematrix = definematrix, 
             getmatrix = getmatrix, 
             defineinverted = defineinverted, 
             getinverted = getinverted)
}



## The next function calls the matrix object created with the one above, checks if
## the inverted matrix already exists and retrieves it, or calculates the inverted
## matrix if it was done already. It also prompts a message if the matrix to be
## inverted is either not square or has a zero determinant (non-invertible)

cacheSolve <- function(x, ...) {
        
        i <- x$getinverted()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        thematrix <- x$getmatrix()

        
        ## This next 'if' checks if the matrix can be inverted:

        
        if((is.matrix(thematrix))&&(ncol(thematrix) == nrow(thematrix))&&!(det(thematrix) == 0)) {
                i <- solve(thematrix,...)              
        }else{
                message("Not a matrix or the matrix is either not invertible or not square")
                return(i)
        }
        
        x$defineinverted(i)
        i
}

## You may source the script above and try:

## mymatrix <- makeCacheMatrix(matrix(runif(16),4,4))
## cacheSolve(mymatrix)

## and again:

## cacheSolve(mymatrix)

## to see it working