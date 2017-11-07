#########################################
## HOW TO RUN THE PROGRAM?
#########################################
##source("cachematrix.R")
##mamat<-makeCacheMatrix(matrix(1:4,2,2))
##cacheSolve(mamat)


#########################################
## CODE 
#########################################

##Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## Your assignment is to write a pair of functions that cache the inverse of a matrix.

## makeCacheMatrix define getter and setter and assigns each of these functions as an element of a list
## makeCacheMatrix is used by cacheSolve
makeCacheMatrix <- function(x = matrix()) {
        # initialization of variable i to NULL
        i <- NULL

        # assignment of the value on the right to an object in the parent environment
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) i <<- inverse
        getInverse <- function() i

        # function return
        list(	set = set
				,get = get
				,setInverse = setInverse
				,getInverse = getInverse
				)
} ## fin makeCacheMatrix

## Return a matrix that is the inverse of the parameter
##if in cache, return the pre-calculated value, else calculate the inverse

cacheSolve <- function(x, ...) {
        ## check if value exists in cache
        i <- x$getInverse()

		## if exists, return the value 
        if (!is.null(i)) {
                message("Getting cached data")
                return(i)
        }

        # if not exists, calculate the value
        matrix <- x$get()
		i <- solve(matrix, ...)
        x$setInverse(i)
		message("Calculating the inverse")
        return (i)
} ## fin cacheSolve
