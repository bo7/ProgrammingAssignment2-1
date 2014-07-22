## The following two functions
## are designed for calculating the inverse of a matrix
## and storing this in a cache
## the purpose is that the recalculation of
## an inversed matrix could be time spending
## therefore it is possible to check wether the calculation is already done
## functions do

## makeCacheMAtrix has getter and setter methods for creating an instance of a matrix
## and getter and setter methods for the inversed matrix. 
## The initial constructor for an instance is the parameter x in the makeCacheMAtrix
## with the set and get method the instance can be changed
## with setinverse and get inverse methods the inversed matrix gan be handled

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL ## cahced inverse mattrix
        set <- function(y) {
                ## set new matrix on same instance
                x <<- y
                inv <<- NULL # deltes before calculated inverse matrix
        }
        get <- function() x ## return none inversed matrix
        setinverse <- function(inverse) inv <<- inverse ## set new calculated inverse (calculation in cacheSolve)
        getinverse <- function() inv ## returns invers matrix, if is null calculation will be done in cacheSolve
        list(set = set, get = get, ## return getter and setter methods
             setinverse = setinverse,
             getinverse = getinverse)

}


## this function gets an instance of makeCacheMatrix
## checks if the inversed matrix is already calculated
## if yes then the cached one will be returned from the received instance
## if not the inverse will be calcultated and sdtored in the makeCacheMatrix instance

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
