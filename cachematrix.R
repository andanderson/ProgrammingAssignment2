## The following funcions (makeCacheMatrix and cacheSolve) need one another to work.

## The makeCacheMatrix function creates a special list containing functions to
#set the value of a matrix
#get the value of the matrix
#set the value of the inverse of the matrix
#get the value of the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        #The set function will only be used if we want to reset the original matrix (x).
        set <- function (y){
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list( set = set,get = get, setinv = setinv, getinv = getinv)
}

##The cacheSolve funcion calculates the inverse of the matrix of the special matrix created with the above function.
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation.
#Otherwise, it calculates the inverse of the data (which is a matrix) and sets the value of it's inverse in the cache via the setinv function.

cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data,...)
        x$setinv(inv)
        inv
        ## Returns a matrix that is the inverse of 'x'
}

#Testing the results
mat <- matrix(c(-1, -2, 1, 1), 2,2)

x <- makeCacheMatrix(mat)
x$get()

inver<-cacheSolve(x)
inver

