## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    inverted <- NULL
    #cache the matrix
    set <- function(y) {
        x <<- y
        inverted <<- NULL
    }
    #get the matrix
    get <- function() x
    setInverted <- function(inv) inverted <<- inv  #cache the inverted matrix
    getInverted <- function() inverted #get the inverted matrix
    list(set = set, get = get, setInverted = setInverted, getInverted = getInverted)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inverted <- x$getInverted() #assign inverted matrix to a variable and return the value if it's not NULL
    if(!is.null(inverted)) {    
        message("getting cached data")
        return(inverted)
    }
    data <- x$get() #get the matrix from cache
    inverted <- solve(data, ...) #calculate inverted matrix
    x$setInverted(inverted) #cache the inverted matrix
}

#test case
newx <- makeCacheMatrix(matrix(5:12,nrow = 3, ncol = 3)) #create sample matrix
newx$get() #check the created matrix
newx$getInverted() #check the inverted matrix (should be NULL)
cacheSolve(newx) #create the inverted matrix
newx$getInverted() #check the inverted matrix again (should have values)
