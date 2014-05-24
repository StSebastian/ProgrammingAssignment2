## Function 'makeCacheMatrix' takes a matrix as an input argument 
## and returns an object of type 'list'. In addition it stores the matrix in
## the variable 'x' and its inverse in the variable 'inv'. The list object 
## which is returned by the function is actually a list of four other functions.
## This four functions can be used to retrieve and set the values of 'x' (the
## matrix) and 'inv' (its inverse). 

## Function 'cacheSolve' takes an object 'z' created by the function makeCacheMatrix 
## as input argument. If the inverse 'inv' hasn't been calculated and stored in object 'z') 
## it will be computed, returned and stored it the variable 'inv' of object 'z'.
## Otherwise the message 'getting cached data' will be returned together with the stored
## inverse 'inv'.


## When using 'makeCacheMatrix' it should be assigned to a variable 'DEMO' which
## can be used to:
## retrieve the matrix 'x'   --> DEMO$get() 
## retrieve its inverse 'inv'   --> DEMO$getinv()
## set the matrix 'x' and delete the inverse 'inv'  --> DEMO$set(new matrix)
## set the inverse 'inv'  --> DEMO$setinv(inverse matrix) 

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function (y) {
		x   <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(x) inv <<- (x) 
	getinv <- function() inv
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}



## Function 'cacheSolve' takes an object 'z' created by the function makeCacheMatrix as 
## input argument. If the inverse 'inv' hasn't been calculated yet (and stored in object 'z') 
## the inverse 'inv' of matrix 'x' (of object 'z') will be computed, returned and stored 
## in the variable 'inv' of object 'z'. Otherwise the message 'getting cached data' will 
## be returned followed by the inverse 'inv'.

cacheSolve <- function(z, ...) {
      inv <- z$getinv()
	if(!is.null(inv))  {
			message("getting cached data")
			return(inv)
	}
	data <- z$get()
	z$setinv(solve(data))
	z$getinv()
}
