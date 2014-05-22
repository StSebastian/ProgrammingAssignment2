## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	inv <- NULL
	set <- function (y) {
		x   <<- y
		inv <<- NULL
	}
	get <- function() x
	setinv <- function(x) inv <<- solve(x) 
	getinv <- function() inv
	list(set=set, get=get, setinv=setinv, getinv=getinv)
}
## Write a short comment describing this function

cacheSolve <- function(x, ...) {
      inv <- x$getinv()
	if(!is.null(inv))  {
			message("getting cached data")
			return(inv)
	}
	data <- x$get()
	x$setinv(data)
	x$getinv()
	## Return a matrix that is the inverse of 'x'
}

a<-matrix(c(3,2,6,4),nrow=2)
Fehler in solve.default(x) : 
  System ist für den Rechner singulär: reziproke Konditionszahl = 7.40149e-18

