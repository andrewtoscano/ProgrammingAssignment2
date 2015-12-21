## This program encapsulates a matrix and its inverse in an object accessible with get
## and set methods. The inverse of the matrix should only be calculated one time
## unless the matrix is changed.


## makeCacheMatrix creates an object that can store a matrix and an arbitrary value
## accessible with set and get methods. We use this to store the inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
	inverseMatrix <- NULL
	set <- function(y) {
		x <<- y
		inverseMatrix <<- NULL
	}
	get <- function(){
		x
	}
	setinverse <- function(inverse){
		inverseMatrix <<- inverse
	}
	getinverse <- function(){
		inverseMatrix
	}
	list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve is a method that checks the object created to see if the
## inverse matrix is stored. If not, it calculates the inverse and sets it.

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
	inverse <-x$getinverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}
	data <- x$get()
	inverse <- solve(data, ...)
	x$setinverse(inverse)
	inverse
}