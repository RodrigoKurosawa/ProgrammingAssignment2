# Function 1: makeCacheMatrix
# returns an object that has a list with 4 functions.
# These functions get and set values.
# Example: matrix1 <- makeCacheMatrix(matrix(c(1,2,3,4),nrow=2))
# To get the matrix, you can call matrix1$get()
# To get the inverse:  matrix1$getinverse() 
##########
# Function 2: cacheSolve
# returns the inverse of the argument x
# If x has a NULL inverse, then calculates it.
# If the inverse is not NULL, then just return it.
# Example: cacheSolve(matrix1)
# If you are calling it for the first time, it will
# print 'Calculating the inverse'
##########


#Creates an object that is a list of 4 functions.
#Very similar to 'makeVector' from the example
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL
	
	set <- function(matr) {
		x <<- matr
		inverse <<- NULL
	}
	
	get <- function() x
	
	setinverse <- function(inv) inverse <<- inv
	
	getinverse <- function() inverse
	
	#Return the list with the functions
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


#Calculates the inverse if it's NULL
#Otherwise, use the function getinverse: no calculation needed.
cacheSolve <- function(x, ...) {
		inverse <- x$getinverse()
		
		if(!is.null(inverse))
			return(inverse)
		
		matr <- x$get()
		message("Calculating the inverse")
		inverse <- solve(matr)
		x$setinverse(inverse)
		return(inverse)
}
