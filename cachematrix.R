## "makeCacheMatrix" function creates a special object that stores a matrix and cache's its inverse.
## It creates a list object that stores functions to :
##	- set the value of the matrix
## 	- get the value of the matrix
## 	- set the value of the matrix inverse
## 	- get the value of the matrix inverse

makeCacheMatrix <- function(matrix = matrix()) {

		# Initialize inverse value with NULL
		inv <- NULL

		# function to set value of the matrix
        	set <- function(newMatrix) {
                matrix <<- newMatrix
                inv <<- NULL
        	}

		# function to get value of the matrix
        	get <- function() { 
			matrix
		}

		# function to set value of the inverse of the matrix
        	setinverse <- function(matrixInverse) {
			inv  <<- matrixInverse
		}

		# function to get value of the inverse of the matrix
        	getinverse <- function() {
			inv
		}

		# List of all the functions
        	list(set = set, get = get,
            setinverse = setinverse,
            getinverse = getinverse)
}




## "cacheSolve" function computes the inverse of the given matrix. 
## If the matrix has not changed and inverse has already been calculated and stored in the cache 
## then this method retrieves the inverse from the cache. If the inverse is not found in cache 
## then this function calculates the inverse, stores it in the cache and returns it.

cacheSolve <- function(matrix, ...) {
		
		# Get value of the inverse from cache
	  	inv <- matrix$getinverse()

		# If value is available in cache, return it.
        	if(!is.null(inv)) {
                	message("getting cached inverse of given matrix")
                	return(inv)
        	}

		# Get data of the given matrix in a local variable.
        	data <- matrix$get()

		# Calculate inverse of the matrix.
        	inv <- solve(data, ...)

		# Store value of inverse in the cache using setinverse() function.
        	matrix$setinverse(inv)
		
	 	## Return the inverse of 'matrix'
       	inv

}

## Execute below commands to test these functions -
## 		> b <- makeCacheMatrix(my_matrix)
## 		> cacheSolve(b)
## Here "my_matrix" is a square invertible matrix.