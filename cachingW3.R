###########################################################################
## Function makeCacheMatrix creates a special "matrix",                   #
## that has a list containing 4 functions:                                # 
## 1)set the value of the input matrix (set_matrix)                       #  
## 2)get the value of the input matrix (get_matrix)                       #
## 3)set the value of the inverse matrix (set_inverse)                    #
## 4)get the value of the inverse matrix (get_inverse)                    #
###########################################################################
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL 			## Assigning 'NULL' to inverse
	set_matrix <- function(y) {			
		x <<- y 			## Setting the matrix 'x'
		inverse <<- NULL
	}
	get_matrix <- function() x 	## Returning matrix 'x'
	set_inverse <- function(solve) inverse <<- solve ## Cache inverse 
	get_inverse <- function() inverse 			 ## Returning inverse
	list(set_matrix = set_matrix, get_matrix = get_matrix,
	     set_inverse = set_inverse,
	     get_inverse = get_inverse)
}

## CacheSolve calculates the inverse of the special "matrix" created with makeCacheMatrix function.
## It checks if the inverse has been calculated. 
## If yes it gets the inverse from the cache and skips computation. 
## Otherwise, it calculates the inverse matrix and sets the inverse in the cache  
## using 'set_inverse' function.

cacheSolve <- function(x, ...) {				## Setting inverse matrix
	inverse <- x$get_inverse()				## Getting inverse
	if(!is.null(inverse)) {					## Checking if inverse already computed
		message("getting cached data")			## Displaying cached message
		return(inverse)
	}
	data <- x$get_matrix()					## Getting Matrix
	inverse <- solve(data, ...)				## Computing inverse by using solve() 
	x$set_inverse(inverse)					## To cache the inverse
	inverse 						## Returning the inverse
}

m <- matrix(c(-1, -2, 1, 1,5,7,0,9,-10), 3,3)
# m input matrix 3*3 that is invertible
m  #displays input matrix
x <- makeCacheMatrix(m)
x$get_matrix()#displays original matrix
inv <- cacheSolve(x) #calcualtes inverse if cachematriz  doesn’t exist
inv <- cacheSolve(x) #Takes the value from cache skipping computation
inv    # displays inverse
inv %*% m   #multiply inverse with original input matrix m. Result is I(3)      
