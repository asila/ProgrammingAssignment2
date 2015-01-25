##  This function is able to cache time consuming inverse computations of a matrix. Which includes two functions
## 

##1. This function creates a special "matrix" object then cache the matrix to its inverse.

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y){
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setmatrix <- function (solve) m <<- solve
	getmatrix <- function( ) m
	list (set = set, get = get,
		setmatrix = setmatrix,
		getmatrix = getmatrix)
	}

##2. Write a short comment describing this function

cacheSolve <- function(x=matrix(), ...){
    		m <- x$getmatrix()
		if(!is.null(m)){
			message("getting cached matrix")
			return(m)
		}
		matrix <- x$get()
		  ## Return a matrix that is the inverse of 'x'
		  m <- solve(matrix, ...)
		  x$setmatrix(m)
		  m
}


#Example
a<-makeCacheMatrix()
aa<-c(8.7,2.4,1.5,7,3.7,1.3,15,1.7,7.4,4,1,4.7,8.7,2,5,2.7)/100
a$set(matrix(aa,4,4))
cacheSolve(a)
