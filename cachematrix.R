## Put comments here that give an overall description of what your
## functions do

# create the object that can cache the inverse of matrix
# Note: Assuming the input is a reversible matrix
makeCacheMatrix <- function(m = matrix()) {
	m_inv <- NULL
	# set function
	setMatrix <- function(y) {
		m <<- y
		m_inv <<- NULL
	}
	# get function
	getMatrix <- function() { m }
	# set inverse function
	setInverse <- function(inv) { m_inv <<- inv }
	# get inverse function
	getInverse <- function() { m_inv }
	# create the object for cache matrix inverse
	list(setMatrix = setMatrix, getMatrix = getMatrix, setInverse = setInverse, getInverse = getInverse)
}


# computes the inverse of object created using function makeCacheMatrix 
# compute and cache inverse of matrix if not done already
# return cached value if value exists in cache
cacheSolve <- function(x, ...) {
	# retrive value from the cache
	m_inv <- x$getInverse()
	# if value exists return
	if (!(is.null(m_inv))) {
		#message("getting cached data")
		return(m_inv)
	}
	# value doesn't exists
	# get matrix data
	data <- x$getMatrix();
	# compute inverse of the matrix and save
	m_inv <- solve(data, ...)
	# set the value in the cache
	x$setInverse(m_inv)
	# return value
	m_inv
}
