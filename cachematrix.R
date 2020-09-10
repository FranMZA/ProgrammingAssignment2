## Assignment: Caching the Inverse of a Matrix
## 
## Your assignment is to write a pair of functions that cache the 
## inverse of a matrix. Write the following functions:
## 
## 1 - makeCacheMatrix: This function creates a special "matrix" object that 
##     can cache its inverse.
## 2 - cacheSolve: This function computes the inverse of the special "matrix" 
##     returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.


## makeCacheMatrix
## - Creates an object that holds a matrix and it's inverse
## - Receives a numeric matrix as argument and returns a list with 
##   the getters and setters
## - Does NOT check if the matrix is invertible
## - Does NOT check if the inverse matrix values are correct

makeCacheMatrix <- function(mat_data = matrix()) {
        inv_matrix <- NULL
        set <- function(new_mat){
                mat_data <<- new_mat
                inv_matrix <<- NULL
        }
        get <- function() {mat_data}
        set_inv <- function(new_inv) {
                inv_matrix <<- new_inv
        }
        get_inv <- function() {inv_matrix}
        list(set = set, get = get, set_inv = set_inv, get_inv = get_inv)
}

##cacheSolve
## - Shows the cached inverse matrix of a matrix created with 'makeCacheMatrix'
## - If the matrix does not have an inverse cached, the function calculates it
## - In any case, returns the inverse matrix

cacheSolve <- function(mat_obj, ...) {
        inv_matrix <- mat_obj$get_inv()
        if (!is.null(inv_matrix)) {
                message("getting cached data")
                return(inv_matrix)
        } 
        mat_data <- mat_obj$get()
        inv_matrix <- solve(mat_data, ...)
        mat_obj$set_inv(inv_matrix)
        
        inv_matrix
}

## TEST #############################
my_matrix <- makeCacheMatrix(matrix(data = c(1, -1, 1, 2), nrow = 2, ncol = 2))

my_matrix$get()
my_matrix$get_inv()
cacheSolve(my_matrix)
my_matrix$get_inv()
cacheSolve(my_matrix)

my_matrix$set(matrix(c(1, -2, 2, 1), 2, 2))
my_matrix$get_inv()
cacheSolve(my_matrix)
my_matrix$set_inv(NULL)
cacheSolve(my_matrix)
