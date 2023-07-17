##### ProgrammingAssignment2 #####

##  Creates a CacheMatrix object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
    hold_inverse <- NULL
    set <- function(y) {
        x <<- y
        hold_inverse <<- NULL
    }
    get <- function() x
    set_inverse <- function(inverse) hold_inverse <<- inverse
    get_inverse <- function() hold_inverse
    list(set = set, get = get,
         set_inverse = set_inverse,
         get_inverse = get_inverse)
}

## Returns a matrix that is the inverse of the matrix held by the CacheMatrix input value (x)
## Computes and saves the matrix inverse value if its inverse has not yet been computed
cacheSolve <- function(x, ...) {
    hold_inverse <- x$get_inverse()
    if(!is.null(hold_inverse)) {
        message("getting cached data")
        return(hold_inverse)
    }
    data <- x$get()
    hold_inverse <- solve(data, ...)
    x$set_inverse(hold_inverse)
    hold_inverse
}

##### HAPPY PATH TESTING #####

# create sample matrix and inverse matrix for verification
og_matrix = matrix(c(1,2,3,4), 2, 2)
og_matrix
#      [,1] [,2]
# [1,]    1    3
# [2,]    2    4
inv_matrix = solve(og_matrix)
inv_matrix
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

# validate initial values
og_matrix %*% inv_matrix
#      [,1] [,2]
# [1,]    1    0
# [2,]    0    1

m1 <- makeCacheMatrix(og_matrix)
cacheSolve(m1)
# verified - same as inv_matrix above
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5

cacheSolve(m1)
# verified - returns cached version
# getting cached data
#      [,1] [,2]
# [1,]   -2  1.5
# [2,]    1 -0.5
