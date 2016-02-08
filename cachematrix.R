## The x in makeCacheMatrix(x) should be a matrix which you want to
## get its inverse matrix
## If then you use cacheSolve(something) to get something's inverse
## AND if sth. is as SAME as x, then this function will skip the
## caculation and directly get the cached inverse matrix
## Otherwise, cacheSolve(x) could inverse any invertible matrix x
## as usual

## This function will return a larger matrix within inverse x and
## x itself

makeCacheMatrix <- function(x = matrix())
{
        cm <- matrix(nrow = 2 * nrow(x), ncol = ncol(x))
        cm[1:nrow(x), 1:ncol(x)] <- solve(x)
        cm[(nrow(x) + 1):nrow(cm), 1:ncol(x)] <- x
        c_m <<- cm
}


## This function will first compare input matrix to cached data, if
## the matrix isn't changed, it will skip the caculation instead of
## directly getting the cached inverse matrix

cacheSolve <- function(x, ...)
{
        if(identical(x, c_m[(nrow(x) + 1):nrow(c_m), 1:ncol(c_m)]))
        {
                message("getting cached data...")
                return(c_m[1:(0.5 * nrow(c_m)), 1:ncol(c_m)])
        }
        else
        {
                return(solve(x))
        }
}

## Because an invertible matrix maybe a little triky to find, there
## is a 3*3 one for your test:
## A <- matrix(c(1, -3, 1, -3, 0, 1, 2, 1, -1), nrow = 3, ncol = 3)
## and the inverse A is as follow:
## B <- matrix(c(1, 2, 3, 1, 3, 4, 3, 7, 9), nrow = 3, ncol = 3)