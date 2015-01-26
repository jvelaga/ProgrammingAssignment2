## Put comments here that give an overall description of what your
## functions do

## intakes a matrix and keeps in cache
makeCacheMatrix <- function(x = matrix()) 
{
    cacheI <- NULL
    
    set <- function(y)
    {
        x <<- y
        cacheI <<- NULL
    }
    get <- function() x
    
    setCache <- function(y) cacheI <<- y
    getCache <- function() cacheI
    
    list(set = set, get = get, setCache = setCache, getCache = getCache)
}


## For a matrix that is not in cache creates and returns inverse matrix.
cacheSolve <- function(x, ...) 
{
    z <- makeCacheMatrix(x)
        ## Return a matrix that is the inverse of 'x'
    if(!is.null(z$getCache())) 
            return(z$getCache())
       else
           z$setCache(solve(z$get()))
}

