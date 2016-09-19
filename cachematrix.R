#makeCacheMatrix is a function that creates a kind of special matrix, one that can be cached. 
#While cacheSolve was created to calculate the inverse  of a square matrix, and if the inverse has 
#already been calculated it returns the inverse matrix from memory, instead of computing all over again. 


#On makeCacheMatrix 2 variables are created. The "x" variable is a variable setted by default as matrix and is used as argument in this
#function. "i" is the inverse of the matrix. 
#Four others functions are also defined in the makeCacheMatrix function, one for setting the matrix and its inverse, 
#and the other one for their retrieval. When a new matrix is set,
# "i" is set to null so that a new inverse has to be calculated.
#the list at the end is used to organize all the values into a list


makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    get <- function() x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)

}



# cacheSolve uses the solve function to compute the inverse of a matrix(that was set on makeCacheMatrix). If the value of i,
#the inverse of the matrix, is not equal to NULL (meaning that it already have a inverse) then cacheSolve will return the 
#stored value of i.  If is equal to null, then it will calculate the inverse of the matrix and return it. 

cacheSolve <- function(x, ...) {
    i <- x$getinverse()
    if(!is.null(i)) {
        print("The cached data is: ")
        return(i)
    }
    
    else{
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
    }
    
}

