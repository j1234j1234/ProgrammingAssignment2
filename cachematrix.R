#The makeCacheMatrix function creates a special "matrix", 
#which is really a list containing a function to
#1) set the value of the matrix
#2) get the value of the matrix
#3) set the value of the matrix inverse
#4) get the value of the matrix inverse

makeCacheMatrix <- function(x = matrix()) {

        #The first time the matrix is initialised, the inverse is initialised as NULL. 
        inverse <- NULL

        #Calling the set function will update the matrix as per the input argument. 
        #At the same time the stored inverse will be reset, since it has not been calculated. 
        set <- function(y) {
                x <<- y
                inverse <<- NULL
        }

        #Calling the get function will return the inputted matrix. 
        get <- function() x

        #The setinverse function should never be called by the user. 
        #This function is used to update the inverse stored in the special "matrix"
        #when it is calculated by the cacheSolve function. 
        setinverse <- function(solve) inverse <<- solve
        
        #The getinverse function will return the matrix inverse if it has been calculated, 
        #using the cacheSolve function, or return NULL otherwise. 
        getinverse <- function() inverse

        #The list function below sets the four values in the special "matrix" 
        #as per the function description. 
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
        
}



#The cacheSolve function calculates the matrix inverse of the special 
#"matrix" created with the makeCacheMatrix function. 
#However, it first checks to see if the inverse has already been calculated. 
#If so, it gets the inverse from the cache and skips the computation. 
#Otherwise, it calculates the inverse of the data and sets the value of 
#the inverse in the cache via the setinverse function.
#This function assumes that the given matrix is invertible.

cacheSolve <- function(x, ...) {
        ## Returns a matrix that is the inverse of 'x'

        ##First extract the stored value of the inverse from the special matrix.        
        inverse <- x$getinverse()

        #If the inverse has previously been calculated, it is extracted 
        #from the cache and returned. 
        if(!is.null(inverse)) {
                message("getting cached data")
                return(inverse)
        }

        #Otherwise, we calculate the matrix inverse and store it in the cache. 

        #First, extract the actual matrix 'x' that we want to invert.  
        data <- x$get()
        #Apply the solve function to calculate the matrix inverse. 
        inverse <- solve(data, ...)
        #Store the calculated inverse in the cache. 
        x$setinverse(inverse)
        #Return the calculated inverse. 
        inverse
}


