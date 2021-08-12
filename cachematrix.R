#This function creates a special "matrix" that can cache its inverse such that 
#it can both set and get the matrix as wella s its inverse.
makeCacheMatrix <- function(x = matrix()) {

        #initializing the inverse matrix
        im <- NULL
        
        #Setting and storing the matrix in cache
        set_matrix <- function(y) {
                x <<- y
                im <<- NULL
        }

        #Obtaining the matrix from cache memory
        get_matrix <- function() {
                x
        }

        #Setting and storing the inverse matrix in cache
        set_inverse_matrix <- function(z) {
                im <<- z
        }

        #Getting the inverse matrix from cache
        get_inverse_matrix <- function() {
                im
        }

        #The final list containing the various functions to set and get the matrix and its inverse
        list(set = set, get = get,
             set_inverse_matrix = set_inverse_matrix,
             get_inverse_matrix = get_inverse_matrix)
}


#The following function calculates the inverse of the special matrix created above.
#It checks to see if the inverse matrix already exists in cache in which case it retrieves that.
#Otherwise it calculates the inverse and stores it in cache
cacheSolve <- function(x, ...) {
        
        ## Return a matrix that is the inverse of 'x'
        m <- x$get_inverse_matrix()

        #Condition to check if the inverse already exists in cache
        if(!is.null(m)) {
                message("Retrieving cahced inverse matrix")
                #Retrieving the pre existing inverse from cache
                return(m)
        }

        #First get the matrix from cache, then calculate its inverse using the solve() function.
        stored_data <- x$get()
        m <- solve(stored_data,...) %*% stored_data
        x$set_inverse_matrix(m)
        m
}
