# R6 vignettes:

library(R6)

# A simple example of an R6 instantiation
Simple <- R6Class("Simple",
            public = list(
              x = 10,
              getx = function() self$x
            )
          )

s <- Simple$new()

s$getx()  

# Debugging
# https://cran.r-project.org/web/packages/R6/vignettes/Debugging.html
# we start debugging by using the debug() method

Simple$debug("getx") # <- debug the getx method
s <- Simple$new()
s$getx()

# we stop debugging by using undebug()
Simple$undebug("getx")
s <- Simple$new()
s$getx()

# And what if its an individual object that we need to debug?
debug(s$getx)
s$getx()
# and turn it off again using undebug()
undebug(s$getx)
s$getx()


# An introduction to R6
# https://cran.r-project.org/web/packages/R6/vignettes/Introduction.html

Person <- R6Class("Person",
                  # start listing public attributes (private also an option)
                  public = list(
                    name = NULL,  # these are our attributes
                    hair = NULL,
                    # when the class is initialized, give it some initial values for name and hair
                    initialize = function(name = NA, hair = NA){ 
                      self$name <- name
                      self$hair <- hair
                      self$greet() # a method defined below
                    },
                    # a function that lets you change the hair color
                    set_hair = function(val){
                      self$hair <- val
                    },
                    # a function that prints a statement, called upon initialization
                    greet = function() {
                      cat(paste0("Hello, my name is ", self$name, ".\n"))
                    }
                  ) # end listing public attributes
                )
# An instantiation of the class using the $new() method
ann <- Person$new("Ann", "black")
ann
ann$hair # return an attribute
ann$set_hair("blue") # use a public method
ann$hair


# Another example, using private members:
Queue <- R6Class("Queue",
                 public = list(
                   # create a list of arbitrary length
                   initialize = function(...) {
                     for (item in list(...)) {
                       self$add(item)
                     }
                   },
                   add = function(x){
                     # add something to queue
                     private$queue <- c(private$queue, list(x))
                     invisible(self)
                   },
                   remove = function(){
                     # functions much the same as pop(), where we remove the first item from the queue
                     if (private$length() == 0) return(NULL)
                     # Can use private$queue for explicit access
                     head <- private$queue[[1]]
                     private$queue <- private$queue[-1]
                     head
                   }
                 ),
                 private = list(
                   queue = list(),
                   length = function() base::length(private$queue)
                 )
                 )
q <- Queue$new(5,6, "foo")

# Public members can be accessed
q$add("something...")
q$remove()
# Private members cannot be accessed directly, only indirectly? Why?
q$queue


# Another example, with active bindings - these look like fields but every time they are accessed they call a function
# Active bindings are always public
Nubmers <- R6Class("Numbers",
                   public = list(
                     x = 100
                   ),
                   active = list(
                     x2= function(value) {
                       if (missing(value)) return(self$x * 2)
                       else self$x <- value/2
                     },
                     rand = function() rnorm(1)
                   )
                   )

n <- Nubmers$new()
n$x
# The first time we call the active binding x2, the value is missing, and so the first clause works
n$x2
# If we then assign a value to it, the second clause will wokr
n$x2 <- 1000
n$x2
n$x


# Inheritance
HistoryQueue <- R6Class("HistoryQueue",
                        inherit = Queue, 
                        # referring to the queue function defined above, with all functions and attributes
                        public = list(
                          show = function() {
                            # queue didn't have a method for showing its contents befor
                            cat("Next item is at index", private$head_idx + 1, "\n")
                            for (i in seq_along(private$queue)) {
                              cat(i, ": ", private$queue[[i]], "\n", sep = "")
                            }
                          },
                          remove = function() {
                            # here's a use of private, note that it's inherited from before
                            if (private$length() - private$head_idx == 0) return(NULL)
                            private$head_idx <<- private$head_idx + 1
                            private$queue[[private$head_idx]]
                          }
                        ),
                        private = list(
                          head_idx = 0
                        )
)

hq <- HistoryQueue$new(5, 6, "foo")
hq$show()
hq$add("something new")
hq$show()

# set() is used to add members to an existing class
# For example, when using the Setup() functions, new functions are added to objects in our simulation

# Why do we bother doing this?  Saves an enormous amount of memory, 
# and also saves time instantiating, accessing, and other interactions
# https://cran.r-project.org/web/packages/R6/vignettes/Performance.html
