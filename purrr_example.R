####Introduction Why Purrr
my_matrix <- list(c(1,2,3),
                  c(2,3,4),
                  c(3,4,5))
my_mat_mean <- c(NA,NA,NA)
for (idx in 1:3){
  my_mat_mean[idx] <- mean(my_matrix[[idx]])
}

library(purrr)
my_mat_mean <- map_dbl(my_matrix,mean)




####Map family
###map(): calculate 2*x for every x in my_matrix
my_matrix <- list(1:5,
                  2:6,
                  3:7)
##1. 函数
multiply_2 <- function(x){
  return(2*x)
}
my_matrix_mul2_1 <- map(my_matrix,multiply_2)
##2. 公式
my_matrix_mul2_2 <- map(my_matrix,~ .*2)
##3. 函数句柄
my_matrix_mul2_3 <- map(my_matrix,function(x) x*2)

###map_if(): calculate 2*x for every odd number in my_vector
my_vector <- 1:100
##1.判断函数
p_condition <- function(x){
  return (x %% 2 == 1)
}
my_odd_mul2_1 <- map_if(my_vector, p_condition, ~ .*2,.else =  ~ .)
##2.判断公式
my_odd_mul2_2 <- map_if(my_vector, ~ .%%2 == 1, ~ .*2,.else =  ~ .)
##3.逻辑值向量
logical_vector <- (my_vector %% 2) == 1 
my_odd_mul2_3 <- map_if(my_vector, logical_vector, ~ .*2,.else =  ~ .)


###lgl,int,dbl,chr: The return value of .f must be of length one for each element of .x
my_matrix <- list(1:5,
                  2:6,
                  3:7)
my_matrix_dbl_wrong <- map_dbl(my_matrix,~ .*2) #the wrong case
my_matrix_dbl_correct <- map_dbl(my_matrix,mean) 

###walk(): set values for a empty matrix
my_null_matrix =matrix(nrow = 10,ncol =10)
a = 1:10
walk(a, ~walk(a,function(x,y) {my_null_matrix[x,y] <<- x},.))
my_null_matrix
walk(walk(a,function(x) my_null_matrix[,x] <<- my_null_matrix[x,]*x),function(x) my_null_matrix[x,] <<- my_null_matrix[,x]*x)
my_null_matrix

####Map Variants
x = rnorm(10)
y = rnorm(10)
z = rnorm(10)
###map2()
my_max <- function(xi,yi){
  if (xi > yi)
    return (xi)
  else
    return (yi)
}
my_max_results <- map2(x, y ,my_max)
my_max_results
my_max_results <- map2_dbl(x, y ,my_max)
my_max_results
###pmap()
my_max <- function(xi,yi,zi){
  return (max(xi,yi,zi))
}
my_max_results <- pmap(list(x,y,z), my_max)
my_max_results
my_max_results <- pmap_dbl(list(x,y,z), my_max)
my_max_results
###imap()
a = 2:10
my_results <- imap(a,function(x,y) x**y)
my_results



#Predicate functionals(A predicate function is a function that either returns TRUE or FALSE. The predicate functionals take a vector and a predicate function and do something useful.)
#head_while/tail_while(Find head that all satisfies a predicate.)
library(magrittr)
library(purrr)
library(tidyverse)
mtcars[1,]
mtcars[1,]%>% head_while(~.>5)

#every/some(Do every or some elements of a list satisfy a predicate?)
y<-list(0:10,5.5)
y%>% every(is.numeric)
y%>% every(is.integer)

#has_element(Does a list contain an object?)
x <- list(1:10, 5, 9.9)
x %>% has_element(1:10)
x %>% has_element(3)


#plucking (Getting or setting a single element)
#pluck() chuck() (Pluck or chuck a single element from a vector or environment)
# Let's create a list of data structures:
obj1 <- list("a", list(1, elt = "foo"))
obj2 <- list("b", list(2, elt = "bar"))
x <- list(obj1, obj2)
# pluck() provides a way of retrieving objects from such data
# structures using a combination of numeric positions, vector or
# list names, and accessor functions.
# Numeric positions index into the list by position, just like `[[`:
pluck(x, 1)
x[[1]]
pluck(x, 1, 2)
x[[1]][[2]]
# Supply names to index into named vectors:
pluck(x, 1, 2, "elt")
x[[1]][[2]][["elt"]]

# By default, pluck() consistently returns `NULL` when an element
# does not exist:
pluck(x, 10, .default = NA)
x[[10]]   #error

# If you prefer to consistently fail for non-existing elements, use
# the opinionated variant chuck():
chuck(x, 1)
chuck(x,10)     #error
chuck(x,1,10)   #error



#modify_in assign_in()  (Modify a pluck location)
# Recall that pluck() returns a component of a data structure that might be arbitrarily deep
x <- list(list(bar = 1, foo = 2))
pluck(x, 1, "foo")
# Use assign_in() to modify the pluck location:
assign_in(x, list(1, "foo"), 100)
# modify_in() applies a function to that location and update the
# element in place:
modify_in(x, list(1, "foo"), ~ .x * 200)
# Additional arguments are passed to the function in the ordinary way:
modify_in(x, list(1, "foo"), `+`, 100)





