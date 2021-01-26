## Basic arithmetic operations
10/0
10/inf
10/Inf
3 + 7 #Addition
17+24
15+19
1000+5

7 - 3 #Subtraction
5-7
25-18

3 * 7
7*5# Multiplication

7/3 # Divison

2^4 # Exponentiation
3^4

# Modulo: returns the remainder of the division of 8/2
8%%2

abs(-25)
abs(-100)# absolute value of x
sqrt(37)# square root of x
sqrt(25)



## Assigning values to variables
# Price of a lemon = 2 euros
lemon_price <- 2
lemon_price <- 2
# or use this
lemon_price = 2
lemon_price

## To See the output
lemon_price  # or use 
print(lemon_price)

# Multiply lemon price by 5
5 * lemon_price

# Change the value
lemon_price <- 5
# Print again
lemon_price
print(lemon_price) 
5*lemon_price

# Creating a Rectangle 
##Rectangle height
height <- 10
# rectangle width
width <- 5
# compute rectangle area
area <- height*width
print(area)
area


##The function ls() can be used to see the list of objects we have created
ls()

# Remove height and width variable
rm(height, width)
#rm(my_friends)
#my_friends[-2]
# Display the remaining variables
ls()


##Basic data types

##Basic data types are numeric, character and logical.

# Numeric datatype: How old are you?
my_age <- 20
#myage <- 20
class(my_age)
my_age1 <- as.character(my_age)
class(my_age1)
my_age1 <- as.numeric(my_age1)
class(my_age1)


# Character  datatype: What's your name?
my_name <- "Nicolas"
class(my_name)
# logical datatype: Are you a data scientist?
# (yes/no) <=> (TRUE/FALSE)
is_datascientist <- TRUE
class(is_datascientist)
a <- TRUE
class(a)
a <- 5
class(a)

## To print a Variable 
"My friend's name is Jerome"

##It’s possible to use the function class() to see what type a variable is:
class(my_age)
class(my_name)

#You can also use the functions is.numeric(), is.character(), is.logical() to check whether 
##a variable is numeric, character or logical, respectively. For instance:
is.numeric(my_age1)
is.character(my_name)
is.numeric(my_age)
is.logical(is_datascientist)

# Convert my_age to a character variable
my_age1 = as.character(my_age)
my_age1
(my_age)
class(my_age1)

##Vectors

##A vector is a combination of multiple values (numeric, character or logical) in
##the same object. In this case, you can have numeric vectors, character vectors or logical vectors.

# Store your friends'age in a numeric vector
friend_ages <- c(27, 25, 29, 26) # Create
friend_ages1 <- c(27, 25, 29, 26)
friend_ages1

friend_ages # Print

# Store your friend names in a character vector
my_friends <- c("Nicolas", "Thierry", "Bernard", "Jerome")
print(my_friends)

# Store your friends marital status in a logical vector
# Are they married? (yes/no <=> TRUE/FALSE)
are_married <- c(TRUE, FALSE, TRUE, TRUE)
are_married
print(are_married)

#assign names (there are 2 ways)
names(are_married)<- c("Nicolas", "Thierry", "Bernard", "Jerome")
are_married

#my_data <- cbind(are_married, friend_ages)
#my_data

# Vector with element names
names(friend_ages) <- c("Nicolas", "Thierry", "Bernard", "Jerome")
friend_ages


# You can also create a named vector as follow
friend_ages <- c(Nicolas = 27, Thierry = 25, 
                 Bernard = 29, Jerome = 26)
friend_ages

friend_ages <-c(Nicolas = 27, Thierry = 25, 
                Bernard = 29, Jerome = 26, Varun = 25)
friend_ages
my_friends
# for example
my_friends1 <- append(my_friends, "varun", after=2)
my_friends1

# Number of friends
length(my_friends1)

##In Case wehave missing values
##Suppose two of your friends do not have informatio about their childre. How ca we handle it?
have_child <-  c(Nicolas = "yes", Thierry = "yes", 
                Bernard = NA, Jerome = NA)
have_child
class(have_child)


# Check if have_child contains missing values
is.na(have_child)


##Subset:
my_friends

# Select my friend number 2
my_friends[2]

# Select my friends number 1 and 3
my_friends[c(1,3)]

# Select my friends number 1 to 3
my_friends[1:3]

friend_ages
#In Case we have a named vector
friend_ages["Bernard"]

# Exclude my friend number 2
my_friends
my_friends[-2] ##Negatie Indexing

# Exclude my friends number 2 and 4
my_friends[-c(2, 4)]

# Exclude my friends number 1 to 3
my_friends[-(1:3)]

###Selection by logical Vector:
my_friends
are_married
# Select only married friends
my_friends[are_married == TRUE]

friend_ages
my_friends
# Friends with age >=27
my_friends[friend_ages >= 27]

# Friends with age <= 27
my_friends1
my_friends1[friend_ages <= 27]

# Data with missing values
rm(have_child)
have_child

# Keep only values different from NA (!is.na())
have_child[!is.na(have_child)]

# Or, replace NA value by "NO" and then print
have_child[is.na(have_child)] <- "NO"
have_child


##Note that, the “logical” comparison operators available in R are:
  
## <: for less than
## >: for greater than
## <=: for less than or equal to
## >=: for greater than or equal to
## ==: for equal to each other
## !=: not equal to each other

##Calculations with vectors

# My friends' salary in dollars
salaries <- c(2000, 1800, 2500, 3000)
names(salaries) <- c("Nicolas", "Thierry", "Bernard", "Jerome")
salaries

# Multiply salaries by 2
salaries*2

# create coefs vector with the same length as salaries
coefs <- c(2, 1.5, 1, 3)
# Multiply salaries by coeff
salaries*coefs

## Sq.rt fuction
my_vector <- c(4, 16, 9)
sqrt(my_vector)

x = c(1,2.5,6,7,4,9,-1,0,20)


max(x) # Get the maximum value of x

min(x) # Get the minimum value of x

# Get the range of x. Returns a vector containing
# the minimum and the maximum of x
range(x) 

length(x) # Get the number of elements in x

sum(x) # Get the total of the elements in x

prod(x) # Get the product of the elements in x

# The mean value of the elements in x
# sum(x)/length(x)
mean(x) 

sd(x) # Standard deviation of x

var(x) # Variance of x

# Sort the element of x in ascending order
sort(x,decreasing = FALSE)
?sort
salaries
sum(salaries) ## Addition

## Matrix

##A matrix is like an Excel sheet containing multiple rows and columns.
## It’s used to combine vectors with the same type, which can be either numeric, character or logical.
## Matrices are used to store a data table in R.
## The rows of a matrix are generally individuals/observations and the columns are variables.

# Numeric vectors
col1 <- c(5, 6, 7, 8, 9)
col2 <- c(2, 4, 5, 9, 8)
col3 <- c(7, 3, 4, 8, 7)
# Combine the vectors by column
my_data <- cbind(col1, col2, col3)
my_data

# Change rownames
rownames(my_data) <- c("row1", "row2", "row3", "row4", "row5")
my_data

##  cbind(): combine R objects by columns
##  rbind(): combine R objects by rows
##  rownames(): retrieve or set row names of a matrix-like object
##  colnames(): retrieve or set column names of a matrix-like object

##If you want to transpose your data, use the function t():
t(my_data)

## Matrix:
matrix(data = NA, nrow = 1, ncol = 1, byrow = FALSE,
       dimnames = NULL)

## data: an optional data vector
## nrow, ncol: the desired number of rows and columns, respectively.
## byrow: logical value. If FALSE (the default) the matrix is filled by columns, 
## otherwise the matrix is filled by rows.
## dimnames: A list of two vectors giving the row and column names respectively.

mdat <- matrix(
  data = c(1,2,3, 11,12,13), 
  nrow = 2, byrow = TRUE,
  dimnames = list(c("row1", "row2"), c("C.1", "C.2", "C.3"))
)
mdat
?dimnames

## Dimensions of a matrix
## The R functions nrow() and ncol() return the number of rows and columns present in the data, respectively
my_data
ncol(my_data) # Number of columns
nrow(my_data) # Number of rows
dim(my_data) # Number of rows and columns

## Get a subset of a matrix

## Select rows/columns by positive indexing
# Select row number 2
my_data[2, ]
my_data

# Select row number 2 to 4
my_data[2:4, ]

# Select multiple rows that aren't contiguous
# e.g.: rows 2 and 4 but not 3
my_data[c(2,4), ]

# Select column number 3
my_data1 <- my_data[, 3]
cbind(my_data1)

# Select the value at row 2 and column  3
my_data
my_data[2, 3]
my_data[2, "col3"]

# Select column 2
my_data[, "col2"]

# Select by index and names: row 3 and column 2
my_data[3, "col2"]

##Exclude rows/columns by negative indexing

# Exclude column 1
my_data[, -1]

# Create a data frame
friends_data <- data.frame(
  name = my_friends,
  age = friend_ages,
  height = c(180, 170, 185, 169),
  married = are_married
)
?data.frame
# Print
friends_data
print(friends_data)

# Identify rows that meet the condition
friend_ages
friend_ages >= 27

# Select the rows that meet the condition
friends_data
friends_data[friends_data$age >= 27, ]

# Use column locations
friends_data[friends_data$age >= 27,  c(1, 2,3)]

# Or use column names
friends_data[friends_data$age >= 27, c("name", "age","height")]

## If you’re finding that your selection statement is starting to be 
## inconvenient, you can put your row and column selections into variables first, such as
friends_data
age27 <- friends_data$age >= 27
age27
cols <- c("name", "age")
cols

friends_data[age27, cols]
friends_data
# Attach a data frame
attach(friends_data)
# === Data manipulation ====
friends_data[age>=27,c(1,2,3) ]
# === End of data manipulation ====
# Detach the data frame
detach(friends_data)


# Create a factor variable
friend_groups <- factor(c(1, 2, 1, 2))
class(friend_groups)

# Get group names (or levels)
levels(friend_groups)

# Change levels
levels(friend_groups) <- c("best_friend", "not_best_friend")
friend_groups


# Change the order of levels
#friend_groups <- factor(friend_groups, 
                        levels = c("not_best_friend", "best_friend"))
# Print
friend_groups
class(friend_groups)

# Add group column to friends_data
friends_data$group <- friend_groups
friends_data

# Check if friend_groups is a factor
is.factor(friend_groups)
is.factor(friends_data)
str(friends_data)

# to extend a dataframe
cbind(friends_data, group = friend_groups)

## Calculations with data frame
## Lists
## A list is an ordered collection of objects, 
## which can be vectors, matrices, data frames, etc. In other words, a list can contain all kind of R objects.

# Create a list
my_family <- list(
  mother = "Veronique", 
  father = "Michel",
  sisters = c("Alicia", "Monica"),
  sister_age = c(12, 22)
)
# Print
my_family

# Names of elements in the list
names(my_family)

# Number of elements in the list
length(my_family)

## Subset a list

# Select by name (1/2)
my_family$father

# Select by name (2/2)
my_family[["father"]]

# Select by index
my_family[[1]]

my_family[[3]]

# Select a specific element of a component
# select the first ([1]) element of my_family[[3]]
my_family[[3]][2] 

## Extend a list

# Extend the list
my_family$grand_father <- "John"
my_family$grand_mother <- "Mary"
# Print
my_family

## Concatenating two lists
list_abc <- c(list_a, list_b, list_c)































