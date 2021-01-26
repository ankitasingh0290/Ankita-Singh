# Decision Making Statements in R
# If Statement
# Syntax: 
if (test_expression) {
  statement
  }

x <- 5

if(x > 0){
  print("Positive number")
  print(x)
  x=x+1
}

# If Else Statement
# syntax: 
if (test_expression) {
  statement1
} else {
  statement2
  }

x <- 7
if(x > 0){
  print("Non-negative number")
} else {
  print("Negative number")
}

# Nested If Else Statement
if ( test_expression1) {
  statement1
} else if ( test_expression2) {
    statement2
  } else if ( test_expression3) {
  statement3
  } else  statement4

x <- 3
if (x < 0) {
  print("Negative number")
} else if (x > 0) {
  print("Positive number")
} else print("Zero")

# There is an easier way to use if...else statement specifically for vectors in R programming
# Syntax: ifelse(test_expression,x,y)
a = c(5,7,2,9,8)
help()
?ifelse
ifelse(a %% 2 == 0,"even","odd")

a/2
a%%2
# for more than 2 conditions
client <- c("private", "public", "other",'private')
VAT <- ifelse(client =='private',"PRT", ifelse(client == 'public', "PUB", "OTH"))
VAT
#ifelse(contition , true , ifelse(condition , 1 , ifelse(condition , 2 , 3)))
# Loops in R
# While loop
# Syntax
while (test_expression) {
  statement
}

i <- 5

while (i < 10) {
  print(i)
   i = i+1
}

  # Repeat loop
# Syntax 
repeat { 
  commands 
  if(condition) {
    break
  }
}

v <- c("Hello")
cnt <- 5

repeat {
  print(v)
  cnt <- cnt+1
  
  if(cnt > 6) {
    break
  }
}

# For Loop 
# Syntax
for (value in vector) {
  statements
}

v <- LETTERS[1:4]
v
for ( a in v) {
  print(a)
  
}

for (i in 1:length(v)){
  print(v[i])
}

# Loop Control Statements
# break Statement - terminates the loop statement and transfers execution to the statement 
# immediately following the loop
v <- c("Hello","loop")
cnt <- 2

repeat {
  print(v)
  cnt <- cnt + 1
  
  if(cnt > 5) {
   
    print("inside If")
    break
  }
}

# next Statement - useful when we want to skip the current iteration of a loop without terminating it
v <- LETTERS[1:6]
for ( i in v) {
  
  if (i == "D") {
    #next
    break
  }
  print(i)
}

# Function 
function_name <- function(arg_1, arg_2, ...) {
  Function body 
}
# User-defined Function
x <- function(a,b){
  print(sqrt(a))
  print(b^2)
}
v = 1:100
length(v)
sum(v)/100
mean(v)
x(7,3)

s <- function(a){
  print(sum(a)/length(a))
}
s(v)
a = c(10,15,23,1000)
s(a)

range(a)
rag<- function(a){
  max(a)-min(a)
}
rag(a)

new.function <- function(a=6) {
  for(i in 1:a) {
    b <- i^2
    print(b)
  }
}
new.function(7)
# Calling a Function
new.function <- function(a) {
  for(i in 1:a) {
    b <- i^2
    print(b)
  }
}
new.function(4)

# Calling a Function without an Argument
new.function <- function() {
  for(i in 1:5) {
    print(i^2)
  }
}	
new.function()

# Calling a Function with Default Argument
# Create a function with arguments.
new.function <- function(a = 3, b = 6) {
  result <- a * b
  print(result)
}
# Call the function without giving any argument.
new.function()
# Call the function with giving new values of the argument.
new.function(9)

# Lazy Evaluation of Function
# Create a function with arguments.
new.function <- function(a, b) {
  print(a^2)
  print(a)
  print(b)
}
# Evaluate the function without supplying one of the arguments.
new.function(10,8)


