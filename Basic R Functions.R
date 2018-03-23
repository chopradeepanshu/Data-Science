# This command is going to give the Mean of First Five Numbers
mean(1:5) # This command is going to give the Mean of First Five Numbers

#R is case-sensitive Language

# Data Structures of R

# Assignment of Value
a=5
a

# Vector

# Display first 10 numbers
x = 1:10
x

# Type of x 
class(x) #- x is an "integer" Vector

x1 = c(1,2,4,5,6,7,33.5) # 'c' - Combined Vector
class(x1) # x1 is Numeric.

# Difference between numeric and Integer Vector is - Numeric can have decimal values.

# Character Vector
x2 = c("Deepanshu", "Chopra","Amit", "Mohan")
class(x2) # Character Vector

# Logical Vector
x3 = c(TRUE, FALSE, T, F)
class(x3) # Logical Vector
  
#For Loop
x4 = seq(from= 2, to = 100, by = 2)
x4
class (x4) # Numeric

# Complex Vector
x5 = c(2-3i, 1+5i, 9*6i)
class(x5)


x6 = c(12,3,4,5, "Deepanshu")
class(x6)

# Get the First value from the vector. 
x1[1]

class(x1[1])
class(x6[1])

# Experiment
# To check if we combine the different vectors in one what will be the final class of the result vecotr

x7 = c(4-5i, 5+4i,"Deepanshu" , F )
class(x7) 

# Perform Operations on Vector.
1:5
6:10

# Here we have the vector of Same Lenght
v1 = 1:5
v2 = 6:10
v1+v2
#Or we can do in this way

1:5 + 6:10 #Vectorized Operation

# What if we have the vector of different lenght, we the Vector with the shorter lenght will be repeated by its own values till it matches the second vector

v3 = 1:5
v4 = 6:13
#In v3 + v4 : longer object length is not a multiple of shorter object length


#Comparison - Each element of the vector is going to be compared with 3 and display True or False.
c(3,4,56-4,76,5,2) == 3
# We can also do - <=, >=, >, <, == during comparison.

v5 = c(1,2,3,4,5,6,7)
length(v5)

# Matrices - It is Generalization of Vector

m = 1:8
dim(m) = c(2,4) # Adding Dimesion to M which saye 2 Rows and 4 Columns
m
class(m) # Matrix

# If we give the Matrix Size different than the data set its going to give an error
dim(m) = c(2,5) # Adding Dimesion to M which saye 2 Rows and 5 Columns will give an error.
# Error will be - dims [product 10] do not match the length of object [8]

# Single Matrix, Defining Data Set and Rows and Columns

m1 = matrix(1:8, 2,4)
m1

# Single Matrix, Defining Data Set and Rows and Columns By Type Rows

m2 = matrix(1:16, 4, 4, byrow = TRUE)
m2

# Access Element of Mmatrix
m2[2, 3]

# Getting the whole row in a matrix
m2[2,]
# Getting the whole column in a matrix
m2[,2]


# Array Range is from 1 to 500 with 2,4 Matrix and 5 Individual Matrix
my_array = array(1:500, dim = c(2,4,5))
my_array

# Get the 3rd Matrix

my_array[ , ,3]

# Access 1st Row of ALL Matrix
my_array[1, , ]

#----------------------------------------------------
#Data Frames - Most used Data Structure.
#----------------------------------------------------

#If you are loading a Excel Sheet in R - Excel Sheet will be said to be Data Frames.

#Lets Load the data in R
salary_url = "http://www.justinmrao.com/salary_data.csv"
salary_data = read.csv(salary_url)

class(salary_data)
class(salary_data$salary_year)


#----------------------------------------------------
#List - Slightly Complex Data Structure. 
#It is a master data structure and can contain variety of combination of Data Structures.
# We have created a List of Vector, Matrix, Array, Data Frame
#----------------------------------------------------

# List 1 - Keeping only Vectors

list1 = list(x1, x2, m1, my_array, salary_data)
list1

# Giving the name of the List Attributes instead of 1, 3 or any number.
names(list1) = c("my_vector", "my_vector2", "matrix1", "my_array", "my_dataset")
list1

# Getting individual Element of the List
list1$my_vector
list1$my_vector2
list1$matrix1
list1$my_array
list1$my_dataset


#----------------------------------------------------
# Understanding Factors in Vectors
# Difference between Vector and Factor is Factor creates a Numeric value of each variable as in the Vector
# Factor is going to take less space
# Factor will be fast in processing.
# In Some statistical Algo, we need to represent Characters Values in Number.

#----------------------------------------------------

gender = c("male", "female", "female", "male", "male")
gender
class(gender)
# Output: "male"   "female" "female" "male"   "male"  

gender_factor = factor(gender)
class(gender_factor)
gender_factor
# Output: male   female female male   male  

gender_numeric = as.integer(gender_factor)
gender_numeric
# Output will be Levels: 2 1 1 2 2
# 2 --> Female and 1 --> Male. As per Alphabetical order

#----------------------------------------------------
#Lets Load Data in R and we will manipulate the data 
#----------------------------------------------------

salary_url = "http://www.justinmrao.com/salary_data.csv"
salary_data = read.csv(salary_url)

class(salary_data)

# CHecking How many Columns & Rows the data frame have

nrow(salary_data) # Give Number of Rows
ncol(salary_data) # Give Number of Colums
dim(salary_data) # Give number of Rows and Colums
colnames(salary_data) # Give the names of the Columns
names(salary_data) # Give the names of the Columns
str(salary_data) # Give the complete information of the Column Data Structure 

# The output will be like:
#team                    : Factor w/ 32 levels (This means the Column has 32 different values)

# By Default, if there is some repeating values of String in the columns R will make this column as Factor.
# In some of the scenarios, we do not want to make change to the column structure to Factor rather we want to have it character only.

salary_data = read.csv(salary_url, stringsAsFactors = FALSE)
str(salary_data)

# After above statement, we will get the Structure same as chr instead of Factor.

# If we wanted to convert Some column to Factor we can do it like below:

salary_data$team = factor(salary_data$team)
class(salary_data$team)
salary_data$team

# lenght = ncols . Lenght Command on Data Frame is same as ncols.
length(salary_data)


# To see first few records of the data frame. Defaut will be 6.
head(salary_data)

# To see first few records of the data frame. NON Defaut which can be give for 10
head(salary_data, 10)

# To See Last default rows: 6
tail(salary_data)

# To See Last non default rows: 10
tail(salary_data, 10)

# View the complete data frame
View(salary_data)

# I want create a new data frame and I want to have column from 2 - 5 columns into it.
salary_data_column_2_to_5 = salary_data[, 2:5]

# To get only specified rows and columns in the data set
salary_data_specifi_rows_colmns = salary_data[c(2,3,4,5,7,777), c(2,3,4,5,6)]


# Creation of data frames based on Conditions
salary_data_single_condition = subset(salary_data, salary_data$team == "Chicago Bulls")

# Creation of data frames based on Multiple Conditions
salary_data_multiple_condition = subset(salary_data, salary_data$team == "Chicago Bulls" & salary_data$contract_years_remaining > 4)
# Or Condition
salary_data_multiple_condition = subset(salary_data, salary_data$team == "Chicago Bulls" | salary_data$contract_years_remaining > 4)


# We want to combine 2 values separated by Separator.
paste("Hello", "Deepanshu", sep = "*")

# Going back to our dataset
# We want to add a Column in the Data Frame with some values 

salary_data$team_year = paste(salary_data$team, salary_data$year, sep="*")


# We want to bind 2 files of same dimension. The columns names should be same.

data1 = c(a="Deepanshu", b="chopra")
data2 = c(a="Akansha", b="chopra")
rbind(data1, data2)


