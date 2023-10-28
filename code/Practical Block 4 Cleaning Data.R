# Practical: Data Cleaning with R
# Block 4
# 28.10.2023
# Sarka Ondrouchova

# Part 1: Using read.csv() --------------------------

# Use read.csv to read a file we created
read.csv("data/unnamed.txt")

# The first row is a header
# Read again with header = FALSE
read.csv("data/unnamed.txt", header = FALSE)

# Set column names
read.csv("data/unnamed.txt", header = FALSE, col.names = c('age', 'height'))

# Read into an R object
person <- read.csv("data/unnamed.txt", header = FALSE, 
                   col.names = c('age', 'height'))
person

# Examine structure
str(person)

# Height is read as character.
# Use colClasses to set it to numeric
person <- read.csv("data/unnamed.txt", 
                   header = FALSE, 
                   col.names = c('age', 'height'),
                   colClass=c("age" = "numeric", "height" = "numeric"))

# Error message: scan() expected 'a real', got '5.7*'
# Instead read as a character and convert to numeric
person <- read.csv("data/unnamed.txt", header = FALSE, 
                   col.names = c('age', 'height'),
                   stringsAsFactors = FALSE) # character strings are not interpreted as factors
str(person)

# Coerce (convert) the contents of height to numeric
person$height <- as.numeric(person$height)

# if it can't be coerced it introduces NAs
person


# Part 2: Dealing with unstructured data --------------------

# Step 1
# readLines() returns a character vector where each element is one line from the file
(txt <- readLines("data/daltons.txt"))

# Step 2
# remove comments starting with % using regular expressions
# find lines with %
(I <- grepl("^%", txt))

# remove those lines
(dat <- txt[!I])

# Step 3
# strsplit() splits a string into a list of substrings
(fieldList <- strsplit(dat, split = ","))

# Step 4
# Standardise rows (same number of fields)
# lapply() applies a function to each element in a list
# use our own function

assignFields <- function(x) {
  # create a character vector to hold the extracted fields
  out <- character(3)
  
  # extract the name value and put in the first position
  i <- grepl("[[:alpha:]]", x)
  out[1] <- x[i]
  
  # extract the birth date (all born before 1890)
  i <- which(as.numeric(x) < 1890)
  out[2] <- ifelse(length(i)>0, x[i], NA)
  
  # get death date > 1890
  i <- which(as.numeric(x) > 1890)
  out[3] <- x[i]
  out
}

# run on fieldList
standardFields <- lapply(fieldList, assignFields)
standardFields

# Step 5
# convert the list into a dataframe
# turn the list into character vector
(U <- unlist(standardFields))

# convert this in a matrix
(M <- matrix(U, nrow = length(standardFields), byrow = TRUE))

#coerce this into a dataframe
colnames(M) <- c('name', 'birth', 'death')
(daltons <- as.data.frame(M, stringsAsFactors = FALSE))

# Step 6
# normalize and coerce to correct type
(daltons$birth <- as.numeric(daltons$birth))
(daltons$death <- as.numeric(daltons$death))
str(daltons)


