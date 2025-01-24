##readme:
#lesson 1 contains basic information about R syntax and object types, data manipulation and structures and basic arithmetic and statistical operations

##Start by setting your working directory using the following command:

#setwd("path/to/directory")

# R will ignore anything written to the right of a "#" in a line, therefore you need to omit any leading #s in order for the command to be executed upon hitting enter. Just replace "path/to/directory" with whatever directory you want to use as your working folder:

#(on Linux, you can also simply open a terminal window in whatever directory you want to work in, start R within the terminal, and you will be in that working directory automatically)

##THING TO NOTE HERE: in Windows, file or folder paths are usually written with backslashes (\) between the levels, i.e. something like "C:\User\Documents\R-project". However, R expects you to put your the paths in unix format, using normal slashes (/), not backslashes. I.e., you need to type "C:/User/Documents/R-project", even if you are using Windows. 
##

#If in doubt as to whether you are already in the right directory, use:
getwd()

#As R is an interactive programming language, you can write your code line by line, hitting enter after each command to execute it. This is useful when learning new commands to experiment and see how they work. However, you can also copy larger chunks of code into your R console all at once, to execute them in one go, or directly load your R code from a script file (which you would usually want to do for more complicated analyses).


##basic working principles: different types (classes) of objects
#R stores everything it needs to work in so-called "objects". The most basic type is a data object (this is what most programming languages would call a "variable"). Data objects can simply be referred to by their name, which must always contain and start with characters, not numbers. Numbers can also be input directly, but in most cases it makes sense to save longer, more complex or potentially changing numbers as objects.

##objects and the assignment operator
1->a #does the same as
a<-1 #This is to assign a value (at blunt end of arrow) to an object (pointy side of arrow)
#the arrow direction does not matter, use whatever you prefer. Some people feel very strongly that you should stick to a single direction, e.g. always use a left-facing arrow <- for assignments. I personally don’t care, even though it makes my code slightly more chaotic.

##basic operations: you can use the normal arithmetic operations you are familiar with
a<-1
b<-2
a+b->c
a*b->d


#if we don’t assign output to a variable, it will be printed (displayed) on screen by default:
a #prints content of object named "a"
a/b #prints result of a/b

5/2 #this is normal division, which results in decimal numbers
5%/%2 #this is integer division, which only returns an integer
#another way of doing the same would be to divide normally, then truncate the output to an integer using a the function trunc()
trunc(5/2)

5%%2 #this returns the remainder for the integer division

b^b # this returns the value of b to the power of b

##a more in depth guide to the basic operators in R can be found here: https://www.datamentor.io/r-programming/operator

#there are also a few functions that are useful here:
sqrt(4) #returns the square root of 4
round(4.556776, 1) #rounds the number to the desired number of decimal paces
signif(4.556776, 1) #rounds the number to the desired number of significant digits

#we can also compute the logarithm of a number using:
log(5) # natural log to base e
exp(log(5)) #this is e ^ log(5) = 5

log10(5) #log to base 10, the inverse wold then be
10^log10(5)

#Functions can be very complex or very simple. Some functions (e.g. sum()) perform operations that can also easily be accomplished using a single operator, but may be useful in slightly different situations. Others are perform tasks that you cannot do in any other way. Some functions are written in R itself (you can look at their code by simply typing their name), others are written in C or Fortran. You can also write your own functions, more on that later.

c(1,2,3,4,5)->somenumbers
sum(somenumbers) #sum
prod(somenumbers) #product

##You can always recognize a function by it having parentheses(). Some functions also have a set of swirly brackets{} in addition to parentheses to supply additional arguments, but this is rare. An example would be an if-clause:

a #what was a again? ah, right…

if(a==1){
print("a is 1")
}

#something you can observe here: when you want to enter text in some capacity (a so-called character-string), you have to use quotation marks, either "yourtext" or 'yourtext'

NA->e #NA denotes a missing value. In a single-value element like this, this rarely makes sense, but in an element containing multiple elements, this is important to denote missing ones

#you can delete any objects you make again by using rm(). 
rm(d)

#rm() itself is also an object, but an object of class "function". functions are characterized by always being followed by two parentheses () (even if those parentheses are sometimes empty), occasionally followed by two curly brackets {}. You can verify what type an object is by using the class() function. Try it:
class(c)
#this class should be "numeric", indicating that it contains numbers/a number. If you do the same to rm, it will return "function", because rm is a function
##Functions are simply objects of class "function"!
#For example:
class(rm)

#Besides that, the most basic classes are character, numeric, logical and factor:
a<-"this is a text, class character"
class(a)

a<-12345 # this is a number, class numeric
class(a)

a<-TRUE # this is a boolean value/logical statement, class logical
class(a)

a<-as.Date("2025-01-24") #this is a date. Dates are stored internally as a numeric value of days relative to an origin (by default "1970-01-01"), but are displayed in date format
class(a)
as.numeric(a)

a<-as.POSIXct("2025-01-24 20:00:00", tz="CET") #this is a Posix time, similar to a date but more precise (by default, internal storage is as number of seconds since 1970)
class(a)
as.numeric(a)

a<-factor(c("A","B","C")) # this is a categorical variable, class factor
class(a) #NOTE: in practice, it is rare for you to have to manually "make" a factor, as character() or numeric() vectors are often automatically coerced to factor.

#There are many other classes, some are part of packages, but they are mostly built upon, or contain, the previous few basic classes.

###########
##More complex objects
#So far, we have looked at basic functions and at the simplest type of data object, one containing a single number. However, because we want to do real science here, our sample size should ideally be more than 1 (otherwise we might as well use a pocket calculator). 
#Hence, an object can also contain several numbers in a specific order. A single column containing numbers is called a vector.

#You can build vectors using the following functions:
c(a,b,c,2)->d #c() is for combine, it combines the comma-separated objects you list into a vector
seq(1,10,1) #seq() is for sequence, it allows you to produce a sequence of numbers with a given increment, in this case numbers from 1 to 10, with an increment of 1
rep(5,5) # this simply repeats one number or string the given number of times, here 5 times

#you can also reverse the order of any vector using:
rev(d)

#to determine the number of elements in a vector, use:
length(d)

length(c(1,2,3,NA))#this returns the length of the vector INCLUDING the NA, which is 4
#to determine the number of elements that are not NA, use:
sum(!is.na(c(1,2,3,NA)))#this works because the is.na()-function returns values as true (1) or false (0), so the sum of all values is the number of times "TRUE" is returned. The !-operator here is the negation operator, which we need because we want a "TRUE" if the value is NOT NA

##subsetting operators
#each element in a vector can be referred to individually using the subset operator []
d[1] #the first element
d[2] #the second element, etc.

#another type of data object is a matrix, which is basically a 2-dimensional extension of the vector:
matrix(seq(1,12,1), nrow=3, ncol=4, byrow=T)->matrixA

#a matrix can also be made by binding multiple vectors together
cbind(c(1,2,3), c(4,5,6), c(7,8,9), c(10,11,12))->matrixB #binds several columns into a matrix
rbind(c(1,2,3,4), c(5,6,7,8), c(9,10,11,12))->matrixC #binds several rows into a matrix

#since the matrix is two-dimensional, when referring to elements we now need to specify both the row (first) and the column (second). If we want to select the entire row/column, we just leave the value empty
matrixA[1,] #select first row
matrixA[,1] #select first column
matrixA[1,1] #select cell in first row and first column

#you can multiply compatible matrices using the operator for matrix multiplication:
matrixB %*% t(matrixC) #in this case we must first transpose matrixC using t(), because the number of rows in the second matrix must equal the number of columns in the first matrix

#an array is an extension of a matrix into the third dimension (this is esp. important in geometric morphometrics, where you deal with 2D or 3D landmark data for several specimens)
array(c(matrixA, matrixB, matrixC), dim=c(3,4,3))

#A data-frame is similar to a matrix, but is more versatile and can contain several different types of data (e.g. numbers, text, logicals etc.). Technically speaking, a data.frame() is a special type of a list()-object that only contains vectors of the same length (i.e. different columns of a table)
data.frame(a=c(1,2,3), b=c(2,3,4), ID=c("first","second","third"))->dframe #here the ID-column contains not numbers, but text, which is characterized by always being entered in quotation marks ""
colnames(dframe) #this shows the column names of the data frame, and can also be used to modify them. For that, simply do:
colnames(dframe)<-c("a","b","ID")

#finally, there’s the more generalized list()-object, which can contain basically anything. This is useful for keeping your workspace tidy. If you think of a data.frame() as a table, think of a list() as a folder, into which you can put any number of other folders, tables, matrixes, single numbers, etc.. You can make an empty list, and then simply add objects to it as you go along:
list()->l
l$a<-a
l$dframe<-dframe

#as you can see above, objects within a list() can be addressed using the $-operator, e.g.
l$dframe
#this does the same as using the extraction operator: double square brackets [[]]
l[["dframe"]]
#we can also use single square brackets []
l["dframe"]
#this is the subset operator that we already learned about. The difference is not immediately apparent, but it becomes so if we check what class of object they each return:
class(l["dframe"])#returns "list" – the output is as if you had created a list, with only the single element "dframe"
class(l[["dframe"]])#returns "data.frame", i.e. just the element "dframe", not the list()-structure surrounding it
##[[]] extracts an object from the parent object, while [] subsets the parent object. This becomes clearer when we apply it to columns within our data.frame:
class(dframe["ID"])#returns a single-column data.frame
class(dframe[["ID"]])#returns a character vector

#Often this difference won’t matter, because the content is essentially the same, but sometimes it does, so watch out for this if you are getting any weird error messages that result from the data not being read correctly

#Ultimately a data.frame is just a special case of list. You can also individually extract columns of a data.frame using $:
dframe$ID#this is the same as:
dframe[["ID"]]#…but not the same as:
dframe["ID"]#although in many situations, you will find that both work


#this even works if your data.frame is part of another list()-type object:
l$dframe$ID#when doing this, dataframe collumns will be treated like vectors
#these are the main types of objects you are going to encounter and understanding them is essential, but also sufficient, to use R productively. However, understand that there are also various more specialized objects, usually consisting of some specific derivation of the basic object types you have just seen

#with data.frame() and matrix() objects, it is often important to check how many rows and columns they have. You do this using:
ncol(matrixB)#n of columns
nrow(matrixB)#n of rows

#now that we have created quite a bunch of objects, we might want to list them all to retain an overview. The ls() function, displays a list of all the objects saved in the current workspace:
ls()

#During the rest of this course, we will be dealing with an increasing number of function, and will not have time to explain each and every one in detail (especially since most functions have many different options, called parameters or arguments). If you need help with any function, you can always consult the various R documentation sites that can be found online (google: function_name R + package the function is from if needed), or alternatively, type ?function_name or help(function_name) to get a full documentation of the function, including working examples. e.g.:
?cbind()

#once you have opened it, you can close the documentation page by typing the letter q (for "quit")

###########
##reading and writing files
#you can save R data objects as rdata files. These are only readable in R and not in any other programs, but they have the advantage of quickly restoring the same objects to whatever workplace they are imported into, and have smaller file sizes.
save(l, file="l.rdata")
#we can load such an object into the current workspace using the load() function:
load("l.rdata")

#this is the simplest way of exchanging an R object between different people or workspaces. However, it is not very flexible.
##also keep in mind that this will overwrite any objects already present in the workspace if there are identically-named objects in the rdata file, so be careful when you import rdata files into workspaces where you do not want to do this.

save.image() #this is a special form of save() that saves your entire current workspace. You can use it with or without a filename. If you use it without, it will save an object just named .RData, which is also the workspace environment that is automatically loaded each time you start up R in the folder it is saved in.

##you can output your data as text using the write() or write.table() functions
write(c,"c.txt")
write.table(dframe, "dframe.txt")

#and you can read such data files using the scan- and read.table functions
scan("c.txt")#basic function for scanning text file into vector, note that if the data type is anything other than numeric, this must be specified using the parameter 'what', e.g. scan("c.txt",what=character())
read.table("dframe.txt", header=T)#reads tables, produces data.frame

#read.csv()#this reads a csv (comma-separated values), basically a specialized form of read.table() that has sep="," (separator) and header=T as presets

##you can also load an R script from a text file
write("sum(c(1,2,3))->a\nprint(a)", file="examplescript.R")#write a short example R code to a file \n inserts a line break
source("examplescript.R")#load the script we just made

######
##Some more basic statistical operations XXX
mean(c(1,2)) #this does exactly what it looks like, take the (arithmetic) mean of a vector.
mean(matrixB[1,])

mean(c(1,2,3,NA))#important thing to consider: mean returns NA if there are missing values, unless you specify:
mean(c(1,2,3,NA), na.rm=T)#…which ignores the missing values. Generally, it is important to consider how different functions deal with NAs in their input

sd(c(1,2))#This returns the standard deviation
sd(matrixB[1,])
sd(c(1,2,3,NA), na.rm=T)


##plotting
A<-rnorm(30,mean=10,sd=1)#rnorm produces a "random" normal distribution, results will differ a little. to make results based on random methods reproducible, you need to specify set.seed(somenumber) first
B<-rnorm(30,mean=8, sd=0.5)

#we’ll do some simple plots to visualize these data. We will cover more in-depth information on these plotting functions and their settings in the next lesson.
boxplot(A,B)
hist(A)
hist(B)
rug(B)


#Now let’s do a t-test to see if we can find a significant difference between the samples. Default is two-sided, for one-sided, specify alternative="less" (true difference in means less than 0) or "greater". or just use .9 instead of .95, then you can see both sides in the confidence interval
t.test(A,B, conf.level=0.95)#welch t-test, unless var.equal=T (then students t test)

var.test(A,B, conf.level=0.9)#test for inequality of variances (if equal, do students t test)
#wilcox.text(A,B) (this would be a non-parametric alternative if data are not normally distributed)

##basic scatterplot (pretending A and B are one bivariate rather than two univariate samples)
plot(A,B, type="p") #giving a setting for type is optional (the default is a scatterplot). You can also specify different settings for type, e.g. "l" for lines, or "b" or "o" for both, but we’ll keep it at this basic scenario for now.

#note that there are also specialized plotting functions for specific object types, which you can usually call by simply handing that object to plot() as its first argument. You will see this later when plotting phylogenies.

##example
data.frame(males=c(2.9,2.1,2.11,4.1,2.13,3.18,2.15,5.5, 2.17,2.18),females=c(3.2,3.9,3.4,3.5,6.2,3.7,4.5,3.9,3.4,3.11))->exmpl

mex<-data.frame(value=c(exmpl$males, exmpl$females), sex=c(rep("male",10), rep("female",10)))#you can reformat these data into a dataframe containing a single vector with values and another vector with categories "male" and "female"
boxplot(data=mex, value~sex, col="grey")
#or alternatively simply do:
boxplot(exmpl$female,exmpl$male, col="grey")

rug(exmpl$male, side=4)#this is a rug-plot, which is a 1-dimensional plot looking like a barcode and showing the distribution of a single variable
rug(exmpl$female, side=2)

t.test(exmpl$male, exmpl$female, conf.level=0.95, alternative="two.sided")#the p value gives us the probability of the observed difference between the samples, given that the null-hypothesis (no difference) is correct. if p<0.05, we can likely reject the Null hypothesis

t.test(data=mex, value~sex, conf.level=0.95, alternative="two.sided")

t.test(exmpl$male, exmpl$female, conf.level=0.95, alternative="greater")#this is a one-sided t.test testing for the alternative that the true difference in means (male-female) is >0

t.test(exmpl$male, exmpl$female, conf.level=0.95, alternative="less")#this is a one-sided t.test testing for the alternative that the true difference in means (male-female) is <0


##closing thoughts:
q()#will quit R (at least when running in a command line). You will be asked whether to save changes. typing y will save your current workspace and leave you with the same objects that you had when you closed it (on the windows RGUI console, you will have to use "load history" and "load workspace" from the file menu to load those files manually). 
#Typing n will not save anything, and the next time you start R you will start with a fresh workspace (or the last one you saved before). This goes for every working directory you work with, as each directory will have its own hidden files containing the R environment you saved. So if you want separate workspaces for separate projects, the best way is to simply use separate folders
#if you restart R and did not manage to save your workplace, you can just execute this entire script file using source("R_lesson_01.R"). Be sure to leave this line commented out though, otherwise the script will end by executing itself again in an endless loop, and you wouldn’t want that

##you should now:

#Understand the basic types of objects: functions (function), numbers (numeric), text (character), and how to tell R which is which

#Understand types of data objects: vectors (1D), matrices and data.frames (2D) and arrays (3D) as well as lists() and how they can be used to save and structure your data

#Be able to use R like a normal calculator to perform basic operations +-*/

#Be able to build vectors, matrices or data.frames with your own data, read text files with data and save them in appropriate data objects or files

#Apply basic functions and calculate basic statistical parameters on the data, e.g. log(), mean(), sd(), t.test()…

#^Be able to make simple plots such as boxplots and histograms
