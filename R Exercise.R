data(mtcars)
?mtcars
head(mtcars)
mydata<-mtcars
mydata
mydata<-data[mtcars$mpg > 15 & mtcars$mpg < 20,]
mydata<-mtcars$mpg > 15 &mtcars$mpg < 20
mydata[mydata$mpg > 15 & mydata$mpg < 20,]

mydata[mydata$cyl == 6 & mydata$am != 0,]

mydata[mydata$gear == 4 | mydata$carb == 4,]

mydata[c(F,T),]

mydata$mpg[c(F,F,F,T)] <- 0

mydata[mydata$vs & mydata$am,]

(TRUE + TRUE) * FALSE #== 0 (TRUE + TRUE = TRUE, TRUE * FALSE = FALSE/0)

mydata[mydata$vs | mydata$am,]

mydata$am[mydata$am ==0] <-2

mydata$vs<-mydata$vs+2 * TRUE

mydata[xor(mydata$vs,mydata$am),]

x <- c(NA,3,14,NA,33,17,NA,41)
#show no NA values only
x[!is.na(x)]

Y = c(1,3,12,NA,33,7,NA,21)
#replace NA with 11
Y[is.na(Y)]= 11

X = c(34,33,65,37,89,NA,43,NA,11,NA,23,NA)
#Count no. of NAs in vector
sum(is.na(X))

W <- c (11, 3, 5, NA, 6)
#highlight NA value as True
is.na(W)

data("Orange")
#assign age = 118 as NA
Orange$age[Orange$age ==118] <-NA

A <- c (33, 21, 12, NA, 7, 8)
mean(A,na.rm = TRUE)

c1 <- c(1,2,3,NA) 
c2 <- c(2,4,6,89) 
c3 <- c(45,NA,66,101)

X <- rbind (c1,c2,c3, deparse.level=1)
#Show fields with NA
X[!complete.cases(X), ]
#Show fields without NA
X[complete.cases(X), ]

df <- data.frame(Name = c(NA, "Joseph", "Martin", NA, "Andrea"),
                 Sales = c(15, 18, 21, 56, 60),
                 Price = c(34, 52, 21, 44, 20),
                 stringsAsFactors = FALSE)
#Show names without NA
df[!is.na(df$Name),]

#Show names, Sales, Price without NA
df[!is.na(df$Name) | is.na(df$Sales) | is.na(df$Price),]

#strlen
x<-c("Natures", "At its Best")
nchar(x)

#String concatenation
fname<-"James"
lname<-"Bond"
paste(fname,lname)

m<-"captial of America is Washington"
substr(m,1,19)

# String Replace
s<-"Success is not final, failed is not fatal"
sub("failed","failure",s)

#Create Data Frame
Names <- c("John", "Andrew", "Thomas")
Designation <- c("Manager", "Project Head", "Marketing Head")
data.frame(Names,Designation)

#Initialize character vector with fixed length of 10
vector(mode="character",length=10)
vector(mode="numeric",length=10)

c(outer(letters[1:5], letters[1:5], FUN=paste, sep=""))

#Display date in format 2000-12-12 12:11:10 GMT"
df <- data.frame(Date = c("12/12/2000 12:11:10"))
strptime(df$Date, "%m/%d/%Y %H:%M:%S")

x <- c("ww", "ee", "ff", "uu", "kk")
x[c(2,3)]

x <- c("ss", "aa", "ff", "kk", "bb")
x[c(2, 4, 4)]

x <- c("pp", "aa", "gg", "kk", "bb")
x[-2]

a <- c(2, 4, 6, 8) 
b <- c(TRUE, FALSE, TRUE, FALSE)
max(a[b])

a <- c (3, 4, 7, 8)
b <- c(TRUE, TRUE, FALSE, FALSE)
sum(a[b])

#Return a sum of 10
x <- c(2, 1, 4, 2, 1, NA)
sum(x,na.rm = TRUE)
sum(x[-6])

#Display 1, 3, 5, 7 only
x <- c(1, 3, 5, 7, NA)
x[!is.na(X)]
x[-5]

#Display 2 4 10 from the below data frame
s <- data.frame(first= as.factor(c("x", "y", "a", "b", "x", "z")),
                second=c(2, 4, 6, 8, 10, 12))
s$second[(s$first=='x') | (s$first=='y')]

(c(FALSE, TRUE)) || (c(TRUE, TRUE))

#Return position of 3 and 7 in the below vector
x <- c(1, 3, 6, 7, 3, 7, 8, 9, 3, 7, 2)
which(x %in% c(3, 7))


mode(c('a', 'b', 'c'))

pressure
mode(pressure)
lm
mode(lm)
rivers
mode(rivers)

x <- list(LETTERS, TRUE, print(1:10), print, 1:10)
mode(x)
mode(x[[1]])
mode(x[[2]])
mode(x[[3]])
mode(x[[4]])
mode(x[[5]])
#alternative solution
sapply(x, mode)

x <- 1:100
is.numeric(x)
mode(x)
x<-as.character(x)
mode(x)
x<-as.numeric(x)
mode(x)

x <- c('1', '2', 'three')
x<-as.numeric(x)
x

#create vector '2', 4', '6'
x <- c('1', '2', '3')
y <- as.numeric(x) * 2
y


x = c(1, 2, 3, 3, 5, 3, 2, 4, NA)
levels(factor(x))

x <- c(11, 22, 47, 47, 11, 47, 11)
factor(x, levels=c(11, 22, 47), ordered=TRUE)


z <- c("p", "a" , "g", "t", "b")
z[3] <- "b"
z

z <- factor(c("p", "q", "p", "r", "q"))
levels(z)[1] <- "w"
z

s1 <- factor(sample(letters, size=5, replace=TRUE))
s2 <- factor(sample(letters, size=5, replace=TRUE))
paste(s1,s2)

factor(c(levels(s1)[s1], levels(s2)[s2]))

data("iris")

#cut' the Sepal.Length variable and create the following factor with five levels.
table(cut(iris$Sepal.Length, 5))

#Sepal.length (less than 5: TRUE or FALSE) and columns to Species
table(iris$Sepal.Length < 5, factor(iris$Species))


#Add Strongly Agree and Strongly Disagree to the list of responses
responses <- factor(c("Agree", "Agree", "Strongly Agree", "Disagree", "Agree"))
factor(responses, levels=c("Strongly Agree", "Agree", "Disagree", "Strongly Disagree"))

# replace levels a, b, c with labels "fertiliser1", "fertliser2", "fertiliser3".
x <- data.frame(q=c(2, 4, 6), p=c("a", "b", "c"))
x$p <- factor(x$p, levels=c("a", "b", "c"), labels=c("fertiliser1", "fertiliser2", "fertiliser3"))
x

#provide unique numeric values for various levels of x
x <- factor(c("high", "low", "medium", "high", "high", "low", "medium"))
data.frame(levels = unique(x), value = as.numeric(unique(x)))#levels = rows, values= column of array

x<-c(1,2,3)
y<-c(4,5,6)
z<-c(7,8,9)

a<-cbind(x,y,z)
rownames(a)<-c("a","b","c")

a<-rbind(x,y,z)
rownames(a)<-c("a","b","c")
a

is.matrix(a)

#Create a vector with 12 integers. Convert the vector to a 4*3 matrix  B  using  matrix(). 
#Please change the column names to  x, y, z  and row names to  a, b, c, d.
b<-c(1:12)
c<-matrix(b,4,3,dimnames = list(c("a","b","c","d"),c("x","y","z")))

c

#Transpose of matrix
tC<-t(c)
tC

tM<-tC*tC
tM

a%*%tC

a%*%a

datA<-data.frame(a)
datA*datA
datA%*%datA

#Submatrix 3×3 matrix which includes the last three rows of matrix  B 
#and their corresponding columns.
c
subB<-c[2:dim(c)[1],1:3]
subB

3*a
a + subB
a- subB

#Generate a n * n matrix (square matrix)  A1  with proper number of random numbers, 
#then generate another n * m matrix  A2.
#A1 * M = A2
A1<-matrix(runif(16),4,4) # uniform 4*4 matrix
A2<-matrix(runif(8),4,2)  # uniform 4*2 matrix
M<-solve(A1,A2) # A1 * M = A2
A1
A2
M


M=matrix(c(1:10),nrow=5,ncol=2,
         dimnames=list(c('a','b','c','d','e'),c('A','B')))

M

M[1,]
M[,1]
M[3,2]
M[1,1]
M['e','A']

N=matrix(c(1:9),nrow=3,ncol=3,
         dimnames=list(c('a','b','c'),c('A','B','C')))
N
diag(N)
diag(4,3,3)

M=matrix(c(1:9),3,3,byrow=T,
         dimnames=list(c('a','b','c'),c('d','e','f')))
rownames(M)
colnames(M)
upper.tri(M)
lower.tri(M)
lower.tri(M,diag=T)
upper.tri(M,diag=T)

M=matrix(c(1:9),3,3,byrow=T)
N=matrix(c(1:9),3,3)

M
N
M*N


M=matrix(c(1:9),3,3,byrow=T)
N=matrix(c(1:9),3,3)

M
N
M%*%N

M=matrix(c(1:9),3,3,byrow=T)
N=matrix(c(1:9),3,3)

(M+N)^2

M/N

?dim
x<-1:24
dim(x)<-c(2,3,4)
x
?array
array(1:24,dim=24)
?dim

x <- 1:24 ; dim(x) <- c(4,6)
x

vector1 <- c(5,9,3)
vector2 <- c(10,11,12,13,14,15)

result<-array(c(vector1,vector2),dim=c(3,3))
result

rT <-t(result)
rT

a <- 1:5 ; b <- 1:5
m<-cbind(a,b)

m

a <- 1:5 ; b <- c('1', '2', '3', '4', '5')
m<-cbind(a,b)

m

a <- 1:5 ; b <- 1:4; c <- 1:3
m<-cbind(a,b)

a <- 1:5; b <- 1:5
m <- rbind(a, b, deparse.level=0)
m

m <- rbind(a, b)
m

a <- matrix(1:12, ncol=4); b <- matrix(21:35, ncol=5)
m <- rbind(a, b, deparse.level=0)
m


a <- matrix(1:12, ncol=4); b <- matrix(21:35, ncol=3)
m <- rbind(a, b, deparse.level=0)
m
m<-a <- matrix(1:39, ncol=3); b <- matrix(LETTERS, ncol=2)
m <- rbind(a, b, deparse.level=0)
m

a <- matrix(1:1089, ncol=33)
m<-cbind(a)

a <- matrix(1:1089, ncol=33)
a1 <- a[,rep(1:33, 21)]    # possible solution
a2 <- matrix(a, ncol=21*33, nrow=33)   # another solution
all.equal(a1, a2)
a1
a2

A <- matrix(c(2,0,1,3), ncol=2)
B <- matrix(c(5,2,4,-1),ncol=2)
A+B
A-B

#diagonal matrix with values 4,1,2,3
diag(4)*c(4,1,2,3)

#Find the solution for Ab, where A is the same as in the previous question and b=c(7,4)
b<- c(7,4)
b%*%A

A%*%B
#Transpose
t(A)
#Inverse
solve(A)

#Ax=b
solve(A,b)

#eigen values
eigen(A)$values

#eigen of crossprod
#A'A -> Crossprod
eigen(crossprod(A))


year=c(2005:2017)
month=c(1:12)
day=c(1:31)
Date = list()
Date$year=year
Date$month=month
Date$day=day
Date

Date$year=c(2000:2010)
Date
Date$month = Date$mont[-4]
Date

x=c(1,3,4,7,11,18,29)
X2=list('x*2'=x*2,'x/2'=x/2,'sqrt(x)'=sqrt(x))
X2

X2[[3]][3:5]

A=letters[1:4];B=letters[5:10];C=letters[11:15]
D=c(1:10);E=c(20:5)

x=list(A,B,C)

y=list(D,E)

z=list(x,y)
z

M=c(Date,X2)
M
n=M[c('year','x*2', 'day')]
n

length(n[[2]]) #length comprising all elements in list
length(n[2]) #length of the first element in the list

z[[1]][[2]][3]

z[[2]][[3]][5]


# Data Frames
Name <- c("Alex", "Lilly", "Mark", "Oliver", "Martha", "Lucas", "Caroline")
Age <- c(25, 31, 23, 52, 76, 49, 26)
Height <- c(177, 163, 190, 179, 163, 183, 164)
Weight <- c(57, 69, 83, 75, 70, 83, 53)
Sex <- as.factor(c("F", "F", "M", "M", "F", "M", "F"))
df <- data.frame (row.names = Name, Age, Height, Weight, Sex)
df
levels(df$Sex)<-c("M","F") # Reverse Sex data
df

Name <- c("Alex", "Lilly", "Mark", "Oliver", "Martha", "Lucas", "Caroline")
Working <- c("Yes", "No", "No", "Yes", "Yes", "No", "Yes")

df1<-data.frame(row.names = Name,Working)
df1
df2<-cbind(df,df1)
nrow(df2)
dim(df2)
ncol(df2)
sapply(df2,class) #Which class
str(df2) #Which class

class(state.center)

df<-as.data.frame(state.center)
df


v <- c(45:41, 30:33)
b <- LETTERS[rep(1:3, 3)]
n <- round(rnorm(9, 65, 5))

df <- data.frame(Age = v, Class = b, Grade = n)

df[with (df, order(Age)),]
df[order(df$Age), ]  # alternative solution

class(VADeaths)

df<-as.data.frame(VADeaths)
df

df$Total <- df[, 1] + df[, 2] + df[, 3] + df[, 4]
df$Total <- rowSums(df[1:4]) #Alternate Solution
df$Total
df <- df[, c(5, 1:4)]
df

class(state.x77)
df<-as.data.frame(state.x77)
df
nrow(subset(df, df$Income < 4300)) #no of rows with income < 4300
row.names(df)[(which(max(df$Income) == df$Income))] #State with max income


