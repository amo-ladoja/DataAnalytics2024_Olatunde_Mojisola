#First, we view the data set
View(epi2024results06022024)

#We attach the data set as the default object
attach(epi2024results06022024)

#We print the data for the data frame EPI.new
epi2024results06022024$EPI.new

#We set set true values for NA values
is.na(EPI.new)

#We filter out NA values from the row
EPI.new[!is.na(EPI.new)]

#Get the statistical summary for data frame EPI.new (Mean, Median, 25th & 75th quartiles, min & max)
summary(EPI.new)

#Get the five-number summary of the data frame EPI.new
fivenum(EPI.new)

#Get the stem-and-leaf plot of the data frame EPI.new
stem(EPI.new)

#produce a histogram (frequencies of values in bars) of the data frame EPI.new
hist(EPI.new)

#Produce a histogram sequence of distribution in the data frame EPI.new
hist(EPI.new,seq(20.,80.,1.0), prob=TRUE)

#Compute kernel density estimation in the data frame EPI.new
lines(density(EPI.new, na.rm = TRUE, bw=1.))

#Visualize the distribution of data using rug
rug(EPI.new)

#Visualize data distribution between EPI.new and APO.new
boxplot(EPI.new,APO.new)

#Produce a histogram sequence of distribution in the data frame EPI.new
hist(EPI.new,seq(20.,80.,1.0), prob=TRUE)

#Compute kernel density estimation in the data frame EPI.new
lines(density(EPI.new, na.rm = TRUE, bw=1.))

#Visualize the distribution of data using rug
rug(EPI.new)

#Make x a sequence of the values
x <- seq(20,80,1)

#Make q a probability density using the values
q <- dnorm(x,mean = 42, sd=5, log = FALSE)

#create lines between x and q
lines(x,q)

lines(x,.4*q)

#Change the values of q
q <- dnorm(x,mean = 65, sd=5, log = FALSE)

#Create lines x and q
lines(x,.12*q)

#Return the cumulative density
plot(ecdf(EPI.new), do.points=FALSE, verticals = TRUE)

#Return Quantile-Quantile
qqnorm(EPI.new); qqline(EPI.new)

#Make a Q-Q plot against the generating distribution
qqplot(rnorm(250), EPI.new, xlab = "Q-Q plot for norm dsn")
qqline(EPI.new)

qqplot(rt(250, df=5), EPI.new, xlab = "Q-Q plot for t dsn")
qqline(EPI.new)





#Exercise 2A -- Data frame: APO.new

#We print the data for the data frame APO.new
epi2024results06022024$APO.new

#We set set true values for NA values in APO.new
is.na(APO.new)

#We filter out NA values from the row
E <- APO.new[!is.na(APO.new)]
APO.new[!is.na(APO.new)]

#Get the statistical summary for data frame APO.new (Mean, Median, 25th & 75th quartiles, min & max)
summary(APO.new)

#Get the five-number summary of the data frame APO.new
fivenum(APO.new)

#Get the stem-and-leaf plot of the data frame APO.new
stem(APO.new)

#produce a histogram (frequencies of values in bars) of the data frame APO.new
hist(APO.new)

#Produce a histogram sequence of distribution in the data frame APO.new
hist(APO.new,seq(0,110.,10.0), prob=TRUE)

#Compute kernel density estimation in the data frame APO.new
lines(density(APO.new, na.rm = TRUE, bw=1.))

#Visualize the distribution of data using rug
rug(APO.new)

#Visualize data distribution between APO.new and WRS.new
boxplot(APO.new,WRS.new)

#Produce a histogram sequence of distribution in the data frame APO.new
hist(APO.new,seq(0,110.,10.0), prob=TRUE)

#Compute kernel density estimation in the data frame APO.new
lines(density(APO.new, na.rm = TRUE, bw=1.))

#Visualize the distribution of data using rug
rug(APO.new)

#Make x a sequence of the values
x <- seq(0,110,10)

#Make q a probability density using the values
q <- dnorm(x,mean = 42, sd=5, log = FALSE)

#create lines between x and q
lines(x,q)
lines(x,.4*q)

#Change the values of q
q <- dnorm(x,mean = 65, sd=5, log = FALSE)

#Create lines x and q
lines(x,.12*q)

#Return the cumulative density
plot(ecdf(APO.new), do.points=FALSE, verticals = TRUE)

#Return Quantile-Quantile
qqnorm(APO.new); qqline(APO.new)

#Make a Q-Q plot against the generating distribution
qqplot(rnorm(250), APO.new, xlab = "Q-Q plot for norm dsn")
qqline(APO.new)

qqplot(rt(250, df=5), APO.new, xlab = "Q-Q plot for t dsn")
qqline(APO.new)
