#First, we view the data set
View(epi2024results06022024)

#We attach the data set as the default object
attach(epi2024results06022024)

#We print the data for the variable EPI.new
epi2024results06022024$EPI.new

#We set set true values for NA values
is.na(EPI.new)

#We filter out NA values from the row
EPI.new[!is.na(EPI.new)]

#Get the statistical summary for variable EPI.new (Mean, Median, 25th & 75th quartiles, min & max)
summary(EPI.new)

#Get the five-number summary of the variables EPI.new
fivenum(EPI.new)

#Get the stem-and-leaf plot of the variables EPI.new
stem(EPI.new)

#produce a histogram (frequencies of values in bars) of the variables EPI.new
hist(EPI.new)

#Produce a histogram sequence of distribution in the variable EPI.new
hist(EPI.new,seq(20.,80.,1.0), prob=TRUE)

#Compute kernel density estimation in the variable EPI.new
lines(density(EPI.new, na.rm = TRUE, bw=1.))

#Visualize the distribution of data using rug
rug(EPI.new)

#Visualize data distribution between EPI.new and APO.new
boxplot(EPI.new,APO.new)

#Produce a histogram sequence of distribution in the variable EPI.new
hist(EPI.new,seq(20.,80.,1.0), prob=TRUE)

#Compute kernel density estimation in the variable EPI.new
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

#We print the data for the variable APO.new
epi2024results06022024$APO.new

#We set set true values for NA values in APO.new
is.na(APO.new)

#We filter out NA values from the row
E <- APO.new[!is.na(APO.new)]
APO.new[!is.na(APO.new)]

#Get the statistical summary for variable APO.new (Mean, Median, 25th & 75th quartiles, min & max)
summary(APO.new)

#Get the five-number summary of the variable APO.new
fivenum(APO.new)

#Get the stem-and-leaf plot of the variable APO.new
stem(APO.new)

#produce a histogram (frequencies of values in bars) of the variables APO.new
hist(APO.new)

#Produce a histogram sequence of distribution in the variables APO.new
hist(APO.new,seq(0,110.,10.0), prob=TRUE)

#Compute kernel density estimation in the variable APO.new
lines(density(APO.new, na.rm = TRUE, bw=1.))

#Visualize the distribution of data using rug
rug(APO.new)

#Visualize data distribution between APO.new and WRS.new
boxplot(APO.new,WRS.new)

#Produce a histogram sequence of distribution in the variables APO.new
hist(APO.new,seq(0,110.,10.0), prob=TRUE)

#Compute kernel density estimation in the variables APO.new
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



#Exercise 2A -- Data frame: WRS.new
#We print the data for the variables WRS.new
epi2024results06022024$WRS.new

#First, we view the variables
View(epi2024results06022024)

attach(epi2024results06022024)

#We print the data for the variables WRS.new
epi2024results06022024$WRS.new

#We set set true values for NA values in APO.new
is.na(WRS.new)
tf<- is.na(WRS.new)

#We filter out NA values from the row
  E <- WRS.new[!tf]

#Get the statistical summary for variables APO.new (Mean, Median, 25th & 75th quartiles, min & max)
  summary(WRS.new)


  #Get the five-number summary of  WRS.new
  fivenum(WRS.new)
  
  #Get the stem-and-leaf plot of  WRS.new
  stem(WRS.new)

  #Get the stem-and-leaf plot of WRS.new
  stem(WRS.new)

  #produce a histogram (frequencies of values in bars) of WRS.new
  hist(WRS.new)
  
  #Produce a histogram sequence of distribution in  WRS.new
  hist(WRS.new,seq(0,110.,10.0), prob=TRUE)
  
  #Compute kernel density estimation in WRS.new
  lines(density(WRS.new, na.rm = TRUE, bw=1.))

  #Visualize the distribution of data using rug
  rug(WRS.new)

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
  plot(ecdf(WRS.new), do.points=FALSE, verticals = TRUE)

  #Return Quantile-Quantile
  qqnorm(WRS.new); qqline(WRS.new)

  #Make a Q-Q plot against the generating distribution
  qqplot(rnorm(250), WRS.new, xlab = "Q-Q plot for norm dsn")
  qqline(WRS.new)
  
  qqplot(rt(250, df=5), WRS.new, xlab = "Q-Q plot for t dsn")
  qqline(WRS.new)
  