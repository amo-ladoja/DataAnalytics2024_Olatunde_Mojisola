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

#We set set true values for NA values in WRS.new
is.na(WRS.new)
tf<- is.na(WRS.new)

#We filter out NA values from the row
  E <- WRS.new[!tf]

#Get the statistical summary for variables WRS.new (Mean, Median, 25th & 75th quartiles, min & max)
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
  
  
  
  #### Lab 02 Beginning population dataset ### Lab 02
  
### fitting a distribution beyond histograms for EPI.new
  
  attach(epi2024results06022024)
  qqnorm(EPI.new); qqline(EPI.new)

  ### Make a Q-Q plot against the generating distribution
  x <- seq(20., 80., 1.0)
  qqplot(qnorm(ppoints(200)), x)  
  qqline(x)  
  qqplot(qnorm(ppoints(200)),EPI.new)  
  qqline(EPI.new)  
  
  ### Cumulative density function for EPI.new
  
  plot(ecdf(EPI.new), do.points=FALSE)
  plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE)   
  lines(ecdf(EPI.new))  
  
  
  
  ### fitting a distribution beyond histograms for APO.new
  
  qqnorm(APO.new); qqline(APO.new)
  
  ### ### Make a Q-Q plot against the generating distribution for APO.new
  x <- seq(0,110,10)
  qqplot(qnorm(ppoints(200)), x)  
  qqline(x)
  qqplot(qnorm(ppoints(200)),APO.new)  
  qqline(APO.new)  
  
  ### Cumulative density function for APO.new
  
  plot(ecdf(APO.new), do.points=FALSE)
  plot(ecdf(rnorm(1000, 45, 10)), do.points=FALSE)  
  lines(ecdf(APO.new))  
  
  
  ### fitting a distribution beyond histograms for WRS.new
  qqnorm(WRS.new); qqline(WRS.new)

  ### ### Make a Q-Q plot against the generatning distribution for WRS.new
  x <- seq (0,110,10)
  qqplot(qnorm(ppoints(200)), x)  
  qqline(x)  
  qqplot(qnorm(ppoints(200)),WRS.new) 
  qqline(WRS.new)
  
  ## Cumulative density function for WRS.new
  
  plot(ecdf(WRS.new), do.points=FALSE)
  plot(ecdf(rnorm(1000,45,10)), do.points=FALSE)  
  lines(ecdf(WRS.new))  
  
  
  ### Comparing distribution APO.new and APO.old -- Boxplot
  
  boxplot(APO.old, APO.new, names=c("APO.old","APO.new"))
  
  
  
  ###Lab 02 Exercise 02
  
  #Read in data population_newyear, epi.results, epi.weights
  
  population_newyear <- read.csv("~/ITWS-1100/DataAnalytics2024_Olatunde_Mojisola/Lab02/countries_populations_2023.csv")
View(population_newyear)  

epi.result <- epi2024results06022024
epi.weights <- read.csv("~/ITWS-1100/DataAnalytics2024_Olatunde_Mojisola/Lab02/epi2024weights.csv")

# Drop countries not in epi results
populations <- population_newyear[-which(!population_newyear$Country %in% epi.result$country),]


## sort populations by country
populations <- populations[order(populations$Country),]

#drop countries not in populations
epi.result.sub <- epi.result[-which(!epi.result$country %in% populations$Country),]

### sort epi results by country
epi.result.sub <- epi.result.sub[order(epi.result.sub$country),]

##only keep necessary columns
epi.result.sub <- epi.result.sub[,c("country","EPI.old","EPI.new")]

##convert population to numeric
epi.result.sub$population <- as.numeric(populations$Population)

#compute population log base 10
epi.result.sub$population_log <- log10(epi.result.sub$population)


##Linear Model  for EPI

lin.mod.epinew <- lm(EPI.new~population_log,epi.result.sub)
plot(EPI.new~population_log)
abline(lin.mod.epinew)
summary(lin.mod.epinew)
plot(lin.mod.epinew)


##ggplot
ggplot(epi.result.sub, aes(x = population_log, y=EPI.new)) +
  geom_point() +
  stat_smooth(method ="lm")

ggplot(lin.mod.epinew, aes(x = .fitted, y = .resid)) +
  geom_point() +
  geom_hline(yintercept = 0) +
  labs(title='Residual vs. Fitted Values Plot', x='Fitted Values', y='Residuals')



#boxplot comparing 3 variables

attach(epi2024results06022024)

#We print the data for the variable PFL.new
epi2024results06022024$PFL.new

#We set set true values for NA values in PFL.new
is.na(PFL.new)
pf<- is.na(PFL.new)

#Filter False values from PFL
E <- PFL.new[!pf]

#Get the statistical summary for variables PFL.new (Mean, Median, 25th & 75th quartiles, min & max)
summary(PFL.new)


#Get the five-number summary of  PFL.new
fivenum(PFL.new)

#Get the stem-and-leaf plot of  PFL.new
stem(PFL.new)

#Get the stem-and-leaf plot of PFL.new
stem(PFL.new)


#boxplot 
boxplot (EPI.new, APO.new, PFL.new)

#boxplot named
boxplot(EPI.new, APO.new, PFL.new, names=c("EPI.new","APO.new","APL.new"))




##Comparing ECDFs of EPI.new & WRS.new

plot(ecdf(EPI.new), col = "red", main = "Comparison of ECDFs", xlab = "X-axis Label", ylab = "Y-axis Label")
lines(ecdf(WRS.new), col = "blue")

##Adding APO.new to the EPI.new & WRS.new comparison 
lines(ecdf(APO.new), col = "green")


