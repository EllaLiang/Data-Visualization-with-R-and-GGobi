# ------------------------------------#
#             Question 1              #
# ------------------------------------#
# Using R to visualize the data
setwd("/Users/ella/Downloads")
library(readxl)
cars <- read_excel("cars.xls")
str(cars)
summary(cars)
table(cars$Cylinders)
table(cars$Year)
table(cars$Origin)

######## Histogram ########
library(ggplot2)
# Origin
qplot(cars$Origin,ylab ="Frequency",xlab ="Origin",binwidth=0.5,main="Frequency Histogram:Origin")
# Cylinders
qplot(cars$Cylinders,ylab ="Frequency",xlab ="Cylinders",binwidth=1,main="Frequency Histogram:Cylinders")
# remove rows where the Cylinders is 3 and 5
cars<-cars[!cars$Cylinders%in%c(3, 5),]

######## Scatterplot Matrix ########
pairs(cars)
attach(cars)
cars$Origin <- factor(cars$Origin)
cars.col<-Origin
pairs(cars,col=cars.col)

######## Scatterplt matrix with histogram ########
panel.smooth.asp<-function(x,y,col=par("col"),bg=NA,pch=par("pch"),
                           cex=1,col.smooth="red",span=2/3,iter=3,asp,...)
{
  points(x,y,pch=pch,col=col,bg=bg,cex=cex,asp=1) 
  ok<-is.finite(x) & is.finite(y)
  if (any(ok))
    lines(lowess(x[ok], y[ok], f=span, iter=iter), col=col.smooth,...)
}
# put (absolute) correlations on the upper panels,
# with size proportional to the correlations.
panel.cor<-function(x, y, digits=2, prefix="",cex.cor) {
  usr<-par("usr"); on.exit(par(usr))
  par(usr=c(0, 1, 0, 1))
  r<-abs(cor(x,y))
  txt<-format(c(r,0.123456789), digits=digits)[1] 
  txt<-paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex.cor<-0.8/strwidth(txt)*r 
  text(0.5, 0.5, txt, cex=cex.cor)
}
# put histograms on the diagonal
panel.hist<-function(x,...){
  usr<-par("usr");on.exit(par(usr))
  par(usr=c(usr[1:2],0,1.5))
  h<- hist(x, plot=FALSE)
  breaks<-h$breaks;nB<-length(breaks)
  y<-h$counts;y<-y/max(y)
  rect(breaks[-nB],0,breaks[-1],y,col="cyan",...)
}
pairs(cars,upper.panel=panel.cor,diag.panel=panel.hist)

# use region's name to represent different origin
Origin<-c("USA","Japan","Europe")
cars$Origin <- factor(cars$Origin, labels =Origin)

######## Boxplot ########
# convert Year variable to a factor
cars$Year <- factor(cars$Year)
# MPG comparison by Origin
ggplot(data = cars, aes(x=Origin, y =MPG)) +
  geom_boxplot() +
  xlab("Origin") +
  ylab("MPG") +
  ggtitle("Boxplot: MPG Comparison by Origin")
# Weight Comparison by Origin
ggplot(data = cars, aes(x = Origin, y = Weight)) +
  geom_boxplot() +
  xlab("Origin") +
  ylab("Weight") +
  ggtitle("Boxplot: Weight Comparison by Origin")

# convert Cylinders variable to a factor
cars$Cylinders<- factor(cars$Cylinders)
# MPG Comparison by Cylinders
ggplot(data = cars, aes(x = Cylinders, y = MPG)) +
  geom_boxplot() +
  xlab("Cylinders") +
  ylab("MPG") +
  ggtitle("Boxplot: MPG Comparison by Cylinders") 
# Weight Comparison by Cylinders
ggplot(data = cars, aes(x = Cylinders, y = Weight)) +
  geom_boxplot() +
  xlab("Cylinders") +
  ylab("Weight") +
  ggtitle("Boxplot: Weight Comparison by Cylinders")
# illustrate the type of cars in different regions
ggplot(data = cars, aes(x =Cylinders, fill = Origin)) +
  geom_bar() +
  xlab("Cylinders") +
  ylab("Count") +
  ggtitle("Cars from Each Region by Cylinders")
# illustrate each region of origin's product changes over time  
ggplot(data = cars, aes(x = Year, fill = Cylinders)) +
  geom_bar() +
  facet_wrap(~ Origin, ncol = 1) +
  xlab("Year") +
  ylab("Count") +
  ggtitle("Each Manufanture\'s Product Mix Over Time")
# Change of MPG Over Time by Origin
ggplot(data = cars, aes(x = Year, y = MPG)) +
  geom_boxplot() +
  facet_wrap(~ Origin) +
  xlab('Year') +
  ylab('MPG') +
  ggtitle('Change of MPG Over Time by Origin')
# Weight Changes Over Time by Origin
ggplot(data = cars, aes(x = Year, y = Weight)) +
  geom_boxplot() +
  facet_wrap(~ Origin) +
  xlab("Year") +
  ylab("Weight") +
  ggtitle("Weight Changes Over Time by Origin")

######## Conditional plots ########
coplot(MPG~Weight|Horsepower,data=cars,col=cars.col,pch=16)
coplot(MPG~Weight|Horsepower,data=cars,overlap=0.1,col=cars.col,pch=16)

# Using Ggobi to visualize the data
library(rggobi)
g<-ggobi(cars)
display(g[1],'Scatterplot Matrix')
display(g[1],'2D Tour')
display(g[1],'Parallel Coordinates Display')

# ------------------------------------#
#             Question 2              #
# ------------------------------------#
library(readxl)
Online_Retail<-read_excel('Online Retail.xlsx')
str(Online_Retail)
# Data cleaning
# Remove cancelled orders
retail<- Online_Retail[-grep("[C]",Online_Retail$InvoiceNo),]
# Remove orders with invalid item description
retail_1 <- retail[-grep("[a-z]",retail$Description),]
retail_2 <- retail_1[-grep("\\?",retail_1$Description),]
# Find missing values in the description column then remove corresponding rows
complete.cases(retail_2)
which(!complete.cases(retail_2$Description))
na_vec<-which(!complete.cases(retail_2$Description))
retail_no_na<-retail_2[-na_vec,]
which(!complete.cases(retail_no_na$Description))
# Use aggregate function to convert our data to transactions, "basket" format
transaction_item<- aggregate(retail_no_na$Description~retail_no_na$InvoiceNo,FUN=paste,collapse=',')
# Remove the first column
itemsets<-transaction_item[,-1]
# Export data to a csv file
write(itemsets,"itemlist.csv")
library(arules)
# Use read.transaction function to read a transaction data then create an object
itemlist<-read.transactions("itemlist.csv",format="basket",rm.duplicates=TRUE,sep=",",quote="")
summary(itemlist)
# Use arules to identify top 10 rules
rules<-apriori(itemlist,parameter=list(supp=0.02,conf=0.8,target="rules"))
summary(rules)
inspect(rules[1:10])
rules<-apriori(itemlist,parameter=list(supp=0.03,conf=0.5,target="rules"))
summary(rules)
inspect(rules[1:10])
