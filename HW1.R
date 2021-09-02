# HW 1.1

getwd()

# setwd("/Users/Aaron/Dropbox/Documents/Big Data")

#### Purchases of Ben and Jerry's Ice Cream
benjer <- read.csv("BenAndJerry.csv")

## explore a bit
names(benjer)

## create a new variable for price per unit
priceper1 = (benjer$price_paid_deal + benjer$price_paid_non_deal)/benjer$quantity
y <- log(1+priceper1)

## grab some covariates of interest
## we'll create a properly formatted data.frame
x <- benjer[,c("flavor_descr","size1_descr",
               "household_income","household_size")]

head(x)

## relevel 'flavor' to have baseline of vanilla
x$flavor_descr <- relevel(x$flavor_descr,"VAN")
## coupon usage
x$usecoup = factor(benjer$coupon_value>0)
x$couponper1 <- benjer$coupon_value/benjer$quantity
## organize some demographics
x$region <- factor(benjer$region, 
                   levels=1:4, labels=c("East","Central","South","West"))
x$married <- factor(benjer$marital_status==1)
x$race <- factor(benjer$race,
                 levels=1:4,labels=c("white","black","asian","other"))
x$hispanic_origin <- benjer$hispanic_origin==1
x$microwave <- benjer$kitchen_appliances %in% c(1,4,5,7)
x$dishwasher <- benjer$kitchen_appliances %in% c(2,4,6,7)
x$sfh <- benjer$type_of_residence==1
x$internet <- benjer$household_internet_connection==1
x$tvcable <- benjer$tv_items>1

## combine x and y, just to follow my recommended `way to use glm'
## cbind is `column bind'.  It takes two dataframes and makes one.
xy <- cbind(x,y)

## fit the regression
fit <- lm(y~., data=xy)
summary(fit)

summary(fit)

## grab the non-intercept p-values from a glm
## -1 to drop the intercept, 4 is 4th column
pvals <- summary(fit)$coef[-1,4]

## source the fdr_cut function
source("fdr.R")

summary(fit)$coef[-1,4]

fdr_cut(pvals,.1,TRUE)

summary(fit)$adj.r.squared
pvals


qtest <- function(pvals, start, end, index){
  ## A function to find relationship b/t Q value and
  ## number of "significant" coefficients
  qvals <- seq(start,end,by=index)
  ## create a range of q-values to test
  pstar <- sapply(qvals,function(x,y) fdr_cut(y,x), y=pvals)
  ## run the fdr_cut function on each potential q value
  ## spits out a vector of the resulting p-stars
  numco_gross <- sapply(pstar,function(x) length(pvals[pvals<x]))
  ## for each p-star find the number of pvals that are less than it
  numco_net <- numco_gross*(1-qvals)
  ## find the "net" pvals -- i.e., the gross minus the expected # of false positives
  delta <- numco_gross-numco_net
  z <- cbind(qvals,pstar,numco_gross,numco_net,delta)
  ## combine all the vectors into a matrix for convenience
  plot(qvals,numco_gross,xlab="Q Value",ylab="Significant Coefficients: Gross in Black, Net in Red")
  points(qvals,numco_net,col="Red")
  return(z)
}

## you can enter your own values for "start", "end", and "index"
qtest(pvals,.001,.2,.001)

sigpvals <- names(pvals[pvals<.02])
sigflavor <- sigpvals[grepl("flavor",sigpvals)]
sigflavor <- sapply(strsplit(sigflavor, split='descr', fixed=TRUE), function(x) (x[2]))

flavorlevels <- levels(xy$flavor_descr)
sigflavorlevels <- ifelse(flavorlevels %in% sigflavor, flavorlevels, "Other")
xy$sigflavor_descr <- xy$flavor_descr
levels(xy$sigflavor_descr) <- sigflavorlevels