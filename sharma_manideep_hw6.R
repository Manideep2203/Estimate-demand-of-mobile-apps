
##################################### HOMEWORK 6 - WDA ####################################################

#Setting up the working directory
setwd("/Users/manideep_sharma/Documents/Purdue documents/Fall mod 2/MGMT 590 WDA/Homework 6 due Nov 30")


# Laading Mobile apps data

gmapps <- read.csv("GlobalMobileAppsExercise.csv", sep=",", header=T); # data is loaded to a frame named gmapps

# making objects within the dataframe accessible with fewer keystrokes
attach(gmapps) 

#Features
names(gmapps)

#Create summary
summary(gmapps)

# looking at some characteristics

hist(rank);
hist(log(gmapps$rank));

hist(gmapps$price, xlab="Price", ylab="Apps", main="Distribution of Price", density=10);
hist(log(1+gmapps$price), xlab="Log(Price)", ylab="Apps", main="Distribution of Logged Price", density=10);

hist(filesize); 
hist(filesize, breaks=5);


########################
#   CORRELATION        #
########################

cor(gmapps$rank, gmapps$price);
cor(log(gmapps$rank),log(1+gmapps$price));

#########################
#      T-test           #
#########################

#Difference between app prices for different devices

tabletprice <- gmapps[deviceindex==2,]
smprice <- gmapps[!deviceindex==2,]

hist(tabletprice$price)
hist(smprice$price)

hist(log(1+tabletprice$price), col='blue');
hist(log(1+smprice$price), col = 'red');

summary (tabletprice$price)
summary (smprice$price)

t.test( tabletprice$price, smprice$price, alternative="greater")

t.test( tabletprice$price, smprice$price, alternative="two.sided") # default is two.sided

t.test( tabletprice$filesize, smprice$filesize, alternative="two.sided")

#An alternative to creating new data subsets

t.test( gmapps[deviceindex==2,]$filesize, gmapps[!deviceindex==2,]$filesize, alternative="two.sided") 
t.test(filesize~deviceindex)

#########################
#  Linear Regression    #
#########################

#create new variables by log transforming skewed variables identified above
gmapps$lrank = log(gmapps$rank)
gmapps$lprice = log(1+gmapps$price)
gmapps$lfilesize = log(gmapps$filesize)

# building a model to estimate demand for mobile apps
r1 <- lm(lrank~lprice+in_app_ads+lfilesize+average_rating+in_app_purchase+num_screenshot, data=gmapps)
summary(r1)
summary(r1)
exp(coef(r1))


#Run a  split sample analysis by Region

summary(gmapps$region)

#model for US
r2US <- lm(lrank~lprice+in_app_ads+lfilesize+average_rating+in_app_purchase+num_screenshot, data=gmapps[region=="US",])
summary(r2US)
exp(coef(r2US))

#model for China
r2China <- lm(lrank~lprice+in_app_ads+lfilesize+average_rating+in_app_purchase+num_screenshot, data=gmapps[region=="CN",])
summary(r2China)
exp(coef(r2China))

#95% CI for the lprice coeff
confint(r2US, 'lprice', level=0.95)
confint(r2China, 'lprice', level=0.95)


#Run a  split sample analysis by Device

summary(gmapps$device)

#model for smartphone
rSmrtphone <- lm(lrank~lprice+in_app_ads+lfilesize+average_rating+in_app_purchase+num_screenshot, data=gmapps[device=="smart_phone",])
summary(rSmrtphone)
exp(coef(rSmrtphone))

#model for tablet
rTab <- lm(lrank~lprice+in_app_ads+lfilesize+average_rating+in_app_purchase+num_screenshot, data=gmapps[device=="tablet",])
summary(rTab)
exp(coef(rTab))

#95% CI for the lprice coeff
confint(rSmrtphone, 'lprice', level=0.95)
confint(rTab, 'lprice', level=0.95)


#Run a  split sample analysis by platform

summary(gmapps$device)

#model for Apple
rApple <- lm(lrank~lprice+in_app_ads+lfilesize+average_rating+in_app_purchase+num_screenshot, data=gmapps[app_store=="Apple",])
summary(rApple)
exp(coef(rApple))

#model for Google
rGoogle <- lm(lrank~lprice+in_app_ads+lfilesize+average_rating+in_app_purchase+num_screenshot, data=gmapps[app_store=="Google Play",])
summary(rGoogle)
exp(coef(rGoogle))


#Run a  split sample analysis by app price

summary(gmapps$app_type)

#model for Free
rFree <- lm(lrank~lprice+in_app_ads+lfilesize+average_rating+in_app_purchase+num_screenshot, data=gmapps[app_type=="free",])
summary(rFree)
exp(coef(rFree))

#model for Paid
rPaid <- lm(lrank~lprice+in_app_ads+lfilesize+average_rating+in_app_purchase+num_screenshot, data=gmapps[app_type=="paid",])
summary(rPaid)
exp(coef(rPaid))

