
nor <- read.csv('/Users/sarahkatz/Downloads/CS 350 /CountyCSVs/norfolk_subset.csv',header=T)

smallerdataset <- nor[,1:30]
write.csv(smallerdataset,'/Users/sarahkatz/Downloads/CS 350 /norfolksmallerdataset.csv')


# explore data ----
dim(nor) # 24638 houses, 937 variables  
sum(duplicated(nor)) # 0 duplicates 

# clean data ---- 
sum(is.na(nor))
sum(is.na(nor$lastSoldPrice)) # 0 NA for response variable 
sum(nor$lastSoldPrice=='') # 0 blank for response variable 
sum(nor$lastSoldPrice==0) # 1 zero value for response variable, not possible, remove below  
nor <- nor[nor$lastSoldPrice!=0,] # removed 1 row 
nrow(nor[nor$address_zipcode==20171,]) # remove 1 row, not a legitimate zip code 
nor <- nor[nor$address_zipcode!=20171,]

zipsvector <- unique(nor$address_zipcode)
# for each zip code, find the number of houses 
counts = c() 
for (i in 1:length(zipsvector)){
  count <- nrow(nor[nor$address_zipcode==zipsvector[i],])
  counts <- c(counts, count)
}
counts 

# remove all zip codes with fewer than 10 houses 
dfzipscounts <- data.frame(zipsvector, counts)
smallzips <- dfzipscounts$zipsvector[dfzipscounts$counts < 10]
length(smallzips) # 11 zip codes to remove 
nor1 <- nor[-which(nor$address_zipcode %in% smallzips),]

length(unique(nor1$address_zipcode)) # 58 
dim(nor1) # 24608 houses 

# boxplot of lastSoldPrice 
boxplot(nor1$lastSoldPrice)
median(nor1$lastSoldPrice) # 650000 
mean(nor1$lastSoldPrice) # 836151.9

# visualize data ---- 
# for each zip code, find the mean and median and put those separately into vectors 
zipsvector <- unique(nor1$address_zipcode)
length(zipsvector) # 69 unique zip codes 
pricemeanszips <- c() 
pricemedianszips <- c() 
for (i in 1:length(zipsvector)){
  temp <- nor1$lastSoldPrice[nor1$address_zipcode==zipsvector[i]]
  pricemeanszips <- c(pricemeanszips,mean(temp)) 
  pricemedianszips <- c(pricemedianszips, median(temp))
}
pricemeanszips 
pricemedianszips 

pricemeanszipsdf <- data.frame(zipsvector, pricemeanszips) 
pricemedianszipsdf <- data.frame(zipsvector, pricemedianszips)

pricemeanszipsdfsorted <- pricemeanszipsdf[order(pricemeanszipsdf$pricemeanszips),]
pricemedianszipsdfsorted <- pricemedianszipsdf[order(pricemedianszipsdf$pricemedianszips),]

par(mar = c(6, 5.5, 4, 4))
barplot(pricemeanszipsdfsorted$pricemeanszips / 1000000, 
        names.arg = paste0('0',as.character(pricemeanszipsdfsorted$zipsvector)),
        main = 'Average Last Sold Prices for Norfolk County Zip Codes',
        xlab = "Zip Code",
        ylab = "Average Last Sold Price \n(in millions of USD)", 
        las=2,
        cex.names = 0.85,
        col='darkblue',
        cex.main = 2,
        cex.axis = 1,
        ylim = c(0,3.5))
abline(h=mean(nor1$lastSoldPrice)/ 1000000, lwd = 2,col='red')

par(mar = c(6, 5.5, 4, 4))
barplot(pricemedianszipsdfsorted$pricemedianszips / 1000000, 
        names.arg = paste0('0',as.character(pricemedianszipsdfsorted$zipsvector)),
        main = 'Median Last Sold Prices for Norfolk County Zip Codes',
        xlab = "Zip Code",
        ylab = "Median Last Sold Price \n(in millions of USD)", 
        las=2,
        cex.names = 0.85,
        col='darkblue',
        cex.main = 2,
        cex.axis = 1,
        ylim = c(0,3.5)) 
abline(h=median(nor1$lastSoldPrice)/ 1000000, lwd = 2,col='red')

plot(nor1$livingAreaValue, nor1$lastSoldPrice)

x = nor1$livingAreaValue[nor1$livingAreaValue<40000 & nor1$lastSoldPrice<20000000]
y = nor1$lastSoldPrice[nor1$livingAreaValue<40000 & nor1$lastSoldPrice<20000000]/1000000
lmodel <- lm(y~x)
plot(x, y, las=1, 
     main="Relationship between Square Footage and Home Price \nfor Norfolk County Homes",
     xlab="Living Area (in square feet)",
     ylab="Home Price (in millions of USD)")
abline(a=coef(lmodel)[1], b=coef(lmodel)[2])

# need to remove NAs 
cor(x,y)
cor(nor1$livingAreaValue,nor1$lastSoldPrice)
# plot(log(nor1$livingAreaValue[nor1$livingAreaValue<40000 & nor1$lastSoldPrice<20000000]), 
#      log(nor1$lastSoldPrice[nor1$livingAreaValue<40000 & nor1$lastSoldPrice<20000000]))



length((nor1$livingAreaValue[nor1$livingAreaValue<40000 & nor1$lastSoldPrice<20000000]))
dim(nor1) # excluded 7 outliers 

# look closer at numeric variables 
numericCols <- which(sapply(nor1, is.numeric))

newmodel <- lm(lastSoldPrice ~ 
               bedrooms+
  bathrooms+livingAreaValue+resoFacts_garageParkingCapacity
  +schools_0_distance
  +schools_0_rating
+resoFacts_parkingCapacity
+resoFacts_rooms_0_roomArea
+priceHistory_0_price
#+priceHistory_0_priceChangeRate,
+taxHistory_0_taxPaid
#+taxHistory_0_taxIncreaseRate
+taxHistory_0_value,
#+taxHistory_0_valueIncreaseRate,
data=nor1) 
summary(newmodel) # Adjusted R-squared:  0.978  


table(nor1$taxHistory_0_taxPaid)


attach(nor1)
xmat <- cbind(bedrooms,
                bathrooms,livingAreaValue,resoFacts_garageParkingCapacity,schools_0_distance,
                schools_0_rating,
              resoFacts_parkingCapacity,
              resoFacts_rooms_0_roomArea,
              priceHistory_0_price,
              priceHistory_0_priceChangeRate,
              taxHistory_0_taxPaid,
              #taxHistory_0_taxIncreaseRate,
              taxHistory_0_value)
              #taxHistory_0_valueIncreaseRate)

cor(xmat)
library(usdm) 
vifstep(xmat)

# remove variables with multicollinearity issues: 
# taxHistory_0_taxIncreaseRate taxHistory_0_value 

newmodel2 <- lm(lastSoldPrice ~ #factor(address_zipcode) + 
                  bedrooms+
                 bathrooms+livingAreaValue+resoFacts_garageParkingCapacity+schools_0_distance+
                 schools_0_rating
               +resoFacts_parkingCapacity
               +resoFacts_rooms_0_roomArea,
               #+priceHistory_0_price
               #+taxHistory_0_taxPaid,
               data=nor1) 
summary(newmodel2) 


newmodel2test <- lm(lastSoldPrice ~ #resoFacts_garageParkingCapacity+schools_0_distance
                #+resoFacts_parkingCapacity
                priceHistory_0_price
                +taxHistory_0_taxPaid,
                data=nor1) 
summary(newmodel2test)



xmat2 <- cbind(bedrooms,
                 bathrooms,livingAreaValue,resoFacts_garageParkingCapacity,schools_0_distance,
                 schools_0_rating,
               resoFacts_parkingCapacity,
               resoFacts_rooms_0_roomArea,
               priceHistory_0_price,
               priceHistory_0_priceChangeRate,
               taxHistory_0_taxPaid,
               taxHistory_0_valueIncreaseRate)

vifstep(xmat2) # no more multicollinearity issues. 

# check what the output would be for variables that are most straightforward. 
newmodel3 <- lm(lastSoldPrice ~ 
                  bedrooms+
                  bathrooms+livingAreaValue+resoFacts_garageParkingCapacity+schools_0_distance+
                  schools_0_rating, data=nor1)
summary(newmodel3) # Adjusted R-squared:  0.6601 
xmat3 <- cbind(bedrooms,
                 bathrooms,livingAreaValue,resoFacts_garageParkingCapacity,schools_0_distance,
                 schools_0_rating)
vifstep(xmat3) # no multicollinearity 

# best not to use price or tax history - too highly correlated with last sold price. 

newmodel4 <- lm(lastSoldPrice ~ factor(address_zipcode)+
                  bedrooms+bathrooms+livingAreaValue+resoFacts_garageParkingCapacity+
                  schools_0_distance+
                  schools_0_rating+ 
                  resoFacts_parkingCapacity,
                resoFacts_rooms_0_roomArea,
                data=nor1) 
summary(newmodel4) # Adjusted R-squared:  0.8729  

xmat4 <- cbind(factor(address_zipcode),bedrooms,bathrooms,livingAreaValue,resoFacts_garageParkingCapacity,schools_0_distance,
                schools_0_rating, resoFacts_parkingCapacity,
                resoFacts_rooms_0_roomArea)
vifstep(xmat4) # no multicollinearity issues 


#################################################### issues with stepwise and transformation ----
modelstep <- step(newmodel)

sum(is.na(nor1$bathrooms))
plot(fitted(newmodel2),resid(newmodel2))
abline(h=0)

library(MASS)
boxcox(newmodel2) # about 0.8 
nor1$transformedlastSoldPrice <- nor1$lastSoldPrice^(0.8) 
newmodel2transformed <- lm(transformedlastSoldPrice ~ 
                                          bedrooms+
                                          bathrooms+livingAreaValue+resoFacts_garageParkingCapacity+schools_0_distance+
                                          schools_0_rating
                                        +resoFacts_parkingCapacity
                                        +resoFacts_rooms_0_roomArea
                                        +priceHistory_0_price
                                        +taxHistory_0_taxPaid,
                                        data=nor1) # Adjusted R-squared:  0.9721  
summary(newmodel2transformed)

plot(fitted(newmodel2transformed),resid(newmodel2transformed))
abline(h=0)
####################################################


# line graphs (may be used or excluded) ----
# note: 11/2023 will be excluded as it does not fit into the last 3 month period. 
add3MonthPeriods <- function(df){
  df$p1 <- ifelse(df$dateSoldStringMY >= '2020-11' & df$dateSoldStringMY <= '2021-01',1,0)
  df$p2 <- ifelse(df$dateSoldStringMY >= '2021-02' & df$dateSoldStringMY <= '2021-04',1,0)
  df$p3 <- ifelse(df$dateSoldStringMY >= '2021-05' & df$dateSoldStringMY <= '2021-07',1,0)
  df$p4 <- ifelse(df$dateSoldStringMY >= '2021-08' & df$dateSoldStringMY <= '2021-10',1,0)
  
  df$p5 <- ifelse(df$dateSoldStringMY >= '2021-11' & df$dateSoldStringMY <= '2022-01',1,0)
  df$p6 <- ifelse(df$dateSoldStringMY >= '2022-02' & df$dateSoldStringMY <= '2022-04',1,0)
  df$p7 <- ifelse(df$dateSoldStringMY >= '2022-05' & df$dateSoldStringMY <= '2022-07',1,0)
  df$p8 <- ifelse(df$dateSoldStringMY >= '2022-08' & df$dateSoldStringMY <= '2022-10',1,0)
  
  df$p9 <- ifelse(df$dateSoldStringMY >= '2022-11' & df$dateSoldStringMY <= '2023-01',1,0)
  df$p10 <- ifelse(df$dateSoldStringMY >= '2023-02' & df$dateSoldStringMY <= '2023-04',1,0)
  df$p11 <- ifelse(df$dateSoldStringMY >= '2023-05' & df$dateSoldStringMY <= '2023-07',1,0)
  df$p12 <- ifelse(df$dateSoldStringMY >= '2023-08' & df$dateSoldStringMY <= '2023-10',1,0) 
  
  return(df)
} 

nor2 <- add3MonthPeriods(nor4) 
df = nor2 



zipsvector <- unique(nor2$address_zipcode)
which(colnames(df)=='p1') # 928 
which(colnames(df)=='p12') 

mat1 = matrix(data=NA, nrow=length(zipsvector)+1, ncol=12) 
rownames(mat1) = c(zipsvector,'total')  
periodnames <- c('Nov20-Jan21','Feb21-Apr21','May21-Jul21','Aug21-Oct21',
                 'Nov21-Jan22','Feb22-Apr22','May22-Jul22','Aug22-Oct22',
                 'Nov22-Jan23','Feb23-Apr23','May23-Jul23','Aug23-Oct23')
colnames(mat1) = periodnames 

num = 928
for (j in 1:12) {
  for (i in 1:length(zipsvector)+1){
    meanprice = mean(df$lastSoldPrice[df$address_zipcode==zipsvector[i] & df[,num]==1])
    if (is.nan(meanprice)) {
      meanprice <- NA
    }
    if (i==length(zipsvector)+1){
      meanprice = mean(df$lastSoldPrice[df[,num]==1])
    }
    mat1[i,j] = meanprice
  }
  num = num + 1
}

test <- c(1:12)

length(test)
length(mat1[1,])

par(mar = c(6, 5.5, 4, 4))

plot(test, mat1[1,], type = "l", main = "Norfolk Average Sold Price by Zip Code \nfrom Nov 2020 to Oct 2023", xlab='',ylab='',ylim=c(0, 2500000), las=2, cex.main = 1.5,cex.axis = 0.75,xaxt = "n" )

for (rownum in 1:nrow(mat1)){
  lines(test, mat1[rownum,], type = "l", col=generate.random.colors(24)) 
} 

lines(test, mat1[length(zipsvector)+1,], type = "l", col='black',lwd=3.5)
title(xlab="Time Frame (3-Month Intervals)", mgp=c(4.75,1,0), cex.lab=1)
title(ylab = "Average Sold Price (in USD)", mgp=c(4,1,0),cex.lab=1) 
axis(1, at = seq(length(test)), labels = periodnames, las=2,cex.axis=0.7)

#### ---- 

# earliest dates graph ----

remNA_addMY <- function(df){
  df1 <- df[which(!is.na(df$lastSoldPrice)),]
  df2 <- df1[which(!is.na(df1$dateSoldString)),]
  df3 <- df2[which(df2$lastSoldPrice!=''),]
  if (!is.na(sum(df$dateSoldString==''))){
    df3 <- df3[-which(df3$dateSoldString==''),]
  } 
  df3$dateSoldString <- as.Date(df3$dateSoldString) 
  df3$dateSoldStringMY <- format(df3$dateSoldString, "%Y-%m")
  return(df3)
}
nor2 <- remNA_addMY(nor1)


uniquezips <- function(df){ 
  vec <- unique(df$address_zipcode)
  return(vec)
}
length(uniquezips(nor2)) 

minmaxdate <- function(df){
  mind <- min(df$dateSoldString)
  maxd <- max(df$dateSoldString)
  vec <- c(mind,maxd)
  return(vec)
}
minmaxdate(nor2)  

barPlotEarliestDates <- function(df,ymax,labelheight,loc_title){
  earliestdates <- c() 
  zipsvector <- uniquezips(df)
  for (i in 1:length(zipsvector)) {
    temp <- df$dateSoldStringMY[df$address_zipcode==zipsvector[i]] 
    earliestdates <- c(earliestdates, min(temp))
  }
  
  earlydf <- as.data.frame(table(earliestdates))
  earlydates_zips <- data.frame(zipsvector, earliestdates)
  
  x <- barplot(earlydf$Freq,names.arg=earlydf$earliestdates,cex.names=0.7,las=2, ylim=c(0,ymax), xlab='',ylab='Number of Zip Codes',main=paste('Earliest Sold Dates for',loc_title, 'Zip Codes \n(Total =', length(uniquezips(df)), 'Zip Codes)'))
  title(xlab = "Earliest Sold Dates", mgp=c(3.5,1,0))
  y <- as.matrix(earlydf$Freq)
  text(x,y+labelheight,labels=as.character(y),cex=0.7)
  
  return(earlydates_zips)
} 

norearly <- barPlotEarliestDates(nor2, 30, 1,'Norfolk')
#### ----






