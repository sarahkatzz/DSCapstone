# DSCapstoneFinalCode 

# Read in county csv's. Make sure all have the same columns. -----
library("data.table")

nor_ <- read.csv('/Users/sarahkatz/Downloads/DS Capstone/norfolk_sub_and_loc_basic_03232024.csv',header=T)
ess_ <- read.csv('/Users/sarahkatz/Downloads/DS Capstone/essex_sub_and_loc_basic_03232024.csv',header=T)
suf_ <- read.csv('/Users/sarahkatz/Downloads/DS Capstone/suffolk_sub_and_loc_basic_03232024.csv',header=T)
mid_ <- read.csv('/Users/sarahkatz/Downloads/DS Capstone/middlesex_sub_and_loc_basic_03232024.csv',header=T) 


dim(nor_) # 936 
dim(ess_) # 935 
dim(suf_) # 926, fewest columns 
dim(mid_) # 936 

# make all the same columns 
nor_ <- subset(nor_, select = colnames(suf_))
ess_ <- subset(ess_, select = colnames(suf_))
mid_ <- subset(mid_, select = colnames(suf_))

# all now 926 columns  
dim(nor_) # 24638 rows 
dim(ess_) # 32055 rows 
dim(suf_) # 15768 rows 
dim(mid_) # 58075 rows 

# confirming columns are in the same order 
vecnor <- names(nor_)
vecess <- names(ess_)
vecsuf <- names(suf_) 
vecmid <- names(mid_)
sum(vecnor!=vecess)
sum(vecnor!=vecsuf)
sum(vecnor!=vecmid) 


# bring in school rating and school distance columns (2 columns) from the large, non-subsetted datasets 

norschoolinfo <- fread('/Users/sarahkatz/Downloads/CS 350 /CountyCSVs/norfolk_subset.csv', 
                       select = c('schools_0_distance','schools_0_rating'))
essschoolinfo <- fread('/Users/sarahkatz/Downloads/CS 350 /CountyCSVs/essex.csv', 
                       select = c('schools_0_distance','schools_0_rating')) 
sufschoolinfo <- fread('/Users/sarahkatz/Downloads/CS 350 /CountyCSVs/suffolk.csv', 
                       select = c('schools_0_distance','schools_0_rating'))
midschoolinfo <- fread('/Users/sarahkatz/Downloads/CS 350 /CountyCSVs/middlesex.csv', 
                       select = c('schools_0_distance','schools_0_rating')) 

# number of rows match 
dim(norschoolinfo)
dim(essschoolinfo)
dim(sufschoolinfo)
dim(midschoolinfo)

nor <- cbind(nor_, norschoolinfo) 
ess <- cbind(ess_, essschoolinfo) 
suf <- cbind(suf_, sufschoolinfo) 
mid <- cbind(mid_, midschoolinfo) 

dim(nor)
dim(ess)
dim(suf)
dim(mid)

# 1) Make complete dataset with all four counties. ----
# 2) Remove missing and/or unusual values from lastSoldPrice and dateSoldString columns. 
# 3) Make function to calculate percent of rows removed from all datasets. 
# 4) Make function to add month-year column and year column from dateSoldString.  
# 5) Result in 5 new datasets with month-yr column, yr column and no missingness / strange values in lastSoldPrice and dateSoldString columns: all2, nor2, ess2, suf2, mid2. 

all <- rbind(nor,ess,suf,mid)
dim(all) # 130536  

# what to remove: homes sold for less than 30,000, empty values, NA values  
nrow(nor[nor$lastSoldPrice<30000 | nor$lastSoldPrice=='' | nor$dateSoldString=='' | is.na(nor$lastSoldPrice) | is.na(nor$dateSoldString),]) # 256 
nrow(ess[ess$lastSoldPrice<30000 | ess$lastSoldPrice=='' | ess$dateSoldString=='' | is.na(ess$lastSoldPrice) | is.na(ess$dateSoldString),]) # 229 
nrow(suf[suf$lastSoldPrice<30000 | suf$lastSoldPrice=='' | suf$dateSoldString=='' | is.na(suf$lastSoldPrice) | is.na(suf$dateSoldString),]) # 336 
nrow(mid[mid$lastSoldPrice<30000 | mid$lastSoldPrice=='' | mid$dateSoldString=='' | is.na(mid$lastSoldPrice) | is.na(mid$dateSoldString),]) # 497 

nor2 <- nor[-which(nor$lastSoldPrice<30000 | nor$lastSoldPrice=='' | nor$dateSoldString=='' | is.na(nor$lastSoldPrice) | is.na(nor$dateSoldString)),] 
ess2 <- ess[-which(ess$lastSoldPrice<30000 | ess$lastSoldPrice=='' | ess$dateSoldString=='' | is.na(ess$lastSoldPrice) | is.na(ess$dateSoldString)),] 
suf2 <- suf[-which(suf$lastSoldPrice<30000 | suf$lastSoldPrice=='' | suf$dateSoldString=='' | is.na(suf$lastSoldPrice) | is.na(suf$dateSoldString)),] 
mid2 <- mid[-which(mid$lastSoldPrice<30000 | mid$lastSoldPrice=='' | mid$dateSoldString=='' | is.na(mid$lastSoldPrice) | is.na(mid$dateSoldString)),]

all2 <- rbind(nor2,ess2,suf2,mid2)


dim(nor) # 24638 
dim(nor2) # 24382

dim(ess) # 32055 
dim(ess2) # 31826

dim(suf) # 15768 
dim(suf2) # 15432 

dim(mid) # 58075 
dim(mid2) # 57578 


dim(all2) # 129218

pctrem <- function(df_i,df_f){
  pct <- round(((nrow(df_i) - nrow(df_f)) / nrow(df_i))*100,2) 
  return(pct)
}

# Percent that were removed (all very low): 
pctrem(all,all2) # 1.01 % 
pctrem(nor,nor2) # 1.04 % 
pctrem(ess,ess2) # 0.71 % 
pctrem(suf,suf2) # 2.13 % 
pctrem(mid,mid2) # 0.86 % 

addMonthYear <- function(df){
  df$dateSoldString <- as.Date(df$dateSoldString) 
  df$dateSoldStringMY <- format(df$dateSoldString, "%Y-%m")
  df$dateSoldStringY <- format(df$dateSoldString, "%Y")
  return(df)
} 
all2 <- addMonthYear(all2)
nor2 <- addMonthYear(nor2)
ess2 <- addMonthYear(ess2)
suf2 <- addMonthYear(suf2)
mid2 <- addMonthYear(mid2)

dim(all2) # 129218 


 
# Ensure the counties all have the same start month and end month, as well as the large dataset: 11/2020 to 11/2023 ---- 
minmaxdate <- function(df){
  mind <- min(df$dateSoldStringMY)
  maxd <- max(df$dateSoldStringMY)
  vec <- c(mind,maxd)
  return(vec)
}
minmaxdate(nor2)
minmaxdate(ess2)
minmaxdate(suf2)
minmaxdate(mid2)

# Norfolk has the narrowest interval, fit other counties to this window 
nor3 <- nor2 
ess3 <- ess2[-which(ess2$dateSoldStringMY < min(nor2$dateSoldStringMY) | ess2$dateSoldStringMY > max(nor2$dateSoldStringMY)),]
suf3 <- suf2[-which(suf2$dateSoldStringMY < min(nor2$dateSoldStringMY) | suf2$dateSoldStringMY > max(nor2$dateSoldStringMY)),]
mid3 <- mid2[-which(mid2$dateSoldStringMY < min(nor2$dateSoldStringMY) | mid2$dateSoldStringMY > max(nor2$dateSoldStringMY)),] 

all3 <- rbind(nor3,ess3,suf3,mid3) 
minmaxdate(all3) 
dim(all3) # 128284 

# Now have dataset all3: want to remove any duplicates ---- 
sum(duplicated(all3$zpid)) # 27740 
all4 <- all3[!duplicated(all3[,c("zpid")]),] 
sum(duplicated(all4$zpid)) # 0 now, good 
dim(all4) # 100544 now  

# Remove all home types that are not residential. Keep only: SingleFamily, Townhouse, Condo. ---- 
table(all4$resoFacts_homeType) 
sum(is.na(all4$resoFacts_homeType)) # 0 NA 
all4 <- all4[all4$resoFacts_homeType=="Condo" | all4$resoFacts_homeType=="SingleFamily" | all4$resoFacts_homeType=="Townhouse",] 
dim(all4) # 89816 

# Dataset all4: Remove rows that have impossible year built; Create another variable to show age of the house. ---- 
table(all4$resoFacts_yearBuilt) 
table(all4$dateSoldStringY) 
# possible range of years for year built: limiting from 1650 to 2023 

nrow(all4[all4$resoFacts_yearBuilt <1650 | all4$resoFacts_yearBuilt>2023 | as.numeric(all4$dateSoldStringY) < all4$resoFacts_yearBuilt,]) # 336 

# remove all rows from dataset where yearbuilt does not make sense 
# "not make sense" means: year before 1650, year after 2023, and year built is greater than dateSoldString 

all5 <- all4[all4$resoFacts_yearBuilt >=1650 & all4$resoFacts_yearBuilt<=2023 & as.numeric(all4$dateSoldStringY) >= all4$resoFacts_yearBuilt,] 
table(all5$resoFacts_yearBuilt)
dim(all5)


all5$age <- as.numeric(all5$dateSoldStringY) - all5$resoFacts_yearBuilt
table(all5$age) 
names(all5) 

dim(all5) # 89669

# Dataset all5: Create another (binary) variable based on description to show renovated or not ---- 
# this makes a vector of all the house descriptions that include renovation words or parts of renovation words 
nrow(all5[all5$description=='',]) # 359
all5 <- all5[which(all5$description!=''),] 
dim(all5) # 89310 

renovationwords <- grep(pattern="renovation|renovate|brand new|remodel|rebuil|renovat|restor|update|updat|modernize|upgrade|refurbish", all5$description, value=TRUE) 
length(renovationwords) # 37712 

# the variable ren_ind is binary, where 1 means that the house description includes "renovation words" 
all5$ren_ind <- ifelse(all5$description %in% renovationwords, 1,0) 
table(all5$ren_ind) # 37712 / 89310 = 42.23% of houses indicate renovation words in description 

# Examine important variables for missingness or unusual values for --> bedrooms, bathrooms, livingAreaValue ----
# unusual bedrooms: NA, 0, more than 25 
# unusual bathrooms: NA, 0, more than 25  
# unusual livingAreaValue: NA, 0, less than 100 sq ft (might want to increase this)

dim(all5) # 89310 
table(all5$bedrooms) 
table(all5$bathrooms) 

length(which(all5$bedrooms == 0)) # 394 
length(which(all5$bedrooms > 25)) # 3  

length(which(all5$bathrooms == 0)) # 54 
length(which(all5$bathrooms > 25)) # 1  

length(which(all5$livingAreaValue < 100)) # 11

sum(is.na(all5$bedrooms)) # 164 
sum(is.na(all5$bathrooms)) # 55
sum(is.na(all5$livingAreaValue)) # 8

all6 <- all5[-which(all5$bedrooms==0 | all5$bedrooms > 25 | 
                      all5$bathrooms == 0 | all5$bathrooms > 25 | 
                      all5$livingAreaValue < 100),] 
dim(all6) # 88871 (initial)
all6 <- all6[!is.na(all6$bedrooms) & !is.na(all6$bathrooms) & !is.na(all6$livingAreaValue),]
dim(all6) # 88690 (final) 

sum(is.na(all6$bedrooms)) 
sum(is.na(all6$bathrooms)) 
sum(is.na(all6$livingAreaValue)) 

# Examine important variables for missingness or unusual values. ---- 
# Create binary garage variable: "garage_ind" 
# Create general parking only variable: "generalparkonly_ind" 
# --> resoFacts_garageParkingCapacity, resoFacts_parkingCapacity, schools_0_distance, schools_0_rating 
# Garage parking capacity 
dim(all6) # 88690 
table(all6$resoFacts_garageParkingCapacity) # unusual if > 10 
sum(is.na(all6$resoFacts_garageParkingCapacity)) # 14121 NA, make 0 
all6$resoFacts_garageParkingCapacity[is.na(all6$resoFacts_garageParkingCapacity)] <- 0 # now all 0, no NA 
all6 <- all6[-which(all6$resoFacts_garageParkingCapacity > 10),] # remove all with > 10 
dim(all6) # 88679 

# create binary indicator variable for having a garage or not --> already contained in dataset 
all6$garage_ind <- ifelse(all6$resoFacts_garageParkingCapacity>=1,1,0)
table(all6$garage_ind) 

# Parking capacity in general 
table(all6$resoFacts_parkingCapacity)
sum(is.na(all6$resoFacts_parkingCapacity)) # no NA 

# create binary indicator for having general / shared parking (and no garage) or not 
all6$generalparkonly_ind <- ifelse(all6$garage_ind == 0 & all6$resoFacts_parkingCapacity >= 1,1,0)
table(all6$generalparkonly_ind)

nrow(all6[all6$garage_ind==0 & all6$generalparkonly_ind == 0,]) # 14975: no gen park, no garage 
nrow(all6[all6$garage_ind==1 & all6$generalparkonly_ind == 0,]) # 43871: no gen park, garage 
nrow(all6[all6$garage_ind==1 & all6$generalparkonly_ind == 1,]) # 0 (makes sense bc of the conditional I put in)
nrow(all6[all6$garage_ind==0 & all6$generalparkonly_ind == 1,]) # 29833: gen park, no garage 


# School Distance 
dim(all6) # 88679
nrow(all6[all6$schools_0_distance==0,]) # unusual if == 0, there are 236 of these 
sum(is.na(all6$schools_0_distance)) # 6 NA, remove these 
all6 <- all6[!is.na(all6$schools_0_distance),] # removed NA 
all6 <- all6[-which(all6$schools_0_distance==0),]
dim(all6) # 88443 

# School Rating 
table(all6$schools_0_rating) # no unusual values 
sum(is.na(all6$schools_0_rating)) # 7267 missing values. This is a lot. Might not want to use this variable, will see. 


# Examine zip codes. Remove any zip codes with fewer than 10 houses. Create new dataset all7  ---- 
zipsvector <- unique(all6$address_zipcode)
length(zipsvector) # 203 unique zip codes 
# for each zip code, find the number of houses 
counts = c() 
for (i in 1:length(zipsvector)){
  count <- nrow(all6[all6$address_zipcode==zipsvector[i],])
  counts <- c(counts, count)
}
counts 

# remove all zip codes with fewer than 10 houses 
dfzipscounts <- data.frame(zipsvector, counts)
smallzips <- dfzipscounts$zipsvector[dfzipscounts$counts < 10]
length(smallzips) # 11 zip codes to remove 
all7 <- all6[-which(all6$address_zipcode %in% smallzips),] 
zipsvector7 <- unique(all7$address_zipcode) 
length(zipsvector7) # now 192 unique zip codes 
sum(nchar(zipsvector7)==4) # all in the correct format of four characters (will add in zero in front later)

dim(all7) # 88417 

# Find the distance between houses and city center in miles. Remove outliers. Make urbanlevel variable. ---- 
nrow(all7[all7$lastSoldPrice>35000000,])
all7[all7$lastSoldPrice>35000000,]
all7 <- all7[-which(all7$lastSoldPrice>35000000),] # remove all 2, they're incorrect 
all7 <- all7[-which(all7$bathrooms>15 & all7$lastSoldPrice<1000000),]

all7[which(all7$lastSoldPrice>15000000),] 
all7[which(all7$lastSoldPrice<=15000000 & all7$lastSoldPrice >= 10000000),] 
all7[which(all7$lastSoldPrice<10000000 & all7$lastSoldPrice >= 5000000),] 
all7 <- all7[-which(all7$livingAreaValue==99999),]
#zpid_toremove <- c(57055115,123755115,189967013,2066768768,2103784288,56431824)

dim(all7) # 88412 

sum(is.na(all7$latitude))
sum(is.na(all7$longitude)) 

head(all7$latitude)
head(all7$longitude)

##### NOTE: distHaversine function puts LONGITUDE first THEN latitude 

distances <- rep(NA, nrow(all7))
for (i in 1:nrow(all7)){
  distances[i] <- distHaversine(c(all7$longitude[i], all7$latitude[i]),
                                c(-71.05802449953353,42.36059765667728), 
                                r=3957.2)
}

all7$dist_cityhall <- distances 

max(all7$lastSoldPrice) # 30000000 (30 mil) at 1 Dalton St APT 6101, Boston MA 02115

# 5 very urban = within 3 miles of city hall 
# 4 urban outskirts = 3-6 miles from city hall 
# 3 suburb = 6-12 miles from city hall 
# 2 outside = 12-20 miles from city hall 
# 1 outside = 20+ miles from city hall 

all7$urbanlevel <- rep(NA,nrow(all7))
all7$urbanlevel[all7$dist_cityhall <=3] <- 5 
all7$urbanlevel[all7$dist_cityhall >3 & all7$dist_cityhall <=6] <- 4 
all7$urbanlevel[all7$dist_cityhall >6 & all7$dist_cityhall <=12] <- 3 
all7$urbanlevel[all7$dist_cityhall >12 & all7$dist_cityhall <=20] <- 2
all7$urbanlevel[all7$dist_cityhall >20] <- 1 
table(all7$urbanlevel)
sum(is.na(all7$urbanlevel)) # no missing 

dim(all7) # 88412 

# Explore other variables. ---- 
sum(is.na(all7$resoFacts_basement))
nrow(all7[all7$resoFacts_basement=='',]) # 38387, almost half, will not use variable with that much missingness 

# not necessary if zipcode already included 
sum(is.na(all7$resoFacts_cityRegion))
nrow(all7[all7$resoFacts_cityRegion=='',])

# add column for cleaned has cooling 
table(all7$resoFacts_hasCooling) 
all7$cooling <- rep(NA,nrow(all7))
all7$cooling[all7$resoFacts_hasCooling=='True' | all7$resoFacts_hasCooling==1] <- 1 
all7$cooling[all7$resoFacts_hasCooling=='False' | all7$resoFacts_hasCooling==0] <- 0 
table(all7$cooling) 
sum(is.na(all7$cooling)) # 470 

# add column for cleaned has heating 
table(all7$resoFacts_hasHeating) 
all7$heating <- rep(NA,nrow(all7))
all7$heating[all7$resoFacts_hasHeating=='True' | all7$resoFacts_hasHeating==1] <- 1 
all7$heating[all7$resoFacts_hasHeating=='False' | all7$resoFacts_hasHeating==0] <- 0 
table(all7$heating) 
sum(is.na(all7$heating)) # 34


table(all7$resoFacts_hasGarage) 
sum(is.na(all7$resoFacts_hasGarage)) # 0 
# (don't need garage_ind variable) 


# remove rows where no heating or cooling information 
all7 <- all7[!is.na(all7$cooling),]
dim(all7)
all7 <- all7[!is.na(all7$heating),]
dim(all7) # 87942 


nrow(all5) - nrow(all7) 

# Visualization with Zip Codes Bar Plot ---- 

zipsvector <- unique(all7$address_zipcode)
length(zipsvector) 
pricemeanszips <- c() 
pricemedianszips <- c() 
for (i in 1:length(zipsvector)){
  temp <- all7$lastSoldPrice[all7$address_zipcode==zipsvector[i]]
  pricemeanszips <- c(pricemeanszips,mean(temp)) 
  pricemedianszips <- c(pricemedianszips, median(temp))
}
pricemeanszips 
pricemedianszips 

pricemeanszipsdf <- data.frame(zipsvector, pricemeanszips) 
pricemedianszipsdf <- data.frame(zipsvector, pricemedianszips)

pricemeanszipsdfsorted <- pricemeanszipsdf[order(pricemeanszipsdf$pricemeanszips),]
pricemedianszipsdfsorted <- pricemedianszipsdf[order(pricemedianszipsdf$pricemedianszips),]

# 2199 has largest mean and largest median 
# see how many houses in 2199 
nrow(all7[all7$address_zipcode==2199,]) # 02199 is Back Bay, Boston 
table(all7$lastSoldPrice[all7$address_zipcode==2199])

par(mar = c(6, 5.5, 4, 4))
barplot(pricemeanszipsdfsorted$pricemeanszips / 1000000, 
        names.arg = paste0('0',as.character(pricemeanszipsdfsorted$zipsvector)),
        main = 'Average Last Sold Prices for Boston Area Zip Codes',
        xlab = "Zip Code",
        ylab = "Average Last Sold Price \n(in millions of USD)", 
        las=2,
        cex.names = 0.3,
        col='darkblue',
        cex.main = 2,
        cex.axis = 1,
        ylim = c(0,8))
abline(h=mean(all7$lastSoldPrice)/ 1000000, lwd = 2,col='red')


par(mar = c(6, 5.5, 4, 4))
barplot(pricemedianszipsdfsorted$pricemedianszips / 1000000, 
        names.arg = paste0('0',as.character(pricemedianszipsdfsorted$zipsvector)),
        main = '',
        xlab = "Zip Code",
        ylab = "Median Last Sold Price \n(in millions of USD)", 
        las=2,
        cex.names = 0.35,
        col='darkblue',
        cex.main = 2,
        cex.axis = 1,
        ylim = c(0,5)) 
abline(h=median(all7$lastSoldPrice)/ 1000000, lwd = 5,col='red') 


# Visualization of Distribution of Home Types ---- 
hometypedf <- as.data.frame(table(all7$resoFacts_homeType))  
hometypedfsorted <- hometypedf[order(hometypedf$Freq),]
barplot(hometypedfsorted$Freq, names.arg = hometypedfsorted$Var1, col="forestgreen")

# mgp=c(3.5,1,0) 
par(mar = c(6, 5.5, 4, 4))
x <- barplot(hometypedfsorted$Freq, names.arg = hometypedfsorted$Var1, col="forestgreen", ylim=c(0,55000),ylab='Number of Sold Houses',main='Distribution of Home Types',
             xlab = "Home Type", cex.main = 2, las=1, mgp=c(4,1,0), cex.lab=1.5)
#title(xlab = "Home Type")
y <- as.matrix(hometypedfsorted$Freq) 
text(x,y+2000,labels=as.character(y),cex = 1) 

# Visualization with Distance, Zip Codes and Price  ---- 
# for each zip code, show median sold price and median distance 
pricemedianszipsdfsorted
mediandist <- c() 
for (i in pricemedianszipsdfsorted$zipsvector){
  alldistances <- c() 
  alldistances <- c(alldistances, all7$dist_cityhall[all7$address_zipcode==i])
  mediandist <- c(mediandist, median(alldistances))
} 
mediandist 
pricemedianszipsdfsorted$mediandist <- mediandist 

lmdistplot <- lm(pricemedianszipsdfsorted$pricemedianszips/1000000 ~ pricemedianszipsdfsorted$mediandist)

plot(pricemedianszipsdfsorted$pricemedianszips/1000000 ~ pricemedianszipsdfsorted$mediandist, 
     las=1, 
     xlab="Distance from Boston City Hall (in miles)", 
     ylab="Median Home Price \n(in millions of USD)", 
     main="Zip Codes by Home Price and Distance from City Center",
     mgp=c(2,1,0))
abline(a=coef(lmdistplot)[1], b=coef(lmdistplot)[2]) 

# Multiple Linear Regression Model ---- 

lm1 <- lm(lastSoldPrice~livingAreaValue,data=all7)  
summary(lm1) # Adjusted R-squared:  0.4224 

lm_bed <- lm(lastSoldPrice~bedrooms,data=all7) 
summary(lm_bed) # Adjusted R-squared:  0.207 

lm2 <- lm(lastSoldPrice~livingAreaValue+bedrooms,data=all7)  
summary(lm2) # Adjusted R-squared:  0.4224 

lm3 <- lm(lastSoldPrice~livingAreaValue+bathrooms,data=all7)  
summary(lm3) # Adjusted R-squared: 0.4601

lm4 <- lm(lastSoldPrice~livingAreaValue+bedrooms+bathrooms,data=all7)  
summary(lm4) # Adjusted R-squared: 0.4607 

lm4 <- lm(lastSoldPrice~livingAreaValue+bedrooms+bathrooms+dist_cityhall,data=all7)  
summary(lm4) # Adjusted R-squared:  0.5358 

lm5 <- lm(lastSoldPrice~livingAreaValue+bedrooms+bathrooms+dist_cityhall+as.factor(ren_ind),data=all7)  
summary(lm5) # Adjusted R-squared:  0.5358

lm6 <- lm(lastSoldPrice~livingAreaValue+bedrooms+bathrooms+dist_cityhall+as.factor(resoFacts_homeType),data=all7)  
summary(lm6) # Adjusted R-squared:   0.541

lm7 <- lm(lastSoldPrice~livingAreaValue+bedrooms+bathrooms+dist_cityhall+as.factor(ren_ind)+as.factor(resoFacts_homeType),data=all7)  
summary(lm7) # Adjusted R-squared: 0.5411

lm8 <- lm(lastSoldPrice~livingAreaValue+bedrooms+bathrooms+dist_cityhall+as.factor(ren_ind)+as.factor(resoFacts_homeType)+age,data=all7)  
summary(lm8) # Adjusted R-squared:  0.5422

lm9 <- lm(lastSoldPrice~livingAreaValue+bedrooms+bathrooms+dist_cityhall+as.factor(ren_ind)+as.factor(resoFacts_homeType)+age+ as.factor(resoFacts_hasGarage) ,data=all7)  
summary(lm9) # Adjusted R-squared:  0.5441

lm10 <- lm(lastSoldPrice~livingAreaValue+bedrooms+bathrooms+dist_cityhall+as.factor(ren_ind)+as.factor(resoFacts_homeType)+age+ as.factor(generalparkonly_ind) ,data=all7)  
summary(lm10) # Adjusted R-squared: 0.543  

lm11 <- lm(lastSoldPrice~livingAreaValue+bedrooms+bathrooms+dist_cityhall+as.factor(ren_ind)+as.factor(resoFacts_homeType)+age+ as.factor(resoFacts_hasGarage) +  as.factor(generalparkonly_ind) ,data=all7)  
summary(lm11) # Adjusted R-squared:  0.5441 --> same as lm9, take out generalparkonly_ind and build on lm9 

lm12 <- lm(lastSoldPrice~livingAreaValue+bedrooms+bathrooms+dist_cityhall+as.factor(ren_ind)+as.factor(resoFacts_homeType)+age+ as.factor(resoFacts_hasGarage) + schools_0_distance,data=all7)  
summary(lm12) # Adjusted R-squared: 0.5442 

lm13 <- lm(lastSoldPrice~livingAreaValue+bedrooms+bathrooms+dist_cityhall+as.factor(ren_ind)+as.factor(resoFacts_homeType)+age+ as.factor(resoFacts_hasGarage) + schools_0_distance + resoFacts_garageParkingCapacity,data=all7) 
summary(lm13) # Adjusted R-squared:  0.5448

lm14 <- lm(lastSoldPrice~livingAreaValue+bedrooms+bathrooms+dist_cityhall+as.factor(ren_ind)+as.factor(resoFacts_homeType)+age+ as.factor(resoFacts_hasGarage) + schools_0_distance + resoFacts_parkingCapacity,data=all7) 
summary(lm14) # Adjusted R-squared:  0.5443

lm15 <- lm(lastSoldPrice~livingAreaValue+bedrooms+bathrooms+dist_cityhall+as.factor(ren_ind)+as.factor(resoFacts_homeType)+age+ as.factor(resoFacts_hasGarage) + schools_0_distance + resoFacts_garageParkingCapacity + resoFacts_parkingCapacity,data=all7) 
summary(lm15) # Adjusted R-squared:  0.5451  

# when zip code taken into account (many variables, however --> 203)
lm16 <- lm(lastSoldPrice~livingAreaValue+bedrooms+bathrooms+dist_cityhall+as.factor(ren_ind)+as.factor(resoFacts_homeType)+age+ as.factor(resoFacts_hasGarage) + schools_0_distance + resoFacts_garageParkingCapacity + resoFacts_parkingCapacity + as.factor(address_zipcode),data=all7) 
summary(lm16) # Adjusted R-squared: 0.6736 

# remove bedrooms ... 
# add as.factor(resoFacts_hasGarage) 

lm17 <- lm(lastSoldPrice~livingAreaValue+bedrooms
           +bathrooms 
           + as.factor(ren_ind)
           +as.factor(resoFacts_homeType)
           +age 
           + resoFacts_garageParkingCapacity + resoFacts_parkingCapacity
           +as.factor(resoFacts_hasGarage)
           + as.factor(heating) 
           +as.factor(urbanlevel) ,
           data=all7)  
summary(lm17) # Adjusted R-squared: 0.5688   
# remove: cooling, schools_0_distance, as.factor(resoFacts_hasGarage)  

BIC(lm16) # 2515965
BIC(lm17) # 2538501


# move forward with lm17, more parsimonious and want to focus on specific variables anyway 

# look at some interactions 
lm18 <- lm(lastSoldPrice~livingAreaValue+#bedrooms
             +bathrooms
           + as.factor(ren_ind)
           +as.factor(resoFacts_homeType)
           +age
           + resoFacts_garageParkingCapacity + resoFacts_parkingCapacity
           +as.factor(resoFacts_hasGarage)
           + heating
           +as.factor(urbanlevel)
           
           + age:as.factor(ren_ind)
           + dist_cityhall:schools_0_distance
           + dist_cityhall:as.factor(resoFacts_hasGarage)
           + age:dist_cityhall
           + dist_cityhall:as.factor(ren_ind)
           + as.factor(resoFacts_homeType):dist_cityhall
           + livingAreaValue:dist_cityhall,
           data=all7)  
summary(lm18)
# 0.5613 --> urban level is better to use instead of dist_cityhall 

lm19 <- lm(lastSoldPrice~livingAreaValue+bedrooms
           +bathrooms 
           + as.factor(ren_ind)
           +as.factor(resoFacts_homeType)
           +age 
           + resoFacts_garageParkingCapacity + resoFacts_parkingCapacity
           +as.factor(resoFacts_hasGarage)
           + as.factor(heating)
           +as.factor(urbanlevel)
           # + as.factor(urbanlevel):as.factor(resoFacts_hasGarage)
           #  + age:as.factor(urbanlevel)
           # + age:as.factor(ren_ind)
           #  + as.factor(urbanlevel):as.factor(ren_ind)
           #  + as.factor(resoFacts_homeType):as.factor(urbanlevel)
           + livingAreaValue:as.factor(urbanlevel),
           
           data=all7)  
summary(lm19) 
# 0.6363 


# interactions: age:ren_ind # went up to 0.5313 
# dist_cityhall:schools_0_distance # went up to 0.5318 
# dist_cityhall:as.factor(resoFacts_hasGarage) # went up to 0.539 
# age:dist_cityhall # went up to 0.5396  

# multicollinearity test 


attach(all7)
xmat <- cbind(livingAreaValue,bedrooms,bathrooms,as.factor(ren_ind),as.factor(resoFacts_homeType),age, resoFacts_garageParkingCapacity, resoFacts_parkingCapacity,as.factor(resoFacts_hasGarage), as.factor(heating),as.factor(urbanlevel))
cor(xmat) 
library(usdm) 
vifstep(xmat) # no collinearity issues 

# removing ren_ind gives the same adj R squared 

plot(fitted(lm17),resid(lm17))
abline(h=0)


library(leaps)
attach(all7)
x = cbind(livingAreaValue,bedrooms,bathrooms,as.factor(ren_ind),as.factor(resoFacts_homeType),age, resoFacts_garageParkingCapacity, resoFacts_parkingCapacity,as.factor(resoFacts_hasGarage), as.factor(heating),as.factor(urbanlevel))
dim(x)
y = lastSoldPrice
result = leaps(x, y, method="adjr2")
which.max(result$adjr2) # 91 
result$which[91,]
# REMOVE "age" 


# transforming the response variable 
hist(all7$lastSoldPrice) # heavily right skewed 
library(MASS)
boxcox(lm17) # since lambda = 0, take log of response 
all7$priceTransformed <- log(all7$lastSoldPrice)
hist(all7$priceTransformed)

lmtrans <- lm(priceTransformed~livingAreaValue+bedrooms
              +bathrooms 
              + as.factor(ren_ind)
              +as.factor(resoFacts_homeType)
              + resoFacts_garageParkingCapacity + resoFacts_parkingCapacity
              +as.factor(resoFacts_hasGarage)
              + as.factor(heating)
              +as.factor(urbanlevel) ,
              data=all7)  
summary(lmtrans) # Adjusted R-squared:  0.6801 

#Check for outliers wrtx
cutoff <- 2 * (14/87927) # cutoff = 2 * p / n 
sum(hatvalues(lmtrans)>cutoff) # 3049 

#Identify influential observations
cdperc <- pf(0.5, df1=15, df2=87927) # df1 = p, df2 = n - p 
sum((cooks.distance(lmtrans)>cdperc)==TRUE) # 2

obs = cooks.distance(lmtrans) 

index.inf = which(obs >= cdperc) 

all7[index.inf,] # 2 

# take out values and refit the model
all7_noinf <- all7[-index.inf,]
dim(all7_noinf)

lm_noinf <- lm(priceTransformed~livingAreaValue+bedrooms
               +bathrooms 
               + as.factor(ren_ind)
               +as.factor(resoFacts_homeType)
               + resoFacts_garageParkingCapacity + resoFacts_parkingCapacity
               +as.factor(resoFacts_hasGarage)
               +  as.factor(heating)
               +as.factor(urbanlevel),
               data=all7_noinf)  
summary(lm_noinf) # 0.6855 

####################################### some more exploration, not included in final 
##### explored model with interactions, but not much improvement to justify adding that many variables 
# adj R squared is 0.02 better here, but there are 40 variables as opposed to 15. 
lm19trans <- lm(priceTransformed~livingAreaValue+bedrooms
                +bathrooms 
                + as.factor(ren_ind)
                +as.factor(resoFacts_homeType)
                +age 
                + resoFacts_garageParkingCapacity + resoFacts_parkingCapacity
                +as.factor(resoFacts_hasGarage)
                + heating 
                +as.factor(urbanlevel)
                + as.factor(urbanlevel):as.factor(resoFacts_hasGarage)
                + age:as.factor(urbanlevel)
                + age:as.factor(ren_ind)
                + as.factor(urbanlevel):as.factor(ren_ind)
                + as.factor(resoFacts_homeType):as.factor(urbanlevel)
                + livingAreaValue:as.factor(urbanlevel),
                
                data=all7_noinf2)  
summary(lm19trans) 


cdperc2 <- pf(0.5, df1=40, df2=87901) # df1 = p, df2 = n - p 
sum((cooks.distance(lm19trans)>cdperc2)==TRUE) # 32 
obs2 = cooks.distance(lm19trans) 

index.inf2 = which(obs2 >= cdperc2) 

all7[index.inf2,] # 13

# take out values and refit the model
all7_noinf2 <- all7[-index.inf2,]
dim(all7_noinf2)

####################################  end of exploration that is not included in final 


plot(fitted(lm_noinf),resid(lm_noinf), xlab="Fitted Values",ylab="Residuals", col=alpha('black',0.1))
abline(h=0) 
points(smooth.spline(fitted(lm_noinf),resid(lm_noinf)), col="red",type='l')

qqnorm(lm_noinf$residuals, main="Normal Probability Plot")
qqline(lm_noinf$residuals) 


# Visualizations ---- 
colfunc<-colorRampPalette(c("dodgerblue4","cyan")) # "darkblue","slategray2","red" 
#plot(rep(1,50),col=(colfunc(5)), pch=19,cex=2) 

vec <- colfunc(5) 
all7$usecolor5 <- rep(NA,nrow(all7)) 
all7$usecolor5[all7$urbanlevel==5] <- vec[1]
all7$usecolor5[all7$urbanlevel==4] <- vec[2]
all7$usecolor5[all7$urbanlevel==3] <- vec[3]
all7$usecolor5[all7$urbanlevel==2] <- vec[4]
all7$usecolor5[all7$urbanlevel==1] <- vec[5]

lmbath5 <- lm(lastSoldPrice/1000000~bathrooms,data=all7[all7$urbanlevel==5,])
lmbath4 <- lm(lastSoldPrice/1000000~bathrooms,data=all7[all7$urbanlevel==4,])
lmbath3 <- lm(lastSoldPrice/1000000~bathrooms,data=all7[all7$urbanlevel==3,])
lmbath2 <- lm(lastSoldPrice/1000000~bathrooms,data=all7[all7$urbanlevel==2,])
lmbath1 <- lm(lastSoldPrice/1000000~bathrooms,data=all7[all7$urbanlevel==1,])
lmbath <- lm(lastSoldPrice/1000000~bathrooms,data=all7)
par(mar = c(6, 5.5, 4, 4))
plot(lastSoldPrice/1000000 ~ bathrooms, data=all7, col=usecolor5, pch = 20, cex=2, xlab="Number of Bathrooms",ylab="Home Price \n(in millions of USD)",
     mgp=c(2.5,1,0), las=1) 
abline(a=coef(lmbath5)[1], b=coef(lmbath5)[2],col=vec[1],lwd=3)
abline(a=coef(lmbath4)[1], b=coef(lmbath4)[2],col=vec[2],lwd=3)
abline(a=coef(lmbath3)[1], b=coef(lmbath3)[2],col=vec[3],lwd=3)
abline(a=coef(lmbath2)[1], b=coef(lmbath2)[2],col=vec[4],lwd=3)
abline(a=coef(lmbath1)[1], b=coef(lmbath1)[2],col=vec[5],lwd=3)
abline(a=coef(lmbath)[1], b=coef(lmbath)[2],col='black',lwd=6) 
legend(14.5,30, legend=c(5,4,3,2,1),fill=vec,title="Urban Level",cex=0.7)


lmbed5 <- lm(lastSoldPrice/1000000~bedrooms,data=all7[all7$urbanlevel==5,])
lmbed4 <- lm(lastSoldPrice/1000000~bedrooms,data=all7[all7$urbanlevel==4,])
lmbed3 <- lm(lastSoldPrice/1000000~bedrooms,data=all7[all7$urbanlevel==3,])
lmbed2 <- lm(lastSoldPrice/1000000~bedrooms,data=all7[all7$urbanlevel==2,])
lmbed1 <- lm(lastSoldPrice/1000000~bedrooms,data=all7[all7$urbanlevel==1,])
lmbed <- lm(lastSoldPrice/1000000~bedrooms,data=all7)
par(mar = c(6, 5.5, 4, 4))
plot(lastSoldPrice/1000000 ~ bedrooms, data=all7, col=usecolor5, pch = 20, cex=2, xlab="Number of Bedrooms",ylab="Home Price \n(in millions of USD)",
     mgp=c(2.5,1,0), las=1) 
abline(a=coef(lmbed5)[1], b=coef(lmbed5)[2],col=vec[1],lwd=3)
abline(a=coef(lmbed4)[1], b=coef(lmbed4)[2],col=vec[2],lwd=3)
abline(a=coef(lmbed3)[1], b=coef(lmbed3)[2],col=vec[3],lwd=3)
abline(a=coef(lmbed2)[1], b=coef(lmbed2)[2],col=vec[4],lwd=3)
abline(a=coef(lmbed1)[1], b=coef(lmbed1)[2],col=vec[5],lwd=3)
abline(a=coef(lmbed)[1], b=coef(lmbed)[2],col='black',lwd=6) 
legend(14.5,30, legend=c(5,4,3,2,1),fill=vec,title="Urban Level",cex=0.7)


lmsqft5 <- lm(lastSoldPrice/1000000~livingAreaValue,data=all7[all7$urbanlevel==5,])
lmsqft4 <- lm(lastSoldPrice/1000000~livingAreaValue,data=all7[all7$urbanlevel==4,])
lmsqft3 <- lm(lastSoldPrice/1000000~livingAreaValue,data=all7[all7$urbanlevel==3,])
lmsqft2 <- lm(lastSoldPrice/1000000~livingAreaValue,data=all7[all7$urbanlevel==2,])
lmsqft1 <- lm(lastSoldPrice/1000000~livingAreaValue,data=all7[all7$urbanlevel==1,])
lmsqft <- lm(lastSoldPrice/1000000~livingAreaValue,data=all7)
par(mar = c(6, 5.5, 4, 4))
plot(lastSoldPrice/1000000 ~ livingAreaValue, data=all7[all7$lastSoldPrice<20000000 & all7$livingAreaValue <25000,], col=usecolor5, pch = 20, cex=2, xlab="Square Footage",ylab="Home Price \n(in millions of USD)",
     mgp=c(2.5,1,0), las=1) 
abline(a=coef(lmsqft5)[1], b=coef(lmsqft5)[2],col=vec[1],lwd=3)
abline(a=coef(lmsqft4)[1], b=coef(lmsqft4)[2],col=vec[2],lwd=3)
abline(a=coef(lmsqft3)[1], b=coef(lmsqft3)[2],col=vec[3],lwd=3)
abline(a=coef(lmsqft2)[1], b=coef(lmsqft2)[2],col=vec[4],lwd=3)
abline(a=coef(lmsqft1)[1], b=coef(lmsqft1)[2],col=vec[5],lwd=3)
abline(a=coef(lmsqft)[1], b=coef(lmsqft)[2],col='black',lwd=6) 
legend(19000,19, legend=c(5,4,3,2,1),fill=vec,title="Urban Level",cex=0.6) 


# Histograms of response before and after transformation ---- 

par(mar = c(6, 5.5, 2, 2))
hist(all7$lastSoldPrice/1000000,col="cyan4",main='',xlab="Home Price (in millions of USD)",ylab="Number of Houses",las=1, mgp=c(3.65,1,0)) 

exponentiated <- round(c(exp(10),exp(11),exp(12), exp(13), exp(14), exp(15),exp(16), exp(17))) 
exponentiated_commas <- c('22,026', '59,874',   '162,755',   '442,413',  '1,202,604',  '3,269,017',  '8,886,111', '24,154,953')

par(mar = c(6, 6, 2, 2))
hist(all7_noinf$priceTransformed,col="cyan4",main='',xlab="Home Price (in USD)",ylab="Number of Houses",las=1, mgp=c(4.75,1,0), xaxt="n")
axis(side=1,at=seq(10,17,1),labels= exponentiated_commas, las=2) 

sum(all7$lastSoldPrice) # 72,126,286,695 $ 72 billion 
sum(all7$lastSoldPrice[all7$urbanlevel==5]) # 6,121,183,277 
sum(all7$lastSoldPrice[all7$urbanlevel==5 | all7$urbanlevel==4]) # 14,340,745,466 
sum(all7$lastSoldPrice[all7$urbanlevel==5 | all7$urbanlevel==4]) / sum(all7$lastSoldPrice) # 20% 

# Visualization with garage and home type ---- 
t1 <- median(all7$lastSoldPrice[all7$resoFacts_homeType=='Townhouse' & all7$resoFacts_hasGarage=='True'])
t2 <- median(all7$lastSoldPrice[all7$resoFacts_homeType=='Townhouse' & all7$resoFacts_hasGarage=='False'])
c1 <- median(all7$lastSoldPrice[all7$resoFacts_homeType=='Condo' & all7$resoFacts_hasGarage=='True'])
c2 <- median(all7$lastSoldPrice[all7$resoFacts_homeType=='Condo' & all7$resoFacts_hasGarage=='False'])
s1 <- median(all7$lastSoldPrice[all7$resoFacts_homeType=='SingleFamily' & all7$resoFacts_hasGarage=='True'])
s2 <- median(all7$lastSoldPrice[all7$resoFacts_homeType=='SingleFamily' & all7$resoFacts_hasGarage=='False'])

# length(all7$lastSoldPrice[all7$resoFacts_homeType=='Townhouse' & all7$resoFacts_hasGarage=='True'])
# length(all7$lastSoldPrice[all7$resoFacts_homeType=='Townhouse' & all7$resoFacts_hasGarage=='False'])
# length(all7$lastSoldPrice[all7$resoFacts_homeType=='Condo' & all7$resoFacts_hasGarage=='True'])
# length(all7$lastSoldPrice[all7$resoFacts_homeType=='Condo' & all7$resoFacts_hasGarage=='False'])
# length(all7$lastSoldPrice[all7$resoFacts_homeType=='SingleFamily' & all7$resoFacts_hasGarage=='True'])
# length(all7$lastSoldPrice[all7$resoFacts_homeType=='SingleFamily' & all7$resoFacts_hasGarage=='False'])

group_labels <- c("Townhouse", "Condo", "SingleFamily")
set1 <- c(t1, t2)
set2 <- c(c1, c2)
set3 <- c(s1, s2)

# Combine the data for all sets
data <- rbind(set1, set2, set3)
options(scipen = 999)

par(mar = c(7, 5, 5, 7))
par(mgp = c(4.1, 1, 0))
# Barplot
bp <- barplot(t(data), beside = TRUE, col = c("chartreuse3", "red"), ylim = c(0, 1100000),
              xlab = "Home Type", ylab = "Median Home Price", 
              names.arg = group_labels,las=1)

# Add legend
legend("topright", title = 'Has Garage',cex=0.8,legend = c("Yes", "No"), fill = c("chartreuse3", "red"),x.intersp = 0.75, y.intersp = 0.75)

# Add x-axis labels
axis(1, at = colMeans(bp), labels = group_labels) 

# Best range from residual plot is 270,000 to 5 million, see how many houses in that range ---- 
nrow(all7[all7$lastSoldPrice>=270000 & all7$lastSoldPrice<=5000000,]) # 83931
nrow(all7[all7$lastSoldPrice>=270000 & all7$lastSoldPrice<=5000000,]) / nrow(all7)
# 95.43% of the houses 

sum(all7$lastSoldPrice[all7$lastSoldPrice>=270000 & all7$lastSoldPrice<=5000000]) # 69347502134 
sum(all7$lastSoldPrice) # 72126286695 
sum(all7$lastSoldPrice[all7$lastSoldPrice>=270000 & all7$lastSoldPrice<=5000000])  / sum(all7$lastSoldPrice)
# 96.15%  

# Narrowed-down data to submit ---- 
names(all7)
n <- all7[,-c(1:5, 11,12, 14:19, 25, 28,32:44)]
names(n)  

23/76 # keep only 30% of rows to fit github size requirement 
0.3*nrow(n) # 26383 

n <- n[1:26383,]


write.csv(n, "smallerdatasetFINAL.csv", row.names = FALSE)
  
  
  
  
  
  
  