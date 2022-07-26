#### --------------------------------- df1980 & 1990--------------------------------------
df1980 <- read.fwf(file = 'data/National-intercensal-data-1980.TXT', widths =c(2,2,2,3,1,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10))
df1990 <- read.fwf(file = 'data/National-intercensal-data-1990.TXT', widths = c(2,2,2,3,1,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10,10))

df8090<- rbind(df1980,df1990)
# combine two txt files into one table: 
colnames(df8090) <- c('Series','Month','Year','Age','blank','Total population','Total male population',
                      'Total female population','White male population','White female population','Black male population',
                      'Black female population','American Indian, Eskimo, and Aleut male population',
                      'American Indian, Eskimo, and Aleut female population','Asian and Pacific Islander male population', 'Asian and Pacific Islander female population', 'Hispanic male population', 'Hispanic female population',
                      'White, non-Hispanic male population','White, non-Hispanic female population','Black, non-Hispanic male population',
                      'Black, non-Hispanic female population', 'American Indian, Eskimo, and Aleut, non-Hispanic male population','American Indian, Eskimo, and Aleut, non-Hispanic female population',
                      'Asian and Pacific Islander, non-Hispanic male population','Asian and Pacific Islander, non-Hispanic female population')
#head(df8090)
str(df8090)

# change data type to character.
df8090$Series <- as.character(df8090$Series) 

nrow(df8090) #920 row
#only retain rows thatmonth = 4: 
df8090<-df8090[df8090$Month ==4,]
nrow(df8090) #308 rows  remains 

# change data type to character.
df8090<-subset(df8090, Age!='999')
nrow(df8090) # 303 rows

# For Year column, drop '89', only contain '90' and '80'. 
df8090<-subset(df8090, Year!='89')
nrow(df8090) # 202 rows



#After delete incorrect value '999' & '89', I use 'cut' function to group age by 5 year from 0 to inf. 
df8090["AGEGRP"] = cut(df8090$Age, c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74, Inf),c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"), include.lowest=TRUE) 
nrow(df8090) #202
#head(df8090) 

#drop useless column 'Age'
df8090<-df8090[ , !names(df8090) %in% c("Age")] 
#head(df8090)
nrow(df8090) #202
ncol(df8090) #26

#drop useless clumns about 'Hispanic':
df8090<-df8090[ , !names(df8090) %in% c('Hispanic male population', 'Hispanic female population',
                                        'White, non-Hispanic male population','White, non-Hispanic female population','Black, non-Hispanic male population',
                                        'Black, non-Hispanic female population', 'American Indian, Eskimo, and Aleut, non-Hispanic male population','American Indian, Eskimo, and Aleut, non-Hispanic female population',
                                        'Asian and Pacific Islander, non-Hispanic male population','Asian and Pacific Islander, non-Hispanic female population')] 
nrow(df8090) #202 
ncol(df8090) #16 

#Continute drop useless columns about'total population'&'month'&'series': 
df8090<-df8090[ , !names(df8090) %in% c('blank','Total population','Total male population',
                                        'Total female population')] 
nrow(df8090) #202 rows
ncol(df8090) #12 columns

# separate columns as white-male, white-female,non-white-male, non-white-female 
# because we already have white male and white female, we just need to created non-white-male, non-white-female 


#create non white male
df8090$Non_white_male<- rowSums(cbind(df8090$`Black male population`, df8090$`American Indian, Eskimo, and Aleut male population`,df1990$`Asian and Pacific Islander male population`), na.rm=TRUE)
ncol(df8090) # 13 columns

#create non white demale
df8090$Non_white_female<- rowSums(cbind(df8090$`Black female population`, df8090$`American Indian, Eskimo, and Aleut female population`,df1990$`Asian and Pacific Islander female population`), na.rm=TRUE)
ncol(df8090) # 14 columns
nrow(df8090) #202 

#reshape the data with group by Age to have only one row of 1 , 2 ...:
library(tidyverse)

df8090_revised <- df8090 %>% group_by(AGEGRP,Year) %>% summarise( Year=mean(Year),
                                                                  White_Male=sum(`White male population`), 
                                                                  White_Female=sum(`White female population`), 
                                                                  NonWhite_Male=sum(Non_white_male), 
                                                                  NonWhite_Female=sum(Non_white_female))
head(df8090_revised)

#After groupby, I then use melt function to melt column of White_male, White_female, Nonwhite_Male, and Nonwhite_Female: 
library(reshape2)

df8090_final <- melt(df8090_revised, id = c("AGEGRP","Year")) # melt df8090_revised
#head(df8090_final) 

#split varibales by "_" into "Race" and "Sex"
# link reference: https://www.delftstack.com/howto/r/separate-in-r/
df8090_final[c('Race', 'Sex')]<-str_split_fixed(df8090_final$variable, "_", 2)
#head(df8090_final)

#Drop column 'Variable'
df8090_final <- df8090_final[ , !names(df8090_final) %in% c("variable")]
#head(df8090_final) # check again, correct

#link for refernece: how to rename column name column:https://www.datanovia.com/en/lessons/rename-data-frame-columns-in-r/
names(df8090_final)[names(df8090_final) == "value"] <- "Population"
#str(df8090_final)

#rename 90 to 1990, 80 to 1980:
df8090_final$Year[which(df8090_final$Year=="80")] <-"1980"
df8090_final$Year[which(df8090_final$Year=="90")] <-"1990"

head(df8090_final) # final check, correct
### ----------------------------- df 1980 ~ df 1990 end ---------------------------------------

### -------------------------- df1900 ~ df 1950 begin ---------------------------------- 
## Read csv files and Add year column:
df1900 <- read.csv('data/pe-11-1900.csv', header = F)
df1900["Year"]= 1900
df1910 <- read.csv('data/pe-11-1910.csv', header = F)
df1910["Year"]= 1910
df1920 <- read.csv('data/pe-11-1920.csv', header = F)
df1920["Year"]= 1920
df1930 <- read.csv('data/pe-11-1930.csv', header = F)
df1930["Year"]= 1930
df1940 <- read.csv('data/pe-11-1940.csv', header = F)
df1940["Year"]= 1940
df1950 <- read.csv('data/pe-11-1950.csv', header = F)
df1950["Year"]= 1950



# combine these 6 years into 1 table:
df0050 <- rbind(df1900,df1910,df1920,df1930,df1940,df1950)


#head(df0050) 

str(df0050) 
df0050$V1<- as.character(df0050$V1)
df0050$V6<- as.character(df0050$V6)
df0050$V7<- as.character(df0050$V7)
df0050$V9<- as.character(df0050$V9)
df0050$V10<- as.character(df0050$V10)
#drop useless column 'v2' to 'v8'
df0050<-df0050[ , !names(df0050) %in% c("V2","V3","V4","V5","V8")] 
head(df0050)
#rename multiple column names at the same time:
colnames(df0050) <- c("Age","White_Male","White_Female","NonWhite_Male","NonWhite_Female","Year")

# drop the first 7 and last several rows:
df0050 <- df0050[-c(1:8,85:107,184:206,283:305,382:404,491:513,600:614), ]
nrow(df0050) #  476rows
#head(df0050) # check, correct

# delete comma to convert data type from factor to integer
df0050$White_Male <- as.integer(gsub(",","", df0050$White_Male))
df0050$White_Female <- as.integer(gsub(",","", df0050$White_Female))
df0050$NonWhite_Male <- as.integer(gsub(",","", df0050$NonWhite_Male))
df0050$NonWhite_Female <- as.integer(gsub(",","", df0050$NonWhite_Female))


df0050$Age[which(df0050$Age=="75+")] <- "75"
df0050$Age[which(df0050$Age=="85+")] <- "85"
df0050$Age <- as.integer(df0050$Age)
str(df0050)



#After delete incorrect value '999' & '89', I use 'cut' function to group age by 5 year from 0 to inf. 
df0050["AGEGRP"] = cut(df0050$Age, c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74, Inf),c("1","2","3","4","5","6","7","8","9","10","11","12","13","14","15","16"), include.lowest=TRUE) 
nrow(df0050) #76
head(df0050) 


#reshape the data with group by Age to have only one row of 1 , 2 ...:
library(tidyverse)

df0050_revised <- df0050 %>% group_by(AGEGRP,Year) %>% summarise( Year=mean(Year),
                                                                  White_Male=sum(`White_Male`), 
                                                                  White_Female=sum(`White_Female`), 
                                                                  NonWhite_Male=sum(NonWhite_Male), 
                                                                  NonWhite_Female=sum(NonWhite_Female))
head(df0050_revised)

#After groupby, I then use melt function to melt column of White_male, White_female, Nonwhite_Male, and Nonwhite_Female: 
library(reshape2)

df0050_final <- melt(df0050_revised, id = c("AGEGRP","Year")) # melt df1990_revised
head(df0050_final) 

#split varibales by "_" into "Race" and "Sex"
# link reference: https://www.delftstack.com/howto/r/separate-in-r/
df0050_final[c('Race', 'Sex')]<-str_split_fixed(df0050_final$variable, "_", 2)
head(df0050_final)

#Drop column 'Variable'
df0050_final <- df0050_final[ , !names(df0050_final) %in% c("variable")]
head(df0050_final) # check again, correct

#link for refernece: how to rename column name column:https://www.datanovia.com/en/lessons/rename-data-frame-columns-in-r/
names(df0050_final)[names(df0050_final) == "value"] <- "Population"
head(df0050_final) # final check, correct
###--------------------------- df1900 ~ df 1950 end ---------------------------------




### ----------------------- df1960 ~  df1970 begin ----------------------------------
# read csv:
df1960<- read.csv("data/pe-11-1960.csv",header=F)
# Add Year column: 1960
df1960["Year"]= 1960
df1970<- read.csv("data/pe-11-1970.csv",header=F)
# Add Year column: 1970
df1970["Year"]= 1970

df6070<- rbind(df1960,df1970)
head(df6070) 
str(df6070) 


#drop useless column 'v8' to 'v13'
df6070<-df6070[ , !names(df6070) %in% c("V8","V9","V10","V11","V12","V13")] 
head(df6070)

#rename multiple column names at the same time:
colnames(df6070) <- c("Age","Race_total","Male_total","Female_total","White_total","White_Male","White_Female","Year")

nrow(df6070)#216 rows

# drop the first 7 and last several rows:
df6070<- df6070[-c(1:7,94:115,202:216), ]
nrow(df6070) # 172 rows
head(df6070) # check, correct

#I use 'cut' function to group age by 5 year from 0 to inf. 
str(df6070)
df6070$Male_total<- as.character(df6070$Male_total)
df6070$Female_total<- as.character(df6070$Female_total)
df6070$White_Male<- as.character(df6070$White_Male)
df6070$White_Female<- as.character(df6070$White_Female)
df6070$Age<- as.character(df6070$Age)
str(df6070)
nrow(df6070) #172

# delete comma to convert data type from factor to integer
df6070$Male_total <- as.integer(gsub(",","", df6070$Male_total))
df6070$Female_total <- as.integer(gsub(",","", df6070$Female_total))
df6070$White_Male <- as.integer(gsub(",","", df6070$White_Male))
df6070$White_Female <- as.integer(gsub(",","", df6070$White_Female))

df6070$Age[which(df6070$Age=="75+")] <- "75"
df6070$Age[which(df6070$Age=="85+")] <- "85"
df6070$Age <- as.integer(df6070$Age)
#df6070$Age <- as.numeric(df6070$Age) 


# create AGEGRP by 5 years 
str(df6070)
df6070$AGEGRP <- cut(df6070$Age, c(0,4,9,14,19,24,29,34,39,44,49,54,59,64,69,74, Inf), 
                     c( "1", "2", "3","4","5","6","7","8","9","10","11","12","13","14","15","16"),
                     include.lowest=TRUE)

nrow(df6070) #172
#head(df6070) 

# create nonwhite columns:
df6070$NonWhite_Male <- df6070$Male_total - df6070$White_Male
df6070$NonWhite_Female <- df6070$Female_total - df6070$White_Female
head(df6070) 

#drop unless columns:
df6070<-df6070[ , !names(df6070) %in% c("Age","Race_total","Male_total","Female_total","White_total")] 
head(df6070)


#reshape the data with group by Age to have only one row of 1 , 2 ...:
library(tidyverse)

df6070_revised <- df6070 %>% group_by(AGEGRP,Year) %>% summarise( Year=mean(Year),
                                                                  White_Male=sum(`White_Male`), 
                                                                  White_Female=sum(`White_Female`), 
                                                                  NonWhite_Male=sum(NonWhite_Male), 
                                                                  NonWhite_Female=sum(NonWhite_Female))
head(df6070_revised)

#After groupby, I then use melt function to melt column of White_male, White_female, Nonwhite_Male, and Nonwhite_Female: 
library(reshape2)

df6070_final <- melt(df6070_revised, id = c("AGEGRP","Year")) # melt df1990_revised
#head(df6070_final) 

#split varibales by "_" into "Race" and "Sex"
# link reference: https://www.delftstack.com/howto/r/separate-in-r/
df6070_final[c('Race', 'Sex')]<-str_split_fixed(df6070_final$variable, "_", 2)
#head(df6070_final)

#Drop column 'Variable'
df6070_final <- df6070_final[ , !names(df6070_final) %in% c("variable")]
#head(df6070_final) # check again, correct

#link for refernece: how to rename column name column:https://www.datanovia.com/en/lessons/rename-data-frame-columns-in-r/
names(df6070_final)[names(df6070_final) == "value"] <- "Population"
head(df6070_final) # final check, correct

### ----------------------- df1960 ~  df1970 end ----------------------------------


## ------------------------ df2000 & 2010 begin ------------------------------------------------ 
df2000 <- read.csv('data/National-intercensal-data-2000-2010.csv')
#head(df2000) 
#only retain selected cells into , month = 4
df2000 <- df2000[df2000$year == 2000 & df2000$month == 4 |df2000$year == 2010 & df2000$month == 4,]
#nrow(df2000) # 44
str(df2000) # all numeric
df2000 <- df2000[-c(1,23),c(2,3,7:18)]
# set number to specific cell
df2000[c(17:21,37:42),2]<-16
# create NonWhite columns:
df2000$NonWhite_Female <- rowSums(df2000[c(6,8,10,12,14)])
df2000$NonWhite_Male <- rowSums(df2000[c(5,7,9,11,13)])

#only contain usefull columns --> better overview:
df2000_revised<- df2000[,c("year","AGEGRP","WA_MALE","WA_FEMALE","NonWhite_Male","NonWhite_Female")]
names(df2000_revised)[names(df2000_revised) == "WA_MALE"] <- "White_Male"  # rename to a better way
names(df2000_revised)[names(df2000_revised) == "WA_FEMALE"] <- "White_Female" 
names(df2000_revised)[names(df2000_revised) == "year"] <- "Year" 


#reshape the data with group by Age to have only one row of 1 , 2 ...:
library(tidyverse)

df2000_revised <- df2000_revised %>% group_by(AGEGRP,Year) %>% summarise( Year=mean(Year),
                                                                          White_Male=sum(`White_Male`), 
                                                                          White_Female=sum(`White_Female`), 
                                                                          NonWhite_Male=sum(NonWhite_Male), 
                                                                          NonWhite_Female=sum(NonWhite_Female))
head(df2000_revised)

#After groupby, I then use melt function to melt column of White_male, White_female, Nonwhite_Male, and Nonwhite_Female: 
library(reshape2)

df2000_final <- melt(df2000_revised, id = c("AGEGRP","Year")) # melt df1990_revised
head(df2000_final) 

#split varibales by "_" into "Race" and "Sex"
# link reference: https://www.delftstack.com/howto/r/separate-in-r/
df2000_final[c('Race', 'Sex')]<-str_split_fixed(df2000_final$variable, "_", 2)
head(df2000_final)

#Drop column 'Variable'
df2000_final <- df2000_final[ , !names(df2000_final) %in% c("variable")]
head(df2000_final) # check again, correct

#link for refernece: how to rename column name column:https://www.datanovia.com/en/lessons/rename-data-frame-columns-in-r/
names(df2000_final)[names(df2000_final) == "value"] <- "Population"
head(df2000_final) # final check, correct

###-------------------------------- df2000 & 2010 end ----------------------------------


###---------------------- Merge & Create part2-analytical-dataset.csv -----------------------
# merge two data frames by ID and Country
head(df0050_final)
head(df6070_final)
head(df8090_final)
head(df2000_final)


part2_dataset<-rbind(df0050_final,df6070_final,df8090_final,df2000_final)
head(part2_dataset)

# order the dataset by 'Year' and 'AGEGRP':
part2_dataset<- part2_dataset[order(part2_dataset$Year,part2_dataset$AGEGRP),]
nrow(part2_dataset) #768 rows

# write dataset to csv file:
write.csv(part2_dataset,"part2-analytical-dataset.csv", row.names = FALSE)



