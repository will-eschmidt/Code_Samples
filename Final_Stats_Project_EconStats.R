#first things first, set working directory 
setwd("C:/Users/willi/Documents/Lab2")
#load necessary packages for this project 
library(tidyverse)
library(readxl)
#use readxl to import the downloaded spreadshett into r and store it as UN
UN<- read_excel("Download-GDPconstant-USD-countries.xlsx",
                sheet = "Download-GDPconstant-USD-countr",
                skip = 2)
#view UN
head(UN)                
library(reshape2)                

wide_UN<-UN
#change parameters 
wide_UN = wide_UN[,-1]
#melt the variables to long_UN through the countries identity and indicator names 
long_UN = melt(wide_UN, id.vars = c("Country", "IndicatorName"),
               value.vars = 4:ncol(UN))
head(long_UN)
#change the name of "variable to something that'll adequetely explain what it is representing
names(long_UN)[names(long_UN)=="variable"]<-"Year"
#create a new subset cons (consumption)
cons = subset(long_UN, IndicatorName == "Final consumption expenditure")
#use a piping function to find the variables that are missing for each country and store into the environment
missing_by_country = cons %>% group_by(Country) %>% summarize(available_years=sum(!is.na(value))) %>% print()
sum(missing_by_country$available_years == max(
  missing_by_country$available_years))
#the next several commands create names for several importatn variables so we can visulize them 
#as the known components of GDP according to the expenditure approach 
long_UN$IndicatorName[long_UN$IndicatorName == 
                        strwrap("Household consumption expenditure (including Non-profit institutions serving households)")] <- 
  "HH.Expenditure"

long_UN$IndicatorName[long_UN$IndicatorName == 
                        "General government final consumption expenditure"] <- 
  "Gov.Expenditure"

long_UN$IndicatorName[long_UN$IndicatorName == 
                        "Final consumption expenditure"] <-
  "Final.Expenditure"

long_UN$IndicatorName[long_UN$IndicatorName == 
                        "Gross capital formation"] <-
  "Capital"

long_UN$IndicatorName[long_UN$IndicatorName == 
                        "Imports of goods and services"] <-
  "Imports"

long_UN$IndicatorName[long_UN$IndicatorName == 
                        "Exports of goods and services"] <-
  "Exports"
#now we table these values so we can create the value of net exports 
table_UN <- dcast(long_UN, Country + Year ~ IndicatorName)
table_UN$Net.Exports <- 
  table_UN[, "Exports"]-table_UN[, "Imports"]
#Choose these countries because they represnt the largest economies in their respective region and carry
#hefty geopolitical power
sel_countries = c("Brazil", "United States", "China")
#compare net exports between these three countries 
sel_UN1 = subset(table_UN, 
                 subset = (Country %in% sel_countries), 
                 select = c("Country", "Year", "Exports",
                            "Imports", "Net.Exports"))
#visulaize this comparison with the head() command
head(sel_UN1)
#create a new datafram comp that stands for comparision
#I picked Chile because I have always been fascinated by the economies of Latin America as they have at 
#one time been the most powerful economies in the world, and decades of poor policy has prevented them from
#retaining that power, making them an interesting way of looking at economic phenomena 
comp = subset(long_UN, Country %in%  c("United States","Chile"))

comp$value = comp$value / 1e9   
#subset comp by the variables necessary in calculating GDP 
comp = subset(comp, 
              select = c("Country", "Year",
                         "IndicatorName", "value"),
              subset = IndicatorName %in% c("Gov.Expenditure", 
                                            "HH.Expenditure", "Capital", "Imports", "Exports"))              
#create the graph pl which stands for plot, with respect to the United States so we can see how the 
#components make up GDP over time
pl = ggplot(subset(comp, Country=="United States"), aes(x=Year, y=value))
pl = pl+geom_line(aes(group = IndicatorName, color=IndicatorName), size=1)
pl
#add the title and axis titles so our graph is more descriptive 
pl= pl + title("GDP Components Over Time")
pl= pl + scale_x_discrete(breaks = seq(1970,2016, by=10))
pl= pl + scale_y_continuous(name="Billion US Dollars")                          
pl = pl + ggtitle("GDP components over time")
#differentiate by color so the components are easily distinguishable
pl= pl + scale_color_discrete(name="Components of GDP",
                              labels=c("Gross capital formation", "Exports",
                                       "Government expenditure", "Household expenditure",
                                       "Imports"))

pl= pl + theme_bw()
#add the great recession line to see when it happened and it's affect on the various components of GDP
pl= pl + annotate("text",x=37, y=850,
                  label = "Great Recession")
#view the graph 
pl






#create a plot 2 so we can compare Chile and the United States side by side 
pl2 = ggplot(comp,aes(x = Year, y = value, 
                           color = IndicatorName))


pl2 = pl2 + geom_line(aes(group = IndicatorName), size = 1)

pl2 = pl2 + scale_x_discrete(
  breaks = seq(1970, 2016, by = 10))

pl2 = pl2 + scale_y_continuous(name = "Billion US$")

pl2 = pl2 + ggtitle("GDP components over time")
pl2 = pl2 + scale_colour_discrete(name= "Component")
pl2 = pl2 + theme_bw()
pl2
#this facet wrap comand will split the two into seperate graphs so we can make comparisons 
pl2 = pl2 + facet_wrap(~Country)
pl2 = pl2 + scale_color_discrete(
  name = "Components of GDP",   
  labels = c("Gross capital formation",  
             "Exports",
             "Government expenditure",
             "Household expenditure",
             "Imports"))
#view the graph 
pl2
#now we use the cast comand to create a new datafram 
comp_wide <- dcast(comp, Country + Year ~ IndicatorName)

head(comp_wide)
#this will allow us to view net exports with the context of Chile and the United States 
comp_wide$Net.Exports<-comp_wide[, "Exports"] - comp_wide[, "Imports"]
head(comp_wide)
comp2_wide<-subset(comp_wide, select = -c(Exports, Imports))
comp2<-melt(comp2_wide, id.vars = c("Year","Country"))
#create props to compare the proportion of GDP that each variable represents 
props = comp2 %>% group_by(Country, Year) %>% mutate(proportion = value / sum(value))
#now we graph these proportions 
pl3<- ggplot(props, aes(x=Year, y=proportion, color=variable))
pl3 = pl3 + geom_line(aes(group=variable), size=1)
pl3 = pl3 + scale_x_discrete(breaks = seq(1970, 2016,
                                          by = 10))
pl3 = pl3 + title("GDP component proportions over time")
pl3 = pl3 + theme_bw()
pl3 = pl3 + facet_wrap(~Country)
pl3 = pl3 + scale_colour_discrete(
  name = "Components of GDP",
  labels = c("Gross capital formation",
             "Government expenditure",
             "Household expenditure", 
             "Net Exports"))
#view this new graph 
pl3
#create tables for all these values for future reference 
table_UN$p_Capital <- 
  table_UN$Capital / 
  (table_UN$Capital +
     table_UN$Final.Expenditure +
     table_UN$Net.Exports)

table_UN$p_FinalExp <-
  table_UN$Final.Expenditure / 
  (table_UN$Capital +
     table_UN$Final.Expenditure +
     table_UN$Net.Exports)

table_UN$p_NetExports <-
  table_UN$Net.Exports /
  (table_UN$Capital +
     table_UN$Final.Expenditure + 
     table_UN$Net.Exports)
#select new countries that represent developed, developing, and transition economies. #sticking with the theme
#of latin America to better see trends within the wider region\
sel_countries2<- c("United States", "Canada", "Denmark", "Brazil",
                   "Chile", "Argentina", "Guatemala", "Colombia", "Bolivia (Plurinational State of)")
sel_2015 <- 
  subset(table_UN, subset =
           (Country %in% sel_countries2) & (Year == 2015),
         select = c("Country", "Year", "p_FinalExp",
                    "p_Capital", "p_NetExports"))
#reduce sel 2 to several variables 
sel_2015_m <- melt(sel_2015, id.vars = 
                     c("Year", "Country"))
#now we plot
g <- ggplot(sel_2015_m, 
            aes(x = Country, y = value, fill = variable)) + 
  geom_bar(stat = "identity") + coord_flip() +
  ggtitle("GDP component proportions in 2015") +
  scale_fill_discrete(name = "Components of GDP",
                      labels = c("Final expenditure",
                                 "Gross capital formation",
                                 "Net Exports")) +
  theme_bw()
plot(g)
#now we order them, to help us better visulaize possible trends 
sel_2015_m$Country <- 
  factor(sel_2015_m$Country, levels = sel_countries2)
g2 <- ggplot(sel_2015_m, 
            aes(x = Country, y = value, fill = variable)) +
  geom_bar(stat = "identity") + coord_flip() + 
  ggtitle("GDP component proportions in 2015 (ordered)") + 
  scale_fill_discrete(name = "Components of GDP", 
                      labels = c("Final expenditure",
                                 "Gross capital formation",
                                 "Net Exports")) + theme_bw()
  
#view the final product 
plot(g2)

#Part 2 HDI 

HDI2019<-read_excel("hdro_statistical_data_table_1.xlsx",
                skip = 2,
                sheet = "Table 1")
head(HDI2019)
str(HDI2019)
#now we need to rename the columns to get a better sense of what the data is telling us 
names(HDI2019)[1] <-"HDI.rank"
names(HDI2019)[2]<-"Country"
names(HDI2019)[names(HDI2019)=="...15"]<-"HDI Rank 2017"
HDI2019<-subset(HDI2019, !is.na(HDI.rank) & HDI.rank !="HDI Rank")
#still don't have a good picture of what the variables are however 
names(HDI2019)[3]<-"HDI"
names(HDI2019)[5]<- "Life Expectancy"
names(HDI2019)[7]<- "Expected School"
names(HDI2019)[9]<- "Mean School"
names(HDI2019)[11]<- "GNI Per Capita"
names(HDI2019)[13]<-"GNI HDI Rank"
#we still have extra columns, so we would like to subset these out so we can get a better view of the variables 
#of interest 
sel_columns<- !startsWith(names(HDI2019), "...")
#found this 'starts with' command through an r cheat sheet I found online 
#now we have can subset these columns out easily 
HDI2019<- subset(HDI2019, select = sel_columns)
head(HDI2019)
#now we have made a table that is far easier to comprehend 
#However, the variables are still being read as character variables and not numeric, so we must force this change 
#through the as.numeric command
HDI019$HDI.rank <- as.numeric(HDI2019$HDI.rank)
HDI2019$Country <- as.factor(HDI2019$Country)
HDI2019$HDI <- as.numeric(HDI2019$HDI)
HDI2019$"Life Expectancy" <- as.numeric(HDI2019$"Life Expectancy")
HDI2019$"Expected School" <- as.numeric(HDI2019$"Expected School")
HDI2019$"Mean School" <- as.numeric(HDI2019$"Mean School")
HDI2019$"GNI Per Capita" <- as.numeric(HDI2019$"GNI Per Capita")
HDI2019$"GNI HDI Rank" <- as.numeric(HDI2019$"GNI HDI Rank")
HDI2019$HDI.rank.2017 <- as.numeric(HDI2019$HDI.rank.2017)
str(HDI2019)

#now we are in a position to calculate the three different indicies 

HDI2019$I.Health<-(HDI2019$"Life Expectancy" - 20) / (85 - 20)
HDI2019$I.Education<- ((pmin(HDI2019$"Expected School", 18) - 0) /
                         (18-0)+(HDI2019$"Mean School" - 0) / 
                         (15 - 0)) / 2
HDI2019$I.Income <- (log(HDI2019$"GNI Per Capita") - log(100)) / 
  (log(75000) - log(100))
HDI2019$HDI.calc <- (HDI2019$I.Health * HDI2019$I.Education * HDI2019$I.Income) ^(1 / 3)  
HDI2019[, c("HDI", "HDI.calc")]
#Now we have calculated HDI using their equations!! 

#next we import the new dataset into the enviornment
#note that I have edited this graph so that table 1 includes the new variables that I plan on constructing 
#my own HDI measure as I have moved the variables of interest from their respective tables to Table 1
#for convience 
AllHDI2019<- read_excel("hdro_statistical_data_tables_1_15_d1_d5 (2).xlsx",
                        sheet = "Table 1")
AllHDI2019
head(AllHDI2019)
str(AllHDI2019)

# Rename the first column, currently named X_1
names(AllHDI2019)[1] <- "HDI.rank"

# Rename the second column, currently named X_2 
names(AllHDI2019)[2] <- "Country"

#rename variables that we are interested in 
names(AllHDI2019)[17]<- "Pop.sec.edu"
names(AllHDI2019)[18]<- "Primary.school.drop"
names(AllHDI2019)[19]<- "Gov.exp.edu"
names(AllHDI2019)[20]<- "Current.health.exp"

names(AllHDI2019)[21]<- "HIV.prev.adult"
names(AllHDI2019)[22]<-"Child.malnutrition, stunting"



# Eliminate the row that contains the column title

AllHDI2019
AllHDI2019 <- subset(AllHDI2019,
                  !is.na(HDI.rank) & HDI.rank != "HDI rank")
select_columns <- !startsWith(names(AllHDI2019), "...")
AllHDI2019 <- subset(AllHDI2019, select = select_columns)

str(AllHDI2019)
#once again we need to convert these values into numeric so the data is easily comprehend 
#we must ignore the NA values in several of the columns and force these to become true
AllHDI2019$HDI.rank <- as.numeric(AllHDI2019$HDI.rank)
AllHDI2019$Country <- as.factor(AllHDI2019$Country)
AllHDI2019$Pop.sec.edu <- as.numeric(AllHDI2019$Pop.sec.edu)
AllHDI2019$Primary.school.drop <- as.numeric(AllHDI2019$Primary.school.drop)
AllHDI2019$Gov.exp.edu <- as.numeric(AllHDI2019$Gov.exp.edu)
AllHDI2019$Current.health.exp <- as.numeric(AllHDI2019$Current.health.exp)
AllHDI2019$HIV.prev.adult <- as.numeric(AllHDI2019$HIV.prev.adult)
AllHDI2019$"Child.malnutrition, stunting" <- as.numeric(AllHDI2019$"Child.malnutrition, stunting")
str(AllHDI2019)

summary(AllHDI2019)
#using the max and min values that i got using the summary command, you can now calculuate the indinces 
#needed for HDI 
AllHDI2019$I.Pop.sec.edu <- (AllHDI2019$Pop.sec.edu - 6.026) / (100 - 6.026)
AllHDI2019$I.Primary.school.drop <- (AllHDI2019$Primary.school.drop - 0.06538) / (71.44060 - 0.06538)
AllHDI2019$I.Gov.exp.edu <- (AllHDI2019$Gov.exp.edu - 0.9811) / (12.4595 - 0.9811)
AllHDI2019$I.Current.health.exp <- (AllHDI2019$Current.health.exp - 1.750) / (23.287 - 1.750)
AllHDI2019$I.HIV.prev.adult <- (AllHDI2019$HIV.prev.adult - 0.100) / (27.400 - 0.100)
AllHDI2019$I.Child.malnutrition.stunting <- (AllHDI2019$`Child.malnutrition, stunting` - 1.80) / (55.65-1.80)
AllHDI2019$I.Health.Alt <- (AllHDI2019$I.HIV.prev.adult+
                              AllHDI2019$I.Current.health.exp +
                              AllHDI2019$I.Child.malnutrition.stunting) / 3
AllHDI2019$I.Health.Alt <- (1 - AllHDI2019$I.Health.Alt)
AllHDI2019$I.Education.Alt <- (AllHDI2019$I.Gov.exp.edu +
                                 AllHDI2019$I.Primary.school.drop +
                                 AllHDI2019$I.Pop.sec.edu) / 3
#All these equations are the ones found on the project website, we just need to plug in 
summary(AllHDI2019$I.Education.Alt)
summary(AllHDI2019$I.Health.Alt)

#now we merge, so we can put these new observations with the old observations and compare 
HDI2019 <- merge(HDI2019, AllHDI2019)

#calculate alternative overall HDI

HDI2019$HDI.own <- (HDI2019$I.Education.Alt * HDI2019$I.Health.Alt * HDI2019$I.Income) ^ (1/3)
summary(HDI2019$HDI.own)

HDI2019_sub <- subset(HDI2019, !is.na(HDI.rank) & !is.na(HDI.own))
HDI2019_sub$HDI.own.rank <- rank(-HDI2019_sub$HDI.own, na.last = "keep")
HDI2019_sub$HDI.rank <- rank(-HDI2019_sub$HDI, na.last = "keep")
ggplot(HDI2019_sub, aes(x= HDI.rank, y=HDI.own.rank)) +
  geom_point(shape= 16) + labs(y = "Alternative HDI rank", x= "HDI rank") +
  ggtitle("Comparing ranks between HDI and HDI.own") + theme_bw()

#now we calculate the largest fall in rank with regards to real HDI and the HDi we calculated 
temp<- HDI2019_sub[order(HDI2019_sub$HDI.rank - HDI2019_sub$HDI.own.rank),
                   c("Country", "HDI.rank", "HDI.own.rank")]
head(temp, 5)

#what about the smallest falls in rank 

tail(temp, 5)
View(temp)

#Now we must rank by GDP per capita in graph form to see how GNI Per Capita compares with HDI Rank 

HDI2019_GDPsub <- subset(HDI2019, !is.na("HDI") & !is.na("GNI Per Capita"))
HDI2019_GDPsub$GNI.capita.rank<- rank(-HDI2019_GDPsub$"GNI Per Capita", na.last = "keep")
HDI2019_GDPsub$HDI.rank<-rank(-HDI2019_GDPsub$HDI, na.last = "keep")
ggplot(HDI2019_GDPsub, aes(x=HDI.rank, y=GNI.capita.rank)) + 
  geom_point(shape=16)+
  labs(y= "GNI per capita rank", x="HDI Rank") +
  ggtitle("Comparing ranks between HDI and GNI Per Capita Rank") + 
  theme_bw()


