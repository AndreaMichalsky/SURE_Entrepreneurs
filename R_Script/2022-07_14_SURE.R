# The SURE Program Data set
# Exploratory Data Analysis (EDA)

# Import the data from CSV file
data <- read.csv("sure_database.csv")
View(data)
dim(data)

# Let's convert all characters to factors
data[sapply(data, is.character)] <- lapply(data[sapply(data, is.character)], as.factor)
str(data)

# Replacing blank & space by NA
data.df <- data
data.df[data.df == "" | data.df == " "] <- NA
View(data.df)
is.na(data.df)

# Dimension and structure of the Data set
dim(data.df)  # Our data frame consist of 1808 rows and 255 columns
str(data.df)

# Determine the number of NA values in a column
na_count <- sapply(data.df, function(data.df) sum(length(which(is.na(data.df)))))
na_count <- data.frame(na_count)
hist(na_count$na_count/nrow(data))

# Data visualization

#install.packages("ggplot2")
#install.packages("tidyverse")
#install.packages("naniar")
library("tidyverse")
library("ggplot2")
library("naniar")
vis_miss(data.df)
levels(factor(data.df$Year_Enrollment))
table(data.df$Year_Enrollment)
plot(table(data.df$Year_Enrollment))
table(data.df$ImpactStudy20192020)
table(data.df$ImpactStudy2016)

# Business Variables

# Histogram of total full-time employees hired
data_num <- as.numeric(data.df$Total_Full_Time_Employees_Hired)

png("Full_time.png", width = 140, height = 140, units = "mm", res = 600)
hist(data_num, xlab = "Number of employees", main = "", breaks = 50, col = "blue3",
     las = 1)
box()
dev.off()

sum(table(data.df$Total_Full_Time_Employees_Hired)) # Number of observation 
summary(data.df$Total_Full_Time_Employees_Hired)

# Histogram of total part-time employees hired
data_num2 <- as.numeric(data.df$Total_Part_Time_Employees_Hired)

png("Part_time.png", width = 140, height = 140, units = "mm", res = 600)
hist(data_num2, xlab = "Number of employees", main = "", breaks = 50, col = "cornflowerblue",
     las = 1)
box()
dev.off()

sum(table(data.df$Total_Part_Time_Employees_Hired)) # Number of observation 
summary(data.df$Total_Part_Time_Employees_Hired)

# Histogram of wages
wages <- data.df$Wages_Spent_Ever
png("wages.png", width = 140, height = 140, units = "mm", res = 600)
hist(wages/1000, xlab = "Wages in thousand U.S. dollars", main = "", breaks = 50, col = "darkgreen",
     las = 1)
box()
dev.off()

sum(table(wages)) # Number of observations 
summary(wages)

# Side-by-side box plot of Wages by gender (wages is expressed in log scale)

data.df2 = data.df[!data.df$Gender %in% c("Prefer Not to Say", "transmale"),]
data.df2$Wages_Spent_Ever[data.df2$Wages_Spent_Ever == 0] = NA
levels(data.df2$Gender)[5] = "Other"

png("wages_gender.png", width = 140, height = 140, units = "mm", res = 600)
data.df2$Gender = as.factor(as.character(data.df2$Gender))
par(mar=c(6.1, 4.1, 0.1, 2.1), mgp = c(3, 1, 0))
plot(Wages_Spent_Ever ~ Gender, data = data.df2, log = "y", las = 2, yaxt = "n", xlab = "", col = "darkgreen", ylab = "Wages spent ever in U.S. dollars")
axis(side = 2, at = c(10, 10^2, 10^3, 10^4, 10^5, 10^6), labels = c(expression(10^1), expression(10^2), expression(10^3), expression(10^4), expression(10^5), expression(10^6)), las = 1)
dev.off()

# Summary statistics of wages by gender
aggregate(Wages_Spent_Ever ~ Gender, data.df2, FUN = summary)
# or
#install.packages("dplyr")
#install.packages("statsr")
library("dplyr")
library("statsr")
data.df2 %>%
  filter(!is.na(Gender)) %>%
  filter(!is.na(Wages_Spent_Ever)) %>%
  group_by(Gender) %>%
  summarise(n = n(), mean = mean(Wages_Spent_Ever), sd = sd(Wages_Spent_Ever), min = min(Wages_Spent_Ever),
             max = max(Wages_Spent_Ever))

# Pie chart of entrepreneurs who received external funding
data.df$External_Funding = as.factor(as.character(data.df$External_Funding))

names(table(data.df$External_Funding))
sum(table(data.df$External_Funding))
round(100*table(data.df$External_Funding)/248)
lbls2 <- paste(names(table(data.df$External_Funding)), round(100*table(data.df$External_Funding)/248), "%")
png("Funding.png", width = 140, height = 140, units = "mm", res = 600)
pie(table(data.df$External_Funding), labels = lbls2, main = "",
    col = c("dodgerblue3", "darkolivegreen3"))
dev.off() 

summary(data.df$External_Funding)

# Pie chart of business that have been mentioned in the media 
data.df$Media_Mention = as.factor(as.character(data.df$Media_Mention))

names(table(data.df$Media_Mention))
sum(table(data.df$Media_Mention))
round(100*table(data.df$Media_Mention)/1808)
lbls3 <- paste(names(table(data.df$Media_Mention)), round(100*table(data.df$Media_Mention)/1808), "%")
png("Media.png", width = 140, height = 140, units = "mm", res = 600)
pie(table(data.df$Media_Mention), labels = lbls3, main = "",
    col = c("darkorange1", "ivory4"))
dev.off()
summary(data.df$Media_Mention)

# Histogram of revenue
class(data.df$Monthly_Revenue)
revenue <- as.numeric(data.df$Monthly_Revenue)
png("Revenue.png", width = 140, height = 140, units = "mm", res = 600)
hist(revenue/1000, xlab = "Monthly revenue in thousands of U.S. dollar", main = "", breaks = 50, col = "gold1",
     las = 1)
box()
dev.off()

sum(table(data.df$Monthly_Revenue)) # Number of observation 
summary(data.df$Monthly_Revenue)

# Box plot of revenue by gender #!!!
names(table(data.df$Gender))
data.df3 = data.df[!data.df$Gender %in% c("Other/Prefer Not to Say", "transmale"),]
data.df3$Monthly_Revenue[data.df3$Monthly_Revenue == 0] = NA
levels(data.df3$Gender)[5] = "Other"


data.df3$Gender = as.factor(as.character(data.df3$Gender))
par(mar=c(6.1, 4.1, 0.1, 2.1), mgp = c(3, 1, 0))
plot(Monthly_Revenue ~ Gender, data = data.df3, log = "y", las = 2, yaxt = "n", xlab = "", col = "gold1", ylab = "Monthly revenue in U.S. dollars")
axis(side = 2, at = c(10, 10^2, 10^3, 10^4, 10^5, 10^6), labels = c(expression(10^1), expression(10^2), expression(10^3), expression(10^4), expression(10^5), expression(10^6)), las = 1)



# Summary statistics 
data.df3 %>%
  filter(!is.na(Gender)) %>%
  filter(!is.na(Monthly_Revenue)) %>%
  group_by(Gender) %>%
  summarise(n = n(), mean = mean(Monthly_Revenue), sd = sd(Monthly_Revenue), min = min(Monthly_Revenue),
            max = max(Monthly_Revenue))

# Side-by-side Box plot
ggplot(data.df[!is.na(data.df$Gender),], aes(x = Gender, y = Monthly_Revenue)) +
  geom_boxplot()
# Or
boxplot(Monthly_Revenue ~ Gender, data = data.df)

# Box plot of revenue by race
# Summary statistics 
data.df %>%
  filter(!is.na(Race)) %>%
  filter(!is.na(Monthly_Revenue)) %>%
  group_by(Race) %>%
  summarise(n = n(), mean = mean(Monthly_Revenue), sd = sd(Monthly_Revenue), min = min(Monthly_Revenue),
            max = max(Monthly_Revenue))

# Side-by-side Box plot
ggplot(data.df[!is.na(data.df$Race),], aes(x = Race, y = Monthly_Revenue)) +
  geom_boxplot()
# Or
boxplot(Monthly_Revenue ~ Race, data = data.df)

# Histogram of monthly expenses
class(data.df$Monthly_Burn_Rate)
expenses <- data.df$Monthly_Burn_Rate
hist(expenses, xlab = "US Dollar", main = "Monthly Expenses")
sum(table(data.df$Monthly_Burn_Rate)) # Number of observation 
summary(data.df$Monthly_Burn_Rate)

#	Box plot of monthly expenses by gender
# Summary statistics 
data.df %>%
  filter(!is.na(Gender)) %>%
  filter(!is.na(Monthly_Burn_Rate)) %>%
  group_by(Gender) %>%
  summarise(n = n(), mean = mean(Monthly_Burn_Rate), sd = sd(Monthly_Burn_Rate), min = min(Monthly_Burn_Rate),
            max = max(Monthly_Burn_Rate))

# Side-by-side Box plot
ggplot(data.df[!is.na(data.df$Gender),], aes(x = Gender, y = Monthly_Burn_Rate)) +
  geom_boxplot()

# Box plot of monthly expenses by race
# Summary statistics 
data.df %>%
  filter(!is.na(Race)) %>%
  filter(!is.na(Monthly_Burn_Rate)) %>%
  group_by(Race) %>%
  summarise(n = n(), mean = mean(Monthly_Burn_Rate), sd = sd(Monthly_Burn_Rate), min = min(Monthly_Burn_Rate),
            max = max(Monthly_Burn_Rate))

# Side-by-side Box plot
ggplot(data.df[!is.na(data.df$Race),], aes(x = Race, y = Monthly_Burn_Rate)) +
  geom_boxplot()


# Demographics variables

# Pie chart of gender
#install.packages("RColorBrewer")

levels(data.df$Gender) = c("Female", "Male", "Non-Binary", "Non-Binary", "Non-Binary", "Non-Binary")

library("RColorBrewer")
data.df$Gender = as.factor(as.character(data.df$Gender))
names(table(data.df$Gender))
sum(table(data.df$Gender))
round(100*table(data.df$Gender)/1543)
lbls <- paste(names(table(data.df$Gender)), round(100*table(data.df$Gender)/1543),"%")
png("Gender.png", width = 160, height = 140, units = "mm", res = 600)
pie(table(data.df$Gender), labels = lbls, main = "",
    col = c("hotpink1", "blue4", "turquoise2", "turquoise2","turquoise2", "turquoise2"))
dev.off()

summary(data.df$Gender)

# Pie chart of Race by gender
data.df$Race = as.factor(as.character(data.df$Race))
summary(data.df$Race)
names(table(data.df$Race))
levels(data.df$Race) = c("Asian", "Black or African American", "Other", "Other", "Native American or Alaska", "Other", "White")
names(table(data.df$Race))
summary(data.df$Race)

png("Gender_Race.png", width = 160, height = 140, units = "mm", res = 600)
data.df$count = 1
freq_gender_race = aggregate(count ~ Gender + Race, data.df, FUN = sum)
freq_gender_race$perc = NA

freq_gender_race[freq_gender_race$Race == "Asian", "perc"] = 
  freq_gender_race[freq_gender_race$Race == "Asian", "count"]/
  sum(freq_gender_race[freq_gender_race$Race == "Asian", "count"])*100

freq_gender_race[freq_gender_race$Race == "Black or African American", "perc"] = 
  freq_gender_race[freq_gender_race$Race == "Black or African American", "count"]/
  sum(freq_gender_race[freq_gender_race$Race == "Black or African American", "count"])*100

freq_gender_race[freq_gender_race$Race == "Native American or Alaska", "perc"] = 
  freq_gender_race[freq_gender_race$Race == "Native American or Alaska", "count"]/
  sum(freq_gender_race[freq_gender_race$Race == "Native American or Alaska", "count"])*100

freq_gender_race[freq_gender_race$Race == "White", "perc"] = 
  freq_gender_race[freq_gender_race$Race == "White", "count"]/
  sum(freq_gender_race[freq_gender_race$Race == "White", "count"])*100

freq_gender_race[freq_gender_race$Race == "Other", "perc"] = 
  freq_gender_race[freq_gender_race$Race == "Other", "count"]/
  sum(freq_gender_race[freq_gender_race$Race == "Other", "count"])*100


ggplot(freq_gender_race, aes(x=Gender, y=perc))+
  geom_bar(stat='identity', fill= "forest green")+
  facet_wrap(~Race)

dev.off()

table(data.df$Gender, data.df$Race)

# Pie chart of race
names(table(data.df$Race))
sum(table(data.df$Race))
round(100*table(data.df$Race)/1536)
lbls10 <- paste(names(table(data.df$Race)), round(100*table(data.df$Race)/1536),"%")
png("Race.png", width = 200, height = 160, units = "mm", res = 600)
pie(table(data.df$Race), labels = lbls10, main = "",
    col = c("red4", "blue4", "turquoise2", "yellow1", "olivedrab"))
dev.off()

summary(data.df$Race)


# Pie chart of entrepreneurs who live currently in Third Ward

data.df$Currently_Third_Ward = as.factor(as.character(data.df$Currently_Third_Ward))
names(table(data.df$Currently_Third_Ward))
sum(table(data.df$Currently_Third_Ward))
round(100*table(data.df$Currently_Third_Ward)/428)
lbls4 <- paste(names(table(data.df$Currently_Third_Ward)), round(100*table(data.df$Currently_Third_Ward)/428),"%")
png("Third.png", width = 140, height = 140, units = "mm", res = 600)
pie(table(data.df$Currently_Third_Ward), labels = lbls4, main = "",
    col = topo.colors(4))
dev.off()

summary(data.df$Currently_Third_Ward)

# Pie chart of entrepreneurs who participate in Military services 
data.df$Military_Service = as.factor(as.character(data.df$Military_Service))

names(table(data.df$Military_Service))
sum(table(data.df$Military_Service))
round(100*table(data.df$Military_Service)/1350)
lbls5 <- paste(names(table(data.df$Military_Service)), round(100*table(data.df$Military_Service)/1350),"%")
png("Military.png", width = 250, height = 140, units = "mm", res = 600)
pie(table(data.df$Military_Service), labels = lbls5, main = "",
    col = c("cyan4", "yellow1", "red", "forestgreen", "royalblue3"), cex=0.6)
dev.off()

summary(data.df$Military_Service)

# Pie chart of Refugee
data.df$Refugee = as.factor(as.character(data.df$Refugee))

names(table(data.df$Refugee))
sum(table(data.df$Refugee))
round(100*table(data.df$Refugee)/876)
lbls6 <- paste(names(table(data.df$Refugee)), round(100*table(data.df$Refugee)/876),"%")
png("Refugee.png", width = 140, height = 140, units = "mm", res = 600)
pie(table(data.df$Refugee), labels = lbls6, main = "",
    col = c("royalblue2", "tomato1"))
dev.off()

summary(data.df$Refugee)

# Pie chart of entrepreneurs who speak English as a first language 
names(table(data.df$English_First_Language))
sum(table(data.df$English_First_Language))
round(100*table(data.df$English_First_Language)/882)
lbls7 <- paste(names(table(data.df$English_First_Language)), round(100*table(data.df$English_First_Language)/882),"%")
pie(table(data.df$English_First_Language), labels = lbls7, main = "Pie Chart of Entrepreneurs who speak English as their first language",
    col = rainbow(3))
summary(data.df$English_First_Language)

#	Bar chart of the number of participants by languages 
#install.packages("mosaicData")
library("mosaicData")
library("ggplot2")
plot(data.df$Spoken_Languages, main = "Bar Chart of Spoken Languages")
sum(table(data.df$Spoken_Languages))
x <- 100*table(data.df$Spoken_Languages)/986
barplot(x, main = "Bar Chart of Spoken Languages in %")

# Bar chart of the number of participants by Marital status 
names(table(data.df$Martial_Status))
plot(data.df$Martial_Status, main = "Bar Chart of Marital Status")
sum(table(data.df$Martial_Status))
status <- 100*table(data.df$Martial_Status)/1386
barplot(status, main = "Bar Chart of Marital Status in %")

# Pie chart of entrepreneurs who ever receive Food Stamp or SNAP 
names(table(data.df$Ever_Food_Stamp_Or_SNAP))
sum(table(data.df$Ever_Food_Stamp_Or_SNAP))
round(100*table(data.df$Ever_Food_Stamp_Or_SNAP)/874)
lbls8 <- paste(names(table(data.df$Ever_Food_Stamp_Or_SNAP)), round(100*table(data.df$Ever_Food_Stamp_Or_SNAP)/874),"%")
pie(table(data.df$Ever_Food_Stamp_Or_SNAP), labels = lbls8, main = "Pie Chart of Entrepreneurs who ever received Food Stamp or SNAP",
    col = rainbow(8))
summary(data.df$Ever_Food_Stamp_Or_SNAP)

# Bar chart of number of participants by employment status # !!!
names(table(data.df$Employement_Status))
plot(data.df$Employement_Status, main = "Bar Chart of participants by employment status")
sum(table(data.df$Employement_Status))
empl <- 100*table(data.df$Employement_Status)/869
barplot(empl, main = "Bar Chart of participants by employment status in %")

# Financial literacy variables

# Checking account after SURE
names(table(data.df$Before_SURE_Checking))
plot(data.df$Before_SURE_Checking, main = "Bar Chart of participants who have checking account before SURE")
sum(table(data.df$Before_SURE_Checking))
bcheck <- 100*table(data.df$Before_SURE_Checking)/797
barplot(bcheck, main = "Bar Chart of participants who have checking account before SURE")


aggregate(Wages_Spent_Ever ~ Gender + Race, data.df2, FUN = summary)

###################################################################
# Response variable
###################################################################

# Import the data from CSV file
data <- read.csv("sure_database.csv")
View(data)
dim(data)

# Replacing blank & space by NA
launch.df <- data
launch.df[launch.df == "" | launch.df == " "] <- NA
View(launch.df)
is.na(launch.df)

# Let's convert all characters to factors
launch.df[sapply(launch.df, is.character)] <- lapply(launch.df[sapply(launch.df, is.character)], as.factor)
str(launch.df)

# Pick the variables for business launched 
#install.packages("tidyverse")
#library(tidyverse)
library(dplyr)
launch.df$row_ID = 1:nrow(launch.df)
business_launched <- launch.df %>% select(row_ID, ImpactStudy20192020, ImpactStudy2016, Semester, Year_Enrollment,
                                          Business_Launched, Launch_Month, Launch_Year, Business_Active, legal_structure,
                                          Monthly_Revenue, AngelInvestors_Month_and_Year, BusinessLineofCredit_Month_and_Year,
                                          ConventionalSmallBusinessLoan_Month_and_Year, Microloan_Month_and_Year,
                                          SBA.backed_Small_Business_Bank_Loan_Month_and_Year, CompanyEarningsFunding_Month_and_Year)
                                          

summary(business_launched)

# Exporting the data frame to Excel file in R
#install.packages("writexl")
library("writexl")
write_xlsx(business_launched,"C:/Users/anmic/Documents/UH_Summer Internship 2022/Data//business_launched.xlsx")

# create dummy variables

business_launched$Business_Launched_dummy <- ifelse(business_launched$Business_Launched == "Yes, BEFORE attending the SURE Program" | business_launched$Business_Launched =="Yes, WHILE attending the SURE Program" | 
                                                    business_launched$Business_Launched =="Yes, AFTER attending the SURE Program" | business_launched$Business_Launched == "Yes", 1,0)
business_launched$Business_Launched_dummy[is.na(business_launched$Business_Launched_dummy)] = 0


business_launched$Launch_Month_dummy <- ifelse(is.na(business_launched$Launch_Month), 0,1)
business_launched$Launch_Year_dummy <- ifelse(is.na(business_launched$Launch_Year), 0,1)
business_launched$Business_Active_dummy <- ifelse(business_launched$Business_Active == TRUE, 1,0)
business_launched$legal_structure_dummy <- ifelse(is.na(business_launched$legal_structure), 0,1)
business_launched$Monthly_Revenue_dummy <- ifelse(is.na(business_launched$Monthly_Revenue), 0,1)
business_launched$AngelInvestors_dummy <- ifelse(is.na(business_launched$AngelInvestors_Month_and_Year),0,1)
business_launched$BusinessLineofCredit_dummy <- ifelse(is.na(business_launched$BusinessLineofCredit_Month_and_Year), 0,1)
business_launched$ConventionalSmallBusinessLoan_dummy <- ifelse(is.na(business_launched$ConventionalSmallBusinessLoan_Month_and_Year), 0,1)
business_launched$Microloan_dummy <- ifelse(is.na(business_launched$Microloan_Month_and_Year), 0,1)
business_launched$SBA.backed_Small_Business_Bank_dummy <- ifelse(is.na(business_launched$SBA.backed_Small_Business_Bank_Loan_Month_and_Year), 0,1)
business_launched$CompanyEarningsFunding_dummy <- ifelse(is.na(business_launched$CompanyEarningsFunding_Month_and_Year), 0,1)

business_launched$launched <- ifelse(business_launched$Business_Launched_dummy == 1 | business_launched$Launch_Month_dummy == 1 | business_launched$Launch_Year_dummy == 1 |
                                       business_launched$Business_Active_dummy == 1 | business_launched$legal_structure_dummy == 1 | business_launched$Monthly_Revenue_dummy == 1 |
                                       business_launched$AngelInvestors_dummy == 1 | business_launched$BusinessLineofCredit_dummy == 1 | business_launched$ConventionalSmallBusinessLoan_dummy == 1 |
                                       business_launched$Microloan_dummy == 1 | business_launched$SBA.backed_Small_Business_Bank_dummy == 1 | business_launched$CompanyEarningsFunding_dummy == 1, "Yes","No")

business_launched$launched = as.factor(business_launched$launched)
summary(business_launched$launched)

# creating a data frame for the response variable
launched = business_launched[, c("row_ID", "launched")]
#new.df <- launch.df
#new.df <- subset(new.df, select = -c(Business_Launched, Launch_Month, Launch_Year, Business_Active, legal_structure, Monthly_Revenue, AngelInvestors_Month_and_Year, BusinessLineofCredit_Month_and_Year, 
#                                     ConventionalSmallBusinessLoan_Month_and_Year, Microloan_Month_and_Year, SBA.backed_Small_Business_Bank_Loan_Month_and_Year, CompanyEarningsFunding_Month_and_Year)) 

#SURE1 = merge(new.df, launched, by = "row_ID")

SURE = merge(launch.df, launched, by = "row_ID")  # this data set includes all variables + launched 


###################################################################
# Cleaning the database
###################################################################
str(SURE)
#write_xlsx(SURE,"C:/Users/anmic/Documents/UH_Summer Internship 2022/Data//SUREall.xlsx")
class(SURE)

# Dropping columns by index in a data frame
sure.df <- SURE
sure.df <- sure.df[, -c(1,3:7,9:21,23:26,28:32,34:44,46:130,133:138,140:142,144,147,148,
                        150,153,154,156:171,173,174,176:181,183,184,195:197,199,210:256)]

# Vectors of new categories from old categories 
B2B_serv_categories = c("Accounting", "Advertising", "Business Services", "Finance", "Freelancing/consulting", "Insurance", 
                        "Marketing", "Professional Services", "Technical Services", "Technology")

Retail_and_personal_categories = c("Beauty", "CBD", "E-commerce", "Fitness", "Retail")

B2Consumer_serv_categories = c("Coaching", "Event Planning", "Recreation", "Travel/accommodations")

Heavy_industry_categories = c("Agriculture", "Construction", "Manufacturing", "Real estate, rental and leasing",
                              "Skilled Trades (Electrician, plumbing, carpentry, etc.)", "Transportation", "Wholesale Trade")

Food_categories = c("Food/beverage", "Restaurant")

Not_for_profits_categories = c("Arts/entertainment", "Education", "Healthcare", "Nonprofit", "Shelter/rehabilitation")

# Re-categorize factor variable
levels(sure.df$Primary_Industry)[which(levels(sure.df$Primary_Industry) %in% B2B_serv_categories)] = "B2B_serv"
levels(sure.df$Primary_Industry)[which(levels(sure.df$Primary_Industry) %in% Retail_and_personal_categories)] = "Retail_and_personal"
levels(sure.df$Primary_Industry)[which(levels(sure.df$Primary_Industry) %in% B2Consumer_serv_categories)] = "B2Consumer_serv"
levels(sure.df$Primary_Industry)[which(levels(sure.df$Primary_Industry) %in% Heavy_industry_categories)] = "Heavy_industry"
levels(sure.df$Primary_Industry)[which(levels(sure.df$Primary_Industry) %in% Food_categories)] = "Food"
levels(sure.df$Primary_Industry)[which(levels(sure.df$Primary_Industry) %in% Not_for_profits_categories)] = "Not_for_profits"
levels(sure.df$Primary_Industry)[which(levels(sure.df$Primary_Industry) == "Misc")] = NA

summary(sure.df$Primary_Industry)
View(sure.df)

# Re-categorize factor from product_service variable
levels(sure.df$product_service)
levels(sure.df$product_service)[which(levels(sure.df$product_service) == "www.linkedin.com/in/perpax/")] = NA
summary(sure.df$product_service)

# Get age from DOB_Year
class(sure.df$DOB_Year)
sure.df$age <- 2022 - sure.df$DOB_Year
hist(sure.df$age)
# Dropping DOB_Year = 1900
sure.df$age[sure.df$DOB_Year %in% c("1900")] = NA  
hist(sure.df$age)
summary(as.factor(sure.df$age))
sure.df$DOB_Year <- NULL
 
# Vectors of new categories from old categories of Marital Status variable 
levels(sure.df$Martial_Status)
Married_category = c("Currently married", "Currently Married", "Married")
Separated_category = c("Separated", "Seperated")
Never_Married_category = c("Single", "Singleg", "Never Married")

# Re-categorizing factors from Marital Status variable
levels(sure.df$Martial_Status)[which(levels(sure.df$Martial_Status) %in% Married_category)] = "Married"
levels(sure.df$Martial_Status)[which(levels(sure.df$Martial_Status) %in% Separated_category)] = "Separated"
levels(sure.df$Martial_Status)[which(levels(sure.df$Martial_Status) %in% Never_Married_category)] = "Never_Married"
table(sure.df$Martial_Status)


# Creating a vector to modify a category from Education Level variable
levels(sure.df$Education_Level)
Doctorate_degree_category = c("Doctorate degree", "Doctorate degree (For example: PhD, EdD)")
# Re-categorizing a factor from Education Level variable
levels(sure.df$Education_Level)[which(levels(sure.df$Education_Level) %in% Doctorate_degree_category)] = "Doctorate degree"
table(sure.df$Education_Level)


# create dummy variables for Employment_Status
levels(sure.df$Employement_Status)
sure.df$Self_employed_Part_Time <- ifelse(sure.df$Employement_Status %in% c("Self-employed, Part Time", "Self-employed, Part Time,Employed by an employer, Full Time", 
                                          "Self-employed, Part Time,Employed by an employer, Full Time,Freelance / Gig Work (For example, Uber, Favor, Fiverr)",
                                          "Self-employed, Part Time,Employed by an employer, Part Time", "Self-employed, Part Time,Employed by an employer, Part Time,Employed by an employer, Full Time",
                                          "Self-employed, Part Time,Employed by an employer, Part Time,Freelance / Gig Work (For example, Uber, Favor, Fiverr)",
                                          "Self-employed, Part Time,Freelance / Gig Work (For example, Uber, Favor, Fiverr)", "Self-employed, Part Time,Self-employed, Full Time",
                                          "Self-employed, Part Time,Self-employed, Full Time,Employed by an employer, Full Time", "Self-employed, Part Time,Self-employed, Full Time,Employed by an employer, Part Time",
                                          "Self-employed, Part Time,Self-employed, Full Time,Freelance / Gig Work (For example, Uber, Favor, Fiverr)"), 1,0)
sure.df$Self_employed_Part_Time[sure.df$Employement_Status %in% c("Full-time", "Full Time", "Part-time", "Part Time")] = NA


sure.df$Self_employed_Full_Time <- ifelse(sure.df$Employement_Status %in% c("Self-employed Full-Time, Freelance/Gig Work", "Self-employed Full Time", "Self-employed, Full Time",
                                            "Self-employed, Full Time,Employed by an employer, Full Time", "Self-employed, Full Time,Employed by an employer, Full Time,Freelance / Gig Work (For example, Uber, Favor, Fiverr)",
                                            "Self-employed, Full Time,Employed by an employer, Part Time", "Self-employed, Full Time,Employed by an employer, Part Time,Freelance / Gig Work (For example, Uber, Favor, Fiverr)",
                                            "Self-employed, Full Time,Freelance / Gig Work (For example, Uber, Favor, Fiverr)", "Self-employed, Part Time,Self-employed, Full Time", "Self-employed, Part Time,Self-employed, Full Time,Employed by an employer, Full Time",
                                            "Self-employed, Part Time,Self-employed, Full Time,Employed by an employer, Part Time", "Self-employed, Part Time,Self-employed, Full Time,Freelance / Gig Work (For example, Uber, Favor, Fiverr)"), 1,0)
sure.df$Self_employed_Full_Time[sure.df$Employement_Status %in% c("Full-time", "Full Time", "Part-time", "Part Time")] = NA


sure.df$Employed_by_an_employer_Part_Time <- ifelse(sure.df$Employement_Status %in% c("Employed by an employer, Part Time", "Employed by an employer, Part Time,Employed by an employer, Full Time", "Employed by an employer, Part Time,Employed by an employer, Full Time,Freelance / Gig Work (For example, Uber, Favor, Fiverr)",
                                                    "Part-time", "Part Time", "Self-employed, Full Time,Employed by an employer, Part Time", "Self-employed, Full Time,Employed by an employer, Part Time,Freelance / Gig Work (For example, Uber, Favor, Fiverr)", 
                                                    "Self-employed, Part Time,Employed by an employer, Part Time", "Self-employed, Part Time,Employed by an employer, Part Time,Employed by an employer, Full Time", "Self-employed, Part Time,Employed by an employer, Part Time,Freelance / Gig Work (For example, Uber, Favor, Fiverr)",
                                                    "Self-employed, Part Time,Self-employed, Full Time,Employed by an employer, Part Time"), 1,0)


sure.df$Employed_by_an_employer_Full_Time <- ifelse(sure.df$Employement_Status %in% c("Employed by an employer Full Time", "Employed by an employer, Full Time", "Employed by an employer, Full Time,Freelance / Gig Work (For example, Uber, Favor, Fiverr)",
                                                    "Employed by an employer, Part Time,Employed by an employer, Full Time", "Employed by an employer, Part Time,Employed by an employer, Full Time,Freelance / Gig Work (For example, Uber, Favor, Fiverr)",
                                                    "Full-time", "Full Time", "Self-employed, Full Time,Employed by an employer, Full Time", "Self-employed, Full Time,Employed by an employer, Full Time,Freelance / Gig Work (For example, Uber, Favor, Fiverr)",
                                                    "Self-employed, Part Time,Employed by an employer, Full Time", "Self-employed, Part Time,Employed by an employer, Full Time,Freelance / Gig Work (For example, Uber, Favor, Fiverr)", "Self-employed, Part Time,Employed by an employer, Part Time,Employed by an employer, Full Time",
                                                    "Self-employed, Part Time,Self-employed, Full Time,Employed by an employer, Full Time"), 1,0)



sure.df$Freelance_Gig_Work <- ifelse(sure.df$Employement_Status %in% c("Employed by an employer, Full Time,Freelance / Gig Work (For example, Uber, Favor, Fiverr)", "Employed by an employer, Part Time,Employed by an employer, Full Time,Freelance / Gig Work (For example, Uber, Favor, Fiverr)", "Freelance / Gig Work (For example, Uber, Favor, Fiverr)",
                                    "Self-employed Full-Time, Freelance/Gig Work", "Self-employed, Full Time,Employed by an employer, Full Time,Freelance / Gig Work (For example, Uber, Favor, Fiverr)", "Self-employed, Full Time,Employed by an employer, Part Time,Freelance / Gig Work (For example, Uber, Favor, Fiverr)",
                                    "Self-employed, Full Time,Freelance / Gig Work (For example, Uber, Favor, Fiverr)", "Self-employed, Part Time,Employed by an employer, Full Time,Freelance / Gig Work (For example, Uber, Favor, Fiverr)", "Self-employed, Part Time,Employed by an employer, Part Time,Freelance / Gig Work (For example, Uber, Favor, Fiverr)",
                                    "Self-employed, Part Time,Freelance / Gig Work (For example, Uber, Favor, Fiverr)", "Self-employed, Part Time,Self-employed, Full Time,Freelance / Gig Work (For example, Uber, Favor, Fiverr)"), 1,0)
sure.df$Freelance_Gig_Work[sure.df$Employement_Status %in% c("Full-time", "Full Time", "Part-time", "Part Time")] = NA

# Dropping a column by name 
sure.df$Employement_Status <- NULL 

# Creating a new variable for Household Size
# Rule: Take into account the earliest number of household
sure.df$Household_Size = ifelse(!is.na(sure.df$Household_Size_2012), sure.df$Household_Size_2012, 
                         ifelse(!is.na(sure.df$Household_Size_2013), sure.df$Household_Size_2013, 
                         ifelse(!is.na(sure.df$Household_Size_2014), sure.df$Household_Size_2014, 
                         ifelse(!is.na(sure.df$Household_Size_2015), sure.df$Household_Size_2015, 
                         ifelse(!is.na(sure.df$Household_Size_2016), sure.df$Household_Size_2016, 
                         ifelse(!is.na(sure.df$Household_Size_2017), sure.df$Household_Size_2017, 
                         ifelse(!is.na(sure.df$Household_Size_2018), sure.df$Household_Size_2018, 
                         ifelse(!is.na(sure.df$Household_Size_2019), sure.df$Household_Size_2019, 
                         ifelse(!is.na(sure.df$Household_Size_2020), sure.df$Household_Size_2020, 
                         ifelse(!is.na(sure.df$Household_Size_2021), sure.df$Household_Size_2021, NA))))))))))

summary(factor(sure.df$Household_Size))

# Dropping columns by name 
sure.df[c("Household_Size_2012", "Household_Size_2013", "Household_Size_2014", "Household_Size_2015", "Household_Size_2016",
          "Household_Size_2017", "Household_Size_2018", "Household_Size_2019", "Household_Size_2020", "Household_Size_2021")] <- list(NULL)

# Creating a new variable for Household income
# Rule: Take into account the earliest household income
class(sure.df$Household_Income_2012)
sure.df$Household_Income = ifelse(!is.na(sure.df$Household_Income_2012), as.character(sure.df$Household_Income_2012), 
                                  ifelse(!is.na(sure.df$Household_Income_2013), as.character(sure.df$Household_Income_2013), 
                                  ifelse(!is.na(sure.df$Household_Income_2014), as.character(sure.df$Household_Income_2014), 
                                  ifelse(!is.na(sure.df$Household_Income_2015), as.character(sure.df$Household_Income_2015), 
                                  ifelse(!is.na(sure.df$Household_Income_2016), as.character(sure.df$Household_Income_2016), 
                                  ifelse(!is.na(sure.df$Household_Income_2017), as.character(sure.df$Household_Income_2017), 
                                  ifelse(!is.na(sure.df$Household_Income_2018), as.character(sure.df$Household_Income_2018), 
                                  ifelse(!is.na(sure.df$Household_Income_2019), as.character(sure.df$Household_Income_2019), 
                                  ifelse(!is.na(sure.df$Household_Income_2020), as.character(sure.df$Household_Income_2020), 
                                  ifelse(!is.na(sure.df$Household_Income_2021), as.character(sure.df$Household_Income_2021), NA))))))))))

summary(factor(sure.df$Household_Income))

# Dropping columns by name
sure.df[c("Household_Income_2012", "Household_Income_2013", "Household_Income_2014", "Household_Income_2015", "Household_Income_2016", 
          "Household_Income_2017", "Household_Income_2018", "Household_Income_2019", "Household_Income_2020", "Household_Income_2021")] <- list(NULL)


# Vectors of new categories from old categories of Filing Taxes Method variable 
levels(sure.df$Filing_Taxes_Method)

Do_not_Know_category = c("Don't Know", "Donâ???Tt Know", "Don???Ûªt Know")
Married_Filed_joint_return_category = c("Married-Field joint return", "Married-Filed joint return")


# Re-categorizing factors from Filing Taxes Method variable
levels(sure.df$Filing_Taxes_Method)[which(levels(sure.df$Filing_Taxes_Method) %in% Do_not_Know_category)] = "Don't Know"
levels(sure.df$Filing_Taxes_Method)[which(levels(sure.df$Filing_Taxes_Method) %in% Married_Filed_joint_return_category)] = "Married-Filed joint return"
levels(sure.df$Filing_Taxes_Method)
summary(sure.df$launched)
str(sure.df$launched)




###################################################################
# DATA VISUALIZATION
###################################################################
data.plot <- sure.df
str(data.plot)
# Convert logi variables to factors
data.plot$SURE_Graduate <- as.factor(data.plot$SURE_Graduate)
data.plot$Media_Mention <- as.factor(data.plot$Media_Mention)
str(data.plot)










summary(factor(sure.df$Year_Enrollment))
hist(sure.df$Year_Enrollment)





###################################################################
# Classification and Regression Trees 
###################################################################
library(e1071)
library(caret)
library(rpart)
library(rpart.plot)

# Loading the data
sureDF <- read.csv("sureDF.csv", stringsAsFactors = TRUE)
str(sureDF)
summary(sureDF)
# Convert logi variables to factors
sureDF$SURE_Graduate <- as.factor(sureDF$SURE_Graduate)
sureDF$Media_Mention <- as.factor(sureDF$Media_Mention)
# convert the dependent variable to a factor
sureDF$launched <- ifelse(sureDF$launched == "Yes", 1,0)
sureDF$launched <- as.factor(sureDF$launched)
summary(sureDF$launched)
View(sureDF)

# Dropping unnecessary variables
sureDF[c("Entrepreneurs.Info.EntrepreneurID", "Media_Mention", "Filing_Taxes_Method")] <- list(NULL)
# Taking into account only complete values

# for loop to convert index 1 to NA
for(i in 1:ncol(sureDF)){
  levels(sureDF[,i])[which(levels(sureDF[,i]) == "")] = NA
}
summary(sureDF)
# Eliminate variables with more than 25% missing
sureDF[,c("Seeking_New_Employment", "First_In_Family_In_College", "Ever_Food_Stamp_Or_SNAP", "age",
                                "Military_Service", "Ever_Third_Ward", "product_service", "Primary_Industry", "English_First_Language")] = NULL

# Partitioning the data into two sets
set.seed(1)
train.index <- sample(row.names(sureDF), 0.6*dim(sureDF)[1])  # Taking 60% of the data as the training data set
valid.index <- setdiff(row.names(sureDF), train.index)        # Taking 40% of the data as the test data set
train.df <- sureDF[train.index, ]
valid.df <- sureDF[valid.index, ]
str(valid.df)

#### R-code for creating a default classification tree ####
# classification tree
default.ct <- rpart(launched ~ ., data = train.df, method = "class")
# plot tree
prp(default.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -15, tweak = 1.5, space = 0, gap = 0, compress= TRUE, ycompress = TRUE,
    box.col=ifelse(default.ct$frame$var == "<leaf>", 'gray', 'white')) # I have 7 levels as default.
# count number of leaves for default.ct
length(default.ct$frame$var[default.ct$frame$var == "<leaf>"]) 

#### R-code for creating a deeper classification tree ####
deeper.ct <- rpart(launched ~ ., data = train.df, method = "class", cp = 0, minsplit = 1)    # cp is a penalty parameter.  
# count number of leaves for deeper.ct
length(deeper.ct$frame$var[deeper.ct$frame$var == "<leaf>"])  # There are 293 leaves 
# plot tree
prp(deeper.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10,                   # To make this tree smaller, we need to increase cp (for example cp=0.01, and you will see that the number of leaves will decrease). CP is a small number.
    box.col=ifelse(deeper.ct$frame$var == "<leaf>", 'gray', 'white'))  

# NOTE: 2 problems:  
# 1. How do we decide what is the optimal size of the tree? Large tree could lead to overfitting.
# 2. We care about the stability of the tree. We want a tree to be "stable". A tree that does not depend specifically on the "training data set". 

#### TABLE: CONFUSION MATRICES AND ACCURACY FOR DEFAULT (SMALL) AND DEEPER (FULL) 
# CLASSIFICATION TREES, ON THE TRAINING AND VALIDATION SETS OF THE SURE data set####

##R-code for classifying the validation data using a tree and computing 
# the confusion matrices and accuracy for the training and validation data ##

# classify records in the validation data.
# set argument type = "class" in predict() to generate predicted class membership.

# default tree: training 
default.ct.point.pred.train <- predict(default.ct,train.df,type = "class")
# generate confusion matrix for training data
confusionMatrix(default.ct.point.pred.train, as.factor(train.df$launched)) 
### repeat the code for the validation set, and the deeper tree

# default tree: validation
default.ct.point.pred.valid <- predict(default.ct,valid.df,type = "class")
confusionMatrix(default.ct.point.pred.valid, as.factor(valid.df$launched)) 

# deeper tree: training
deeper.ct.point.pred.train <- predict(deeper.ct,train.df,type = "class")
confusionMatrix(deeper.ct.point.pred.train, as.factor(train.df$launched)) 

# deeper tree: validation
deeper.ct.point.pred.valid <- predict(deeper.ct,valid.df,type = "class")
confusionMatrix(deeper.ct.point.pred.valid, as.factor(valid.df$launched)) 


#### R-code for tabulating tree error as a function of the Complexity Parameter (CP)####

# argument xval refers to the number of folds to use in rpart's built-in
# cross-validation procedure
# argument cp sets the smallest value for the complexity parameter.
cv.ct <- rpart(launched ~ ., data = train.df, method = "class",       # Note: cross-validation: re sampling many trees, run same trees on sample partition. what cp is best for every trees. 
               cp = 0, minsplit = 5, xval = 5)

# use printcp() to print the table. 
printcp(cv.ct) # How to choose the best one? Choose the one that have the lowest error. 
# look for the minimum error, and look for the cp and copy and paste in the formula above. 
# with cp you get the optimal size of the tree. you can add together xerror and xstd and choose the cp with the the small sum. 
# If cp is greater than 1 or 1, we get a dot in the tree. cp is a penalty rate to penalize the number of leaves. Cp correspond to the lowest x-error. 

# prune by lower cp
pruned.ct <- prune(cv.ct,
                   cp = cv.ct$cptable[which.min(cv.ct$cptable[,"xerror"]), "CP"])
prp(pruned.ct, type = 1, extra = 1, split.font = 2, varlen = -10,
    box.col=ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white'))
# count number of leaves for pruned.ct
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])

####Best-pruned tree####
pruned.ct <- prune(cv.ct, cp = 0.01116071)
prp(pruned.ct, type = 1, extra = 1, under = TRUE, split.font = 1, varlen = -10, 
    box.col=ifelse(pruned.ct$frame$var == "<leaf>", 'gray', 'white')) 
# count number of leaves 
length(pruned.ct$frame$var[pruned.ct$frame$var == "<leaf>"])

# confusion Matrix
cv.pred <- predict(cv.ct, valid.df)
confusionMatrix(as.factor(round(cv.pred[,2])), as.factor(valid.df$launched)) 

#### R-code for running a random forest, plotting variable importance plot, and computing accuracy#### 
library(randomForest)

## random forest

# Imputation of Missing Values with a Random Forest
index_y = which(colnames(train.df) == "launched")
train.df_imputed = rfImpute(x = train.df[,-index_y], y = train.df[,index_y], iter = 20)

colnames(train.df_imputed)[1] = "launched"

rf <- randomForest(as.factor(launched) ~ ., data = train.df_imputed, ntree = 500,  # This code is going to create randomly 500 different trees. 
                   mtry = 4, nodesize = 5, importance = TRUE)  

## variable importance plot
varImpPlot(rf, type = 1)

## confusion matrix
rf.pred <- predict(rf, valid.df)
confusionMatrix(rf.pred, valid.df$launched)


####  R-code for running boosted tree####
#install.packages("adabag")
library(adabag) # for boosting 
library(rpart) 
library(caret)

set.seed(1)
boost <- boosting(launched ~ ., data = train.df)
pred <- predict(boost, valid.df)
confusionMatrix(as.factor(pred$class), as.factor(valid.df$launched))

# Boosting will help you increase accuracy rate when "1" are very small in your data. 
# Do both of them boosting and random forest and choose the one with better accuracy rate. 









