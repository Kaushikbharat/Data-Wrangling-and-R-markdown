#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# RETAILING CASE STUDY

#$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$

# Just Setting the working directory as per our requirement
setwd("E:/Bharat/Case Studies/Retailing")

# Reading all the csv files 

Transaction <- read.csv("E:/Bharat/Case Studies/Retailing/Transactions.csv")

Prod_Cat_Info <- read.csv("E:/Bharat/Case Studies/Retailing/prod_cat_info.csv")

Customer <- read.csv("E:/Bharat/Case Studies/Retailing/Customer.csv")

#Qns1 Merge the datasets Customers, Product Heirarchy and Transaction as Customer Final

#1.1 Using the base merge() function

# Merging Customer and Transaction with the help of Customer Id as Cust_Trans

Cust_Trans <- merge(x=Customer,y=Transaction,by.x ="customer_Id",by.y = "cust_id",all=TRUE)

# Merging Cust_Trans with Product Heirarchy with the help of Prod_Subcat_Code

Customer_Final <- merge(x=Cust_Trans,y=Prod_Cat_Info,by.x ="prod_subcat_code",by.y =  "prod_sub_cat_code",all=TRUE)

#1.2 Using the dplyr function

# Loading Dplyr function
require(dplyr)

# Copying Customer in Customer_1 
Customer_1 <- Customer

# Copying Transaction in Transaction_1
Transaction_1 <- Transaction
Transaction_1$customer_Id<- Transaction_1$cust_id
Transaction_1$cust_id <- NULL
Transaction_1 <- Transaction_1[,c(1,10,2:9)]

# Merging the Transaction_1,Customer_1 in Trans_Cust using dplyr function Joining, by = customer_Id

Trans_Cust <- dplyr::full_join(Transaction_1,Customer_1)

# Copying Prod_Cat_Info in Prod_Cat_Info_1
Prod_Cat_Info_1 <- Prod_Cat_Info
Prod_Cat_Info_1$prod_subcat_code <- Prod_Cat_Info_1$prod_sub_cat_code
Prod_Cat_Info_1$prod_sub_cat_code <- NULL
Prod_Cat_Info_1 <- Prod_Cat_Info_1[,c(1,3,2,4)]

# Merging Trans_Cust,Prod_Cat_Info_1 using dplyr function, Joining by prod_sub_cat_code

Customer_Final_dplyr <- dplyr:: full_join(Trans_Cust,Prod_Cat_Info_1,by="prod_subcat_code")


# Qns 2 Prepare a Summary Report for Merged Data Set. Generate histograms for all continuous variables,
# and frequency variables for categorical variables

# Preparing Summary Report for Merged Data  Set
dim(Customer_Final)
summary(Customer_Final)

# Preparing Histograms for Continuous Variables

str(Customer_Final)

hist(Customer_Final$prod_subcat_code)

hist(Customer_Final$customer_Id)

hist(Customer_Final$city_code)

hist(Customer_Final$transaction_id)

hist(Customer_Final$prod_cat_code.x)

hist(Customer_Final$prod_cat_code.y)

hist(Customer_Final$Qty)

hist(Customer_Final$Rate)

hist(Customer_Final$Tax)

hist(Customer_Final$total_amt)


# Preparing Frequency Bars for Categorical Data

Cat1 <- table(Customer_Final$Gender)
barplot(Cat1)

Cat2 <- table(Customer_Final$tran_date)
barplot(Cat2)

Cat3 <- table(Customer_Final$DOB)
barplot(Cat3)

Cat4 <- table(Customer_Final$Store_type)
barplot(Cat4)

Cat5 <- table(Customer_Final$prod_cat)
barplot(Cat5)

Cat6 <- table(Customer_Final$prod_subcat)
barplot(Cat6)

# Qns 3 Calculate the information using merged Data Set

#3.1 Calculate Time Period of Available Data

T1 <- max(as.Date(Customer_Final$tran_date,format="%d-%m-%Y"),na.rm = T)
T2 <- min(as.Date(Customer_Final$tran_date,format="%d-%m-%Y"),na.rm = T)

Time_Period <- T1-T2
Time_Period


#3.2 Count Total Numbers of Transactions that are negative
length(Customer_Final$total_amt[Customer_Final$total_amt<0])



# Qns 4 What is the count and percentage count of male and female customer, Display the information using suitable barchart

# Counting Total Number of values in Gender
Total_Gender <- length(Customer_Final$Gender)
Total_Gender

# Counting Number of Males
Males <- length(Customer_Final$Gender[Customer_Final$Gender=="M"])
Males

# Counting Number of Females
Females <- length(Customer_Final$Gender[Customer_Final$Gender=="F"])
Females

# Calculating Count%age of Males
Count_Male <- (Males/Total_Gender)*100
Count_Male

# Calculating Count%age of Females
Count_Female <- (Females/Total_Gender)*100
Count_Female

# Plotting the comparison of Males and Females using barchart
M_F <- c(Number_of_Males=Males,Number_of_Females=Females)
barplot(M_F)

# Plotting the comparison of Males and Females count %age using barchart
M_F_percent <- c(Male_Count_Percent=Count_Male,Female_Count_Percent=Count_Female)
barplot(M_F_percent)

# Qns 5 From the Column City Create the below visualization using suitable dplyr or data.table function
# Create interactive graphs where necessary

#5.1 Which city has maximum number of customers and what is the percentage of the customers from that city
require(ggplot2)

# Grouping by the city to find the number of customer wh spend their in that city
Max_city <- Customer %>% dplyr::group_by(city_code) %>% dplyr::summarise(No_of_Customers=length(city_code))

# Calculating percentage and round it off upto two decimal places
Max_city$Percentage <- round((Max_city$No_of_Customers/sum(Max_city$No_of_Customers))*100,2)
max(Max_city,na.rm = T)

#5.2 Display city categories and customer count on a bar graph

# Preparing a bar graph with respect to city code and Number of Customers
Max_City_Graph <- ggplot2::ggplot(data = Max_city)+aes(x=city_code,y=No_of_Customers)+geom_bar(stat = "identity",color="black",fill="cyan")
Max_City_Graph



# Qns 6 Which store type sells the maximum products by value and quantity

# Store Type Which sells the maximum product by value
By_Value <- Customer_Final %>% dplyr::group_by(Store_type) %>% dplyr::summarise(Sum=sum(total_amt,na.rm = T))
By_Value[which.max(By_Value$Sum),]

# Store Type Which sells the maximum product by quantity
By_Qty <- Customer_Final %>% dplyr::group_by(Store_type) %>% dplyr::summarise(Qty=length(Qty))
By_Qty[which.max(By_Qty$Qty),]


# Qns 7 What is the Total Revenue earned from the Electronics and Clothing Categories from the Flag Ship Stores

# Extracting the Store type and product category with respect to revenue
Revenue <- Customer_Final %>% dplyr::group_by(Store_type,prod_cat) %>% dplyr::summarise(Revenue=sum(total_amt,na.rm = T))

# Extracting only thse which have rows according to Store type as Flagship Store
Flagship <- Revenue[Revenue$Store_type=="Flagship store",]

# Extracting Only Clothing and Electronics
Flagship[Flagship$prod_cat=="Electronics",]
Flagship[Flagship$prod_cat=="Clothing",]


# Qns 8 How many customers have more than 5 transactions after excluding returns

Excluding_Returns <- Customer_Final %>% dplyr::group_by(customer_Id) %>% dplyr::summarise(Return=length(which(total_amt>0)))
More_Than_5_Transactions <- Excluding_Returns[Excluding_Returns$Return>="5",]


# Qns 9 What is the total revenue generate by the Male Customers under the Electronic Category

# Extracting the prod category with respect to revenue and that too gender wise
Revenue_Male <- Customer_Final %>% dplyr::group_by(Gender,prod_cat) %>% dplyr::summarise(Revenue=sum(total_amt,na.rm = T))

# Extracting only Male Categories where prod category is Electronics row wise
Revenue_Male <- Revenue_Male[Revenue_Male$Gender=="M"&Revenue_Male$prod_cat=="Electronics",]


#Qns 10 For all customers aged between 18-25, find out:

# Converting the DOB in date format
Customer_Final$DOB <-  as.Date(Customer_Final$DOB ,format="%d-%m-%Y")

# Converting the transaction date in date format
Customer_Final$tran_date <- as.Date(Customer_Final$tran_date ,format="%d-%m-%Y")

# Subtracting Transaction date from DOB to calculate the number ofdays
Customer_Final$Age <- Customer_Final$tran_date-Customer_Final$DOB

# Converting days into years to calculate age
Customer_Final$Age <- Customer_Final$Age/365.25

# Rounding off the calcukate age to Zero decimal Point
Customer_Final$Age <- round(Customer_Final$Age,0)

# Imputing all Na's to 0
Customer_Final$Age[is.na(Customer_Final$Age)] <- 0

# 10.1 What is the Total Revenue generated in Electronics and Books ?

# Binning the age to category wise
Customer_Final$AgeCategory <- ifelse(Customer_Final$Age>20&Customer_Final$Age<25,"Young","Middle")

# Summarising the Data with respect to Young Category
Tot_Rev <- Customer_Final %>% dplyr::group_by(AgeCategory="Young",prod_cat) %>% dplyr::summarise(Revenue=sum(total_amt))

# Extracting the Revenue where the product category is Electronics all rows
Tot_Rev[Tot_Rev$prod_cat=="Electronics",] 

# Extracting the Revenue where the product category is Books all rows
Tot_Rev[Tot_Rev$prod_cat=="Books",] 


# 10.2 What was the Total Revenue generated by these customers between 1st Jan, 2014 to 1st Mar,2014

# Summarising the transaction date with respect to total revenue
Revenue_by_Date <- Customer_Final %>% dplyr::group_by(tran_date) %>% summarise(Revenue=sum(total_amt,na.rm = T))

# Extracting the Revenue row wise within the dates between 1st Jan, 2014 to 1st Mar,2014
Date <- Revenue_by_Date[Revenue_by_Date$tran_date>="2014-01-01"&Revenue_by_Date$tran_date<="2014-03-01",]

# Extracting the Revenue
Total_Revenue <- sum(Revenue_by_Date$Revenue,na.rm = T)
Total_Revenue