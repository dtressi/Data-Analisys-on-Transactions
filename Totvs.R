#Data Sample from Totvs.

#The objective of this document is to analyse the correlations of data
#and make descriptions and predictions using machine learning models.

#Necessary Packages. If you need to install, uncoment the one you need. 
#install.packages('jsonlite')
#install.packages('dplyr')
#install.packages('leaps')
#install.packages('ggplot2')

#Package to work with R Tables (Merge columns as rows). 
library(plyr)
#Package to work with Json Files.
library(jsonlite)
#Plot Package.
library(ggplot2)
#Suggest the set of variables to the model.
library(leaps)

#Load Json File Into R.
json_data_flat <- jsonlite::fromJSON("https://raw.githubusercontent.com/dtressi/data-analysis-on-transactions/master/sample.txt", flatten=TRUE);

#View Json file as Row Structure with lists.
View(json_data_flat);

#Get the total number of records of the data.
rows = nrow(json_data_flat);

#Parse the first register of Json file as Table.
baseTable <- cbind(as.data.frame(json_data_flat$dets[1]),as.data.frame(json_data_flat[1,2:30]));

#Parse the Json File structure to a full table in order
#to apply models and study the data.
for (i in 2:rows)
{
  tempBase <- cbind(as.data.frame(json_data_flat$dets[i]),as.data.frame(json_data_flat[i,2:30]));
  baseTable <- rbind.fill(baseTable,tempBase);
}

#Transfrom the new data table "baseTable" as data frame in order to R manipulate the data.
fiscalTable <- as.data.frame(baseTable);

#Since the Data looks like a Cooffe Shop Or Restaurant, we add a new column with 
#the days of the week to analyse the correlation between sold products and the day
#of the week.
fiscalTable$day <- weekdays(as.Date(fiscalTable$ide.dhEmi));

#In order to apply any models, we add a new column with the day 
#of the Week (0 to 6, start from Saturday).
fiscalTable$num.day <- as.POSIXlt(fiscalTable$ide.dhEmi)$wday;

#This two final modifications transform categorial datas into number so
#we can apply any math funcions or models.
fiscalTable$num.day.fac <- factor(fiscalTable$num.day);
fiscalTable$prod.xProd.fac <- factor(fiscalTable$prod.xProd);

#View the final transfomed table.
View(fiscalTable);

#This plot shows on what days of the week are the products been sold. This will provide an ideia
#between the correlation of weekdays and the products consumed.
ggplot(fiscalTable, aes(x = fiscalTable$day, y = fiscalTable$prod.xProd )) + geom_point() + labs(x = "WeekDay", y =  "Product") + ggtitle("Product By Day of the week");
ggplot(fiscalTable, aes(x = fiscalTable$day, y = fiscalTable$prod.vProd, color = prod.xProd )) + labs(x = "WeekDay", y =  "Revenue Value") + geom_point() + ggtitle("Revenue By Day of the week");

#To analise the frequency of the consumed products, we create clustes with the days of the week,
#by doing this, we can get the frequency and the mean days of the wek in wich all products are consumed.

#Data To be Clustered.
dataCluster <- cbind(fiscalTable$num.day,fiscalTable$prod.vProd);
colnames(dataCluster) <- c("WeekDay","Product Value");

#We cluster the data using Kmeans. We use 6 cluster based on the number of 
#the days of the week we have (6).
productCluster <- kmeans(dataCluster, 6 , nstart=20);

#The table will show the frequency of each product given an idea accoding the cluestr means.
table(productCluster$cluster, fiscalTable$prod.xProd.fac);

#We apply the leaps function to following liner model to get the siginifcance of each variable.
#Dependent Variable: prod.vUnCom (Total Revenue for product on each fiscal note on tha day).
#Indepent Variables:  prod.xProd.fac (Product: Buffet, Cha ,...) , num.day.fac (0,1,...,6). 
leaps <- regsubsets(prod.vProd ~ prod.xProd.fac + num.day.fac, data = fiscalTable);
plot(leaps,scale="adjr2");

#The leaps function shows that most some products category should be descarded but we will mantain
#all since this test is made variable by variable and doesnt take into account all of them at the same time.

#Apply a liner regression using as predicted variable the total revenue value
#if each product, for each day of the week.
#Dependent Variable: prod.vUnCom (Total Revenue for product on each fiscal note on tha day).
#Indepent Variables:  prod.xProd.fac (Product: Buffet, Cha ,...) , num.day.fac (0,1,...,6). 
myModel <- lm(formula = prod.vProd ~ prod.xProd.fac + num.day.fac, data = fiscalTable);

#The summary model shows the sensibilities of  Total Product revenue according to each variable.
summary(myModel);

#Example of prediction:The mean revenue of Product "SUCO" on WeekDay "2".
input_data <- data.frame( prod.xProd.fac="SUCO", num.day.fac="2");

predict(myModel,input_data,type="response");

#Prediction Value = 7.019918.
