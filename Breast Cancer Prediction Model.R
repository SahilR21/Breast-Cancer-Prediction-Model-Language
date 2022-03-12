#PREDICTIVE ANALYSIS
##steps involve in predictive analysis - 
### define problem, data collection, data cleaning, data anslysis, build predictive model, validate model and deployment

#problem - Implement predictive analysis usig R to diagnose whether or not a person hasd breast cancer, based on past medical data.
#data from university of california, Irvine(UCI) data set of breast cancer cases which used to build apredictive model that classifies a tumour as either mailgnant or benign based on vertian feaure varibale

#load the data
cancerdata = read.csv("BreastCancer.csv")
head(cancerdata)
#display structure of data
str(cancerdata)

#label the dataset as not any understandable label
names(cancerdata) = c("id","clumpThickness","uniformityOfCellSize",
                     "uniformatyOfShape","marginalAhession","SingleEpithelialCellSize","bareNuclei","blandChromatin","normalNucleoli",
                     "mitoses","class")
str(cancerdata)

##here 11 predictable varible outcome varible is class - to classify whether cancer is maliganant or benign

###data preparation 
cancerdata$id = NULL # as this column not helpful for data prediction hence we removed it.

#converting data into numeric data - as some data actully in num but it's in char
cancerdata$bareNuclei = as.numeric(cancerdata$bareNuclei)
str(cancerdata)

#remove the na value
cancerdata = na.omit(cancerdata)
#other way - cancerdata = cancerdata[complete.cases(cancerdata),]
str(cancerdata)

# here class it our outcome ie. resultant varible - whether tumour is malignant or balign
#here we are tanform the clasees of 2 adn 4  into benigna dn malignant
cancerdata$class = factor(ifelse(cancerdata$class ==2,"benign","malignant"))
str(cancerdata)

### build the mdodel
#Data Slicing into training and test dataset - rem. train data always need to be greater than test data
#library(caTools)
#split = sample.split(cancerdata[,1:9], SplitRatio = 0.7)
#train_set = subset(cancerdata[,1:9], split = "TRUE")
#test_set = subset(cancerdata[,1:9], split = "FALSE")
##another way to split the data
trainingSet = cancerdata[1:477,1:9]
testSet = cancerdata[478:682,1:9]


#split the diagnoses into training and testing outcome sets - class on 10 th column
##library(caTools)
##split_outcome = sample.split(cancerdata[,10], SplitRatio = 0.7)
##train_outcome = subset(cancerdata[,10], split_outcome = "TRUE")
##test_outcome = subset(cancerdata[,10], split_outcome = "FALSE") 
#another way
trainOutcomes = cancerdata[1:477,10]
testOutcomes = cancerdata[478:682,10]

##CREATE THE MODEL
#apply KNN a algorithm to trainingSet and trainignOutcomes - k nearest neighbourer
library(class)
prediction = knn(train = trainingSet,cl = trainOutcomes,k = 21,test = testSet)
#display the predction
prediction #it show 1st is mailgnant , second is benign and many more


#now, let's test efficiency of model - Model Evaluation - comparing actual result to predictable results
table(testOutcomes,prediction)  
## it gives the predict 160 benign as benign and 45 mailgnant as malignant

#finding accuracy
actual_preds = data.frame(cbind(actuals = testOutcomes, predicted = prediction))
correlation_accuracy = cor(actual_preds)
head(correlation_accuracy,4)
## hence 100% accuracy model


















