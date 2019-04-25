cat("\nSTART\n")
startTime = proc.time()[3]
startTime

library(nnet)

modelName <- "neuralNetwork"
InputDataFileName="digits/mnist_train.csv"

trainDataset <- read.csv(InputDataFileName)      
trainDataset <- trainDataset[sample(nrow(trainDataset)),]  

head(trainDataset)   
nrow(trainDataset)   
names(trainDataset)  

target  <- names(trainDataset)[1] 
target

inputs <- setdiff(names(trainDataset),target)
inputs
length(inputs)

head(trainDataset)   
nrow(trainDataset)    

X=trainDataset[,-1] 
Y=trainDataset[,1] 
Xreduced <- X/255

Xcov <- cov(Xreduced)
pcaX <- prcomp(Xcov)

Xfinal <- as.matrix(Xreduced) %*% pcaX$rotation[,1:45]
Y <- class.ind(Y)

testDataset <- read.csv("digits/mnist_test.csv")
head(testDataset)
nrow(testDataset)

test=testDataset[,-1]
testreduced <- test/255 
testreduced <- as.matrix(testreduced) %*% pcaX$rotation[,1:45]
#write.csv(testDataset,file="testDataset.csv",row.names=FALSE);

#model   <- nnet(formula, trainDataset, size=10, linout=TRUE, skip=TRUE, MaxNWts=10000, trace=FALSE, maxit=100,softmax=TRUE)
model <- nnet(Xfinal,Y,size=150,softmax=TRUE,maxit=130,MaxNWts =80000)
model

Predicted <- predict(model, testreduced,type="class")
Predicted <- as.data.frame(Predicted);
head(Predicted) 

Actual <- as.double(unlist(testDataset[target]))
head(Actual)


accuracy <- round(mean(Actual==Predicted) *100,2)
accuracy

totalTime = proc.time()[3] - startTime
totalTime

result <- data.frame(modelName,accuracy, totalTime)[1:1,]
result

write.csv(result, file=paste(modelName,"digits/Evaluation-Result.csv",sep=''), row.names=FALSE)

write.csv(data.frame(Actual,Predicted), file=paste(modelName,"-ActualPredicted-Result.csv",sep=''), row.names=FALSE)


#save.image(file=paste(modelName,"-Model.RData",sep=''))


