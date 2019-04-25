#install.packages("EBImage")
library(EBImage)
ImageId <- readImage('digits/zero.jpg')
plot(Image)
Image<- resize(ImageId,28,28)
plot(Image)
save(Image,'zero1.jpg')
nof=numberOfFrames(Image, type = c('total', 'render'))
if(nof==1)
{
  image=255*imageData(Image[1:28,1:28])
}else 
  if(nof==3)
  {
    r=255*imageData(Image[1:28,1:28,1])
    g=255*imageData(Image[1:28,1:28,2])
    b=255*imageData(Image[1:28,1:28,3])
    image=0.21*r+0.72*g+0.07*b
    #image=(r+g+b)/3
    #image=0.2989*r+0.5870*g+0.1140*b
    #chk 0.2989, 0.5870, 0.1140. chk Red * 0.3 + Green * 0.59 + Blue * 0.11
    image=round(image)
  }

image=t(image)

write.csv(image,'chkimg.csv',row.names = FALSE)
image=as.vector(t(image))
write.csv(t(as.matrix(image)),'px.csv',row.names = FALSE)
testFileName  <-'px.csv'
newTestDataset <- read.csv(testFileName)    # Read the datafile
head(newTestDataset)

testreduced <- newTestDataset/255 
#Xcov <- cov(testreduced)
#pcaX <- prcomp(Xcov)
testreduced <- as.matrix(testreduced) %*% pcaX$rotation[,1:45]

#library(nnet)
Label <- predict(model,testreduced,type="class")
Label <- as.data.frame(NewPredicted);
head(Label)

write.csv(data.frame(newTestDataset,NewPredicted), file=paste(modelName,"-Testing-Result.csv",sep=''), row.names=FALSE)





