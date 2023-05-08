library("e1071")

library(dplyr)
library(rpart)
library(caret)

# Load the library


bb_df<- read.csv("total_data.csv")
bb_df[c("TaggedPitchType")]
bbdf <- bb_df[c("TaggedPitchType", 
                "RelSpeed", "VertRelAngle", "HorzRelAngle", "SpinRate", "SpinAxis",
                "RelHeight", "RelSide", "Extension", "VertBreak", "InducedVertBreak",
                "HorzBreak", "PlateLocHeight", "PlateLocSide", "ZoneSpeed", 
                "VertApprAngle", "HorzApprAngle", "ZoneTime")]

sum(!complete.cases(bbdf))

bbdf <- na.omit(bbdf)

sum(!complete.cases(bbdf))


bbdf <- as.data.frame(bbdf)

bbdf$TaggedPitchType <- as.factor(bbdf$TaggedPitchType)

set.seed(13579)
bbRand= bbdf[sample(1:nrow(bbdf)), ]

sample_size = floor(0.7*nrow(bbRand))
bb_train <- bbRand[1:sample_size, ]
bb_test <- bbRand[sample_size:nrow(bbRand), ]

xtrain = bb_train[,-1]
ytrain = bb_train[,1]
xtest = bb_test[,-1]
ytest = bb_test[, 1]

svm_model <- svm(
  formula = TaggedPitchType ~ RelSpeed + VertRelAngle + HorzRelAngle +
  SpinRate + SpinAxis + RelHeight + RelSide + Extension + VertBreak + InducedVertBreak + 
  HorzBreak + PlateLocHeight + PlateLocSide + ZoneSpeed + VertApprAngle + 
  HorzApprAngle + ZoneTime,
  data = bb_train, kernel = "radial", cost = 10
)
summary(svm_model)


pred <- predict(svm_model,newdata = xtest)
system.time(pred <- predict(svm_model,bb_test))

table(pred,ytest)

num = 0
for(i in 1:length(ytest)){
  if(pred[i]==ytest[i]){
    num = num+ 1
  }
}
accuracy = num/length(ytest)
accuracy

