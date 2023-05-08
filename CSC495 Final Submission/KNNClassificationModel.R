#################################################################
##############               KNN               ##################
#################################################################

setwd("C:/Users/jpsch/OneDrive/Desktop/CSC495")

library(class)
library(caret)

#data
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

knn_model = knn(xtrain, xtest, ytrain, k=3)

cm = confusionMatrix(ytest, knn_model)
print(cm)
