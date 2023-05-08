setwd("C:/Users/jpsch/OneDrive/Desktop/CSC495")
install.packages("randomForest")

# Load the library
library(randomForest)

bb_df<- read.csv("total_data.csv")
bb_df[c("TaggedPitchType")]
bbdf <- bb_df[c("TaggedPitchType", 
                "RelSpeed", "VertRelAngle", "HorzRelAngle", "SpinRate", "SpinAxis", "Tilt",
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

rf_model <- randomForest(
  formula = TaggedPitchType ~ .,
  data = bb_train,
  ntree=750,
)

rf_model

plot(rf_model)

varImpPlot(rf_model) 

bb_test1 <- subset(bb_test, select = -c(TaggedPitchType))

pred <- as.data.frame(predict(rf_model, newdata=bb_test1))
pred
accuracy<-sum(pred$`predict(rf_model, newdata = bb_test1)`==bb_test$TaggedPitchType)/nrow(bb_test)
accuracy
error <- 1-accuracy
error


