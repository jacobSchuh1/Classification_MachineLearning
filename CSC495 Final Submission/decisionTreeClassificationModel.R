setwd("C:/Users/jpsch/OneDrive/Desktop/CSC495")
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

dt_model <- rpart(formula = TaggedPitchType ~.,
                    data = bb_train,
                    method = "class",
                    control = rpart.control(cp = 0),
                    parms = list(split = "information"))

dt_pred <- predict(object = dt_model,
                     newdata = bb_test,
                     type = "class")

# The Confusion Matrix
confusionMatrix(data = dt_pred,
                reference = bb_test$TaggedPitchType)

