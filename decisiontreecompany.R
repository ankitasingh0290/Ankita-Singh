library(party)
library(C50)
library(tree)
library(gmodels)
companydata<- read.csv("Company_Data.csv")
hist(companydata$Sales)
High = ifelse(CompanyData$Sales<10, "No", "Yes")
CD = data.frame(companydata, High)

CD = data.frame(companydata, High)
View(CD)
CD$ShelveLoc<-as.factor(CD$ShelveLoc)
CD$Urban<-as.factor(CD$Urban)
CD$US<- as.factor(CD$US)
CD$High<- as.factor(CD$High)
str(CD)
CD_train <- CD[1:200,]

CD_test <- CD[201:400,]
pfun_tree = ctree(High ~ CompPrice + Income + Advertising + Population + Price + ShelveLoc
                + Age + Education + Urban + US, data = CD_train)
summary(pfun_tree)
plot(pfun_tree)
pred_tree <- as.data.frame(predict(pfun_tree,newdata=CD_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(pfun_tree,newdata=CD_test)


mean(pred_test_df==CD$High)   #accuracy 68%
CrossTable(CD_test$High,pred_test_df)
confusionMatrix(CD_test$High,pred_test_df) ## 79%

#### tree function for data ####

cd_tree_org <- tree(High~.-Sales,data=CD)
summary(cd_tree_org)
plot(cd_tree_org)
text(cd_tree_org,pretty = 0)

#### tree function for train data ###

cd_tree <- tree(High~.-Sales,data=CD_train)
summary(cd_tree)
plot(cd_tree)
text(cd_tree,pretty = 0)

##### evaluating model test data ###
pred_tree <- as.data.frame(predict(cd_tree,newdata=CD_test))
pred_tree["final"] <- NULL
pred_test_df <- predict(cd_tree,newdata=CD_test)

pred_test_df
pred_tree$final <- colnames(pred_test_df)[apply(pred_test_df,1,which.max)]

pred_tree$final <- as.factor(pred_tree$final)
summary(pred_tree$final)
summary(CD_test$High)
mean(pred_tree$final==CD$High) #### 77%
CrossTable(CD_test$High,pred_tree$final)

confusionMatrix(CD_test$High,pred_tree$final) # accuracy 86%

