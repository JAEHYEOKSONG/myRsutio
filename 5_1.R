
ucla = read.csv("https://stats.idre.ucla.edu/stat/data/binary.csv")
str(ucla)
ucla$rank =factor(ucla$rank)

install.packages("randomForest")
library(randomForest)
install.packages("rpart")
library(rpart)
install.packages("class")
library(class)
control = trainControl(method = 'cv', number = 10)
r = train(rank~., data = ucla, method = 'rpart', metric = 'Accuracy', trControl = control)
f50 = train(rank~., data = ucla, method = 'rf',ntree = 50, metric = 'Accuracy', trControl = control)
f1000 = train(rank~., data = ucla, method = 'rf',ntree = 1000, metric = 'Accuracy', trControl = control)
s_Radial = train(rank~., data = ucla, method = 'svmRadial', metric = 'Accuracy', trControl = control)
s_poly = train(rank~., data = ucla, method = 'svmPoly', metric = 'Accuracy', trControl = control)
k = train(rank~., data = ucla, method = 'knn', metric = 'Accuracy', trControl = control)

sort(resamp, decreasing = TRUE) # 가장 높은 정확도 예측 
summary(resamp)

resamp = resamples(list(결정트리 = r, 랜덤포레50= f50, 랜덤포레1000  = f1000, SVM_R = s_Radial, SVM_P = s_poly ,
                            KNN = k))


n = nrow(ucla) # 학습데이터 6대 테스트 데이터4로예측
i = 1:n
train_list = sample(i, n*0.6)
test_list = sample(i, train_list)
ucla_train = ucla[train_list, ]
ucla_test = ucla[test_list, ]


f = randomForest(rank~., data = ucla_train)
p = predict(f , newdata = ucla_test)
p

ucla_test$rank
sort(resamp, decreasing = TRUE)


summary(resamp)














r = rpart(rank~., data = ucla)
par(mfcol = c(1,1), xpd=NA)
text(r, use.n = TRUE)

newdata = predict(r, ucla, type = 'class')
table(newdata, ucla$rank)

#table 함수를 쓰면 혼동 행렬이 나오고 거기서 나온 값을 다더한후 (1,1)(2,2)값을 더한후 나누면 정확도가 나온다.

# EX 정확도 (249 + 54)/400 = 80.5%




# 예측
newd = data.frame(Sepal.Length =c(5.11, 7.01, 6.32, 5.02, 6.09, 9.01),
                  Sepal.Width = c(3.51, 3.21, 3.31, 2.23, 4.54, 6.78),
                  Petal.Length = c(1.41, 4.71, 6.02, 5.11, 7.23, 7.9),
                  Petal.Width = c(0.19, 1.4, 2.49, 1.1, 1.56, 6.19)
                  )

print(newd)
predict(r, newdata = newd)


f_pred = predict(f, iris)
confusionMatrix(f_pred, iris$Species)

#_------------------------------------------

#랜덤 포레스트 -------------------


install.packages("rpart")
install.packages("randomForest")
library(randomForest)
library(rpart)

f = randomForest(admit~., data = ucla)
par(mfcol = c(1,1), xpd=NA)
plot(f)
printcp(r)

p = predict(f, ucla, type = 'class')
table(p, ucla$admit)

# 정확도 (265 + 59)/400 = 81%

# 예측
newd = data.frame(Sepal.Length =c(5.11, 7.01, 6.32, 5.02, 6.09, 6.01),
                  Sepal.Width = c(3.51, 3.21, 3.31, 1.23, 3.54, 6.78),
                  Petal.Length = c(1.41, 1.71, 6.02, 4.11, 7.23, 7.9),
                  Petal.Width = c(0.19, 1.4, 3.49, 1.1, 1.56, 6.19)
)


predict(f, data = newd, type = 'prob')
forest_1 = randomForest(Species~., data=iris, ntree = 50)
treesize(forest_1)



forest_2 = randomForest(Species~., data=iris, ntree = 1000)
treesize(forest_2)

#--------------------------------------------


