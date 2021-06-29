# 로지스틱 회귀모형은 반응변수 Y가 0 또는 1로 구분되는 이분형 자료에 적용되는 모형
# 다음과 같은 선형모형을 가정한다.

# log (p /(1-p))= beta_0 + beta_1*X   : logit변환

data(iris)

d<- iris[iris$Species == "virginica" | iris$Species =="versicolor",]  # 종이 Virginica와 versicolor 선택
d$Species <- factor(d$Species)
str(d)  # 이범주형 자료로 변환

# 만약 회귀모형으로 세운다면
# d$Species가 factor이므로 

d$Species2 = as.numeric(d$Species)-1 #0과 1로 구성된 자료 생성
lm.out <- lm(Species2~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=d)
summary(lm.out)

summary(lm.out$fitted.values) # 예측값을 살펴보면 -0.3, 1.49와 같이 0과 1의 범위를 넘어가는 예측값 존재

# 로지스틱 회귀모형의 예측값은 0과 1 범위 내에서만 존재

out <- glm(Species2~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=d, family=binomial(link="logit"))
summary(out)

out$fitted.values # 예측 확률

pred<-(ifelse(out$fitted.values>0.5,1,0)) #예측값이 0.5이상이면 1로 예측하고 0.5 미만이면 0으로 예측

xtabs(~pred+d$Species2) # 실제값과 예측값에 분류표


# 좋은 모형을 만들려면 먼저 어떤 모형이 좋은 것인지 결정해야한다.
# 이를 평가하기 위해 평가메트릭, ROC 커브, 교차검증 등이 제시됨

# 평가 메트릭
# 분류가 Y, N 두 종류가 있다고 할 때 실제값과 예측값의 빈도를 그리면 
# 혼돈행렬(confusion matrix)로 그려진다. 

#                        실제값
#              Y                           N
# 예측값   Y   True Positive(TP)           False Positive(FP)
#          N   False Nagative(FN)          True Nagative (TN)

# Precision = TP / (TP + FP) : Y로 예측된 것 중에 Y의 비율
# Accuracy = TP+TN / (TP+FP+FN+TN) : 전체 예측 중 옳은 예측의 비율
# Recall   = TP / (TP+FN) : 실제로 Y 인 것들 중 예측이 Y인 비율
# Specificity = TN / (FP + TN) : 실제로 N인 것들 중 예측이 N인 비율
# FP rate   = FP / (FP + TN) : Y가 아닌데 Y로 예측된 비율
# F measure = 2 * Precision*Recall/(Precision+Recall) : Precision과 Recall의 조화 평균

predicted <- c(1,0,0,1,1,1,0,0,0,1,1,1)
actual <- c(1,0,0,1,1,0,1,1,0,1,1,1)

xtabs(~predicted+actual)

# Q1. Accuracy는?

if(!require(caret)) install.packages("caret", repos = "http://cran.us.r-project.org"); library(caret)
# caret 패키지를 이용하면 평가 메트릭을 쉽게 계산할 수 있다.

confusionMatrix(as.factor(predicted), as.factor(actual))

#iris자료에 대한 평가메트릭
confusionMatrix(as.factor(pred), as.factor(d$Species2))

# ROC 커브를 통해 모형분석 결과를 정량화 시킬 수 있다. 

if(!require(ROCR)) install.packages("ROCR", repos = "http://cran.us.r-project.org"); library(ROCR)

pr <- prediction(pred, d$Species2)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)

auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc

# AUC를 판단하는 대략적인 기준
# excellent =  0.9~1
# good = 0.8~0.9
# fair = 0.7~0.8
# poor = 0.6~0.7
# fail = 0.5~0.6

# 주어진 데이터 전체를 사용해 모형을 세울 경우, 해당 데이터에는 잘 동작하지만
# 새로운 데이터에는 좋지 않은 성능을 보이는 모형을 만들수 있다.(과적합 문제)
# 과적합 발생 여부를 알아내려면 주어진 데이터 중 일부는 모델을 만드는 훈련데이터로 사용하고,
# 나머지는 테스트 데이터로 사용해 모형을 평가해야한다. 

# 교차검증은 훈련 데이터와 테스트 데이터를 분리하여 모형을 만드는데 많이 사용되는 방법

if(!require(cvTools)) install.packages("cvTools", repos = "http://cran.us.r-project.org"); library(cvTools)
set.seed(1215124)
cvFolds(10, K=5, type="random")

# 5겹 교차검증에서 K=1일때 9, 4번째를 검증 데이터로 사용하고 나머지는 훈련 데이터로 사용
# K=2일때 5, 7번째를 검증 데이터로 사용하고 나머지는 훈련데이터로 사용

cvFolds(10, K=5, type="consecutive") # 연속된 자료를 검증자료로 사용할 때
cvFolds(10, K=5, type="interleaved") # 순서대로

#iris 자료에 대한 교차검증을 수행한다면....;
set.seed(15142)
cv <- cvFolds(nrow(d), K=5, R=3)

#첫번째 반복 K=1
sel<-cv$subset[which(cv$which==1),1]

train <- d[-sel,]
test <- d[sel,]

out <- glm(Species2~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=train, family="binomial")
pred <- predict(out, newdata=test, type="response")
pred<-(ifelse(pred>0.5,1,0)) #예측값이 0.5이상이면 1로 예측하고 0.5 미만이면 0으로 예측

pr <- prediction(pred, test$Species2)
auc <- performance(pr, measure = "auc")@y.values[[1]]
auc 

#첫번째 반복 K=2
sel<-cv$subset[which(cv$which==2),1]

train <- d[-sel,]
test <- d[sel,]

out <- glm(Species2~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=train, family="binomial")
pred <- predict(out, newdata=test, type="response")
pred<-(ifelse(pred>0.5,1,0)) #예측값이 0.5이상이면 1로 예측하고 0.5 미만이면 0으로 예측

pr <- prediction(pred, test$Species2)
auc <- performance(pr, measure = "auc")@y.values[[1]]
auc 

#첫번째 반복 K=3
sel<-cv$subset[which(cv$which==3),1]

train <- d[-sel,]
test <- d[sel,]

out <- glm(Species2~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=train, family="binomial")
pred <- predict(out, newdata=test, type="response")
pred<-(ifelse(pred>0.5,1,0)) #예측값이 0.5이상이면 1로 예측하고 0.5 미만이면 0으로 예측

pr <- prediction(pred, test$Species2)
auc <- performance(pr, measure = "auc")@y.values[[1]]
auc 

# 병렬처리를 통해 위의 결과를 자동으로 계산되도록 프로그래밍
if(!require(foreach)) install.packages("foreach", repos = "http://cran.us.r-project.org"); library(foreach)

set.seed(325312)
R=3
K=5
cv <- cvFolds(nrow(d), K=K, R=R)
foreach(r=1:R) %do% {
  foreach(k=1:K, .combine=c) %do% {
    sel<-cv$subset[which(cv$which==k),r]
    
    train <- d[-sel,]
    test <- d[sel,]
    
    out <- glm(Species2~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=train, family="binomial")
    pred <- predict(out, newdata=test, type="response")
    
    pred1<-(ifelse(out$fitted.values>0.5,1,0)) # train 자료
    pred2<-(ifelse(pred>0.5,1,0))              # test 자료
    
    pr1 <- prediction(pred1, train$Species2)
    pr2 <- prediction(pred2, test$Species2)
    
    auc1 <- performance(pr1, measure = "auc")@y.values[[1]]  # train 자료
    auc2 <- performance(pr2, measure = "auc")@y.values[[1]]  # test 자료
    
    return(c(auc1, auc2))
  }
}

####################################################################################
# Another example
library(foreign)
mydata <- read.dta("http://dss.princeton.edu/training/Panel101.dta")

head(mydata)

logit <- glm(y_bin~ x1+x2+x3, family=binomial(link="logit"), data=mydata)
summary(logit)

if(!require(stargazer)) install.packages("stargazer", repos = "http://cran.us.r-project.org");library(stargazer)
stargazer(logit, type="text")
# stargazer()함수를 이용하면 로지스틱 모형의 결과를 보기 좋게 만들수 있음

# logistic 모형의 경우 오즈비(odds ratio)가 중요

# 컴퓨터를 통한 오즈비 계산
cbind(Estimate=round(coef(logit),4),  OR=round(exp(coef(logit)),4))

# 팩키지를 이용한 오즈비 계산
if(!require(mfx)) install.packages("mfx", repos = "http://cran.us.r-project.org");library(mfx)
logitor(y_bin~ x1+x2+x3, data=mydata)

logit.or = exp(coef(logit))
stargazer(logit, coef=list(logit.or), p.auto=FALSE, type="text")

allmean <- data.frame(x1=mean(mydata$x1),
                      x2=mean(mydata$x2),
                      x3=mean(mydata$x3))
# 전체 설명변수의 평균 값을 생성
allmean

allmean$pred.prob <- predict(logit, newdata=allmean, type="response")
# 전체 설명변수의 평균값으로 로지스틱회귀모형을 실행했을 때 확률값 계산
# 즉, 전체 설명변수가 평균값일 때, y가 1이될 확률은 약 83% 임
allmean

logit <- glm(y_bin ~ x1+x2+x3+opinion, family=binomial(link="logit"), data=mydata)
# 범주형 자료가 들어간 로지스틱 회귀모형을 세워보자. 

allmean <- data.frame(x1=rep(mean(mydata$x1),4),
                      x2=rep(mean(mydata$x2),4),
                      x3=rep(mean(mydata$x3),4),
                      opinion=as.factor(c("Str agree","Agree","Disag","Str disag")))
# 각 설명변수의 평균값과 의견에 따른 y가 1이될 확률을 계산해보자. 

allmean <- cbind(allmean,predict(logit, newdata=allmean, type="response", se.fit=TRUE))

allmean
# 설명변수가 평균값일 때, 의견에 따라 y가 1이될 확률을 알 수 있다. 
# 예측 확률과 그에 대한 표준오차가 있으므로 95% 예측확률의 신뢰구간도 구할 수 있다. 
# Renaming "fit" and "se.fit" columns
names(allmean)[names(allmean)=="fit"] = "prob"
names(allmean)[names(allmean)=="se.fit"] = "se.prob"

# Estimating confidence intervals
allmean$ll = allmean$prob - 1.96*allmean$se.prob
allmean$ul = allmean$prob + 1.96*allmean$se.prob
allmean

# errorplot을 통한 의견에 따른 결과 시각화
if(!require(Hmisc)) install.packages("Hmisc", repos = "http://cran.us.r-project.org");library(Hmisc)
errbar(allmean$opinion, allmean$prob, allmean$ul, allmean$ll)

# 평가 메트릭을 통한 결과확인
pred.opi<-ifelse(logit$fitted.values>0.5,1,0)
xtabs(~pred.opi+mydata$y_bin)
confusionMatrix(as.factor(pred.opi), as.factor(mydata$y_bin)) 


# 다향 로지스틱 회귀모형은 반응변수 Y가 두개가 아니라 여러 개가 될 수 있는 경우
# nnet 팩키지의 multinom()로 쉽게 모델링 할 수 있다. 

if (!require(nnet)) install.packages("nnet", repos = "http://cran.us.r-project.org"); library(nnet)
m <- multinom(Species~Sepal.Length+Sepal.Width+Petal.Length+Petal.Width, data=iris)
m$fitted.values

# 1번째 50번째 100번째 자료의 모형의 통한 예측결과는?
predict(m, newdata=iris[c(1,50,100),], type='class')

# 만약 각 분류에 속할 확률을 예측하고자 한다면?
predict(m, newdata=iris[c(1,50,100),], type='probs')

# 원자료의 다항회귀모형을 통한 결과
predicted <- predict(m, newdata=iris)

xtabs(~predicted+iris$Species) #분할표를 이용한 결과

confusionMatrix(as.factor(predicted), as.factor(iris$Species)) #혼돈 행렬을 이용한 결과