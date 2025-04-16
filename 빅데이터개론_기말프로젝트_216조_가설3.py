# -*- coding: utf-8 -*-
"""빅데이터개론_기말프로젝트_216조_가설3.ipynb

## 1.1 데이터, 패키지 불러오기
"""

install.packages("tidyverse")
install.packages("dplyr")
install.packages("data.table")
install.packages("caret")

library(caret)
library(tidyverse)
library(dplyr)
library(data.table)
library(repr) #출력그래프 크기 변경
options(repr.plot.width=10, repr.plot.height=10)

install.packages("pROC")
library(pROC)

#https://drive.google.com/file/d/1qQk8OUXl8hSH_8qccRgXNNp8yvdVr9Yt/view?usp=sharing 레드와인
#https://drive.google.com/file/d/1gRbacOsDY6k601lro72yjp2apydjoW3-/view?usp=sharing 화이트와인
system("gdown --id 1qQk8OUXl8hSH_8qccRgXNNp8yvdVr9Yt")
system("gdown --id 1gRbacOsDY6k601lro72yjp2apydjoW3-")
system("ls", TRUE)

#데이터 가져오기
red_wine <- fread("/content/winequality-red.csv", header=T, encoding = "UTF-8") %>% as_tibble()
show(red_wine)

white_wine <- fread("/content/winequality-white.csv", header=T, encoding = "UTF-8") %>% as_tibble()
show(white_wine)

red_wine <- red_wine %>% rename(
  fixed_acidity = `fixed acidity`,
  volatile_acidity = `volatile acidity`,
  citric_acid = `citric acid`,
  residual_sugar = `residual sugar`,
  free_sulfur_dioxide = `free sulfur dioxide`,
  total_sulfur_dioxide = `total sulfur dioxide`
  )

white_wine <- white_wine %>% rename(
  fixed_acidity = `fixed acidity`,
  volatile_acidity = `volatile acidity`,
  citric_acid = `citric acid`,
  residual_sugar = `residual sugar`,
  free_sulfur_dioxide = `free sulfur dioxide`,
  total_sulfur_dioxide = `total sulfur dioxide`
  )

"""0. 상관관계 분석"""

install.packages("corrplot")
library(corrplot)

#레드와인의 상관계수
corr_red <- cor(red_wine[, -12] %>% select_if(is.numeric))
corrplot(corr_red,
         method = "color",
         type = "upper",
         tl.pos = "td",
         addCoef.col = "black",
         number.cex = 0.7,
         diag = FALSE,
         tl.col = "black",
         tl.srt = 45)
mtext("red_wine", side = 1, line = 3, cex = 1.2)

#화이트와인 상관계수
corr_white <- cor(white_wine[, -12] %>% select_if(is.numeric))
corrplot(corr_white,
         method = "color",
         type = "upper",
         tl.pos = "td",
         addCoef.col = "black",
         number.cex = 0.7,
         diag = FALSE,
         tl.col = "black",
         tl.srt = 45)
mtext("white_wine", side = 1, line = 3, cex = 1.2)

"""1. 불필요한 변수 제거"""

str(red_wine)

str(white_wine)

# 레드 와인, 화이트 와인 분류 시 불필요한 변수 : 품질, 밀도, 유리 이산화황

red_wine <- dplyr::select(red_wine, -quality, -density, -free_sulfur_dioxide)
white_wine <- dplyr::select(white_wine, -quality, -density, -free_sulfur_dioxide)

show(red_wine)
show(white_wine)

"""2. 결측값 확인 및 처리"""

table(is.na(red_wine))
summary(red_wine)

# NA값 없음

table(is.na(white_wine))
summary(white_wine)

"""3. 이상값 확인 및 처리"""

options(repr.plot.width=15, repr.plot.height=10)

boxplot(red_wine)

# 1.1. red_wine 이상값 처리

#모든 이상값은 NA로 변환 후 한번에 제거
#pH
# IQR 계산
IQR_pH <- IQR(red_wine$pH)
Q1 <- quantile(red_wine$pH, 0.25)  # 1사분위수
Q3 <- quantile(red_wine$pH, 0.75)  # 3사분위수

# IQR 범위를 벗어나는 값 처리
red_wine$pH <- ifelse(
  red_wine$pH < (Q1 - 1.5 * IQR_pH) | red_wine$pH > (Q3 + 1.5 * IQR_pH),
  NA,  # IQR 범위를 벗어나면 NA로 대체
  red_wine$pH  # 그렇지 않으면 기존 값 유지
)

#알코올
# IQR 계산
IQR_alcohol <- IQR(red_wine$alcohol)
Q1 <- quantile(red_wine$alcohol, 0.25)  # 1사분위수
Q3 <- quantile(red_wine$alcohol, 0.75)  # 3사분위수

# IQR 범위를 벗어나는 값 처리
red_wine$alcohol <- ifelse(
  red_wine$alcohol < (Q1 - 1.5 * IQR_alcohol) | red_wine$alcohol > (Q3 + 1.5 * IQR_alcohol),
  NA,  # IQR 범위를 벗어나면 NA로 대체
  red_wine$alcohol  # 그렇지 않으면 기존 값 유지
)

#황산의 총량
# IQR 계산
IQR_total_sulfur_dioxide <- IQR(red_wine$total_sulfur_dioxide)
Q1 <- quantile(red_wine$total_sulfur_dioxide, 0.25)  # 1사분위수
Q3 <- quantile(red_wine$total_sulfur_dioxide, 0.75)  # 3사분위수

# IQR 범위를 벗어나는 값 처리
red_wine$total_sulfur_dioxide <- ifelse(
  red_wine$total_sulfur_dioxide < (Q1 - 1.5 * IQR_total_sulfur_dioxide) | red_wine$total_sulfur_dioxide > (Q3 + 1.5 * IQR_total_sulfur_dioxide),
  NA,  # IQR 범위를 벗어나면 NA로 대체
  red_wine$total_sulfur_dioxide  # 그렇지 않으면 기존 값 유지
)
#잔여 설탕
# IQR 계산
IQR_residual_sugar <- IQR(red_wine$residual_sugar)
Q1 <- quantile(red_wine$residual_sugar, 0.25)  # 1사분위수
Q3 <- quantile(red_wine$residual_sugar, 0.75)  # 3사분위수

# IQR 범위를 벗어나는 값 처리
red_wine$residual_sugar <- ifelse(
  red_wine$residual_sugar < (Q1 - 1.5 * IQR_residual_sugar) | red_wine$residual_sugar > (Q3 + 1.5 * IQR_residual_sugar),
  NA,  # IQR 범위를 벗어나면 NA로 대체
  red_wine$residual_sugar  # 그렇지 않으면 기존 값 유지
)
#염소
# IQR 계산
IQR_chlorides <- IQR(red_wine$chlorides)
Q1 <- quantile(red_wine$chlorides, 0.25)  # 1사분위수
Q3 <- quantile(red_wine$chlorides, 0.75)  # 3사분위수

# IQR 범위를 벗어나는 값 처리
red_wine$chlorides <- ifelse(
  red_wine$chlorides < (Q1 - 1.5 * IQR_chlorides) | red_wine$chlorides > (Q3 + 1.5 * IQR_chlorides),
  NA,  # IQR 범위를 벗어나면 NA로 대체
  red_wine$chlorides  # 그렇지 않으면 기존 값 유지
)
#아황산염
# IQR 계산
IQR_sulphates <- IQR(red_wine$sulphates)
Q1 <- quantile(red_wine$sulphates, 0.25)  # 1사분위수
Q3 <- quantile(red_wine$sulphates, 0.75)  # 3사분위수

# IQR 범위를 벗어나는 값 처리
red_wine$sulphates <- ifelse(
  red_wine$sulphates < (Q1 - 1.5 * IQR_sulphates) | red_wine$sulphates > (Q3 + 1.5 * IQR_sulphates),
  NA,  # IQR 범위를 벗어나면 NA로 대체
  red_wine$sulphates  # 그렇지 않으면 기존 값 유지
)
#결합산
# IQR 계산
IQR_fixed_acidity <- IQR(red_wine$fixed_acidity)
Q1 <- quantile(red_wine$fixed_acidity, 0.25)  # 1사분위수
Q3 <- quantile(red_wine$fixed_acidity, 0.75)  # 3사분위수

# IQR 범위를 벗어나는 값 처리
red_wine$fixed_acidity <- ifelse(
  red_wine$fixed_acidity < (Q1 - 1.5 * IQR_fixed_acidity) | red_wine$fixed_acidity > (Q3 + 1.5 * IQR_fixed_acidity),
  NA,  # IQR 범위를 벗어나면 NA로 대체
  red_wine$fixed_acidity  # 그렇지 않으면 기존 값 유지
)
#휘발산
# IQR 계산
IQR_volatile_acidity <- IQR(red_wine$volatile_acidity)
Q1 <- quantile(red_wine$volatile_acidity, 0.25)  # 1사분위수
Q3 <- quantile(red_wine$volatile_acidity, 0.75)  # 3사분위수

# IQR 범위를 벗어나는 값 처리
red_wine$volatile_acidity <- ifelse(
  red_wine$volatile_acidity < (Q1 - 1.5 * IQR_volatile_acidity) | red_wine$volatile_acidity > (Q3 + 1.5 * IQR_volatile_acidity),
  NA,  # IQR 범위를 벗어나면 NA로 대체
  red_wine$volatile_acidity  # 그렇지 않으면 기존 값 유지
)


# 구연산
# IQR 계산
IQR_citric_acid <- IQR(red_wine$citric_acid)
Q1 <- quantile(red_wine$citric_acid, 0.25)  # 1사분위수
Q3 <- quantile(red_wine$citric_acid, 0.75)  # 3사분위수

# IQR 범위를 벗어나는 값 처리
red_wine$citric_acid <- ifelse(
  red_wine$citric_acid < (Q1 - 1.5 * IQR_citric_acid) | red_wine$citric_acid > (Q3 + 1.5 * IQR_citric_acid),
  NA,  # IQR 범위를 벗어나면 NA로 대체
  red_wine$citric_acid  # 그렇지 않으면 기존 값 유지
)
# 유리 이산화황
# IQR 계산
#IQR_free_sulfur_dioxide <- IQR(red_wine$free_sulfur_dioxide)
#Q1 <- quantile(red_wine$free_sulfur_dioxide, 0.25)  # 1사분위수
#Q3 <- quantile(red_wine$free_sulfur_dioxide, 0.75)  # 3사분위수

# IQR 범위를 벗어나는 값 처리
#red_wine$free_sulfur_dioxide <- ifelse(
#  red_wine$free_sulfur_dioxide < (Q1 - 1.5 * IQR_free_sulfur_dioxide) | red_wine$free_sulfur_dioxide > (Q3 + 1.5 * IQR_free_sulfur_dioxide),
#  NA,  # IQR 범위를 벗어나면 NA로 대체
#  red_wine$free_sulfur_dioxide  # 그렇지 않으면 기존 값 유지
#)

table(is.na(red_wine))
summary(red_wine)

red_wine_cleaned <- na.omit(red_wine)
table(is.na(red_wine_cleaned))
show(red_wine_cleaned)

boxplot(white_wine)

# A.1.2. white_wine 이상값 처리

#모든 이상값은 NA로 변환 후 한번에 제거
#pH
# IQR 계산
wIQR_pH <- IQR(white_wine$pH)
Q1 <- quantile(white_wine$pH, 0.25)  # 1사분위수
Q3 <- quantile(white_wine$pH, 0.75)  # 3사분위수

# IQR 범위를 벗어나는 값 처리
white_wine$pH <- ifelse(
  white_wine$pH < (Q1 - 1.5 * wIQR_pH) | white_wine$pH > (Q3 + 1.5 * wIQR_pH),
  NA,  # IQR 범위를 벗어나면 NA로 대체
  white_wine$pH  # 그렇지 않으면 기존 값 유지
)

#알코올
# 알코올은 이상값이 없음

#황산의 총량
# IQR 계산
wIQR_total_sulfur_dioxide <- IQR(white_wine$total_sulfur_dioxide)
Q1 <- quantile(white_wine$total_sulfur_dioxide, 0.25)  # 1사분위수
Q3 <- quantile(white_wine$total_sulfur_dioxide, 0.75)  # 3사분위수

# IQR 범위를 벗어나는 값 처리
white_wine$total_sulfur_dioxide <- ifelse(
  white_wine$total_sulfur_dioxide < (Q1 - 1.5 * wIQR_total_sulfur_dioxide) | white_wine$total_sulfur_dioxide > (Q3 + 1.5 * wIQR_total_sulfur_dioxide),
  NA,  # IQR 범위를 벗어나면 NA로 대체
  white_wine$total_sulfur_dioxide  # 그렇지 않으면 기존 값 유지
)
#잔여 설탕
# IQR 계산
wIQR_residual_sugar <- IQR(white_wine$residual_sugar)
Q1 <- quantile(white_wine$residual_sugar, 0.25)  # 1사분위수
Q3 <- quantile(white_wine$residual_sugar, 0.75)  # 3사분위수

# IQR 범위를 벗어나는 값 처리
white_wine$residual_sugar <- ifelse(
  white_wine$residual_sugar < (Q1 - 1.5 * wIQR_residual_sugar) | white_wine$residual_sugar > (Q3 + 1.5 * wIQR_residual_sugar),
  NA,  # IQR 범위를 벗어나면 NA로 대체
  white_wine$residual_sugar  # 그렇지 않으면 기존 값 유지
)
#염소
# IQR 계산
wIQR_chlorides <- IQR(white_wine$chlorides)
Q1 <- quantile(white_wine$chlorides, 0.25)  # 1사분위수
Q3 <- quantile(white_wine$chlorides, 0.75)  # 3사분위수

# IQR 범위를 벗어나는 값 처리
white_wine$chlorides <- ifelse(
  white_wine$chlorides < (Q1 - 1.5 * wIQR_chlorides) | white_wine$chlorides > (Q3 + 1.5 * wIQR_chlorides),
  NA,  # IQR 범위를 벗어나면 NA로 대체
  white_wine$chlorides  # 그렇지 않으면 기존 값 유지
)
#아황산염
# IQR 계산
wIQR_sulphates <- IQR(white_wine$sulphates)
Q1 <- quantile(white_wine$sulphates, 0.25)  # 1사분위수
Q3 <- quantile(white_wine$sulphates, 0.75)  # 3사분위수

# IQR 범위를 벗어나는 값 처리
white_wine$sulphates <- ifelse(
  white_wine$sulphates < (Q1 - 1.5 * wIQR_sulphates) | white_wine$sulphates > (Q3 + 1.5 * wIQR_sulphates),
  NA,  # IQR 범위를 벗어나면 NA로 대체
  white_wine$sulphates  # 그렇지 않으면 기존 값 유지
)
#결합산
# IQR 계산
wIQR_fixed_acidity <- IQR(white_wine$fixed_acidity)
Q1 <- quantile(white_wine$fixed_acidity, 0.25)  # 1사분위수
Q3 <- quantile(white_wine$fixed_acidity, 0.75)  # 3사분위수

# IQR 범위를 벗어나는 값 처리
white_wine$fixed_acidity <- ifelse(
  white_wine$fixed_acidity < (Q1 - 1.5 * wIQR_fixed_acidity) | white_wine$fixed_acidity > (Q3 + 1.5 * wIQR_fixed_acidity),
  NA,  # IQR 범위를 벗어나면 NA로 대체
  white_wine$fixed_acidity  # 그렇지 않으면 기존 값 유지
)
#휘발산
# IQR 계산
wIQR_volatile_acidity <- IQR(white_wine$volatile_acidity)
Q1 <- quantile(white_wine$volatile_acidity, 0.25)  # 1사분위수
Q3 <- quantile(white_wine$volatile_acidity, 0.75)  # 3사분위수

# IQR 범위를 벗어나는 값 처리
white_wine$volatile_acidity <- ifelse(
  white_wine$volatile_acidity < (Q1 - 1.5 * wIQR_volatile_acidity) | white_wine$volatile_acidity > (Q3 + 1.5 * wIQR_volatile_acidity),
  NA,  # IQR 범위를 벗어나면 NA로 대체
  white_wine$volatile_acidity  # 그렇지 않으면 기존 값 유지
)


# 구연산
# IQR 계산
wIQR_citric_acid <- IQR(white_wine$citric_acid)
Q1 <- quantile(white_wine$citric_acid, 0.25)  # 1사분위수
Q3 <- quantile(white_wine$citric_acid, 0.75)  # 3사분위수

# IQR 범위를 벗어나는 값 처리
white_wine$citric_acid <- ifelse(
  white_wine$citric_acid < (Q1 - 1.5 * wIQR_citric_acid) | white_wine$citric_acid > (Q3 + 1.5 * wIQR_citric_acid),
  NA,  # IQR 범위를 벗어나면 NA로 대체
  white_wine$citric_acid  # 그렇지 않으면 기존 값 유지
)

table(is.na(white_wine))
summary(white_wine)

white_wine_cleaned <- na.omit(white_wine)
table(is.na(white_wine_cleaned))
show(white_wine_cleaned)

# A.1.3 데이터 정규화
red_wine_scaled <- as.data.frame(lapply(red_wine_cleaned, function(x) {
  (x - min(x)) / (max(x) - min(x))
}))
white_wine_scaled <- as.data.frame(lapply(white_wine_cleaned, function(x) {
  (x - min(x)) / (max(x) - min(x))
}))

# A.1.4. 타겟값 추가
# 'red_wine_scaled' 데이터프레임에 'target' 열을 추가하고, 모든 값에 0을 할당
red_wine_scaled$target <- rep(0, nrow(red_wine_scaled))

# 'white_wine_scaled' 데이터프레임에 'target' 열을 추가하고, 모든 값에 1을 할당
white_wine_scaled$target <- rep(1, nrow(white_wine_scaled))

head(red_wine_scaled)
head(white_wine_scaled)

# 히스토그램
red_wine_scaled %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, fill = variable)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.7) +
  facet_wrap(~variable, scales = "free", ncol = 4) +
  theme_minimal()

# 히스토그램
white_wine_scaled %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "value") %>%
  ggplot(aes(x = value, fill = variable)) +
  geom_histogram(bins = 30, color = "black", alpha = 0.7) +
  facet_wrap(~variable, scales = "free", ncol = 4) +
  theme_minimal()

"""## 1.2. 데이터 병합 및 이상값 확인"""

# 두 데이터프레임을 결합
wine_data <- rbind(red_wine_scaled, white_wine_scaled)
wine_data$target %>% unique()

table(is.na(wine_data))
summary(wine_data)

# 결측치가 행별로 존재하는 경우, 결측치가 없는 것처럼 나올 수 있음

wine_data <- na.omit(wine_data)
table(is.na(wine_data))

# 범주형 반응변수 데이터
fac_wine_data <- wine_data
fac_wine_data$target <- ifelse(fac_wine_data$target == 0, "white", "red")
fac_wine_data$target <- as.factor(fac_wine_data$target)
unique(fac_wine_data$target)

"""## 1.3. 모델링

### 1.3.1 로지스틱 회귀 모델
"""

index <- createDataPartition(y = wine_data$target, p = 0.7, list = FALSE)
train_wine <- wine_data[index, ] #모델 생성 시 사용
test_wine <- wine_data[-index, ] #모델 검증 시 사용

# 로지스틱 회귀

lorm_wine = glm(target~., data= train_wine, family=binomial)
summary(lorm_wine)

# backward
lorm_wine_back <- step(lorm_wine, direction = "backward")
summary(lorm_wine_back)

predict_lorm_wine <- predict(lorm_wine_back, test_wine, type = "response") %>% tibble(predict_lorm_wine = .)
predict_lorm_wine %>% show()

predict_lorm_wine %>% str()

real_predict_lorm_wine <- test_wine[, "target", drop = FALSE] %>% bind_cols(., predict_lorm_wine)

head(real_predict_lorm_wine)

roc_cutoff <- roc(real_predict_lorm_wine$target, real_predict_lorm_wine$predict_lorm_wine)
plot.roc(roc_cutoff, col = "royalblue", print.auc = TRUE, max.auc.polygon = TRUE, print.thres = TRUE, print.thres.pch = 19, print.thres.col = "red", auc.polygon = TRUE, auc.polygon.col = "#D1F2EB")

roc_cutoff_value <- coords(roc_cutoff, "best", ret = "threshold", transpose = F)[1,1]
roc_cutoff_value

real_predict_lorm_wine <- real_predict_lorm_wine %>% mutate(predict_target_roccut = as.factor(ifelse(predict_lorm_wine > roc_cutoff_value, "white", "red")))
head(real_predict_lorm_wine)

red_count <- sum(real_predict_lorm_wine$predict_target_roccut == "red") %>% print()
white_count <- sum(real_predict_lorm_wine$predict_target_roccut == "white") %>% print()

real_predict_lorm_wine$target <- as.factor(ifelse(real_predict_lorm_wine$target == 0, "red", "white"))
head(real_predict_lorm_wine)

#성능평가
cm_lorm_wine <- confusionMatrix(real_predict_lorm_wine$target, real_predict_lorm_wine$predict_target_roccut)
cm_lorm_wine

varImp(lorm_wine_back)

"""### 1.3.2 Random Forest"""

index <- createDataPartition(y = fac_wine_data$target, p = 0.7, list = FALSE)
train_rfm_wine <- fac_wine_data[index, ]
test_rfm_wine <- fac_wine_data[-index, ]
head(train_rfm_wine)
head(test_rfm_wine)

install.packages("randomForest")
library(randomForest)

rfm_wine <- randomForest(target ~ ., data = train_rfm_wine)
rfm_wine

summary(rfm_wine)

plot(rfm_wine)

train_error <- 1 - mean(rfm_wine$predicted == rfm_wine$y)
oob_error <- rfm_wine$err.rate[nrow(rfm_wine$err.rate), "OOB"]

print(paste("Training Error:", train_error))
print(paste("OOB Error:", oob_error))

varUsed(rfm_wine)

varImpPlot(rfm_wine)

cm_rfm_wine <- confusionMatrix(test_rfm_wine$target, predict(rfm_wine, test_rfm_wine))
cm_rfm_wine

"""### 1.3.3 SVM"""

install.packages(c("e1071", "kernlab"))
library(e1071)
library(kernlab)

index <- createDataPartition(y = fac_wine_data$target, p = 0.7, list = FALSE)
train_svm_wine <- fac_wine_data[index, ]
test_svm_wine <- fac_wine_data[-index, ]
head(train_svm_wine)
head(test_svm_wine)

svmm_wine <- svm(formula = target ~ ., data = train_svm_wine, type =  "C-classification", kernel = "radial")
summary(svmm_wine)

cm_svm <- confusionMatrix(train_svm_wine$target, predict(svmm_wine, train_svm_wine))
cm_svm

cm_svm_test <- confusionMatrix(test_svm_wine$target, predict(svmm_wine, test_svm_wine))
cm_svm_test

tuned_wine <- tune.svm(target ~ ., data = train_svm_wine, gamma = 10^(-8:1), cost = 1:30)
best_param_wine <- summary(tuned_wine)$best.parameters
best_param_wine

train_control <- trainControl(method = "cv", number = 5)

svm_wine_train <- train(target ~ ., data = train_svm_wine,
                   method = "svmRadial",
                   trControl = train_control,
                   tuneGrid = expand.grid(sigma = best_param_wine$gamma, C = best_param_wine$cost))  # 하이퍼파라미터 설정

summary(svm_wine_train)

predictions <- predict(svm_wine_train, newdata = test_svm_wine)

confusionMatrix(predictions, test_svm_wine$target)

importance_svm <- varImp(svm_wine_train, scale = FALSE)
importance_svm

"""## 1.4. 새로운 데이터로 예측해보기"""

# 랜덤 포레스트 모델로 새로운 데이터 예측

# https://drive.google.com/file/d/1fT5QnwIMR8oJXHU1SGOMzziGz2IZTTdO/view?usp=sharing

system("gdown --id 1fT5QnwIMR8oJXHU1SGOMzziGz2IZTTdO")
system("ls", TRUE)

new_wine <- fread("/content/new_wine_data.csv", header=T, encoding = "UTF-8") %>% as_tibble()
show(new_wine)

new_wine <- new_wine %>% rename(
  fixed_acidity = `fixed acidity`,
  volatile_acidity = `volatile acidity`,
  citric_acid = `citric acid`,
  residual_sugar = `residual sugar`,
  free_sulfur_dioxide = `free sulfur dioxide`,
  total_sulfur_dioxide = `total sulfur dioxide`
  )

new_wine <- dplyr::select(new_wine, -quality, -density, -free_sulfur_dioxide)
show(new_wine)

predict_new_rf <- predict(rfm_wine, new_wine, type = "class") %>% tibble(predict_new_rf = .)
predict_new_rf %>% show()
