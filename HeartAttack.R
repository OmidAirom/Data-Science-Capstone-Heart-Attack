################################ Heart Attack ################################

# this is the code for my Harvard & edx final project
# in this code I am going to load my data, take a look at it
# then I am going to explore the data and i am going to
# build models to make predictions based on the data.

library(ggplot2)
library(caret)
library(dplyr)
library(class)

# the data is downloaded from kaggle 
# the data will be uploaded with the files, I renamed it to heart.csv-edx.xls
# loading our data

data <- read.csv("/Users/omid/Desktop/harvard/heart/heart//heart.csv-edx.xls")

# taking a look at the first 6 rows to see what we have
# we have 14 columns, 13 features and 1 target and 1025 rows that are the data 
# of 1025 patients

dim(data)
head(data)
str(data)

# we can see that from 1025 patient we have 499 without and 526 with heart attacks
table(data$target)

# now I check if there is any empty cell in the data
# there is no empty cell in the data

sum(is.na(data))

############################## Data Exploration ############################### 

# going through the columns in order from left to right
# Age 
# We can see that, the mean of the age column is near 54 and due to the 
# distribution most of the patients are near 60.

summary(data$age)

data %>% ggplot(aes(age)) +
  geom_density(color = "#00AFBB") +
  scale_x_log10() +
  xlab("Age") + 
  ylab("Patient")

data %>%
  filter(target == 1) %>%
  ggplot(aes(age)) +
  geom_density(color = "#00AFBB") +
  scale_x_log10() +
  xlab("Age") + 
  ylab("Positive Patient")

# Gender (Sex)
# we can see that, there are more than 700 male and near (less than) 300
# female patients in the heart attack data set.

positive_women <- nrow(data %>% filter(sex == "female" & target == 1))
total_women <- nrow(data %>% filter(sex == "female"))
ratio_women <- positive_women / total_women

positive_men <- nrow(data %>% filter(sex == "male" & target == 1))
total_men <- nrow(data %>% filter(sex == "male"))
ratio_men <- positive_men / total_men

data$sex <- factor(data$sex, levels = c(0, 1),
                   labels = c("female", "male"))
data %>% ggplot(aes(sex, fill = sex)) +
  geom_bar() +
  xlab("Gender") + 
  ylab("Number of Patients")

age_tibble <- tibble(gender = "total women", number = total_women)
age_tibble <- bind_rows(age_tibble, tibble(gender = "positive women" 
                                           ,number = positive_women ))
age_tibble <- bind_rows(age_tibble, tibble(gender = "positive women to total women ratio" 
                                           ,number = ratio_women ))
age_tibble <- bind_rows(age_tibble, tibble(gender = "total men" 
                                           ,number = total_men ))
age_tibble <- bind_rows(age_tibble, tibble(gender = "positive men" 
                                           ,number = positive_men ))
age_tibble <- bind_rows(age_tibble, tibble(gender = "positive men to total men ratio" 
                                           ,number = ratio_men ))
age_tibble %>% knitr::kable()

# Chest Pain (cp)
# Value 0: typical angina
# Value 1: atypical angina
# Value 2: non-anginal pain
# Value 3: asymptomatic

data$cp <- factor(data$cp, levels = c(0, 1, 2, 3),
                   labels = c("0", "1", "2", "3"))
data %>% ggplot(aes(cp, fill = cp)) +
  geom_bar() +
  xlab("Chest Pain Type") + 
  ylab("Number of Patients")

data %>%
  filter(target == 1) %>%
  ggplot(aes(cp, fill = cp)) +
  geom_bar() +
  xlab("Chest Pain Type") + 
  ylab("Number of Patients")

type0_cp_pos <- nrow(data %>% filter(cp == 0 & target == 1))
type0_cp <- nrow(data %>% filter(cp == 0))
type0_cp_ratio <- type0_cp_pos / type0_cp

type1_cp_pos <- nrow(data %>% filter(cp == 1 & target == 1))
type1_cp <- nrow(data %>% filter(cp == 1))
type1_cp_ratio <- type1_cp_pos / type1_cp

type2_cp_pos <- nrow(data %>% filter(cp == 2 & target == 1))
type2_cp <- nrow(data %>% filter(cp == 2))
type2_cp_ratio <- type2_cp_pos / type2_cp

type3_cp_pos <- nrow(data %>% filter(cp == 3 & target == 1))
type3_cp <- nrow(data %>% filter(cp == 3))
type3_cp_ratio <- type3_cp_pos / type3_cp

cp_tibble <- tibble(chest_pain = "type 0 total", number = type0_cp)
cp_tibble <- bind_rows(cp_tibble, tibble(chest_pain = "type 0 positives" 
                                         ,number = type0_cp_pos ))
cp_tibble <- bind_rows(cp_tibble, tibble(chest_pain = "positive type 0 to total ratio" 
                                         ,number = type0_cp_ratio ))
cp_tibble <- bind_rows(cp_tibble, tibble(chest_pain = "type 1 total" 
                                         ,number = type1_cp ))
cp_tibble <- bind_rows(cp_tibble, tibble(chest_pain = "type 1 positive" 
                                         ,number = type1_cp_pos ))
cp_tibble <- bind_rows(cp_tibble, tibble(chest_pain = "positive type 1 to total ratio" 
                                         ,number = type1_cp_ratio ))
cp_tibble <- bind_rows(cp_tibble, tibble(chest_pain = "type 2 total" 
                                         ,number = type2_cp ))
cp_tibble <- bind_rows(cp_tibble, tibble(chest_pain = "type 2 positive" 
                                         ,number = type2_cp_pos ))
cp_tibble <- bind_rows(cp_tibble, tibble(chest_pain = "positive type 2 to total ratio" 
                                         ,number = type2_cp_ratio ))
cp_tibble <- bind_rows(cp_tibble, tibble(chest_pain = "type 3 total" 
                                         ,number = type3_cp ))
cp_tibble <- bind_rows(cp_tibble, tibble(chest_pain = "type 3 positive" 
                                         ,number = type3_cp_pos ))
cp_tibble <- bind_rows(cp_tibble, tibble(chest_pain = "positive type 3 to total ratio" 
                                         ,number = type3_cp_ratio ))
cp_tibble %>% knitr::kable()

# Resting Blood Pressure (trestbps)

summary(data$trestbps)

data %>% ggplot(aes(trestbps)) +
  geom_density(color = "#00AFBB") +
  xlab("Resting Blood Pressure") + 
  ylab("Number of Patients")

data %>%
  filter(target == 1) %>%
  ggplot(aes(trestbps)) +
  geom_density(color = "#00AFBB") +
  xlab("Resting Blood Pressure") + 
  ylab("Patients with Heart Attack")

# Cholesterol (chol)

summary(data$chol)

data %>% ggplot(aes(chol)) +
  geom_density(color = "#00AFBB") +
  xlab("Cholesterol") + 
  ylab("Number of Patients")

data %>%
  filter(target == 1) %>%
  ggplot(aes(chol)) +
  geom_density(color = "#00AFBB") +
  xlab("Cholesterol") + 
  ylab("Patients with Heart Attack")

# Blood Sugar (fbs)

data$fbs <- factor(data$fbs, levels = c(0, 1),
                   labels = c("0", "1"))

data %>% ggplot(aes(fbs, fill = fbs)) +
  geom_bar() +
  xlab("Blood Sugar") + 
  ylab("Number of Patients")

data %>%
  filter(target == 1) %>%
  ggplot(aes(fbs, fill = fbs)) +
  geom_bar() +
  xlab("Blood Sugar") + 
  ylab("Number of Patients")

positive_0 <- nrow(data %>% filter(fbs == 0 & target == 1))
total_0 <- nrow(data %>% filter(fbs == 0))
ratio_0 <- positive_0 / total_0

positive_1 <- nrow(data %>% filter(fbs == 1 & target == 1))
total_1 <- nrow(data %>% filter(fbs == 1))
ratio_1 <- positive_1 / total_1

fbs_tibble <- tibble(fbs = "total type 0", number = total_0)
fbs_tibble <- bind_rows(fbs_tibble, tibble(fbs = "positive type 0" 
                                           ,number = positive_0 ))
fbs_tibble <- bind_rows(fbs_tibble, tibble(fbs = "positive type 0 to total ratio" 
                                           ,number = ratio_0 ))
fbs_tibble <- bind_rows(fbs_tibble, tibble(fbs = "total type 1" 
                                           ,number = total_1 ))
fbs_tibble <- bind_rows(fbs_tibble, tibble(fbs = "positive type 1" 
                                           ,number = positive_1 ))
fbs_tibble <- bind_rows(fbs_tibble, tibble(fbs = "positive type 1 to total ratio" 
                                           ,number = ratio_1 ))
fbs_tibble %>% knitr::kable()

# Electrocardiographic (restecg)

table(data$restecg)

data$restecg <- factor(data$restecg, levels = c(0, 1, 2),
                   labels = c("0", "1", "2"))
data %>% ggplot(aes(restecg, fill = restecg)) +
  geom_bar() +
  xlab("Electrocardiographic") + 
  ylab("Number of Patients")

data %>%
  filter(target == 1) %>%
  ggplot(aes(restecg, fill = restecg)) +
  geom_bar() +
  xlab("Electrocardiographic") + 
  ylab("Patients with Heart Attack")

el_positive_0 <- nrow(data %>% filter(restecg == 0 & target == 1))
el_total_0 <- nrow(data %>% filter(restecg == 0))
el_ratio_0 <- el_positive_0 / el_total_0

el_positive_1 <- nrow(data %>% filter(restecg == 1 & target == 1))
el_total_1 <- nrow(data %>% filter(restecg == 1))
el_ratio_1 <- el_positive_1 / el_total_1

el_positive_2 <- nrow(data %>% filter(restecg == 2 & target == 1))
el_total_2 <- nrow(data %>% filter(restecg == 2))
el_ratio_2 <- el_positive_2 / el_total_2

restecg_tibble <- tibble(restecg = "total type 0", number = el_total_0)
restecg_tibble <- bind_rows(restecg_tibble, tibble(restecg = "positive type 0" 
                                                   ,number = el_positive_0 ))
restecg_tibble <- bind_rows(restecg_tibble, tibble(restecg = "positive type 0 to total ratio" 
                                                   ,number = el_ratio_0 ))
restecg_tibble <- bind_rows(restecg_tibble, tibble(restecg = "total type 1" 
                                                   ,number = el_total_1 ))
restecg_tibble <- bind_rows(restecg_tibble, tibble(restecg = "positive type 1" 
                                                   ,number = el_positive_1 ))
restecg_tibble <- bind_rows(restecg_tibble, tibble(restecg = "positive type 1 to total ratio" 
                                                   ,number = el_ratio_1 ))
restecg_tibble <- bind_rows(restecg_tibble, tibble(restecg = "total type 1" 
                                                   ,number = el_total_2 ))
restecg_tibble <- bind_rows(restecg_tibble, tibble(restecg = "positive type 2" 
                                                   ,number = el_positive_2 ))
restecg_tibble <- bind_rows(restecg_tibble, tibble(restecg = "positive type 2 to total ratio" 
                                                   ,number = el_ratio_2 ))
restecg_tibble %>% knitr::kable()
# Max Heart Rate (thalach)

summary(data$thalach)

data %>% ggplot(aes(thalach)) +
  geom_density(color = "#00AFBB") +
  xlab("Max Heart Rate") + 
  ylab("Number of Patients")

data %>%
  filter(target == 1) %>%
  ggplot(aes(thalach)) +
  geom_density(color = "#00AFBB") +
  xlab("Cholesterol") + 
  ylab("Patients with Heart Attack")

# Angina (exang) 

table(data$exang)

data$exang <- factor(data$exang, levels = c(0, 1),
                       labels = c("0", "1"))
data %>% ggplot(aes(exang, fill = exang)) +
  geom_bar() +
  xlab("Angina") + 
  ylab("Number of Patients")

data %>%
  filter(target == 1) %>%
  ggplot(aes(exang, fill = exang)) +
  geom_bar() +
  xlab("Electrocardiographic") + 
  ylab("Patients with Heart Attack")

ang_positive_0 <- nrow(data %>% filter(exang == 0 & target == 1))
ang_total_0 <- nrow(data %>% filter(exang == 0))
ang_ratio_0 <- ang_positive_0 / ang_total_0

ang_positive_1 <- nrow(data %>% filter(exang == 1 & target == 1))
ang_total_1 <- nrow(data %>% filter(exang == 1))
ang_ratio_1 <- ang_positive_1 / ang_total_1

angina_tibble <- tibble(angina = "total type 0", number = ang_total_0)
angina_tibble <- bind_rows(angina_tibble, tibble(angina = "positive type 0" 
                                                 ,number = ang_positive_0 ))
angina_tibble <- bind_rows(angina_tibble, tibble(angina = "positive type 0 to total ratio" 
                                                 ,number = ang_ratio_0 ))
angina_tibble <- bind_rows(angina_tibble, tibble(angina = "total type 1" 
                                                 ,number = ang_total_1 ))
angina_tibble <- bind_rows(angina_tibble, tibble(angina = "positive type 1" 
                                                 ,number = ang_positive_1 ))
angina_tibble <- bind_rows(angina_tibble, tibble(angina = "positive type 1 to total ratio" 
                                                 ,number = ang_ratio_1 ))
angina_tibble %>% knitr::kable()

# Oldpeak

summary(data$oldpeak)

data %>% ggplot(aes(oldpeak)) +
  geom_density(color = "#00AFBB") +
  xlab("Oldpeak") + 
  ylab("Number of Patients")

data %>%
  filter(target == 1) %>%
  ggplot(aes(oldpeak)) +
  geom_density(color = "#00AFBB") +
  xlab("Oldpeak") + 
  ylab("Patients with Heart Attack")

# Slope

table(data$slope)

data$slope <- factor(data$slope, levels = c(0, 1, 2),
                     labels = c("0", "1", "2"))

data %>% ggplot(aes(slope, fill = slope)) +
  geom_bar() +
  xlab("Electrocardiographic") + 
  ylab("Number of Patients")

data %>%
  filter(target == 1) %>%
  ggplot(aes(slope, fill = slope)) +
  geom_bar() +
  xlab("Electrocardiographic") + 
  ylab("Patients with Heart Attack")

sl_positive_0 <- nrow(data %>% filter(slope == 0 & target == 1))
sl_total_0 <- nrow(data %>% filter(slope == 0))
sl_ratio_0 <- sl_positive_0 / sl_total_0

sl_positive_1 <- nrow(data %>% filter(slope == 1 & target == 1))
sl_total_1 <- nrow(data %>% filter(slope == 1))
sl_ratio_1 <- sl_positive_1 / sl_total_1

sl_positive_2 <- nrow(data %>% filter(slope == 2 & target == 1))
sl_total_2 <- nrow(data %>% filter(slope == 2))
sl_ratio_2 <- sl_positive_2 / sl_total_2

slope_tibble <- tibble(slope = "total type 0", number = sl_total_0)
slope_tibble <- bind_rows(slope_tibble, tibble(slope = "positive type 0" 
                                               ,number = sl_positive_0 ))
slope_tibble <- bind_rows(slope_tibble, tibble(slope = "positive type 0 to total ratio" 
                                               ,number = sl_ratio_0 ))
slope_tibble <- bind_rows(slope_tibble, tibble(slope = "total type 1" 
                                               ,number = sl_total_1 ))
slope_tibble <- bind_rows(slope_tibble, tibble(slope = "positive type 1" 
                                               ,number = sl_positive_1 ))
slope_tibble <- bind_rows(slope_tibble, tibble(slope = "positive type 1 to total ratio" 
                                               ,number = sl_ratio_1 ))
slope_tibble <- bind_rows(slope_tibble, tibble(slope = "total type 1" 
                                               ,number = sl_total_2 ))
slope_tibble <- bind_rows(slope_tibble, tibble(slope = "positive type 2" 
                                               ,number = sl_positive_2 ))
slope_tibble <- bind_rows(slope_tibble, tibble(slope = "positive type 2 to total ratio" 
                                               ,number = sl_ratio_2 ))
slope_tibble %>% knitr::kable()

# CA

table(data$ca)

data$ca <- factor(data$ca, levels = c(0, 1, 2, 3, 4),
                  labels = c("0", "1", "2", "3", "4"))

data %>% ggplot(aes(ca, fill = ca)) +
  geom_bar() +
  xlab("CA") + 
  ylab("Number of Patients")

data %>%
  filter(target == 1) %>%
  ggplot(aes(ca, fill = ca)) +
  geom_bar() +
  xlab("CA") + 
  ylab("Patients with Heart Attack")

ca_positive_0 <- nrow(data %>% filter(ca == 0 & target == 1))
ca_total_0 <- nrow(data %>% filter(ca == 0))
ca_ratio_0 <- ca_positive_0 / ca_total_0

ca_positive_1 <- nrow(data %>% filter(ca == 1 & target == 1))
ca_total_1 <- nrow(data %>% filter(ca == 1))
ca_ratio_1 <- ca_positive_1 / ca_total_1

ca_positive_2 <- nrow(data %>% filter(ca == 2 & target == 1))
ca_total_2 <- nrow(data %>% filter(ca == 2))
ca_ratio_2 <- ca_positive_2 / ca_total_2

ca_positive_3 <- nrow(data %>% filter(ca == 3 & target == 1))
ca_total_3 <- nrow(data %>% filter(ca == 3))
ca_ratio_3 <- ca_positive_3 / ca_total_3

ca_positive_4 <- nrow(data %>% filter(ca == 4 & target == 1))
ca_total_4 <- nrow(data %>% filter(ca == 4))
ca_ratio_4 <- ca_positive_4 / ca_total_4

ca_tibble <- tibble(CA = "total type 0", number = ca_total_0)
ca_tibble <- bind_rows(ca_tibble, tibble(CA = "positive type 0" 
                                         ,number = ca_positive_0 ))
ca_tibble <- bind_rows(ca_tibble, tibble(CA = "positive type 0 to total ratio" 
                                         ,number = ca_ratio_0 ))
ca_tibble <- bind_rows(ca_tibble, tibble(CA = "total type 1" 
                                         ,number = ca_total_1 ))
ca_tibble <- bind_rows(ca_tibble, tibble(CA = "positive type 1" 
                                         ,number = ca_positive_1 ))
ca_tibble <- bind_rows(ca_tibble, tibble(CA = "positive type 1 to total ratio" 
                                         ,number = ca_ratio_1 ))
ca_tibble <- bind_rows(ca_tibble, tibble(CA = "total type 2" 
                                         ,number = ca_total_2 ))
ca_tibble <- bind_rows(ca_tibble, tibble(CA = "positive type 2" 
                                         ,number = ca_positive_2 ))
ca_tibble <- bind_rows(ca_tibble, tibble(CA = "positive type 2 to total ratio" 
                                         ,number = ca_ratio_2 ))
ca_tibble <- bind_rows(ca_tibble, tibble(CA = "total type 3" 
                                         ,number = ca_total_3 ))
ca_tibble <- bind_rows(ca_tibble, tibble(CA = "positive type 3" 
                                         ,number = ca_positive_3 ))
ca_tibble <- bind_rows(ca_tibble, tibble(CA = "positive type 3 to total ratio" 
                                         ,number = ca_ratio_3 ))
ca_tibble <- bind_rows(ca_tibble, tibble(CA = "total type 4" 
                                         ,number = ca_total_4 ))
ca_tibble <- bind_rows(ca_tibble, tibble(CA = "positive type 4" 
                                         ,number = ca_positive_4 ))
ca_tibble <- bind_rows(ca_tibble, tibble(CA = "positive type 4 to total ratio" 
                                         ,number = ca_ratio_4 ))

# Thalassemia

table(data$thal)

data$thal <- factor(data$thal, levels = c(0, 1, 2, 3),
                    labels = c("0", "1", "2", "3"))

data %>% ggplot(aes(thal, fill = thal)) +
  geom_bar() +
  xlab("Thalassemia") + 
  ylab("Number of Patients")

data %>%
  filter(target == 1) %>%
  ggplot(aes(thal, fill = thal)) +
  geom_bar() +
  xlab("Thalassemia") + 
  ylab("Patients with Heart Attack")

t_positive_0 <- nrow(data %>% filter(thal == 0 & target == 1))
t_total_0 <- nrow(data %>% filter(thal == 0))
t_ratio_0 <- t_positive_0 / t_total_0

t_positive_1 <- nrow(data %>% filter(thal == 1 & target == 1))
t_total_1 <- nrow(data %>% filter(thal == 1))
t_ratio_1 <- t_positive_1 / t_total_1

t_positive_2 <- nrow(data %>% filter(thal == 2 & target == 1))
t_total_2 <- nrow(data %>% filter(thal == 2))
t_ratio_2 <- t_positive_2 / t_total_2

t_positive_3 <- nrow(data %>% filter(thal == 3 & target == 1))
t_total_3 <- nrow(data %>% filter(thal == 3))
t_ratio_3 <- t_positive_3 / t_total_3

t_tibble <- tibble(Thalassemia = "total type 0", number = t_total_0)
t_tibble <- bind_rows(t_tibble, tibble(Thalassemia = "positive type 0" 
                                       ,number = t_positive_0 ))
t_tibble <- bind_rows(t_tibble, tibble(Thalassemia = "positive type 0 to total ratio" 
                                       ,number = t_ratio_0 ))
t_tibble <- bind_rows(t_tibble, tibble(Thalassemia = "total type 1" 
                                       ,number = t_total_1 ))
t_tibble <- bind_rows(t_tibble, tibble(Thalassemia = "positive type 1" 
                                       ,number = t_positive_1 ))
t_tibble <- bind_rows(t_tibble, tibble(Thalassemia = "positive type 1 to total ratio" 
                                       ,number = t_ratio_1 ))
t_tibble <- bind_rows(t_tibble, tibble(Thalassemia = "total type 2" 
                                       ,number = t_total_2 ))
t_tibble <- bind_rows(t_tibble, tibble(Thalassemia = "positive type 2" 
                                       ,number = t_positive_2 ))
t_tibble <- bind_rows(t_tibble, tibble(Thalassemia = "positive type 2 to total ratio" 
                                       ,number = t_ratio_2 ))
t_tibble <- bind_rows(t_tibble, tibble(Thalassemia = "total type 3" 
                                       ,number = t_total_3 ))
t_tibble <- bind_rows(t_tibble, tibble(Thalassemia = "positive type 3" 
                                       ,number = t_positive_3 ))
t_tibble <- bind_rows(t_tibble, tibble(Thalassemia = "positive type 3 to total ratio" 
                                       ,number = t_ratio_3 ))

t_tibble %>% knitr::kable()

###################### Models for Predictions & Accuracy ###################### 

# now we are going to build three models with the "data" we had with no changes

# splitting data to train and test sets

set.seed(1)
index <- createDataPartition(y = data$target, times = 1, p = 0.1, list = FALSE)
train <- data[-index,]
test <- data[index,]

head(train)
head(test)
dim(train) # the train data set is 90% of the data
dim(test) # the test data set is 10% of the data

# first model : linear regression, lm

train_1 <- train(target~., method = "lm", data = train)
prediction_1 <- round(predict(train_1, test))
confusionmatrix_1 <- confusionMatrix(as.factor(prediction_1), as.factor(test$target))
acc_1 <- confusionmatrix_1$overall[["Accuracy"]]
results <- tibble(model = "linear regression, lm", Accuracy = acc_1)
results %>% knitr::kable()

# second model : logistic regression, glm

train_2 <- train(target~., method="glm", train)
prediction_2 <- round(predict(train_2, test))
confusionmatrix_2 <- confusionMatrix(as.factor(prediction_2), as.factor(test$target))
acc_2 <- confusionmatrix_2$overall[["Accuracy"]]
results <- bind_rows(results, tibble(model = "logistic regression, glm" 
                                         ,Accuracy = acc_2 ))
results %>% knitr::kable()
results

# last model : knn

train_3 <- train(target~., method="knn", train)
prediction_3 <- round(predict(train_3, test))
confusionmatrix_3 <- confusionMatrix(as.factor(prediction_3), as.factor(test$target))
acc_3 <- confusionmatrix_3$overall[["Accuracy"]]
results <- bind_rows(results, tibble(model = "k nearest neighbors" 
                                     ,Accuracy = acc_3 ))
results %>% knitr::kable()
results

# now we are going to build another time the previous three models but before
# building the models we are going to normalize our data and see how the 
# accuracy of each model will change.
# to do this we need to create a function to normalize each feature of our data

new_data <- read.csv("/Users/omid/Desktop/harvard/heart/heart//heart.csv-edx.xls")

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x)))
}

# now we are going to apply the normalize function to all the numeric columns

normal <- as.data.frame(lapply(new_data, normalize))
head(normal)

# now that we have the "normal" data we split the data again to train and test
# sets and build the models again

set.seed(1)
index_normal <- createDataPartition(y = normal$target, times = 1, p = 0.1, list = FALSE)
train_normal <- normal[-index_normal,]
test_normal <- normal[index_normal,]

head(train_normal)
head(test_normal)
dim(train_normal) # the train data set is 90% of the data
dim(test_normal) # the test data set is 10% of the data

# first model with normalized data : linear regression, lm

train_1_normal <- train(target~., method = "lm", data = normal)
prediction_1_normal <- round(predict(train_1_normal, test_normal))
confusionmatrix_1_normal <- confusionMatrix(
  as.factor(prediction_1_normal), as.factor(test_normal$target))
acc_1_normal <- confusionmatrix_1_normal$overall[["Accuracy"]]
results <- bind_rows(results, tibble(model = "linear regression, lm, normalized data" 
                                     ,Accuracy = acc_1_normal ))
results %>% knitr::kable()
results

# second model with normalized data : logistic regression, glm

train_2_normal <- train(target~., method = "glm", data = normal)
prediction_2_normal <- round(predict(train_2_normal, test_normal))
confusionmatrix_2_normal <- confusionMatrix(
  as.factor(prediction_2_normal), as.factor(test_normal$target))
acc_2_normal <- confusionmatrix_2_normal$overall[["Accuracy"]]
results <- bind_rows(results, tibble(model = "linear regression, glm, normalized data" 
                                     ,Accuracy = acc_2_normal ))
results %>% knitr::kable()
results

# last model with normalized data : knn

train_3_normal <- train(target~., method = "knn", data = normal)
prediction_3_normal <- round(predict(train_3_normal, test_normal))
confusionmatrix_3_normal <- confusionMatrix(
  as.factor(prediction_3_normal), as.factor(test_normal$target))
acc_3_normal <- confusionmatrix_3_normal$overall[["Accuracy"]]
results <- bind_rows(results, tibble(model = "k nearest neighbors normalized data" 
                                     ,Accuracy = acc_3_normal ))
results %>% knitr::kable()
results

##################################### fin #####################################