install.packages("randomForest")
library(randomForest)

data <- read.csv("D:/R portofolio/world wide suicide/world_suicide_rates.csv")

# Buat threshold
threshold <- median(data$Suicide.Rate, na.rm = TRUE)

data$rate_label <- factor(
  ifelse(data$Suicide.Rate >= threshold, "High", "Low"),
  levels = c("Low", "High")
)

library(dplyr)

data_clean <- data %>%
  select(-Country.Name, -Country.Code)

# 3. split 70:30 (kalau belum)
set.seed(123)
index <- sample(1:nrow(data), 0.7 * nrow(data))
train <- data[index, ]
test  <- data[-index, ]

train_x <- train %>% select(-Suicide.Rate)
test_x  <- test  %>% select(-Suicide.Rate)

library(randomForest)

model <- randomForest(
  rate_label ~ .,
  data = train_x,
  ntree = 300
)

# 5. predict (menghasilkan factor kelas)
pred <- predict(model, test_x)
table(pred)

# 6. evaluasi
table(Predicted = pred, Actual = test_x$rate_label)
accuracy <- mean(pred == test_x$rate_label)
accuracy
plot(model)

# confusion matrix
cm <- table(Predicted = pred, Actual = test$rate_label)

TP <- cm["High", "High"]
FP <- cm["High", "Low"]
FN <- cm["Low",  "High"]
TN <- cm["Low",  "Low"]

precision <- TP / (TP + FP)
recall    <- TP / (TP + FN)
f1_score  <- 2 * (precision * recall) / (precision + recall)

precision
recall
f1_score
install.packages("ggplot2")
library(ggplot2)
install.packages("dplyr")
library(dplyr)

data_year <- data %>%
  group_by(Year) %>%
  summarise(avg_suicide_rate = mean(Suicide.Rate, na.rm = TRUE))

install.packages("ggplot2")
library(ggplot2)
ggplot(data_year, aes(x = Year, y = avg_suicide_rate)) +
  geom_line() +
  geom_point() +
  labs(
    title = "Global Suicide Rate Trend (2000–2021)",
    x = "Year",
    y = "Average Suicide Rate"
  ) +
  theme_minimal()

sort(unique(data$Country.Name))

library(dplyr)

country_avg <- data %>%
  group_by(Country.Name) %>%
  summarise(
    avg_suicide_rate = mean(Suicide.Rate, na.rm = TRUE)
  )

top10_high <- country_avg %>%
  arrange(desc(avg_suicide_rate)) %>%
  slice(1:10)

top10_low <- country_avg %>%
  arrange(avg_suicide_rate) %>%
  slice(1:10)

library(ggplot2)

ggplot(top10_high,
       aes(x = reorder(Country.Name, avg_suicide_rate),
           y = avg_suicide_rate)) +
  geom_col(fill = "firebrick") +
  coord_flip() +
  labs(
    title = "Top 10 Countries with Highest Average Suicide Rate (2000–2021)",
    x = "Country",
    y = "Average Suicide Rate"
  ) +
  theme_minimal()

ggplot(top10_low,
       aes(x = reorder(Country.Name, avg_suicide_rate),
           y = avg_suicide_rate)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  labs(
    title = "Top 10 Countries with Lowest Average Suicide Rate (2000–2021)",
    x = "Country",
    y = "Average Suicide Rate"
  ) +
  theme_minimal()

sort(unique(data$Country.Name))

