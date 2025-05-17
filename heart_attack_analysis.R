heart_data <- read.csv("C:\\Users\\Harold goh\\Downloads\\heartAttack\\Medicaldataset.csv") #load data

#Select two numerical data
age <- heart_data$Age 
blood_pressure <- heart_data$Systolic.blood.pressure

install.packages("modeest")
library(modeest)

#Age
summary(age)
age_mean <- mean(age)
mfv(age)
age_sd <- sd(age)
var(age)
diff(range(age))
IQR(age)
cv <- (age_sd / age_mean) 
print(cv)

#Systolic Blood Pressure
summary(blood_pressure)
bp_mean <- mean(blood_pressure)
print(bp_mean)
mfv(blood_pressure)
bp_sd <- sd(blood_pressure)
print(bp_sd)
var(blood_pressure)
range(blood_pressure)
IQR(blood_pressure)
bp_cv <- (bp_sd / bp_mean)
print(bp_cv)

boxplot(age, main="Boxplot of Age", ylab="age", col="skyblue")
hist(age, main="Histogram of resting pulse rate", xlab="Pulse rate", ylab="number of students", col="skyblue")

boxplot(blood_pressure, main="Boxplot of Blood Pressure", ylab="Blood Pressure")
hist(blood_pressure, main="Histogram of resting pulse rate", xlab="Pulse rate", ylab="number of students", col="skyblue")

#Graphs
library(ggplot2)
# Boxplot for Age
p1 <- ggplot(heart_data, aes(x = age)) +
  geom_boxplot() +
  ggtitle("Boxplot of Age") +
  xlab("Age")
p1

age_box_plot <- boxplot(age, main="Boxplot of Age", ylab="Age", col='pink')

# Histogram for Age
p2 <- ggplot(heart_data, aes(x = age)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "white") +
  scale_y_continuous(breaks = seq(0, 400, by = 50)) +  # Customize y-axis
  scale_x_continuous(breaks = seq(10, 100, by = 10)) +   # Optional: x-axis
  labs(title = "Histogram of Age",
       x = "Age",
       y = "Frequency") +
  theme_minimal()
p2

age_hist <- hist(age, main="Histogram of age", xlab="Age", ylab="Number of people", col="pink")



# Boxplot for Systolic blood pressure
p3 <- ggplot(heart_data, aes(x = blood_pressure)) +
  geom_boxplot() + 
  ggtitle("Boxplot of Systolic blood pressure") +
  xlab("Systolic blood pressure")
p3

bp_box_plot <- boxplot(blood_pressure, main="Boxplot of Systolic Blood Pressure", ylab="Systolic Blood Pressure", col='lightblue')

# Histogram for Systolic blood pressure
p4 <- ggplot(heart_data, aes(x = blood_pressure)) +
  geom_histogram(binwidth = 10, fill = "skyblue", color = "white") +
  scale_y_continuous(breaks = seq(0, 250, by = 25)) + 
  scale_x_continuous(breaks = seq(40, 220, by = 10)) +  
  labs(title = "Histogram of Systolic Blood Pressure",
       x = "Systolic blood pressure",
       y = "Frequency") +
  theme_minimal()
p4

bp_hist <- hist(blood_pressure, main="Histogram of Systolic Blood Pressure", ylab="Number of people", xlab="Systolic Blood Pressure", col="lightblue")


# Display plots
install.packages("gridExtra")
library(gridExtra)
grid.arrange(p1, p2, p3, p4, nrow = 2)
grid.arrange(age_box_plot, age_hist, bp_box_plot, bp_hist, nrow=2)

bp_68 <- sum(blood_pressure >= (bp_mean - bp_sd) & blood_pressure <= (bp_mean + bp_sd))
bp_95 <- sum(blood_pressure >= (bp_mean - 2*bp_sd) & blood_pressure <= (bp_mean + 2*bp_sd))
bp_997 <- sum(blood_pressure >= (bp_mean - 3*bp_sd) & blood_pressure <= (bp_mean + 3*bp_sd))
bp_68

proportion_1_sd <- round(bp_68 / length(blood_pressure), 2) * 100
proportion_2_sd <- round(bp_95 / length(blood_pressure), 2) * 100
proportion_3_sd <- round(bp_997 / length(blood_pressure), 2) * 100

print(proportion_1_sd)
print(proportion_2_sd)
print(proportion_3_sd)

qqnorm(blood_pressure, main = "QQ-Plot of Systolic Blood Pressure")
qqline(blood_pressure, col = "red")

shapiro.test(blood_pressure)

# Load ggplot2 package
library(ggplot2)

# Create scatter plot with regression line
ggplot(heart_data, aes(x = age, y = blood_pressure)) +
  geom_point(color = "blue", size = 1) +                   
  geom_smooth(method = "lm", color = "red", se = FALSE) + 
  labs(title = "Scatter Plot of Age vs Systolic Blood Pressure",
       x = "Age",
       y = "Systolic Blood Pressure") +
  theme_minimal()

#Pearson's correlation
Sx_squared <- sum((age - age_mean)^2) #Sxx
Sy_squared <- sum((blood_pressure - bp_mean)^2) #Syy
Sxy <- sum((age - age_mean) * (blood_pressure - bp_mean))
r_manual <- Sxy / sqrt(Sx_squared * Sy_squared)
print(r_manual)

correlation <- cor(age, blood_pressure, method="pearson")
print(correlation)







