
# Loading in the data
data <- read.csv("T1_data.csv")
# data #This is to print the data

str(data) #The structure of the data
summary (data)

# Make Group categorical
data$Group <- factor(data$Group)

#________Visual exploration of raw data________

# Shaprio tests on raw data to test for normality
shapiro.test(data$NfL[data$Group == "Healthy"])
shapiro.test(data$NfL[data$Group == "Disease"])
shapiro.test(data$Tau[data$Group == "Healthy"])
shapiro.test(data$Tau[data$Group == "Disease"])

# QQ plots on the raw data. They show the data is somewhat right-skewed, which indicates that values are logarithmic.
par(mfrow = c(2,2))

qqnorm(data$NfL[data$Group == "Healthy"],
main = "NfL Healthy")
qqline(data$NfL[data$Group == "Healthy"])

qqnorm(data$NfL[data$Group == "Disease"],
main = "NfL Disease")
qqline(data$NfL[data$Group == "Disease"])

qqnorm(data$Tau[data$Group == "Healthy"],
main = "Tau Healthy")
qqline(data$Tau[data$Group == "Healthy"])

qqnorm(data$Tau[data$Group == "Disease"],
main = "Tau Disease")
qqline(data$Tau[data$Group == "Disease"])

# Boxplots show the distributions and differences between Healthy and Disease.
# show the data is somewhat right-skewed, which indicates that values are logarithmic.
par(mfrow = c(1,2))

boxplot(NfL ~ Group, data = data,
main = "NfL by Group",
xlab = "Group",
ylab = "NfL (pg/mL)")

stripchart(NfL ~ Group, data = data,
vertical = TRUE,
method = "jitter",
add = TRUE,
pch = 16)

boxplot(Tau ~ Group, data = data,
main = "Tau by Group",
xlab = "Group",
ylab = "Tau (pg/mL)")

stripchart(Tau ~ Group, data = data,
vertical = TRUE,
method = "jitter",
add = TRUE,
pch = 16)

# Wilcoxon rank-sum test on raw values
# Wilcoxon rank-sum tests do not require normally distributed data
wilcox.test(NfL ~ Group, data = data, exact = FALSE)
wilcox.test(Tau ~ Group, data = data, exact = FALSE)

# t-tests
t.test(NfL ~ Group, data = data)
t.test(Tau ~ Group, data = data)

# Evidence that biomarkers may be better described on logarithmic scale:
# - Raw values are all positive, and values are right-skewed.
# - Means are larger thatn the medians
# - Shapiro-Wilk tests show clear non-normality for several groups
# - Distributions have somewhat large values

# We do the test on a log-based scale:

# Shapiro-Wilk tests after log transformation
shapiro.test(log(data$NfL[data$Group == "Healthy"]))
shapiro.test(log(data$NfL[data$Group == "Disease"]))
shapiro.test(log(data$Tau[data$Group == "Healthy"]))
shapiro.test(log(data$Tau[data$Group == "Disease"]))

# QQ plots after log transformation
graphics.off()     # Reset graphics device
par(mfrow = c(2,2))

qqnorm(log(data$NfL[data$Group == "Healthy"]),
main = "log(NfL) Healthy")
qqline(log(data$NfL[data$Group == "Healthy"]))

qqnorm(log(data$NfL[data$Group == "Disease"]),
main = "log(NfL) Disease")
qqline(log(data$NfL[data$Group == "Disease"]))

qqnorm(log(data$Tau[data$Group == "Healthy"]),
main = "log(Tau) Healthy")
qqline(log(data$Tau[data$Group == "Healthy"]))

qqnorm(log(data$Tau[data$Group == "Disease"]),
main = "log(Tau) Disease")
qqline(log(data$Tau[data$Group == "Disease"]))

# Boxplots of log-transformed data
par(mfrow = c(1,2))

boxplot(log(NfL) ~ Group, data = data,
main = "log(NfL) by Group",
xlab = "Group",
ylab = "log(NfL)")

stripchart(log(NfL) ~ Group, data = data,
vertical = TRUE,
method = "jitter",
add = TRUE,
pch = 16)

boxplot(log(Tau) ~ Group, data = data,
main = "log(Tau) by Group",
xlab = "Group",
ylab = "log(Tau)")

stripchart(log(Tau) ~ Group, data = data,
vertical = TRUE,
method = "jitter",
add = TRUE,
pch = 16)

par(mfrow = c(1,1))

#t-test on log-transformed data

t.test(log(NfL) ~ Group, data = data)
t.test(log(Tau) ~ Group, data = data)

# Results and conclusions:
# When performing log() on the data, and then do the tests, wee se clear normality.
# The conclusion is that the data i log-normal 
