
# Loading in the data
data <- read.csv("T1_data.csv")
data #This is to print the data

str(data) #The structure of the data

summary (data)

# Make Group categorical
data$Group <- factor(data$Group)

# Shaprio tests to test for normality
shapiro.test(data$NfL[data$Group == "Healthy"])
shapiro.test(data$NfL[data$Group == "Disease"])
shapiro.test(data$Tau[data$Group == "Healthy"])
shapiro.test(data$Tau[data$Group == "Disease"])

#Wilcoxon rank-sum test on raw values
wilcox.test(NfL ~ Group, data = data, exact = FALSE)
wilcox.test(Tau ~ Group, data = data, exact = FALSE)

# QQ plots on the raw data. They show the data is somewhat right-skewed, which indicates that values are logarithmic.
qqnorm(data$NfL)
qqline(data$NfL)
qqnorm(data$Tau)
qqline(data$Tau)

# t-tests
t.test(NfL ~ Group, data = data)
t.test(Tau ~ Group, data = data)

# Evidence that biomarkers may be better described on logarithmic scale:
# - Raw values are all positive, and values are right-skewed.
# - Means are larger thatn the medians
# - Shapiro-Wilk tests show clear non-normality for several groups
# - Distributions have somewhat large values

# We try and do the tests on a log-based scale:

shapiro.test(log(data$NfL[data$Group == "Healthy"]))
shapiro.test(log(data$NfL[data$Group == "Disease"]))
shapiro.test(log(data$Tau[data$Group == "Healthy"]))
shapiro.test(log(data$Tau[data$Group == "Disease"]))

hist(log(data$NfL))
hist(log(data$Tau))

qqnorm(log(data$NfL))
qqline(log(data$NfL))
qqnorm(log(data$Tau))
qqline(log(data$Tau))

t.test(log(NfL) ~ Group, data = data)
t.test(log(Tau) ~ Group, data = data)

# Results and conclusions:
# When performing log() on the data, and then do the tests, wee se clear normality.
# The conclusion is that the data i log-normal 