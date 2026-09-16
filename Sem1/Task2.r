
#________Task 2.1 - Suggested number of participants________

# Loading in the data
data <- read.csv("T1_data.csv")

# Make Group categorical
data$Group <- factor(data$Group)

# Selecting only patients with disease, since the trial will only contain
# patients with prion disease
disease_data <- data[data$Group == "Disease", ]

# Taking the natural logarithm of every value in the NfL column
disease_logdata <- log(disease_data$NfL)

# Finding out the standard deviation of the data
sd_NfL <- sd(disease_logdata)
sd_NfL # =0.745827

# Study wants to detect 20% reduction in the geometric mean of NfL.
# We can take log(0.8) which gives us -0.2231.
# So this means we want to detect a difference of 0.223 on the log scale.
# We use this information to make a power t-test:

power.t.test(
  delta = abs(log(0.8)),
  sd = sd_NfL,
  sig.level = 0.05,
  power = 0.8,
  type = "two.sample",
  alternative ="two.sided"
)

# Test above gave n = 176.33. We round up and get 177 per arm.
# Since this is for one arm, we take 177 x 2 and get a total of 354 participants,
# i.e. we have 177 patients in the treatment group and 177 patients in the control group.



#________Task 2.4 - Extracting data________

library(CM2018rpackage)
set.seed(123)
trial_data <- hpd_trial_data(177)

# Inspecting results
trial_data
head(trial_data)
str(trial_data)
names(trial_data)
summary(trial_data)



#________Task 2.5 - The statistical analysis________

# Make Group categorical
trial_data$Group <- factor(trial_data$Group)
# Log-transform NfL
trial_data$logNfL <- log(trial_data$NfL)
# Check number of participants in each group
table(trial_data$Group)

# Geometric mean NfL for each group
tapply(
  trial_data$NfL,
  trial_data$Group,
  function(x) exp(mean(log(x)))
)

# Visualizing log-transformed data
boxplot(
  logNfL ~ Group,
  data = trial_data,
  xlab = "Group",
  ylab = "log(NfL)",
  main = "log(NfL) by treatment group"
)

# Checking the data with Shapiro-Wilk:
shapiro.test(trial_data$logNfL[trial_data$Group == 0])
shapiro.test(trial_data$logNfL[trial_data$Group == 1])

# Now we do the analysis, using Welch two-sample t-test
# We assume group 0 is control and 1 is the treated group.

control <- trial_data$logNfL[trial_data$Group == 0]
treatment <- trial_data$logNfL[trial_data$Group == 1]

t_test <- t.test(
  treatment,
  control,
  alternative = "two.sided",
  conf.level = 0.95
)

t_test

# Lets turn this into something more readable:

# Calculating geometric means
GM_control <- exp(mean(control))
GM_treatment <- exp(mean(treatment))
cat("GM (control group):", GM_control, "\n")
cat("GM (treated group):", GM_treatment, "\n")

# And the ratio between them
GM_ratio <- GM_treatment / GM_control
cat("GM Ratio:", GM_Ratio, "\n")

# We can aslo convert to percentage:
percent_reduction <- (1 - GM_ratio) * 100
cat("Percentage reduction:", percent_reduction, "\n")
