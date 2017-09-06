###Chapter 1 Introduction to t-tests

###Understanding the t-distribution
# Generate a vector of 100 values between -4 and 4
x <- seq(-4, 4, length = 100)

#Simulate the t-distribution
y_1 <- dt(x, df = 4)
y_2 <- dt(x, df = 6)
y_3 <- dt(x, df = 8)
y_4 <- dt(x, df = 10)
y_5 <- dt(x, df = 12)

# Plot the t-distributions
plot(x, y_1, type = "l", lwd = 2, xlab = "T value", ylab = "Density", main = "Comparison of t-distributions")
lines(x, y_2, col = "red")
lines(x, y_3, col = "orange")
lines(x, y_4, col = "green")
lines(x, y_5, col = "blue")

# Add a legend
legend("topright", c("df = 4", "df = 6", "df = 8", "df = 10", "df = 12"), title = "T distributions", col = c("black", "red", "orange", "green", "blue"), lty = 1)

###The Working Memory Dataset
# Take a look at the dataset
wm

# Create training subset of wm
wm_t <- subset(wm, wm$train == "1")

# Summary statistics 
describe(wm_t)

# Create a boxplot with pre- and post-training groups 
boxplot(wm_t$pre, wm_t$post, main = "Boxplot", 
        xlab = "Pre- and Post-Training", ylab = "Intelligence Score", 
        col = c("red", "green"))

###Perform a Dependent t-test
## The training subset, wm_t, is available in your workspace

# Define the sample size
n <- nrow(wm_t)

# Mean of the difference scores
mean_diff <- sum(wm_t$gain) / n

# Standard deviation of the difference scores
sd_diff <- sqrt(sum((wm_t$gain - mean_diff)^2) / (n - 1))

# Observed t-value
t_obs <- mean_diff / (sd_diff / sqrt(n))

# Print observed t-value
t_obs

###Perform a Dependent t-test (2)
## The variables from the previous exercise are preloaded

# Compute the critical value
t_crit <- qt(0.975, df = 79)

# Print the critical value
t_crit

# Print the observed t-value to compare 
t_obs

# Compute Cohen's d
cohens_d <- mean_diff / sd_diff

# View Cohen's d
cohens_d

###Let R Do the Dirty Work
# wm is already loaded in

# Conduct a paired t-test using the t.test function
t.test(wm_t$post, wm_t$pre, paired = T)

# Calculate Cohen's d
cohensD(wm_t$post, wm_t$pre, method = "paired")

###Chapter 2 Independent t-tests

###Preliminary Statistics
# The dataset wm_t is already loaded. Familiarize yourself with it.
wm_t

# Create subsets for each training time
wm_t08 <- subset(wm_t, wm_t$cond == "t08")
wm_t12 <- subset(wm_t, wm_t$cond == "t12")
wm_t17 <- subset(wm_t, wm_t$cond == "t17")
wm_t19 <- subset(wm_t, wm_t$cond == "t19")

# Summary statistics of the change in training scores before and after exercise
describe(wm_t08)
describe(wm_t12)
describe(wm_t17)
describe(wm_t19)

# Create a boxplot of the different training times
ggplot(wm_t, aes(x = cond, y = gain, fill = cond)) + geom_boxplot()

# Levene's test
leveneTest(wm_t$gain ~ wm_t$cond)

###Perform an Independent t-test
# The subsets wm_t08 and wmt_19 are still loaded in

# Calculate mean difference by subtracting the gain for t08 by the gain for t19
mean_t08 <- mean(wm_t08$gain)
mean_t19 <- mean(wm_t19$gain)
mean_diff <- mean_t19 - mean_t08

# Calculate degrees of freedom
n_t08 <- dim(wm_t08)[1]
n_t19 <- dim(wm_t19)[1]
df <- n_t08 + n_t19 - 2

# Calculate the pooled standard error
var_t08 <- var(wm_t08$gain)
var_t19 <- var(wm_t19$gain)
se_pooled <- sqrt((var_t08 / n_t08) + (var_t19 / n_t19))

###Perform an Independent t-test (2)
# All variables from the previous exercises are preloaded in your workspace
# Type ls() to see them

# Calculate the t-value
t_value <- mean_diff/se_pooled

# Calculate p-value
p_value <- 2*(1-pt(t_value,df=df))

# Calculate Cohen's d
sd_t08 <- sd(wm_t08$gain)
sd_t19 <- sd(wm_t19$gain)
pooled_sd <- (sd_t08 + sd_t19) / 2
cohens_d <- mean_diff/pooled_sd

###Independent t-tests, the easy way
# The subsets wm_t08 and wm_t19 are already pre-loaded in the work space

# Conduct an independent t-test 
t.test(wm_t19$gain, wm_t08$gain, var.equal = T)

# Calculate Cohen's d
cohensD(wm_t19$gain, wm_t08$gain, method = "pooled")

