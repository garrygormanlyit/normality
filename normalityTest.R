getwd()

normal_data <- read.csv("NormalData.csv")
non_normal_data <- read.csv("nonnormaldata.csv")

opar <- par(no.readonly = TRUE)
par(mfrow = c(1,2))

# histograms to determine normality
hist(normal_data$x, 
     main = "Frequency chart of data distribution (parametric data)", col = "red")

hist(non_normal_data$x, 
     main = "Frequency chart of data distribution (non-parametric data)", col = "red")

#QQ plot (quartile quartile plot) to help decide on normality
# check if the points fall along a diagonal line
# if points are on the diagonal line, the data is normally distributed
# otherwise, the data is not normally distributed
qqnorm(normal_data$x, main = "Q-Q plot of first dataset variable")
qqline(normal_data$x)

qqnorm(non_normal_data$x, main = "Q-Q plot of second dataset variable")
qqline(non_normal_data$x)

# Shapiro-Wilk test to definitively determine normality
# if p-value > 0.05 - normally distributed, otherwise, not normally distributed
# test also presents a w statistic: we use the p-value
shapiro.test(normal_data$x)
# this shows that the data is normally distributed with a p-value of 0.5356

shapiro.test(non_normal_data$x)
# this shows that the data is not normally distributed with a p-value of 2.2 *10**-16

# kolmogrov - smirnov test
# if p-value is > 0.05 - normally distributed, otherwise, not normally distributed
ks.test(normal_data$x, "pnorm")
# p-value is 0.1178 therefore the data is normally distributed
ks.test(non_normal_data$x, "pnorm")
# p-value is 2.2*10**-16 therefore the data is not normally distributed

# dont do this
# convert non-parametric data to parametric
# log transform

log_nonnormal_data <- log10(non_normal_data$x)

# if badly skewed then use this
neg_skewed_log_non_normal <- log10(max(non_normal_data$x + 1)) - non_normal_data$x
