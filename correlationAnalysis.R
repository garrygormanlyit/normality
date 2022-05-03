?beaver2
bevear_data <- beaver2
str(bevear_data)
head(bevear_data)



# pairs function to look at the correlation between variables
pairs(bevear_data, labels = colnames(bevear_data), 
      main = "Beavers dataset correlation plot")

# we are examining body temp and activity
# so the variables need converting first
# you can add ordered = TRUE if 1 is better than 2
bevear_data$activity <- factor(bevear_data$activ, labels = c("no", "yes"))

#analyse correlation of activity against temp
plot(bevear_data$activity, bevear_data$temp)

hist(bevear_data$activ, 
     main = "Beaver data Activity", col = "red")

# plot the distribution of yes and no against temperature
library("lattice")
attach(bevear_data)
histogram(~ temp | activity,
          data = bevear_data,
          main = "Distribution of beaver activity data",
          xlab = "Temp in degrees C",
          ylab = "activity")

tapply(temp, activity, median) # could be mean either
# report that the the median of temp no is 37.095 suggesting that no is normally distributed
# report that the the median of temp yes is 37.915 suggesting that yes is normally distributed

# Q-Q plot of temperature
qqnorm(bevear_data$temp, main = "Q-Q plot of first dataset variable")
qqline(bevear_data$temp)

# Q-Q plot 
with(bevear_data, {qqnorm(temp[activity == "no"],
                          main = "Inactive data") 
  qqline(temp[activity == "no"])})

with(bevear_data, tapply(temp, activity, shapiro.test))
# activity no is normally distributed as p-value of 0.1231
# activity yes is normally distributed as p-value of 0.5583