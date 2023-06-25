#sample size, n = 6704
#mean salary of male is more than the mean salary of female
#H0: miu1 = miu2
#H1: miu1 != miu2
#signifance level 5%
#variables: gender, salary

male_salaries <- Salary_Data$Salary[Salary_Data$Gender == "Male"]
female_salaries <- Salary_Data$Salary[Salary_Data$Gender == "Female"]

#consider variance not equal
#find mean salaries of each and standard deviations
#find t value
male_salaries_mean <- mean(male_salaries, na.rm = TRUE)
female_salaries_mean <- mean(female_salaries, na.rm = TRUE)

male_salaries_sd <- sd(male_salaries, na.rm = TRUE)
female_salaries_sd <- sd(female_salaries, na.rm = TRUE)

xbar1 = male_salaries_mean
xbar2 = female_salaries_mean

s1 = male_salaries_sd
s2 = female_salaries_sd

n1 = length(male_salaries)
n2 = length(female_salaries)

t0 = (xbar1-xbar2-0)/(sqrt((s1^2/n1)+(s2^2/n2)))

v = ((s1^2/n1)+(s2^2/n2))^2/((((s1^2/n1)^2)/(n1-1))+(((s2^2/n2)^2)/(n2-1)))

alpha = 0.05

t.alpha = qt(alpha/2, floor(v))
print(t.test(male_salaries, female_salaries))

#conclusion
#calculated t-value (10.474) is greater than the t-critical value (-1.96033)
#it falls in the rejection region. 
#reject the null hypothesis 
#conclude that there is evidence to suggest that the mean salary of males differs significantly from the mean salary of females at the 5% significance level.

#Correlation
#variables: age, yearsofexperience
#random sample of 100 data is chosen
#95% confidence level
age <- na.omit(Salary_Data$ï..Age)[1:100]
years_of_experience <- na.omit(Salary_Data$Years.of.Experience)[1:100]
correlation = cor(age, years_of_experience)
plot(age, years_of_experience, xlab="Age", ylab="Years Of Experience")

#correlation coefficient r = 0.982176
#conclusion: Positive correlation relationship between age and years of experience
print("Correlation")
print(cor.test(age, years_of_experience))

#H0: rho = 0 (no linear correlation)
#H1: rho != 0 (linear correlation exists)
#Looking at the result above we could say that we reject the null hypothesis since t0 = 51.728 > t0.025,98=
#  1.960. Therefore, there is sufficient evidence of a linear relationship between age and years of experience at the 5% level of significance.


#Regression
#variables: age, salary
#want to check if salary can predict age at 0.05 level of significance
#dependent variable y is age
#independent variable x is salary
#random sample of 100 data is chosen
salary <- na.omit(Salary_Data$Salary)[1:100]
model <- lm(age~salary)
print(model)
#Coefficients:
#(Intercept)       salary  
#2.357e+01    1.376e-04
print(summary(model))

plot(salary, age, xlab = "Salary (x)", ylab = "Age (y)")
abline(model)

#independence chi square test
#variables: gender: male/female, education level 4 categories
#H0: Education level is independent of gender
#H1: Education level is not independent of gender
male_df <- subset(Salary_Data, Gender == "Male")
female_df <- subset(Salary_Data, Gender == "Female")
male_counts <- table(male_df$Education.Level)
female_counts <- table(female_df$Education.Level)

male_counts_df <- data.frame(EducationLevel = names(male_counts), Count = as.vector(male_counts))
female_counts_df <- data.frame(EducationLevel = names(female_counts), Count = as.vector(female_counts))

#Bsc, Msc, High School, Phd
formatted_male_count_df <- c(male_counts_df$Count[2] + male_counts_df$Count[3], male_counts_df$Count[4], male_counts_df$Count[5] + male_counts_df$Count[6], male_counts_df$Count[7] + male_counts_df$Count[8])
formatted_female_count_df <- c(female_counts_df$Count[1] + female_counts_df$Count[2], female_counts_df$Count[3], female_counts_df$Count[4] + female_counts_df$Count[5], female_counts_df$Count[6] + male_counts_df$Count[8])

d <- data.frame(formatted_male_count_df, formatted_female_count_df)
result <- chisq.test(d, correct = FALSE)
print(result)

alpha <- 0.05
x2.alpha <- qchisq(alpha, df= 3, lower.tail = FALSE)
print(x2.alpha)

#conclusion: X^2 value = 284.04 and critical value X^2(0.05, 3) = 7.814728
#x squared value > critical value we reject null hypothesis.
#Education level is not independent of gender

