#sample size, n = 6704
#mean salary of male is more than the mean salary of female
#H0: miu1 = miu2
#H1: miu1 != miu2
#signifance level 5%
n = nrow(Salary_Data)
male_salaries <- Salary_Data$Salary[Salary_Data$Gender == "Male"]
female_salaries <- Salary_Data$Salary[Salary_Data$Gender == "Female"]



