library(readxl)
data <- read_excel("Sweat_Data.xlsx",col_names = FALSE)
data <- data.frame(data)
# Define constants
n <- 20
p <- 3
alpha <- 0.05 # Significance level for 95% confidence
x_1 <- data[,1]
x_2 <- data[,2]
x_3 <- data[,3]
# bound rate
b_1 <- sqrt(var(x_1)/n)
b_2 <- sqrt(var(x_2)/n)
b_3 <- sqrt(var(x_3)/n)
b<-c(b_1,b_2,b_3)
# mean
m_1 <- mean(x_1)
m_2 <- mean(x_2)
m_3 <- mean(x_3)
m<-c(m_1,m_2,m_3)
# rate for Bonferroni
t_crit <- qt(1 - alpha / (2 * p), df = n - 1)  # t critical value
# calculate
lower_bon = m-b*t_crit
upper_bon = m+b*t_crit
# rate for Simultaneous
f = qf(1-alpha,df1=p,df2=n-p)
f_rate = sqrt(((p*(n-1))/(n-p))*f)
#calculate
lower_si = m-b*f_rate
upper_si = m+b*f_rate
#calculate difference of lower bound: simultaneous - bonferroni
lower_difference = lower_si - lower_bon
print(lower_difference)
print("Simultaneous method has smaller lower bound.")
#calculate difference of upper bound: simultaneous - bonferroni
upper_difference = upper_si - upper_bon
print(upper_difference)
print("Simultaneous method has larger upper bound.")
print("In all three datasets of X, simultaneous method has wider intervals than the bonferroni method.")