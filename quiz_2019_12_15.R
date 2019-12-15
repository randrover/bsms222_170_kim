# 2019/11/6

# Q1. Please write down the two-line commands for Monte Carlo simulation. You will pick up one ball from the box, called ‘box’, and repeat 10,000 times.

B <- 10000
replicate(B, sample(box, 1))

# Q2. Please write down the command to compute the probability of each category from Q1.

events <- replicate(B, sample(box, 1))
tab <- table(events)
prop.table(tab)

# Q3. Please write down the command to set up the random seed for your simulation. You will use today's date (1106) for the seed number.

set.seed(1106)


# 2019/11/11

# Q1. Let’s imagine there are 200 scores for midterm exams. The score ranges from 0 to 100. Please write down the command to compute the probability of having the score greater than 85.

x <- sample(0:100, 200, replace = TRUE)
mean(x>85)

# Q2. There would be two ways for the Q1. Please write down the other command you can use for Q1.

m <- mean(x)
s <- sd(x)
1-pnorm(85, m, s)

# Q3. From the dataset above, please write down the command to compute the probability of having the score between 20 and 55.

pnorm(55, m, s) - pnorm(20, m, s)


# 2019/11/13

# Q1. Let’s assume that we play a game that results in two outcomes (A and B). The probability of A is 0.335. If you win for A, you will get $2. If you win for B, you will lose $1. There are 2000 people playing this game. Please write down the command for the Monte Carlo simulation generate 10000 outcomes. You can define any function name for this. You will need to use `set.seed(1)`

set.seed(1)
p <- 0.335
B <- 10000
bbc <- function(){
  draws <- sample(c(2, -1), 2000, prob = c(p, 1-p) , replace = TRUE)
  sum(draws)
}
outcomes <- replicate( B, bbc() )

# Q2. Please write down the command to compute E[X].

2000*(p*2 + (1-p)*(-1))

# Q3. When you create a random variable Y for the Q1, what is the standard error of Y?

sqrt(2000)*abs(2-(-1))*sqrt(p*(1-p))


# 2019/11/20

# Q1. Let’s assume that we play a game that results in two outcomes (A and B). The probability of A is 0.6. If you win for A, you will get $1. If you win for B, you will lose 3. There are 2000 people playing this game. Please write down the command for the Monte Carlo simulation generate 10000 outcomes. You can define any function name for this. You will need to use `set.seed(1)`

set.seed(1)
p <- 0.6
B <- 10000
bbq <- function(){
  draws <- sample(c(1, -3), 2000, prob = c(p, 1-p) , replace = TRUE)
  sum(draws)
}
outcomes <- replicate( B, bbq() )

# Q2. From Q1, please write down the command to compute the standard error.

sqrt(2000)*4*sqrt(p*(1-p))

# Q3. Please write down the command to compute the probability of the game losing money using the approximation from Q1.

mean(outcomes<0)


# 2019/11/25

# Q1. Please compute the standard error for the sample size of 2000 and p=0.4.
# 개수면 곱하고 비율이면 나눈다? p_hat이 비율인가

p <- 0.4
sqrt(2000)*sqrt(p*(1-p))

# Q2. In a situation, we had 20 blue and 10 red so X̄ = 0.48 and what is our estimate of standard error?

x_hat <- 0.48
sqrt(x_hat*(1-x_hat)/30)

# Q3. From Q2, what is the probability that we are within 1% from the expected value p (using pnorm)?

se <- sqrt(x_hat*(1-x_hat)/30)
pnorm(0.01/se) - pnorm(-0.01/se)


# 2019/11/27

# Q1. Please compute a confidence interval for the sample size of 2000 and p=0.4.

n<-2000
p<-0.4

x <- sample(c(0, 1), size = n, replace = TRUE, prob = c(1-p, p))
x_hat <- mean(x)
se_hat <- sqrt(x_hat * (1 - x_hat) / n)
c(x_hat - 1.96 * se_hat, x_hat + 1.96 * se_hat)

# Q2. Please write a command to run a Monte Carlo simulation (B=10,000) for Q1. 

B<-10000
intervals <- replicate(B, {
  x <- sample(c(0,1), size = n, replace = TRUE, prob = c(1-p, p))
  x_hat <- mean(x)
  se_hat <- sqrt(x_hat * (1 - x_hat) / n)
  c(x_hat - 1.96 * se_hat, x_hat + 1.96 * se_hat)
})
intervals

# Q3. Please make a plot for the first 100 confidence intervals.

library(ggplot2)
library(dplyr)
library(hrbrthemes)

d_plot_i <- as.data.frame(intervals)
Dp <- d_plot_i %>%
   rownames_to_column %>%
   gather(var, value, -rowname) %>%
   spread(rowname, value)
Dp$var=as.numeric(gsub("V", "", Dp$var))
Dp %>% filter(var < 101) %>% ggplot() +
  geom_linerange(aes(x=var, ymin=`1`, ymax=`2`))

Dp %>% filter(var < 101) %>% rename(start = '1', end = '2') %>% ggplot() +
  geom_linerange(aes(x=var, ymin = start, ymax = end)) +
  coord_flip()

# column name이 숫자면 ''을 붙여야 하나 봄?

intervals_d <- data.frame(n = 1:10000, start = intervals[1,], end = intervals[2,])
head(intervals_d, 100) %>% ggplot() +
  geom_segment( aes(x=n, xend=n, y=start, yend=end), color="blue") +
  geom_point( aes(x=n, y=start), color=rgb(0.2,0.7,0.1,0.5), size=1 ) +
  geom_point( aes(x=n, y=end), color=rgb(0.7,0.2,0.1,0.5), size=1 ) +
  coord_flip()



# 범위 지정..
geom_linerange()
geom_segment()
geom_errorbar()
geom_errorbarh()