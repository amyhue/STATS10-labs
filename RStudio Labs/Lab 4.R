n = 100
p = 0.34

E_X = n*p
E_X

SD_X = sqrt(n*p*(1-p))
SD_X

dbinom(27, size = 100, prob = 0.34)

# assume inclusive
pbinom(34, size = n, prob = p) - 
  pbinom (14, size = n, prob = p)

# Condition 1 of CLT
## Large population size relative to sample size
## Have a Simple Random Sample
## n * p and n* (1-p) > 10

n*p
n*(1-p)

pnorm(34, mean = E_X, sd = SD_X) -
  pnorm(15, mean = E_X, sd = SD_X)

pbinom(5, size = 20, prob = 0.5)


grads <- read.csv(file = "new_recent_grads.csv")
grads <- na.omit(grads)

N <- nrow(grads)
N

mean(grads$Median)
mean(grads$Major_category == "Engineering")

set.seed(1256)
sample_indices <- sample(N, size = 100, replace = FALSE)
grads_sample <- grads[sample_indices, ]

head(grads_sample)

mean(grads_sample$Median)

mean(grads_sample$Major_category == "Engineering")

n = 100
p = mean(grads$Major_category == "Engineering")
n*p
n*(1-p)

#p-hat ~ N(p, sqrt(p*(1-p) / n))
#p-hat ~ N(p-hat, sqrt(p-hat * (1-p-hat) / n))

#90% CI
p_hat <- mean(grads_sample$Major_category == "Engineering")
SE_p_hat <- sqrt(p_hat * (1 - p_hat) / n)
z_0.95 <- qnorm(0.95)
p_hat + c(-1, 1) *SE_p_hat *z_0.95

#95% CI

z_0.975 <- qnorm(0.975)
p_hat + c(-1, 1) *SE_p_hat *z_0.975

# 99% CI
z_0.995 <- qnorm(0.995)
p_hat + c(-1, 1) *SE_p_hat *z_0.995

p

#Exercise 3
# We first create objects for common quantities we will use for this exercise. 
n <- 50 # The sample size 
N <- nrow(grads) # The population size 
M <- 1000 # Number of samples/repetitions 
# Create vectors to store the simulated proportions from each repetition. 
phats <- numeric(M) # for sample proportions 
# Set the seed for reproducibility 
set.seed(123) 
# Always set the seed OUTSIDE the for loop. 
# Now we start the loop. Let i cycle over the numbers 1 to 1000 (i.e., iterate 1000 times). 
for(i in seq_len(M)){ 
  # The i-th iteration of the for loop represents a single repetition. 
  # Take a simple random sample of size n from the population of size N. 
  index <- sample(N, size = n) 
  # Save the random sample in the sample_i vector. 
  sample_i <- grads[index, ] 
  # Compute the proportion of the i-th sample with engineering major. 
  phats[i] <- mean(sample_i$ Major_category == "Engineering")} 

hist(phats, prob = TRUE)
curve(dnorm(x, mean(phats), sd(phats)), add = TRUE)

mean(phats)
sd(phats)

n*p

p_true = mean(grads$Major_category == "Engineering")
p_true
se_true = sqrt(p_true* (1-p_true)/n)
se_true
p_true - mean(phats)
se_true - sd(phats)

# We first create objects for common quantities we will use for this exercise. 
n <- 50 # The sample size 
N <- nrow(grads) # The population size 
M <- 1000 # Number of samples/repetitions 
# Create vectors to store the simulated proportions from each repetition. 
xbars <- numeric(M) # for sample proportions 
# Set the seed for reproducibility 
set.seed(1234) 
# Always set the seed OUTSIDE the for loop. 
# Now we start the loop. Let i cycle over the numbers 1 to 1000 (i.e., iterate 1000 times). 
for(i in seq_len(M)){ 
  # The i-th iteration of the for loop represents a single repetition. 
  # Take a simple random sample of size n from the population of size N. 
  index <- sample(N, size = n) 
  # Save the random sample in the sample_i vector. 
  sample_i <- grads[index, ] 
  # Compute the mean Median in the ith sample 
  xbars[i] <- mean(sample_i$Median)} 

xbars[1:10]

hist(xbars, prob = TRUE)
curve(dnorm(x, mean(xbars), sd(xbars)), add = TRUE)