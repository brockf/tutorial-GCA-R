# Let's say we start with a probability of .8
prob <- .8

# we can calculate the odds that we get a "hit"
# .8 / .2
odds <- prob / (1-prob)

# we can then calculate the log-odds, used in logistic regression
log_odds <- log(odds)
log_odds <- log(prob / (1-prob))

# and if we want to convert the log-odds back to probability
prob2 <- exp(log_odds) / (1+exp(log_odds))

# empirical logit is just like log-odds except we add a constant
# to keep from getting odds of -Inf
hits <- 8
total <- 10
empirical_logit <- log( (hits + .5) / (total - hits + .5) )

# compare probabilities to log-odds
probs <- seq(.1,.9,by=.1)
probs_logodds <- log(probs / (1-probs))
probs_arcsin <- asin(sqrt(probs))

# both Arcsin and log-odds do a nice job of extending the tails of the probability
# distribution
plot(probs,probs_logodds)
plot(probs,probs_arcsin)