---
  title: "R Growth Curve Analysis: Tutorial 1: General Linear Models"
  author: "Brock Ferguson"
  date: "November 1, 2014"
  output: html_document
---
  
Load packages we'll be using:

```{r}
library(ggplot2)
library(ez)
```

# load in our vocabulary data again
vocab <- read.csv('data-vocab.csv')

# re-run our correlation as a basic linear model
model <- lm(median ~ age, data=subset(vocab, gender == 'F'))

# what methods are available?
class(model)
  # lm: print (default), summary, anova, plot, predict, fitted, ...

model
print(model)

summary(model)

anova(model)

plot(model)
  # most important: residuals versus fitted (is error variance normally distributed?), also Q-Q plots, etc.

# let's do some analyses with our vocab dataset to compare the different linear modelling
# methods within R

# first, let's begin by visualizing the raw data again
ggplot(vocab, aes(x=age, y=median, color=gender)) +
                    geom_pointrange(aes(min=median-ci.l, max=median+ci.h), position=position_dodge(.4)) +
                    scale_x_continuous(name="Age (Months)") +
                    scale_y_continuous(name="Productive Vocabulary")

# note, we are treating each datapoint here as if it were a different subject
# (thus giving us 1 subject of each gender at each age)

# compare lm() and aov() with different kinds of models
summary(lm(median ~ age, data=subset(vocab, gender == 'F'))) # Type 3 SS, effect of each factor while holding others' constant
anova(lm(median ~ age, data=subset(vocab, gender == 'F'))) # Type 1 SS, effect of each factor in sequence, ignoring factors that follow
summary(aov(median ~ age, data=subset(vocab, gender == 'F'))) # Type 1 SS, effect of each factor in sequence, ignoring factors that follow
  # identical... except for F's being t^2
  
# with two main effects, these models are still identical
summary(lm(median ~ age + gender, data=vocab))
anova(lm(median ~ age + gender, data=vocab))
summary(aov(median ~ age + gender, data=vocab))

# but with interactions... they start to look different
# note: the * indicates main effects and interactions between all parameters
summary(lm(median ~ age*gender, data=vocab))
anova(lm(median ~ age*gender, data=vocab))
summary(aov(median ~ age*gender, data=vocab))

# Why do they differ? Morever, why is the gender effect POSITIVE (i.e., boys have an advantage)
# when we don't see that at all in the raw data?
  # the answer is in the way the model is constructed, and how the variables are coded

  # with aov(), we assess the influence (sums of squares) of each parameter IN ORDER
  
  # order effects with aov()
  # we don't see order effects in these data
  summary(aov(median ~ age + gender, data=vocab))
  summary(aov(median ~ gender + age, data=vocab))
  #... because they are uncorrelated
  cor.test(vocab$age, as.numeric(vocab$gender))
  
  # but when data are correlated in any way... order matters
  test1 <- rnorm(20)
  test2 <- rnorm(20)
  test3 <- rnorm(20)
  summary(aov(test3 ~ test1 + test2))
  summary(aov(test3 ~ test2 + test1))
  # let's abandon aov() now...

  # with lm(), we are now calculating the effect of each parameter (e.g., age, gender)
  # after accounting for each other parameter, including the interaction. this is what
  # most of us are used to in analyses (unlike aov()).
  # because it's now accounting for the interaction, 
  # when we look for the main effect of "gender" -- holding age constant (i.e., at 0) --
  # we don't get what we think of as a main effect of gender. we actually get the model's best guess
  # of the effect at gender at age == 0 months given the main effect of age and its interaction
  # with gender...

# let's illustrate this:
model <- lm(median ~ age*gender, data=vocab)

# see the predictions using this model fit
vocab$predictions <- predict(model, vocab)

# visualize our model predictions
# note the y-axis range
ggplot(vocab, aes(x=age, y=predictions, color=gender)) + 
                    geom_point() +
                    stat_smooth(method="lm") +
                    scale_x_continuous(name="Age (Months)") +
                    scale_y_continuous(name="Predicted Vocabulary")

# what we WANT to know is what the effect of gender is on average, i.e., at the
# average age (here, around ~22 months)

# instead, what the model is doing is setting the Age to 0 months and telling us whether there is
# an estimated effect of gender at that point

# we can visualize this problem by making extrapolated predictions for a dataset that gets
# the model's predictions down to age 0 (which of course doesn't make sense for productive vocabulary)
vocab_extrapolations <- data.frame(
                              c(rep(0:30,each=2)),
                              c(rep(c('M','F'),times=31)),
                              c(rep(0,62))
                              )
colnames(vocab_extrapolations) <- c('age','gender','prediction')

vocab_extrapolations$prediction <- predict(model, vocab_extrapolations)

ggplot(vocab_extrapolations, aes(x=age, y=prediction, color=gender)) + 
                                    geom_point() +
                                    stat_smooth(method="lm") +
                                    scale_x_continuous(name="Age (Months)") +
                                    scale_y_continuous(name="Predicted Vocabulary")

# now we see why the model did what it did: at age 0, the slight estimated (non-significant)
# interaction actually leads to a small predicted *advantage* for boys

# this is giving us an answer to a question we didn't want to ask.
# to fix it, let's Center the age variable so we can estimate the effect of gender
# holding age constant at its average value

vocab$ageC <- scale(vocab$age, center=T, scale=F) # note, setting scale to True will recode as z-scores

summary(lm(median ~ ageC*gender, data=vocab))
  # this is better: a significant, negative effect for gender
  # but the t's and p-values still don't line up perfectly with the anova()
  # and aov() models...

  # this is due to a very similar problem that we had before...
  # gender is treatment-coded (i.e., with a 'reference' level [Female] and treatment level [Male])
  # so what our linear model is showing us for the effect of Age is actually the effect of Age
  # within Females, and the interaction is the effect of Age within Males
  # again, this is answering a question we didn't want to ask. what we want to know is whether
  # age differs when holding gender constant at its "average"
  # we can do this by sum-coding gender.

# we can set a factor to be sum-coded using contrasts()
# this will, for 2 factors, set them to be -1 and +1 respectively
contr.sum(2)

contrasts(vocab$gender) <- 'contr.sum'
summary(lm(median ~ ageC*gender, data=vocab))
  # now, our estimate of Age is OK but our estimate of gender is a bit counterintuitive;
  # it represents 1/2 of the estimated main effect of gender because the difference between
  # our sum-codes is 2 and it's giving us the estimate *for each 1 unit of gender*

# my preferred way is to manually use deviation contrasts (-.5 and +.5 respectively)
# so that we get an estimate equivalent to the main effect of gender holding all other factors
# constant
vocab$genderContrast <- -.5
vocab[which(vocab$gender == 'M'), 'genderContrast'] <- .5
summary(lm(median ~ ageC*genderContrast, data=vocab))

# now we have a proper model!

# we can replicate these analyses with ez's ezANOVA

ezANOVA(vocab, dv = 'median', wid = .(X), between = .(gender,age), type=2)
  # same as our aov() and anova() analyses because the interaction
  # isn't interfering with our main effects

ezANOVA(vocab, dv = 'median', wid = .(X), between = .(genderContrast,ageC), type=3)
  # same problems as with our problematic lm() model, because now
  # the interaction is messing with our main effects

# the moral here is that we need to pay close attention to the way our variables
# are treated (e.g., centered, coded, standardized) because they change the way
# the model's effects are estimated and intepreted.
# it's more of an issue here because, unlike SPSS and other packages which do all
# of this automatically in the background, R will be relatively dumb in the way it
# models your data.

# I recommend deviation-coding all factors and centering all numeric vectors, in general.

# here are some other helpful functions for linear models:
model <- lm(median ~ ageC*genderContrast, data=vocab)

# confint(): get confidence intervals for estimates
confint(model)

# pairwise.t.test(): run pairwise.t.tests between groups (ignoring other factors)
pairwise.t.test(vocab$median, vocab$genderContrast)

# drop1(): drop each parameter and do an F test
# useful if you want to get Type III sums of squares (like SPSS) for
# aov() (i.e., Type I) models

type1_model <- aov(median ~ age*gender, data=vocab)
type3_model <- lm(median ~ age*gender, data=vocab)

summary(type1_model) # type 1 p-values, calculated BEFORE interaction
summary(type3_model) # type 3 p-values, calculated AFTER interaction
drop1(type1_model,~.,test="F") # makes the type 1 model look like a type 3 model

# these are both type 3, so they align perfectly
summary(model)
drop1(model,~.,test="F")

# independent workshop:
# load 'data-vocab-individuals.csv'
  # this is true individual vocabulary data
  # important columns:
    # id: child ID (no repeats)
    # age: age in months
    # gender: M or F, factor
    # birth_order: 1st, 2nd, 3rd, ..., nth child
    # mom_ed: number of years Mom was in school
    # productive: productive vocabulary on MCDI
# visualize the raw data
# model the data
  # play around with various coding and scaling (e.g., centering of the variables)
  # and evaluate how this changes your interpretation of the reliability, size, and direction
  # of effects.
  # pay particular attention to (1) the intercept and what exactly it represents
  # in each kind of model (i.e., the grand mean, cell mean, etc.) and (2) what centering a sum-coded
  # variable (i.e., gender) does to the interpretation of other effects
# visualize your models' predictions
