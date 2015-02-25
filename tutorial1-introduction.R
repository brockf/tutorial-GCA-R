# tutorial 1: our first R script
# ---------------------------------------------------------------

# make sure we have the packages we'll need today
# install.packages('reshape2','plyr','ggplot2','lme4','lmerTest','ez')

# set working directory to workshop folder

# load in a dataset from a CSV file
vocab <- read.csv('data-vocab.csv')

# this dataset is loaded as a "dataframe" -- a series of columns
# you can see this dataframe now in our "Environment" window in RStudio

# each column in a dataframe can be of a unique type
# what types of columns do we have?
summary(vocab)

# we don't need the X column (it's just indicates the row number)
# get rid of it by selecting the columns of the dataframe using a
# vector representing the range 2 to 7
# dataframe[rows, columns]
vocab <- vocab[, c(2:7)]
summary(vocab)

# we could select one column and assign it to a particular vector variable
medians <- vocab[, 'median']
# this vector is listed in the Environment as well, under "values"

# we could also select certain rows
vocab[c(1:5), ]
vocab[c(1,2,3,4,5), ]

# or row/column combinations
vocab[5, 'gender']
vocab[c(1:5), c('age','gender')]

# or by matching values...
vocab[which(vocab$gender == 'F'), ]
vocab[which(vocab$gender == 'F' & vocab$age == 17), ]
vocab[which(vocab$age == 18 | vocab$age == 17), ]
vocab[which(vocab$age > 16 & vocab$age < 19), ]

# we can also overwrite values in a dataframe
# set the median vocab for 17-month-old females to 1000...
vocab[which(vocab$gender == 'F' & vocab$age == 17), 'median'] <- 1000
vocab[which(vocab$gender == 'F' & vocab$age == 17), ] # now shows 1000

vocab[which(vocab$gender == 'F' & vocab$age == 17), c('median','ci.h')] <- c(1000,NA)

# reset it...
vocab[which(vocab$gender == 'F' & vocab$age == 17), c('median','ci.h')] <- c(75,37)

# R is a "vectorized" language, meaning that we can perform operations on entire vectors
# in parallel and not have to operate on each cell one-at-a-time

# here's our vector of medians
medians

# now let's perform some mathematical operations on it
medians + 2
medians*4
(medians^2) / 4
log(medians)
exp(medians)
sqrt(medians)

# we can also use summary functions which boil it down to a meaningful metric
mean(medians)
var(medians)
sqrt(var(medians))
sd(medians)
length(medians)

length(vocab)
nrow(vocab)
ncol(vocab)

# or combinations of summary functions and mathematical operators
# e.g., calculate z-scores
z_scores <- (medians - mean(medians)) / sd(medians)
z_scores

# compare to R's functions for z-score scaling...
# we'll use this a bit
z_scores <- scale(medians,center=T,scale=T)

# almost all functions can apply to vectors, even non-mathematical ones
# e.g., paste() which combines strings together
paste('this','is','a','sentence',sep=' ')
paste('median: ',medians,sep='')

# and, of course, we can perform operations involving multiple vectors
medians * rnorm(1, 0, 1) # vector * integer
medians * rnorm(length(medians), 0, 1) # vector * vector

# R includes basic functions for visualizing your data
plot(medians)
hist(medians)
plot(vocab$age, vocab$median)

# ...but I prefer using ggplot2 which gives us more intuitive options for visualization
# and greater control over the appearance
ggplot(vocab, aes(x=age, y=median, color=gender)) +
                          geom_point()

ggplot(vocab, aes(x=age, y=median, color=gender)) +
        geom_pointrange(aes(min=median-ci.l, max=median+ci.h), position=position_dodge(.4)) +
        scale_x_continuous(name="Age (Months)") +
        scale_y_continuous(name="Productive Vocabulary")

ggplot(vocab, aes(x=age, y=median, fill=gender)) +
        geom_bar(stat='identity', position=position_dodge()) +
        geom_errorbar(aes(ymax=median+ci.h, ymin=median-ci.l), position=position_dodge(.9), width=0.25) +
        scale_x_continuous(name="Age (Months)") +
        scale_y_continuous(name="Productive Vocabulary")

# we'll be using simple ggplots today, but the web documentation is great at helping you
# create publishable figures of any kind

# finally, of course, R provides a host of functions to run statistical tests and models
# on your data

# let's do a basic correlation just to see how these functions work
# first, get just the female data
female_vocab <- vocab[which(vocab$gender == 'F'), c('age','median')]
female_vocab 
                  
# does age correlate with productive vocabulary?
# plot using basic R plots
plot(female_vocab$age, female_vocab$median)
# add a line of best fit
abline(lm(female_vocab$median ~ female_vocab$age))
  # we'll get more into this syntax later, but this function uses a linear model
  # to predict the 'median' vector from the 'age' vector

# plot data with a line of best fit (with shaded 95% confidence interval)
ggplot(female_vocab, aes(x=age, y=median)) +
                                  geom_point() +
                                  stat_smooth(method="lm")

cor(female_vocab$age, female_vocab$median)
  # = .99

# how reliable is this?
cor.test(female_vocab$age, female_vocab$median)
  # gives us all the basic test output we need....

# we can also access specific variables in this output directly:
cor_output <- cor.test(female_vocab$age, female_vocab$median)

# useful command to show the available data in any variable
str(cor_output)

cor_output$p.value
cor_output$statistic

# also:
names(cor_output)
cor_output$conf.int


# round the p-value:
cor_output$p.value
round(cor_output$p.value, 3)

# how much variance does this account for (e.g., R-squared, R^2)?
cor_output$estimate^2

# when we run statistical models, the vector we return is not only a collection of variables
# (e.g., from which we can select the p-value, estimate, etc.) but also an OBJECT
# on which we can run various CLASS methods

class(cor_output)
  # we have an "htest" (i.e., "hypothesis test") object
  # this class is limited to the "print" method
  print(cor_output)
  # ... but other classes include other methods like 'plot', 'summary', etc.

# let's run another statistical test of a different type
t.test(rnorm(30,5,5), rnorm(30,10,5))

# we can pass specific options to these tests
# a common option for 't.test' is var.equal=T so that we run a standard Student t-test
t.test(rnorm(30,5,5), rnorm(30,10,5), var.equal=T)

# also:
t.test(rnorm(30,5,5), rnorm(30,10,5), paired=T)
t.test(rnorm(30,5,5), rnorm(30,10,5), alt='less',var.equal=T) # by default: 'two.sided'

t.test(
      vocab[which(vocab$gender == 'F'), 'median'],
      vocab[which(vocab$gender == 'M'), 'median'],
      var.equal=T
      )

# what class?
class(t.test(rnorm(30,5,5), rnorm(30,10,5), var.equal=T))

# same as cor.test, which is why the output is similar

# we loaded our data from a CSV, now let's save a dataset to a CSV file
write.csv(female_vocab, 'data-vocab-females.csv')

# independent workshop:
# take a new subset of the vocab data (e.g., certain age ranges, or Males)
# visualize these data using a scatterplot with a line of best fit
# run a correlation test
# then, add a bunch of random noise (e.g., with +, rnorm(), and the vector),
#   replot the data, run a new correlation (is it weaker??), and save this data to a CSV file
# bonus task if this is really easy: make your plot better with custom axis labels,
#   a title, a legend, no ugly grey background, etc.

# we're done
# clean up our workspace
ls()
rm(list=ls())