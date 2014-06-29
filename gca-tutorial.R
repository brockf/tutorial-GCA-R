#
# Tutorial: Growth Curve Analyses in R
#
# This tutorial uses simulated data to compare mixed-effects growth curve analyses (GCAs) to
# traditional ANOVA and natural polynomial mixed-effects modelling approaches.
#
# Everything I know about GCA, I learned from Dan Mirman (http://www.danmirman.org/gca)
# and by playing around in R. Here's a good Mirman paper on the topic:
#
#   Mirman, D., Dixon, J., & Magnuson, J. S. (2008). Statistical and computational models of the  
#      visual world paradigm: Growth curves and individual differences. Journal of Memory and
#      Language, 59, 474–494.
#
# Requires: install.packages('lmer','ggplot2','plyr','ez')
#
# @author: Brock Ferguson
# @website: brockferguson.com
#
# @modified June 29, 2014
#

#####################################################
# (1) Simulate Data
#
# Generate a random dataset for an experiment that has 2 conditions (Practice and Control).
# Each participant is assessed for their "accuracy" at 10 timepoints. In the Practice condiition,
# they rapidly become more accurate but then taper ~63%. In the Control condition, they get better
# at a constant rate, and also max out around ~63%. Thus the group's differences do not differ in their
# linear rate of improvement, maximum improvement, or starting point (~50%), but in their 
# growth trajectory. This is an ideal problem for Growth Curve Analyses, because we are interested
# in the "shape" or functional form of their improvement over time, and we have multiple measurements
# from the same individuals (i.e., it's longitudinal, not cross-sectional).
#####################################################

# set parameters for Subjects and Timepoints
# these are configured at 240/10 by default, and things may look different
# if you change them
num_subjects <- 240
num_timepoints <- 10

# create dataframe and begin populating it
data <- data.frame(matrix(nrow=(num_subjects*num_timepoints), ncol=4))
colnames(data) <- c('Subject','Condition','Time','Accuracy')
data$Subject <- paste('SUB',rep(1:num_subjects, each=num_timepoints),sep="")
data$Condition <- rep(c('Control','Practice'), each=((num_subjects*num_timepoints)/2))
data$Time <- rep(1:num_timepoints,times=num_subjects)

# generate data for each participant...

# control condition gets slightly better over time
data[which(data$Condition == 'Control'), 'Accuracy'] <- seq(.5,.64,by=(.15/num_timepoints))

# but the practice+sleep condition improves logarthmically
data[which(data$Condition == 'Practice'), 'Accuracy'] <- .5 + log(1 + ((data[which(data$Condition == 'Practice'), 'Time'] - 1)*.55), 450000)

# and make sure our data has the proper column formats
data$Subject <- factor(data$Subject)
data$Condition <- factor(data$Condition)

#####################################################
# (2) Visualization and summaries
#####################################################

# first, let's take a look at our "perfect" data (before we add some random noise)

library(ggplot2)

# this is what we want our models to eventually discover in the noise...
ggplot(data=data, aes(x=Time, y=Accuracy, group=Condition, colour=Condition)) + geom_line() + geom_point()

# add said noise
data$Accuracy <- data$Accuracy + rnorm(num_subjects*num_timepoints,0,.05)
data[which(data$Accuracy > 1), 'Accuracy'] <- 1
data[which(data$Accuracy < 0), 'Accuracy'] <- 0

# visualize with noise
ggplot(data=data, aes(x=Time, y=Accuracy, group=Condition, colour=Condition)) + geom_point()

# take some summary statistics

library(plyr)

# just grab total accuracy by condition
summary <- ddply(data, .(Condition), summarize, MeanAccuracy = mean(Accuracy), sd = sd(Accuracy), N = length(Accuracy))
summary$se <- summary$sd / sqrt(summary$N)

# hold up though, those N's are way too big. collapse by subjects first.
subjects <- ddply(data, .(Subject,Condition), summarize, SubjectAccuracy = mean(Accuracy))

summary <- ddply(subjects, .(Condition), summarize, MeanAccuracy = mean(SubjectAccuracy), sd = sd(SubjectAccuracy), N = length(SubjectAccuracy))
summary$se <- summary$sd / sqrt(summary$N)

# effects are looking strong

# now breakdown by timepoint...
summary_time <- ddply(data, .(Condition,Time), summarize, MeanAccuracy = mean(Accuracy), sd = sd(Accuracy), N = length(Accuracy))
summary_time$se <- summary_time$sd / sqrt(summary_time$N)

#####################################################
# (3) Standard ANOVA Analysis
#####################################################

library(ez)

# ANOVA (Type-III) predicting Accuracy from Condition (between) and Time (within, nested under Subject)
model <- ezANOVA(data, type = 3, dv = 'Accuracy', wid = Subject, between = .(Condition), within = .(Time))
model
  # effects:
  #   Condition, p < .001
  #   Time, p < .001
  #   Interaction, ???
  #     most of the time, we won't see an interaction here (depends on your random data)
  #     - if it's reliable, we would conclude that Practice participants improved more quickly over time (wrong)
  #     - if it's insignificant, we would conclude that they improved at the same rate (wrong)
  #     this type of analysis just can't capture the form of the growth, and will lead us in wrong directions.

#####################################################
# (4) Hierarchical Linear Regression
#####################################################

library(lme4)

# let's just replicate our ANOVA analysis first...
model <- lmer(Accuracy ~ Time*Condition + (1 | Subject), data = data)
summary(model)
  # weird! now our main effect is less reliable...
  #   what happened?
  #   ezANOVA centered our predictors automatically, and so the main effect in the ANOVA
  #   was captured at t==0 or, theoretically speaking, timepoint 5.5

# center variables and try again...
data$TimeC <- data$Time - mean(data$Time)

model <- lmer(Accuracy ~ TimeC*Condition + (1 | Subject), data = data)
summary(model)
  # better, but still suffering from the same problems as the ANOVA because we are only including
  # a term that can inherently only capture linear growth (i.e., linear time)

#####################################################
# (5) Growth Curve Analysis
#####################################################

# first, let's review what we've been doing in the previous 2 analyses...
# essentially: correlating a perfectly linear variable (TimeC) with our DV (Accuracy)
# and seeing if these correlations differed by an IV (Condition)
# this would isolate differences in linear growth (i.e., slope)

# let's re-create that manually... (of course, breaking all DF rules)
practice <- data[which(data$Condition == 'Practice'), ]
control <- data[which(data$Condition == 'Control'), ]

cor.test(control$TimeC, control$Accuracy)
  # r ~ .67
  
cor.test(practice$TimeC, practice$Accuracy)
  # r ~ .62

# the linear models we used compared these slopes and (probably) told us they were not different
# there, slopes == linear coefficients
# (note: the main effect of condition in prior analyses is because the Time slope in the practice condition
#        is lower than the slope in the control condition... when extrapolating to timepoint=0,
#        this makes the Practice condition slightly higher)

# in GCA, we do the same basic thing except we estimate linear and non-linear coefficients

# let's continue to do this manually just to begin
# TimeC is linear, let's create a quadratic vector (we know our data is quadratic)
data$TimeQuad <- data$Time^2
data$TimeQuadC <- scale(data$TimeQuad, center = T, scale = F)
practice <- data[which(data$Condition == 'Practice'), ]
control <- data[which(data$Condition == 'Control'), ]

# take a look...
data[1:10, 'Time']
data[1:10, 'TimeQuad']

cor.test(control$TimeQuadC, control$Accuracy)
  # r ~ .66

cor.test(practice$TimeQuadC, practice$Accuracy)
  # r ~ .58

# these coefficients are very similar to the linear TimeC
# is the data linear or quadratic? or both? and do they differ by condition?
# in order to answer these questions, we can fit them simultaneously to see which one
# is the best predictor of Accuracy (as we would with any 2 variables of interest)

model <- lmer(Accuracy ~ Condition + TimeC + TimeQuadC + TimeC:Condition + TimeQuadC:Condition + (1 | Subject), data = data)
summary(model)
  # we should now see lots of significant effects, giving us some hint of form.
  # however, this look at form -- using natural polynomials -- is not ideal
  # because natural polynomials correlate with one another and, just as with any 
  # 2 variables, when there is significant correlation between two IV's, our ability to
  # attribute variance to them is hindered.

# to demonstrate: TimeC and TimeQuadC are strongly correlated.
cor.test(data[1:10, 'TimeC'], data[1:10, 'TimeQuadC'])
  # p ~ .97

# so when our linear model is attempting to parcel variance, it has no idea where it goes.
# solution: replace our time codes with *orthogonal* polynomial contrast codes
# these are, by definition, uncorrelated vectors of N length which perfectly fit linear,
# quadratic, cubic, quartic, etc. functions

# let's confirm what I said by generating 3: linear, quadratic, and cubic
polycodes <- poly(1:10, 3)

# linear code
polycodes[, 1]

# quadratic code
polycodes[, 2]

# cubic code
polycodes[, 3]

# visualize these polycodes...
poly_df <- data.frame(matrix(nrow=30, ncol=3))
poly_df[, 1] <- paste('Poly', rep(1:3, each=10), sep='')
poly_df[, 2] <- rep(1:10, times=3)
poly_df[1:10, 3] <- polycodes[, 1]
poly_df[11:20, 3] <- polycodes[, 2]
poly_df[21:30, 3] <- polycodes[, 3]
colnames(poly_df) <- c('Code','Timepoint','Value')

ggplot(data=poly_df, aes(x=Timepoint, y=Value, group=Code, colour=Code)) + geom_line() + geom_point()

# they better not be correlated... or we're going to run into the same problem.
cor(polycodes)
round(cor(polycodes), 2)
  # not at all, nice!

# the linear code correlates 100% with our previous linear time code
cor.test(polycodes[, 1], data[1:10, 'TimeC'])

# the quadratic code correlates only partially with our old code, because this
# is a complete quadratic function and not just our exponential function
cor.test(polycodes[, 2], data[1:10, 'TimeQuadC'])

# but because they don't correlate with eachother, we can substitute them into a model
# and see how they predict our DV
# we will also simultaneously compare the estimated coefficients for each Condition (i.e., the
# interactions) to see if they differ

# add them into our dataset
data$ot1 <- rep(polycodes[, 1],times=num_subjects)
data$ot2 <- rep(polycodes[, 2],times=num_subjects)
data$ot3 <- rep(polycodes[, 3],times=num_subjects)

# now fit a GCA model with these new orthogonal polynomials
model <- lmer(Accuracy ~ ot1*ot2*Condition + (1 | Subject), data = data)
summary(model)

# this is good! but we need to add a couple of things:
# to this point, all we have done is vary the intercept by subject.
# when we are looking at a within-subjects variable (time, or our orthogonal timecodes)
# we should allow the model to vary those by subject too

# fit another model...
model <- lmer(Accuracy ~ ot1*ot2*Condition + (1 + ot1 + ot2 | Subject), data = data)
summary(model)

# in our world of perfect data, it doesn't make much difference, but it's good practice

# what happens we add in the cubic parameter?
model <- lmer(Accuracy ~ ot1*ot2*ot3*Condition + (1 + ot1 + ot2 + ot3 | Subject), data = data)
summary(model)

# pretty messy...
# we can use model comparison to tell us if this parameter is justified

model_null <- lmer(Accuracy ~ ot1*ot2*Condition + (1 + ot1 + ot2 + ot3 | Subject), data = data)
anova(model,model_null)
  # p ~ .67... nope

