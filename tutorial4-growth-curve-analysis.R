# tutorial 4: GCA

# load required packages
library(ggplot2)
library(lme4)
library(plyr)
library(ez)

# load our data
data <- read.csv('data-eyetracking.csv')

# we will do by-subjects time analyses only (aggregating across trials within-subjects) for simplicity
# but you can do by-items/trials analyses in the EXACT same way

# rescale Time to solve convergence issues
data$TimeS <- data$TimeFromSubphaseOnset / 1000
# aggregate to subjects by 50ms
data$Bin <- data$TimeFromSubphaseOnset %/% 50
# aggregate by bins
binned <- ddply(data, .(ParticipantName,Target,Bin), summarize, PropAnimal = mean(Animate), y = sum(Animate), N = length(Animate), TimeS = min(TimeS))

# calculate the empirical logit and weights, if we want them
binned$elog <- log( (binned$y + .5) / (binned$N - binned$y + .5) )
binned$wts <- 1/(binned$y + .5) + 1/(binned$N - binned$y + .5)

# calculate the ArcSin, another option...
binned$Arcsin <- asin(sqrt(binned$PropAnimal))

# here's a model similar to the linear model we had before 
model <- lmer(elog ~ Target*TimeS + (1 + Target + TimeS | ParticipantName), data = binned)
summary(model)

# visualize the data and model fit in a different way than we have before
ggplot(binned, aes(x=TimeS, y=elog, color=Target)) +
                                 stat_summary(fun.y=mean, geom="point") +
                                 stat_summary(aes(y=predict(model,binned,re.form=NA)), fun.y=mean, geom="line")

# but we know that this model isn't a great fit to our data
# by assuming linear growth, it gives us weird estimates, one major one
# being that it thinks our two groups differ at timepoint 0

# let's fit a model which also correlates our DV with natural polynomials
# Time squared, cubed, etc...
binned$TimeS_2 <- binned$TimeS^2
binned$TimeS_3 <- binned$TimeS^3
binned$TimeS_4 <- binned$TimeS^4

head(binned)

plot(binned$TimeS, binned$TimeS_2)
plot(binned$TimeS, binned$TimeS_3)
plot(binned$TimeS, binned$TimeS_4)

model <- lmer(elog ~ Target*(TimeS + TimeS_2 + TimeS_3 + TimeS_4) + (1 + Target + TimeS + TimeS_2 + TimeS_3 + TimeS_4 | ParticipantName), data = binned)
summary(model)
  # convergence issues, let's scale back on these polynomials to something that seems more reasonable
  # a good rule of thumb is to count the number of "bends" in the data
  # 1 bend == quadratic, 2 bends == cubic, etc.

model <- lmer(elog ~ Target*(TimeS + TimeS_2 + TimeS_3) + (1 + Target + TimeS + TimeS_2 + TimeS_3 | ParticipantName), data = binned)
summary(model)
  # still a convergence error, likely because the scale of our variables is off
  # we will ignore this for now, because our final GCA approach will fix this
  # let's just see what this model did for us though

  # looking at the estimates, it seems to have gotten rid of our timepoint 0
  # main effect of Target (yes!), and instead shows some strong interactions over time...

# let's visualize:
ggplot(binned, aes(x=TimeS, y=elog, color=Target)) +
  stat_summary(fun.y=mean, geom="point") +
  stat_summary(aes(y=predict(model,binned,re.form=NA)), fun.y=mean, geom="line")
  # there you go!

# let's store this model because are going to use it for comparison later
natural_model <- model

# we just did our first non-linear growth curve analysis, but it was sub-optimal for two reasons:
# (1) these polynomial terms we generated are correlated with each other
# (2) our model had trouble converging because of the different scales of our DV's
# thankfully, we have something that will help: orthogonal polynomials

# intedependence/correlations

cor(binned[, c('TimeS','TimeS_2','TimeS_3','TimeS_4')])
  # all of our DV's are *highly* correlated with each other...
  # not a good thing when we are trying to attribute variance
  # to each factor independently

# so, what we can do is actually create replacement timecodes
# for linear, quadratic, cubic, etc. change over time

# poly() will generate higher-order polynomials for us, with a vector length
# equivalent to the length of our original time vector
# we'll go up to 6th-order polynomials, but we'll stick to the first 3 for the most part
orthogonal_polynomials <- poly(sort(as.vector(unique(binned$TimeS))), 6)
head(orthogonal_polynomials)
  # column 1 grows linearly
  # column 2 grows quadratically
  # column 3 grows cubicly
  # ....

# verify that they are indeed uncorrelated
cor(orthogonal_polynomials[, c(1:6)])
round(cor(orthogonal_polynomials[, c(1:6)]),5)
  # perfect!

# I like to merge them into the original dataframe using this technique, which allows
# for missing data from any given participant
time_codes <- data.frame(
                        sort(as.vector(unique(binned$TimeS))),
                        orthogonal_polynomials[, c(1:6)]
                      )
colnames(time_codes) <- c('TimeS','ot1','ot2','ot3','ot4','ot5','ot6')

binned <- merge(binned, time_codes, by='TimeS')

# let's model now...
model <- lmer(elog ~ Target*(ot1 + ot2 + ot3) + (1 + Target + ot1 + ot2 + ot3 | ParticipantName), data = binned)
summary(model)
  # great fit, no errors now...
  # main effect of TargetArtefact, though??
  #   important point: our natural polynomials all started at Timepoint 0, meaning that
  #   main effects represented differences at the *start* of the time window.
  #   orthogonal polynomials are CENTERED at 0, meaning that main effects represent
  #   average differences between levels of a factor.

# let's visualize:
ggplot(binned, aes(x=TimeS, y=elog, color=Target)) +
  stat_summary(fun.y=mean, geom="point") +
  stat_summary(aes(y=predict(model,binned,re.form=NA)), fun.y=mean, geom="line")

# compare this model to our natural polynomial model...
summary(natural_model)
summary(model)

# we can also use the same methods as before to get confidence intervals, test for
# Type III significance, etc.

# confint(model)
  # this takes a long with a model this complex....

drop1(model, ~., test="Chisq")
  # shows that all of our parameters are useful, but let's try seeing what different
  # polynomials do anyways...

model_quartic <- lmer(elog ~ Target*(ot1 + ot2 + ot3 + ot4) + (1 + Target + ot1 + ot2 + ot3 + ot4 | ParticipantName), data = binned)
summary(model_quartic)
  # still accounting for decent variance

ggplot(binned, aes(x=TimeS, y=elog, color=Target)) +
  stat_summary(fun.y=mean, geom="point") +
  stat_summary(aes(y=predict(model,binned,re.form=NA)), fun.y=mean, geom="line", linetype='dashed') + # 3rd-order model
  stat_summary(aes(y=predict(model_quartic,binned,re.form=NA)), fun.y=mean, geom="line") # 4th-order model
  
model_quintic <- lmer(elog ~ Target*(ot1 + ot2 + ot3 + ot4 + ot5) + (1 + Target + ot1 + ot2 + ot3 + ot4 + ot5 | ParticipantName), data = binned)
summary(model_quintic)
  # not doing much for us...

ggplot(binned, aes(x=TimeS, y=elog, color=Target)) +
  stat_summary(fun.y=mean, geom="point") +
  stat_summary(aes(y=predict(model_quartic,binned,re.form=NA)), fun.y=mean, geom="line", linetype='dashed') + # 4th-order model
  stat_summary(aes(y=predict(model_quintic,binned,re.form=NA)), fun.y=mean, geom="line") # 5th-order model

# what if we strip away factors?
model_quadratic <- lmer(elog ~ Target*(ot1 + ot2) + (1 + Target + ot1 + ot2 | ParticipantName), data = binned)
summary(model_quadratic)
  # yikes!

ggplot(binned, aes(x=TimeS, y=elog, color=Target)) +
  stat_summary(fun.y=mean, geom="point") +
  stat_summary(aes(y=predict(model,binned,re.form=NA)), fun.y=mean, geom="line", linetype='dashed') + # 3rd-order model
  stat_summary(aes(y=predict(model_quadratic,binned,re.form=NA)), fun.y=mean, geom="line") # 2nd-order model

# and a reminder of how bad that linear model was...
model_linear <- lmer(elog ~ Target*(ot1) + (1 + Target + ot1 | ParticipantName), data = binned)
summary(model_linear)

ggplot(binned, aes(x=TimeS, y=elog, color=Target)) +
  stat_summary(fun.y=mean, geom="point") +
  stat_summary(aes(y=predict(model,binned,re.form=NA)), fun.y=mean, geom="line", linetype='dashed') + # 3rd-order model
  stat_summary(aes(y=predict(model_linear,binned,re.form=NA)), fun.y=mean, geom="line") # 2nd-order model

# GCA with 3 levels:
# I like to design experiments with only 2 levels per factor for simplicity
# but sometimes we have 3 levels in a factor and, now, main effects != simple effects

# let's add a third "Neutral" Target level that will mirror the Animal level
new_condition <- binned[which(binned$Target == 'Animal'), ]
new_condition$Target <- 'Neutral'
#new_condition$y <- new_condition$y - round(new_condition$N / 3)
new_condition$y <- new_condition$y + round(rnorm(length(new_condition$y),0,2))
new_condition$y <- ifelse(new_condition$y > new_condition$N,new_condition$N,new_condition$y)
new_condition[which(new_condition$y < 1), 'y'] <- 1
new_condition$PropAnimal <- new_condition$y / new_condition$N
new_condition$elog <- log( (new_condition$y) / (new_condition$N - new_condition$y + .5) )
new_condition$wts <- 1/(new_condition$y + .5) + 1/(new_condition$N - new_condition$y + .5)
new_condition$Arcsin <- asin(sqrt(new_condition$PropAnimal))

binned_3levels <- rbind(binned,new_condition)
binned_3levels$Target <- factor(binned_3levels$Target)

ggplot(binned_3levels, aes(x=TimeS, y=elog, color=Target)) +
  stat_summary(fun.y=mean, geom="point")

# fit model
model <- lmer(elog ~ Target*(ot1 + ot2 + ot3) + (1 + Target + ot1 + ot2 + ot3 | ParticipantName), data = binned_3levels)
summary(model)

ggplot(binned_3levels, aes(x=TimeS, y=elog, color=Target)) +
  stat_summary(fun.y=mean, geom="point") +
  stat_summary(aes(y=predict(model,binned_3levels,re.form=NA)), fun.y=mean, geom="line", linetype='dashed') # 3rd-order model

# get main effects via model comparison...
# drop1(model,~.,test="Chisq")
  # takes a long time to run... so I'll just do one
  
  model_null <- lmer(elog ~ Target*(ot1 + ot2) + ot3 + (1 + Target + ot1 + ot2 + ot3 | ParticipantName), data = binned_3levels)
  summary(model_null)
  
  anova(model,model_null)

# get simple effects by re-ordering factor levels...
levels(binned_3levels$Target)
binned_3levels$Target <- factor(binned_3levels$Target,c('Neutral','Animal','Artefact'))
levels(binned_3levels$Target)

model <- lmer(elog ~ Target*(ot1 + ot2 + ot3) + (1 + Target + ot1 + ot2 + ot3 | ParticipantName), data = binned_3levels)
summary(model)

# independent work:
#
# try using GCA on your own data, if you brought some, or try
# using GCA to model a subset of these data (e.g., Females only, or Males only)
#
# visualize raw data and model fits







