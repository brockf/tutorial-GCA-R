# tutorial 3: generalized linear models

# load required packages
library(ggplot2)
library(lme4)
library(plyr)
library(ez)

# load our data
data <- read.csv('data-eyetracking.csv')

# what kind of columns do we need for these analyses?
summary(data)

# this set as-is only a few steps removed from what comes out of the standard eyetracking.
# I've done some prep, though:
#   1) removed trackloss
#   2) converted GazeX and Y columns to AOI 1 or 0 columns
#   3) merged in subject information

####### ANALYSIS 1: BY-SUBJECTS and BY-ITEMS ANOVAS
# pros: simple, allows for generalization beyond samples participants and sampled items
# cons: lose all timing information, what to do with ambiguities between analyses?

# we need to aggregate across trials by target within participants
agg_subjects <- ddply(data, .(ParticipantName,Sex,Age,Target),
                      summarize,
                      PropAnimal = mean(Animate))

# visualize...
ggplot(agg_subjects, aes(x=Target, y=PropAnimal, fill=ParticipantName)) +
                                                      geom_bar(stat="identity", position=position_dodge())
  # weird plot... but looks about right.

# use our GLM best practices to prepare and model these data
agg_subjects$TargetCoded <- -.5
agg_subjects[which(agg_subjects$Target == 'Artefact'), 'TargetCoded'] <- .5
# no need to center here, because data are balanced (every subject is present in both conditions)
# agg_subjects$TargetCoded <- scale(agg_subjects$TargetCoded, center=T, scale=F)

# here we use aov() because it allows for a repeated-measures Error() term
# as we learned before, aov() uses Type I sums of squares, but with only one factor (i.e.,
# no correlation issues), it's OK
model <- aov(PropAnimal ~ TargetCoded + Error(ParticipantName/TargetCoded), data = agg_subjects)
summary(model)

# now we can do an ANOVA over items as well...
agg_items <- ddply(data, .(Trial,Target), summarize, PropAnimal = mean(Animate))
agg_items$TargetCoded <- -.5
agg_items[which(agg_items$Target == 'Artefact'), 'TargetCoded'] <- .5

# visualize...
ggplot(agg_items, aes(x=Target, y=PropAnimal, fill=Trial)) +
  geom_bar(stat="identity", position=position_dodge())
  # looks good... 

# no Error() term this time, because these items are between-conditions
model <- aov(PropAnimal ~ TargetCoded, data = agg_items)
summary(model)

# here, these analyses are both crystal clear (reject the null!)
# but, in other cases, there can be ambiguity (what if one is significant and the
# other is marginal?).
# Ideally, we could have one test which allows us to control for random trial AND subject factors
# simultaneously. Enter lmer()...

######### ANALYSIS 3: SIMULTANEOUS TRIAL and SUBJECT RANDOM EFFECTS

# aggregate data by Trials and Participants
# i.e., trial-by-trial data
agg_sub_items <- ddply(data, .(ParticipantName,Trial,Target), summarize, PropAnimal = mean(Animate))
agg_sub_items$TargetCoded <- -.5
agg_sub_items[which(agg_sub_items$Target == 'Artefact'), 'TargetCoded'] <- .5

# fit a model allowing the intercept ("1") to vary by both Participants and Trials
model <- lmer(PropAnimal ~ TargetCoded + (1 | ParticipantName) + (1 | Trial), data = agg_sub_items)
summary(model)
  # this looks good, and converges with our previous estimate.
  # note that very little variance is being taken care of by Trial, and only slightly more
  # by ParticipantName. This is likely the result of (a) small differences between subjects
  # and trials and, (b) a relatively small dataset. We will want to keep an eye on the Std. Dev.
  # of the random effects -- when this is essentially 0, we may want to consider using a regular
  # ANOVA.

# let's dive into this model a bit more...

# we can see the fixed effects:
fixef(model)
  # Intercept: Overall mean looking to animal
  # TargetCoded: decrease in looking when asked to look at the Artefact

# ... and random effects (some people use these to describe individual differences):
ranef(model)

# we can also see what the model's predictions were, for each participant/item:
agg_sub_items$prediction <- predict(model, agg_sub_items)

ggplot(agg_sub_items, aes(x=Trial, y=prediction, color=ParticipantName)) +
                          geom_point() +
                          geom_line() +
                          facet_grid(Target~.)

# importantly, with these models, we aren't limited to varying just the intercept by
# specific grouping factors. We can also vary slopes (i.e., fixed effects) by participants
# as well.
# by adding "Target" as a random slope, we allow the model to vary the magnitude of the
# difference between Target conditions within participants.
model <- lmer(PropAnimal ~ TargetCoded + (1 + TargetCoded | ParticipantName) + (1 | Trial), data = agg_sub_items)
summary(model)
  # we now see an additional random effect by the ParticipantName group: TargetCoded.
  # it's accounting for a decent proportion of variance.

# look at which participants responded stronger/weaker to the Target
# manipulation (negative estimate == stronger, MORE of a difference)
ranef(model)

# we see more variation now in our predictions between subjects
agg_sub_items$prediction <- predict(model, agg_sub_items)
ggplot(agg_sub_items, aes(x=Trial, y=prediction, color=ParticipantName)) +
                                    geom_point() +
                                    geom_line() +
                                    facet_grid(Target~.)

# by now, you might be asking yourself two very important questions...

# First, which random effect structure should we specify?
# ------
#
# One school of thought (see Barr et al., 2013) is to "keep it maximal:"
#   - Include every random effect that your experimental design permits (i.e., every
#     factor that appeared across subjects or trials; and every meaningful grouping)

# A second proposal is to use model comparison to see if the model with more random slopes
# is better. If a significantly better fit, keep it. Otherwise, abandon.

  model1 <- lmer(PropAnimal ~ TargetCoded + (1 + TargetCoded | ParticipantName) + (1 | Trial), data = agg_sub_items)
  model2 <- lmer(PropAnimal ~ TargetCoded + (1 | ParticipantName) + (1 | Trial), data = agg_sub_items)
  anova(model1,model2) # -2 log-likelihood ratio test, gives you Chisq(df) = X and a p-value.

# A third point to consider, already mentioned, is how much work the random effects are actually
# doing for you. Are they accounting for any variance?

# A final thing to keep in mind is that we can overparameterize small datasets fairly quickly,
# leading to convergence issues (i.e., the model fits the data EXACTLY early in its convergence
# algorithm) or an error from lmer()

  model <- lmer(PropAnimal ~ TargetCoded + (1 + TargetCoded + Trial | ParticipantName) + (1 | Trial), data = agg_sub_items)
    # (1 + TargetCoded + Trial | ParticipantName) === accounts for ALL data, because each combination
    # of these random factors specifies a single datapoint

  model <- lmer(PropAnimal ~ TargetCoded + (1 + TargetCoded | ParticipantName), data = agg_subjects)
    # same problem here...

# Second, how do we get p-values for specific factors?
# -------
# Unlike lm() and aov(), lmer() doesn't generate p-values...
# It's unclear, in these kinds of models, how to calculate the degrees of freedom -- required
# by the 't' statistic in order to get a p-value. We have several options, though:

# 1: Use model comparison (like above)

  model <- lmer(PropAnimal ~ TargetCoded + (1 + TargetCoded | ParticipantName) + (1 | Trial), data = agg_sub_items)
  # delete the factor of interest; if null model, add in a "1" to represent an intercept-only model
  model_null <- lmer(PropAnimal ~ 1 + (1 + TargetCoded | ParticipantName) + (1 | Trial), data = agg_sub_items)
  anova(model,model_null)
    # degrees of freedom represent the difference between the number of parameters
    # in the models. Note that this will be == [NUMBER OF LEVELS - 1] for a removed factor

  # automated with drop1:
  drop1(model,~.,test="Chisq")

# 2: Treat the t's as z's (not as dangerous as it sounds with sufficient sample sizes)

model <- lmer(PropAnimal ~ TargetCoded + (1 + TargetCoded | ParticipantName) + (1 | Trial), data = agg_sub_items)
ts <- fixef(model)/sqrt(diag(vcov(model)))
ts
2*pnorm(abs(ts),lower.tail=FALSE)

# 3: Just use confidence intervals (new statistics!)
confint(model)

# 4: Use an available technique to approximate the degrees of freedom

# Kenward-Roger approximation:
library(pbkrtest)
model <- lmer(PropAnimal ~ TargetCoded + (1 + TargetCoded | ParticipantName) + (1 | Trial), data = agg_sub_items)
# delete the factor of interest; if null model, add in a "1" to represent an intercept-only model
model_null <- lmer(PropAnimal ~ 1 + (1 + TargetCoded | ParticipantName) + (1 | Trial), data = agg_sub_items)
KRmodcomp(model,model_null)
detach('package:pbkrtest',unload=TRUE)

# Satterthwaite approximation:
library(lmerTest)
model <- lmer(PropAnimal ~ TargetCoded + (1 + TargetCoded | ParticipantName) + (1 | Trial), data = agg_sub_items)
summary(model)
detach('package:lmerTest',unload=TRUE)

############# ANALYSIS 4: Linear time analyses
# all of the analyses thus far have thrown out the time information as part of step 1.
# how can we look at behaviour over time?

# one way to do this is to use logistic regression (family:"binomial")
# on our nearly-raw dataset, predicting the value of looking to the Animate or not (1 or 0).
# estimates the log-odds (i.e., probability in logit space) of looking at the
# animal by the fixed and random factors specified, including a factor for
# TimeFromSubphaseOnset.
#
# logistic regression solves a problem we are now going to start taking seriously:
# we were predicting bounded, non-linear proportional data using a linear model

model <- glmer(Animate ~ Target*TimeFromSubphaseOnset + (1 | Trial) + (1 | ParticipantName), data = data, family="binomial")
summary(model)
  # we are getting some warnings, here...
  #   1) suggests we should rescale our variables (likely because Target range is -.5 to .5 while
  #      TimeFromSubPhaseOnset is 0-5500).
  #   2) convergence error
  #
  # let's try fixing the scale issue...

# calculate Time in seconds
data$TimeS <- data$TimeFromSubphaseOnset / 1000

model <- glmer(Animate ~ Target*TimeS + (1 | Trial) + (1 | ParticipantName), data = data, family="binomial")
summary(model)
  # no errors!
  
  # now interpret these effects... - what do main effects and interactions mean?

# visualize this fitted model
data$prediction <- predict(model, data)
# in logit space...
ggplot(data, aes(x=TimeS, y=prediction, color=Target)) +
                                                            geom_point()
# as probabilities...
data$prediction_prob <- exp(data$prediction) / (exp(data$prediction) + 1)
ggplot(data, aes(x=TimeS, y=prediction_prob, color=Target)) +
                              stat_summary(fun.y=mean, geom="point")

# this plot doesn't look too much like our actual data, though we are starting to see the
# "gist" of the pattern...
#
# One clear issue is that it thinks there's a difference at timepoint 0 (i.e., the main
# effect of Target) despite the fact that our actual groups don't diverge until later 
# in the trial. This is because we are only looking at linear growth over time,
# but we'll be able to address with growth curve analyses, later...

# Dale Barr suggests a particular kind of linear time analysis: aggregated empirical logit.
# pros:
#    - by time aggregation, we somewhat address the problem of interdependence between
#           ms-by-ms observations.
#    - Empirical logit aggregation allows us to approximate the logistic
#           curve with a linear model. (see also: arcsine-root transformation)
# 
# cons:
#     - because we are aggregating, we might want to do separate subject and items analyses again...
#     - we are still only looking at linear growth over time

# bin data into 50ms bins using the modulus operator
data$Bin <- data$TimeFromSubphaseOnset %/% 50
# equivalent: data$Bin <- floor(data$TimeFromSubphaseOnset / 50)
head(data[, c('TimeFromSubphaseOnset','Bin')])

# now aggregate across bins by participants (note, you can do this by items as well in a
# secondary analysis)
# calculate the proportion of looking to the animal (PropAnimal), as well as the number of
# samples looking to the Animal (y), and the total number of samples (N), and the
# start of the Bin in ms (Time)
#
# note: we lose the Trial grouping variable/random effect, because we aggregate across trials
#       this is why it's recommended that you do a by-subjects aggregation as well
binned <- ddply(data, .(ParticipantName,Target,Bin), summarize, PropAnimal = mean(Animate), y = sum(Animate), N = length(Animate), TimeS = min(TimeS))

# calculate the empirical logit (log-odds with a constant value to keep from getting -Infinity)
binned$elog <- log( (binned$y + .5) / (binned$N - binned$y + .5) )

# he also recommends that we calculate weights which, in weighted linear regression,
# increase/decrease the weight of the error at each timepoint; more samples == higher weighted error
binned$wts <- 1/(binned$y + .5) + 1/(binned$N - binned$y + .5)

# fit an unweighted empirical logit model
model <- lmer(elog ~ Target*TimeS + (1 | ParticipantName), data = binned)
summary(model)
  # notice the empirical logit estimates are very similar to our actual logits
  # from the previous model

# we can also account for Target and TimeS random slopes by Participant
# accounting for Time as a random effect is always recommended -- lots of individual noise here
model <- lmer(elog ~ Target*TimeS + (1 + Target + TimeS | ParticipantName), data = binned)
summary(model)

# ...or use weighted regression...
model <- lmer(elog ~ Target*TimeS + (1 + Target + TimeS | ParticipantName), data = binned, weights = 1/wts)
summary(model)

# another transformation we can use is the Arcsine-root transformation

# just like logits and empirical logits,
# it "stretches out" the ceiling and floor of the distribution, undoing the natural
# logistic curve inherent to probabilities
asin(sqrt(.5))
asin(sqrt(.6))
asin(sqrt(.95))
asin(sqrt(.99))

plot(seq(.0,1,by=.1),asin(sqrt(seq(0,1,by=.1))))

binned$Arcsin <- asin(sqrt(binned$PropAnimal))
head(binned)
model <- lmer(Arcsin ~ Target*TimeS + (1 + Target + TimeS | ParticipantName), data = binned)
summary(model)

# independent practice:
#
# try fitting logistic, empirical logit, and arcsin DV models to the data
# including "Sex", "Age", and "TrialNumber" factors. Should these be random
# or fixed effects? Are they significant predictors?
#
# what happens if you factor Age before fitting the model? note:
# this happens a lot by mistake when R tries to guess what kind of data
# is in a column
#
# advanced: visualize the models' predictions using a new, clean dataframe so that
# we get a nice, publishable model fit figure