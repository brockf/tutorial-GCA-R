# tutorial 3 - independent work

library(ggplot2)
library(lme4)
library(plyr)
library(ez)

# load our data
data <- read.csv('data-eyetracking.csv')

data$TargetCoded <- -.5
data[which(data$Target == 'Artefact'), 'TargetCoded'] <- .5

data$SexCoded <- -.5
data[which(data$Sex == 'F'), 'SexCoded'] <- .5

data$TimeS <- data$TimeFromSubphaseOnset / 1000;

model <- glmer(Animate ~ (TargetCoded + SexCoded + TrialNumber + Age + TimeS)^2 + (1 + TargetCoded + TimeS + TrialNumber | ParticipantName) + (1 | Trial), family="binomial", data=data)
  # takes forever to fit... I stopped after 10 minutes

data$Bin <- data$TimeFromSubphaseOnset %/% 50

binned <- ddply(data, .(ParticipantName,Age,TargetCoded,SexCoded,Bin), summarize, PropAnimal = mean(Animate), y = sum(Animate), N = length(Animate), TimeS = min(TimeS))

# calculate the empirical logit
binned$elog <- log( (binned$y + .5) / (binned$N - binned$y + .5) )
binned$wts <- 1/(binned$y + .5) + 1/(binned$N - binned$y + .5)

model <- lmer(elog ~ (TargetCoded + SexCoded + Age + TimeS)^2 + (1 + TargetCoded + TimeS | ParticipantName), data=binned, weights=1/wts)
summary(model)
  # weird, no main effect of TargetCoded anymore...
  # why?
  # because Age isn't centered, and so it's estimating main effects at Age 0 (newborn...)

model <- lmer(elog ~ (TargetCoded + SexCoded + scale(Age,center=T,scale=F) + TimeS)^2 + (1 + TargetCoded + TimeS | ParticipantName), data=binned, weights=1/wts)
summary(model)
  # fixes it...

# let's center Age for good, now
binned$AgeC <- scale(binned$Age, scale=F, center=T)
  
binned$Arcsin <- asin(sqrt(binned$PropAnimal))

model <- lmer(Arcsin ~ (TargetCoded + SexCoded + AgeC + TimeS)^2 + (1 + TargetCoded + TimeS | ParticipantName), data=binned)
summary(model)

drop1(model,~.,test="Chisq")

# visualize a model's predictions
# note I use PropAnimal here for interpretable DV's
model <- lmer(PropAnimal ~ (TargetCoded + AgeC + TimeS)^2 + (1 + TargetCoded + TimeS | ParticipantName), data=binned)
summary(model)

predictions <- data.frame(
                      rep(as.vector(unique(binned$TimeS)),times=4),
                      rep(c(-1,+1),each=(length(unique(binned$TimeS))*2)), # Age, centered (younger/older)
                      rep(c(-.5,.5,-.5,.5),each=length(unique(binned$TimeS))),
                      rep(c('Animal','Artefact','Animal','Artefact'),each=length(unique(binned$TimeS)))
                        )
colnames(predictions) <- c('TimeS','AgeC','TargetCoded','Target')

predictions$prediction <- predict(model, re.form=NA, predictions)
  # note: re.form = NA sets all random effects to 0 so that
  # it works on new data!!

ggplot(predictions, aes(x=TimeS, y=prediction, color=Target)) +
                                                    geom_point() +
                                                    geom_line() +
                                                    facet_grid(.~AgeC)

  # this is how a linear-only model reconstructs our data...
  # could be better...

# what happens when we mess with the Age column?

binned$AgeC <- factor(binned$AgeC)

model <- lmer(Arcsin ~ (TargetCoded + SexCoded + AgeC + TimeS)^2 + (1 + TargetCoded + TimeS | ParticipantName), data=binned)
summary(model)

binned$AgeC <- as.numeric(as.character(binned$AgeC))

model <- lmer(Arcsin ~ (TargetCoded + SexCoded + AgeC + TimeS)^2 + (1 + TargetCoded + TimeS | ParticipantName), data=binned)
summary(model)
  # better...

