vocab_inds <- read.csv('data-vocab-individuals.csv')
vocab_inds$ageC <- scale(vocab_inds$age, center=T,scale=F)
vocab_inds$birth_orderC <- scale(vocab_inds$birth_order, center=T, scale=F)
vocab_inds$mom_edC <- scale(vocab_inds$mom_ed, center=T, scale=F)
vocab_inds$genderContrast <- -.5
vocab_inds[which(vocab_inds$gender == 'M'), 'genderContrast'] <- .5

model <- lm(productive ~ ageC + genderContrast + birth_orderC + mom_edC, data = vocab_inds)
summary(model)

library(ggplot2)

# visualize gender and age effects
ggplot(vocab_inds, aes(x=age, y=productive, color=gender)) + 
                      geom_point() +
                      stat_smooth(method="lm") +
                      scale_x_continuous(name="Age (Months)") +
                      scale_y_continuous(name="Vocabulary")
                    
# visualize birth order effects
# ugly median split:
vocab_inds$birth_orderSplit <- 'Early'
vocab_inds[which(vocab_inds$birth_order > median(vocab_inds$birth_order, na.rm=T)), 'birth_orderSplit'] <- 'Late'
ggplot(vocab_inds, aes(x=age, y=productive, color=birth_orderSplit)) + 
                                      geom_point() +
                                      stat_smooth(method="lm") +
                                      scale_x_continuous(name="Age (Months)") +
                                      scale_y_continuous(name="Vocabulary")

# right now, our model's intercept (292.37) represents the estimated productive vocabulary
# for a genderless baby at 22.91 months born as the 1.59th child born to a mom with 14.98 years
# of education
mean(vocab_inds$age)
mean(vocab_inds$birth_order, na.rm=T)
mean(vocab_inds$mom_ed, na.rm=T)

# this is neither the overall mean, nor the mean of any particular cell
mean(vocab_inds$productive, na.rm=T)
  # 292.64

# but if we center the genderContrast variable, it should be the overall mean
vocab_inds$genderContrastC <- scale(vocab_inds$genderContrast, center=T, scale=F)
model <- lm(productive ~ ageC + genderContrastC + birth_orderC + mom_edC, data = vocab_inds)
summary(model)

# replicates our model
drop1(model,~.,test="F")

# visualize our model's predictions for our important factors
vocab_inds$predicted <- predict(model, vocab_inds)

vocab_inds$group <- paste(vocab_inds$gender, vocab_inds$birth_orderSplit, sep=' / ')
vocab_inds$group <- factor(vocab_inds$group)

ggplot(vocab_inds, aes(x=age, y=predicted, color=group)) + 
                              geom_point() +
                              stat_smooth(method="lm", se=F) +
                              scale_x_continuous(name="Age (Months)") +
                              scale_y_continuous(name="Predicted Vocabulary")

# fit a crazy interaction model
crazy_model <- lm(productive ~ (ageC + genderContrastC + birth_orderC + mom_edC)^3, data = vocab_inds)
summary(crazy_model)

vocab_inds$predicted <- predict(crazy_model, vocab_inds)

ggplot(vocab_inds, aes(x=age, y=predicted, color=group)) + 
  geom_point() +
  stat_smooth(method="lm", se=F) +
  scale_x_continuous(name="Age (Months)") +
  scale_y_continuous(name="Predicted Vocabulary")

# is the crazy model worth keeping?
anova(crazy_model, model, test="F")
