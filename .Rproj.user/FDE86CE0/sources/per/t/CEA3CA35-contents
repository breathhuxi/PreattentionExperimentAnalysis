summary(allLogs)

source("helper_functions.R")

# create a new numerical OC column "OCNum"
allLogs$OCNum <- ifelse(allLogs$OC == "Small", 9, ifelse(allLogs$OC == "Medium", 25, 49))

correctTrials <- allLogs[allLogs$ErrorCount == "0", ]
correctTrials$OCNum <- ifelse(correctTrials$OC == "Small", 9, ifelse(correctTrials$OC == "Medium", 25, 49))
View(correctTrials)

strokeTrials <- correctTrials[correctTrials$VV == "Stroke", ]
View(strokeTrials)

fillTrials <- correctTrials[correctTrials$VV == "Fill", ]
View(fillTrials)

strokeFillTrials <- correctTrials[correctTrials$VV == "StrokeFill", ]
View(strokeFillTrials)

### analyses and plots

# H1: VV "Stroke color" is preattentive
# Step1: one-way ANOVA that looks at the effect of OC on VisualSearchTime => p<0.05
library(ez)
ezANOVA(data=strokeTrials, dv=.(VisualSearchTime), wid=.(ParticipantID), within=.(OC), detailed=TRUE)
# Step2: in case of a small p-value, try linear regression to see correlation between OC and VisualSearchTime => p<0.05
library(plyr)
stroke_experiment_summary <- summarySE(strokeTrials, measurevar="VisualSearchTime", groupvars=c("ParticipantID","Block1","OCNum"))
View(stroke_experiment_summary)
stroke_linear_model <- lm(stroke_experiment_summary$VisualSearchTime ~ stroke_experiment_summary$OCNum)
summary(stroke_linear_model)
# Step3: linear regression must be interpreted with caution, needs to be visualized
library(ggplot2)
ggplot(stroke_experiment_summary, aes(x=OCNum, y=VisualSearchTime, color=ParticipantID)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) +
  theme(text=element_text(size=10), legend.position="top")

# H2: VV "Fill color" is preattentive
# Step1: one-way ANOVA that looks at the effect of OC on VisualSearchTime => p<0.05
ezANOVA(data=fillTrials, dv=.(VisualSearchTime), wid=.(ParticipantID), within=.(OC), detailed=TRUE)
# Step2: in case of a small p-value, try linear regression to see correlation between OC and VisualSearchTime => p>0.05
fill_experiment_summary <- summarySE(fillTrials, measurevar="VisualSearchTime", groupvars=c("ParticipantID","Block1","OCNum"))
fill_linear_model <- lm(fill_experiment_summary$VisualSearchTime ~ fill_experiment_summary$OCNum)
summary(fill_linear_model)
# no Step3 since no significant regression was found

# H3: VV "Stroke and Fill color" is preattentive
# Step1: one-way ANOVA that looks at the effect of OC on VisualSearchTime => p<0.05
ezANOVA(data=strokeFillTrials, dv=.(VisualSearchTime), wid=.(ParticipantID), within=.(OC), detailed=TRUE)
# Step2: in case of a small p-value, try linear regression to see correlation between OC and VisualSearchTime => p<0.05
stroke_fill_experiment_summary <- summarySE(strokeFillTrials, measurevar="VisualSearchTime", groupvars=c("ParticipantID","Block1","OCNum"))
stroke_fill_linear_model <- lm(stroke_fill_experiment_summary$VisualSearchTime ~ stroke_fill_experiment_summary$OCNum)
summary(stroke_fill_linear_model)
# Step3: linear regression must be interpreted with caution, needs to be visualized
ggplot(stroke_fill_experiment_summary, aes(x=OCNum, y=VisualSearchTime, color=ParticipantID)) +
  geom_point(shape=1) +
  geom_smooth(method=lm) +
  theme(text=element_text(size=10), legend.position="top")

# H4: 
# Step1: one-way ANOVA that looks at the effect of VV on VisualSearchTime
ezANOVA(data=correctTrials, dv=.(VisualSearchTime), wid=.(ParticipantID), within=.(VV), detailed=TRUE)
# Step2: post-hoc test to see where the differences are
pairwise.t.test(allLogs$VisualSearchTime, allLogs$VV, paired=TRUE)
# Step3: plot the collected data
correct_experiment_summary <- summarySE(correctTrials, measurevar="VisualSearchTime", groupvars=c("VV"))
View(correct_experiment_summary)
ggplot(correct_experiment_summary, aes(x=VV, y=VisualSearchTime, fill=VV)) +
         geom_bar(stat="identity") +
         geom_errorbar(aes(ymin=VisualSearchTime-ci, ymax=VisualSearchTime+ci), width=.2) +
         theme(text = element_text(size=10))
