########################################################################
# analysis script for Faulkenberry & Giere operator preview experiment

library(tidyverse)
library(BayesFactor)

# demographic
demo = read.csv("https://raw.githubusercontent.com/tomfaulkenberry/opPreviewExperiment/master/data/demo.csv")
mean(demo$age)
sd(demo$age)
table(demo$gender)



# read in raw data files
rawdata = data.frame()
filestem = "https://raw.githubusercontent.com/tomfaulkenberry/opPreviewExperiment/master/data/subject-"
for (n in 1:length(demo$subject)){
  file = paste(filestem, n, ".csv", sep="")
  temp = read.csv(file)
  d = select(temp, SOAcondition, accuracy, operation, rt, subject_nr, truth)
  d$subject_nr = n
  rawdata = rbind(rawdata,d)
}

# get rid of Subject 7 and any errors / long RTs / etc
clean = rawdata %>%
  filter(subject_nr != 7) %>%
  filter(accuracy==1 & rt > 200 & rt < mean(rt) + 3*sd(rt))


# quick visualization of RTs
clean %>% 
  ggplot(aes(x=rt)) +
  geom_density()

# quick method to see descriptives
clean %>%
  group_by(truth, operation, SOAcondition) %>%
  summarize(mRT = mean(rt), medRT = median(rt))
 

# collapse data and export for JASP

collapsed = clean %>%
  group_by(subject_nr,truth, operation, SOAcondition) %>%
  summarize(mRT = median(rt)) %>%
  write.csv(file="~/Desktop/opPreview.csv")


