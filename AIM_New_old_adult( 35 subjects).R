rm(list=ls())
install.packages('tidyr')
install.packages('ez')
install.packages('ggplot2')
install.packages('dplyr')
library(tidyr)
library(ez)
library(ggplot2)
library(dplyr)


##set working directory
setwd("D:/AIM project/Aim_New_Data/AIM new project")

##read in data, select relevant columns and rows, and merge files
#load the study data
list.files(pattern = glob2rx("Study_Period_804_Mar13_2020*")) %>%
  lapply(read.delim, stringsAsFactors = FALSE) %>%
  do.call(rbind, .) %>%
  select(RECORDING_SESSION_LABEL, EYE_USED, IP_LABEL, IA_LABEL, IA_FIXATION_COUNT, 
         cb, block, condition, listtype, list, label, trialtype, trial, trialnum,
         study_img1, study_img2, study_img1pos, study_img2pos, JOL_resp, JOL_RT) %>%
  filter(trialtype == 1) -> study_period1

list.files(path=".", pattern = glob2rx("AIM_OA_IAReports_Study_Period_April23_2020*")) %>%
  lapply(read.delim, stringsAsFactors = FALSE) %>%
  do.call(rbind, .) %>%
  select(RECORDING_SESSION_LABEL, EYE_USED, IP_LABEL, IA_LABEL, IA_FIXATION_COUNT,
         cb, block, condition, listtype, list, label, trialtype, trial, trialnum,
         study_img1, study_img2, study_img1pos, study_img2pos, JOL_resp, JOL_RT) %>%
  filter(trialtype == 1) -> study_period2


##Covert the variables
study_period1$JOL_resp <- as.integer(study_period1$JOL_resp)
study_period1$JOL_RT <- as.numeric(study_period1$JOL_RT)
study_period2$JOL_resp <- as.integer(study_period2$JOL_resp)
study_period2$JOL_RT <- as.numeric(study_period2$JOL_RT)


#load the test data
list.files(pattern = glob2rx("Test_Period_804_Mar13_2020*")) %>%
  lapply(read.delim, stringsAsFactors = FALSE) %>%
  do.call(rbind, .) %>%
  select(RECORDING_SESSION_LABEL, EYE_USED, IP_LABEL, IA_LABEL, IA_FIXATION_COUNT,
         cb, block, condition, listtype, list, label, trialtype, trial, trialnum,
         test_img1, test_img2, test_img3, test_img2pos, test_img3pos,
         corr_resp, test_resp, test_RT, test_acc) %>%
  filter(trialtype == 2) -> test_period1

list.files(path=".", pattern = glob2rx("AIM_OA_IAReports_Test_Period_April23_2020*")) %>%
  lapply(read.delim, stringsAsFactors = FALSE) %>%
  do.call(rbind, .) %>%
  select(RECORDING_SESSION_LABEL, EYE_USED, IP_LABEL, IA_LABEL, IA_FIXATION_COUNT,
         cb, block, condition, listtype, list, label, trialtype, trial, trialnum,
         test_img1, test_img2, test_img3, test_img2pos, test_img3pos,
         corr_resp, test_resp, test_RT, test_acc) %>%
  filter(trialtype == 2) -> test_period2

##merge the study and test data
rbind(study_period2, study_period1) -> study_period
rbind(test_period2, test_period1) -> test_period

##covert the test variables
test_period$test_acc <- as.integer(test_period$test_acc)
test_period$test_resp <- as.integer(test_period$test_resp)
test_period$test_RT <- as.numeric(test_period$test_RT)

##load subject info, check MoCA scores
sub_info <- read.csv("Aim_OA_Subjectinfo.csv")

## delect the pliot study and the to-be tested participant information
sub_info <- tail(sub_info, -9) 
sub_info <- head(sub_info, -5) 

##modify the variables
sub_info$ID <- substr(sub_info$Filename, 5,6)
sub_info <- filter(sub_info, !ID %in% "pe")
sub_info <- select(sub_info, ID, Age, MOCA=MoCA.score)
sub_info$MOCA <- as.numeric(sub_info$MOCA)
sub_info$MOCA_PF <- ifelse(sub_info$MOCA < 26, "fail", "pass")
group_by(sub_info, MOCA_PF) %>%
  summarize(length(ID), mean_moca = mean(MOCA), mean_age = mean(Age)) -> moca_summary

## filter out the test recording label
study_period <- filter(study_period, !study_period$RECORDING_SESSION_LABEL %in% "test")
study_period <- filter(study_period, !study_period$RECORDING_SESSION_LABEL %in% "test_1")
test_period  <- filter(test_period, !test_period$RECORDING_SESSION_LABEL %in% "test")
test_period  <- filter(test_period, !test_period$RECORDING_SESSION_LABEL %in% "test_1")

## change the subject07 RECORDING_SESSION_LABEL 
study_period$RECORDING_SESSION_LABEL <- sub("aimoni07", "aimo07ni", study_period$RECORDING_SESSION_LABEL)
test_period$RECORDING_SESSION_LABEL <- sub("aimoni07", "aimo07ni", test_period$RECORDING_SESSION_LABEL)


##DATA ORGANIZATION##
##create ID variable
study_period$ID <- ifelse(study_period$RECORDING_SESSION_LABEL == "aim07", "49", substr(study_period$RECORDING_SESSION_LABEL,5,6))
test_period$ID <- ifelse(test_period$RECORDING_SESSION_LABEL == "aim07", "49", substr(test_period$RECORDING_SESSION_LABEL,5,6))

##filter out dropped subjects or missing data
study_period <- filter(study_period, !ID %in% c(07, 50))
test_period <- filter(test_period,   !ID %in% c(07, 50))

##merge subject info
study_period <- left_join(study_period, sub_info, by = "ID")
test_period <-  left_join(test_period, sub_info, by = "ID")

##check counterbalances
group_by(test_period, ID) %>%
  summarize(cb = cb[1]) -> cb_bysub
group_by(test_period, cb) %>%
  summarize(sub = length(unique(ID))) -> cb_check

##filter 0's from RT data (i.e. they didn't have an RT of zero, so we use NA)
study_period$JOL_RT[study_period$JOL_RT == 0] <- NA
study_period$JOL_resp[study_period$JOL_resp == 0] <- NA
test_period$test_RT[test_period$test_RT == 0] <- NA

##rearrange IA data into columns (so that each row corresponds to one trial)
study_period <- spread(study_period, IA_LABEL, IA_FIXATION_COUNT, fill = NA)
test_period <- spread(test_period, IA_LABEL, IA_FIXATION_COUNT, fill = NA)

##filter out trials with no fixations in either left or right IA (we assume this means the eye tracking was bad, or participant wasn't paying attention)
study_period$Study_left_IA[study_period$Study_left_IA == 0 & study_period$Study_right_IA == 0] <- NA
study_period$Study_right_IA[is.na(study_period$Study_left_IA)] <- NA

test_period$Test_Left_IA[test_period$Test_Left_IA == 0 & test_period$Test_Right_IA == 0] <- NA
test_period$Test_Right_IA[is.na(test_period$Test_Left_IA)] <- NA
test_period$Test_Top_IA[is.na(test_period$Test_Left_IA)] <- NA

##recode left/right IA as A/B/ vs B/C/ for study 
study_period$AB <- ifelse(study_period$study_img1pos == "[256, 384]", study_period$Study_left_IA,
                          ifelse(study_period$study_img1pos == "(256, 384)", study_period$Study_left_IA, study_period$Study_right_IA))
study_period$BC <- ifelse(study_period$study_img2pos == "[256, 384]", study_period$Study_left_IA, 
                          ifelse(study_period$study_img2pos == "(256, 384)", study_period$Study_left_IA, study_period$Study_right_IA))

##recode left/right IA as corr/foil for test 
test_period$corr <- ifelse(test_period$test_img2pos == "[256, 576]", test_period$Test_Left_IA, 
                           ifelse(test_period$test_img2pos == "(256, 576)", test_period$Test_Left_IA, test_period$Test_Right_IA))
test_period$foil <- ifelse(test_period$test_img3pos == "[256, 576]", test_period$Test_Left_IA, 
                           ifelse(test_period$test_img3pos == "(256, 576)", test_period$Test_Left_IA, test_period$Test_Right_IA))

##filout out the subject 29 because of missing response 
study_period <- filter(study_period, !ID == 29)
test_period <- filter(test_period, !ID == 29)

##CREATE DATA SUMMARIES##
##test data accuracy summary
filter(test_period, test_resp > 0) %>%
  group_by(ID, listtype, condition) %>%
  summarise(trials = length(ID), 
            MeanACC = mean(test_acc, na.rm = TRUE), 
            MeanRT = mean(test_RT, na.rm = TRUE),
            RT_trials = length(test_RT[!is.na(test_RT)]),
            MOCA_PF = MOCA_PF[1]) -> summary_test


##test data accuracy summary (overall)
filter(test_period, test_resp > 0) %>%
  group_by(ID, listtype) %>%
  summarise(trials = length(ID), 
            MeanACC = mean(test_acc, na.rm = TRUE),
            Age = Age[1],
            MOCA = MOCA[1]) -> acc_cond

##test correlations between accuracy and MOCA for Scene and object conditions
cor.test(x = acc_cond$MeanACC[acc_cond$listtype=="Scene"],y = acc_cond$MOCA[acc_cond$listtype=="Scene"], method = "pearson")
cor.test(x = acc_cond$MeanACC[acc_cond$listtype=="Object"],y = acc_cond$MOCA[acc_cond$listtype=="Object"], method = "pearson")
 
ggplot(acc_cond, aes(x=MOCA, y=MeanACC, color = listtype)) +
  geom_point() + geom_smooth(method = "lm") +
  facet_wrap(~listtype, nrow = 2) +
  ylab("Accuracy") +
  theme_bw() +
  theme(legend.position = "none")  +
  scale_color_manual(values=c("#FFCC33", "#66CC00"))

##rearrange data by IA for fixation summaries
study_data_IA <- gather(study_period, IA, fix, AB:BC) 
test_data_IA <- gather(test_period, IA, fix, corr:foil)


##study and test summaries by condition and IA
group_by(study_data_IA, ID, listtype, condition, IA) %>%
  summarise(trials = length(ID), 
            MeanJOL = mean(JOL_resp, na.rm = TRUE), 
            JOLRT = mean(JOL_RT, na.rm = TRUE), 
            MeanFix = mean(fix, na.rm = TRUE),
            ET_trials = length(fix[!is.na(fix)]),
            MOCA_PF = MOCA_PF[1]) -> summary_studyIA 

group_by(test_data_IA, ID, listtype, condition, IA) %>%
  summarise(trials = length(ID), 
            MeanACC = mean(test_acc), 
            MeanRT = mean(test_RT, na.rm = TRUE),
            RT_trials = length(test_RT[!is.na(test_RT)]),
            MeanFix = mean(fix, na.rm = TRUE),
            ET_trials = length(fix[!is.na(fix)]),
            MOCA_PF = MOCA_PF[1]) -> summary_testIA 

##CREATE GROUP-LEVEL SUMMARIES WITH WITHIN SUBJECT ERROR MEASURES##
library(Rmisc)
summary_ACCtest <- summarySEwithin(data=summary_test, measurevar = "MeanACC", withinvars = c("listtype", "condition"), idvar = "ID", na.rm = TRUE)
summary_ACCtest_moca <- summarySEwithin(data=summary_test, measurevar = "MeanACC", betweenvars = "MOCA_PF", withinvars = c("listtype", "condition"), idvar = "ID", na.rm = TRUE)
summary_RTtest <- summarySEwithin(data=summary_test, measurevar = "MeanRT", withinvars = c("listtype", "condition"), idvar = "ID", na.rm = TRUE)
summary_fixstudy <- summarySEwithin(data=summary_studyIA, measurevar = "MeanFix", withinvars = c("listtype", "condition", "IA"), idvar = "ID", na.rm = TRUE)
summary_fixtest <- summarySEwithin(data=summary_testIA, measurevar = "MeanFix", withinvars = c("listtype", "condition", "IA"), idvar = "ID", na.rm = TRUE)
summary_fixstudy_moca <- summarySEwithin(data=summary_studyIA, measurevar = "MeanFix", betweenvars = "MOCA_PF", withinvars = c("listtype", "condition", "IA"), idvar = "ID", na.rm = TRUE)
summary_fixtest_moca <- summarySEwithin(data=summary_testIA, measurevar = "MeanFix", betweenvars = "MOCA_PF", withinvars = c("listtype", "condition", "IA"), idvar = "ID", na.rm = TRUE)

##ACCURACY ANALYSES
##plot all subjects
ggplot(summary_test, aes(y= MeanACC, x=listtype, group = ID, color = ID)) +
  geom_point(size = 2) + geom_line() +
  facet_wrap(~condition) +
  xlab("Condition") +
  ylab("Accuracy") +
  geom_hline(yintercept = 0.5, linetype = "dashed")

##plot group data
ggplot(summary_ACCtest, aes (x=condition, y=MeanACC, group = listtype, colour = listtype)) +
  geom_point(size = 4, alpha = .8) +
  geom_errorbar(aes(ymin=MeanACC-ci, ymax =MeanACC+ci), width = 0.05) +
  ggtitle("Accuracy for Object vs. Scene Associations") +
  xlab("Trial type") + ylab("Accuracy") +
  ylim(0.2,.99) +
  theme_classic() + theme(plot.title = element_text(size = 14, hjust = 0.4)) +
  theme(legend.title=element_blank(), legend.text=element_text(size = 12)) +
  scale_x_discrete(limits=c("AB","BC","AC")) +
  scale_color_manual(values=c("#FFCC33", "#66CC00")) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size =12)) 

##plot group data (by MoCA)
ggplot(summary_ACCtest_moca, aes (x=condition, y=MeanACC, group = MOCA_PF, colour = MOCA_PF)) +
  geom_point(size = 4, alpha = .8) +
  geom_errorbar(aes(ymin=MeanACC-ci, ymax =MeanACC+ci), width = 0.05) +
  facet_wrap(~listtype) +
  ggtitle("Accuracy for Object vs. Scene Associations") +
  xlab("Trial type") + ylab("Accuracy") +
  ylim(0.5,.9) +
  theme_classic() + theme(plot.title = element_text(size = 14, hjust = 0.4)) +
  theme(legend.title=element_blank(), legend.text=element_text(size = 12)) +
  scale_x_discrete(limits=c("AB","BC","AC")) +
  scale_color_manual(values=c("firebrick", "dodgerblue")) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size =12)) 

##accuracy anova by condition
anova.ACCbycond = ezANOVA(data = summary_test,
                          dv = MeanACC,
                          wid = ID,
                          within = .(listtype, condition),
                          white.adjust = FALSE, 
                          type = 3,
                          return_aov = TRUE)
anova.ACCbycond$ANOVA

##follow up tests, if needed: paired comparisons by trial type (correct p-values for multiple comparisons)
AB.data <- subset(summary_test, condition == "AB")
t.test(AB.data$MeanACC~AB.data$listtype, paired = TRUE)


BC.data <- subset(summary_test, condition == "BC")
t.test(BC.data$MeanACC~BC.data$listtype, paired = TRUE)

AC.data <- subset(summary_test, condition == "AC")
t.test(AC.data$MeanACC~AC.data$listtype, paired = TRUE)

##REACTION TIME ANALYSES
##plot all subjects
ggplot(summary_test, aes(y= MeanRT, x=listtype, group = ID, color = ID)) +
  geom_point(size =2) + geom_line() +
  facet_wrap(~condition) +
  xlab("Condition") +
  ylab("Reaction Time") 

##plot group data
ggplot(summary_RTtest, aes(x=condition, y=MeanRT, group = listtype, colour = listtype)) +
  geom_point(size = 3, alpha = .8) +
  geom_errorbar(aes(ymin=MeanRT-ci, ymax =MeanRT+ci), width = 0.05) +
  ggtitle("Reaction Time") +
  xlab("Trial type") + ylab("Accuracy") +
  theme_classic() + theme(plot.title = element_text(size = 14, hjust = 0.5)) +
  theme(legend.title=element_blank(), legend.text=element_text(size = 10)) +
  scale_x_discrete(limits=c("AB","BC","AC")) +
  scale_color_manual(values=c("#FFCC33", "#66CC00")) +
  theme(axis.text=element_text(size=10), axis.title=element_text(size =10)) 

##accuracy anova by condition
anova.RTbycond = ezANOVA(data = summary_test,
                         dv = MeanRT,
                         wid = ID,
                         within = .(listtype, condition),
                         white.adjust = FALSE, 
                         type = 3,
                         return_aov = TRUE)
anova.RTbycond$ANOVA

##follow up tests, if needed: paired comparisons by trial type (correct p-values for multiple comparisons)
AB.data <- subset(summary_test, condition == "AB")
t.test(AB.data$MeanRT~AB.data$listtype, paired = TRUE)

BC.data <- subset(summary_test, condition == "BC")
t.test(BC.data$MeanRT~BC.data$listtype, paired = TRUE)

AC.data <- subset(summary_test, condition == "AC")
t.test(AC.data$MeanRT~AC.data$listtype, paired = TRUE)

##FIXATIONs AT STUDY ANALYSES
##plot fixation count at study by subject
ggplot(summary_studyIA, aes(x=IA, y = MeanFix, colour=ID, group=ID)) +
  geom_line() + geom_point(size =3, alpha = .5) +
  facet_wrap(~listtype + condition, nrow = 2) +
  ggtitle("Fixations by condition and item at study")

##plot fixation count at study for the group
#rename variables for plot
mutate(summary_fixstudy, IA = case_when(condition == "AB" & IA == "AB" ~ "A",
                                        condition == "AB" & IA == "BC" ~ "B",
                                        condition == "BC" & IA == "AB" ~ "B",
                                        condition == "BC" & IA == "BC" ~ "C")) -> summary_fixstudy


ggplot(summary_fixstudy, aes(x=IA, y=MeanFix, colour=listtype, group=listtype)) + 
  geom_errorbar(aes(ymin=MeanFix-ci, ymax =MeanFix+ci), width = 0.05) +
  geom_point(size = 3, alpha=.7) + geom_line() +
  facet_wrap(~condition, nrow=1, scales = "free_x") +
  ggtitle("Fixation Count at Study") +
  xlab("Study Item") + ylab("Mean Fixation Count") +
  #ylim(-.5,7.5) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size = 14, hjust = 0.5)) +
  theme(legend.title=element_blank()) +
  scale_color_manual(values=c("#FFCC33", "#66CC00")) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size =12), legend.text = element_text(size =12))

##plot fixations at study (by moca)
mutate(summary_fixstudy_moca, IA = case_when(condition == "AB" & IA == "AB" ~ "A",
                                             condition == "AB" & IA == "BC" ~ "B",
                                             condition == "BC" & IA == "AB" ~ "B",
                                             condition == "BC" & IA == "BC" ~ "C")) -> summary_fixstudy

ggplot(summary_fixstudy_moca, aes(x=IA, y=MeanFix, colour=MOCA_PF, group=MOCA_PF)) + 
  geom_errorbar(aes(ymin=MeanFix-ci, ymax =MeanFix+ci), width = 0.05) +
  geom_point(size = 3, alpha=.7) + geom_line() +
  facet_wrap(~condition+listtype, nrow=2, scales = "free_x") +
  ggtitle("Fixation Count at Study") +
  xlab("Study Item") + ylab("Mean Fixation Count") +
  #ylim(-.5,7.5) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size = 14, hjust = 0.25)) +
  theme(legend.title=element_blank()) +
  scale_color_manual(values=c("firebrick", "dodgerblue")) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size =12), legend.text = element_text(size =12))

##anova for fixation count
anova.StudyFixbycond = ezANOVA(data = summary_studyIA,
                               dv = MeanFix,
                               wid = ID,
                               within = .(listtype, condition, IA),
                               white.adjust = FALSE, 
                               type = 3,
                               return_aov = TRUE)
anova.StudyFixbycond$ANOVA

##follow ups by trial type (if necessary)
##AB trials
study_AB <- subset(summary_studyIA, condition == "AB")
anova.study_AB <- ezANOVA(data = study_AB,
                          dv = MeanFix,
                          wid = ID,
                          within = .(listtype, IA),
                          white.adjust = FALSE,
                          type = 3, 
                          return_aov = TRUE)
anova.study_AB$ANOVA

##follow-up t-tests (correct p-values for MC)
study_AB_sc <- subset(study_AB, listtype == "Scene")
study_AB_obj <- subset(study_AB, listtype == "Object")
t.test(study_AB_sc$MeanFix~study_AB_sc$IA, paired = TRUE)
t.test(study_AB_obj$MeanFix~study_AB_obj$IA, paired = TRUE)

study_BC <- subset(summary_studyIA, condition == "BC")
anova.study_BC <- ezANOVA(data = study_BC,
                          dv = MeanFix,
                          wid = ID,
                          within = .(listtype, IA),
                          white.adjust = FALSE,
                          type = 3, 
                          return_aov = TRUE)
anova.study_BC$ANOVA

study_BC_sc <- subset(study_BC, listtype == "Scene")
study_BC_obj <- subset(study_BC, listtype == "Object")
t.test(study_BC_sc$MeanFix~study_BC_sc$IA, paired = TRUE)
t.test(study_BC_obj$MeanFix~study_BC_obj$IA, paired = TRUE)

ggplot(summary_testIA, aes(x=IA, y = MeanFix, colour=ID, group=ID)) +
  geom_line() + geom_point(size =3, alpha = .5) +
  facet_wrap(~listtype + condition, nrow = 2) +
  ggtitle("Fixations by Condition and Item at Test")

##plot fixation count at test for the group
ggplot(summary_fixtest, aes(x=IA, y=MeanFix, colour=listtype, group=listtype)) + 
  geom_errorbar(aes(ymin=MeanFix-ci, ymax =MeanFix+ci), width = 0.05) +
  geom_point(size = 3, alpha=.7) + geom_line() +
  facet_wrap(~condition, nrow=1) +
  ggtitle("Fixation Count at Test") +
  xlab("Test Item") + ylab("Mean Fixation Count") +
  #ylim(-.5,7.5) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size = 14, hjust = 0.5)) +
  theme(legend.title=element_blank()) +
  scale_color_manual(values=c("#FFCC33", "#66CC00")) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size =12), legend.text = element_text(size =12))

##plot fixation count at test (by moca)
ggplot(summary_fixtest_moca, aes(x=IA, y=MeanFix, colour=MOCA_PF, group=MOCA_PF)) + 
  geom_errorbar(aes(ymin=MeanFix-ci, ymax =MeanFix+ci), width = 0.05) +
  geom_point(size = 3, alpha=.7) + geom_line() +
  facet_wrap(~condition+listtype, nrow=2) +
  ggtitle("Fixation Count at Test") +
  xlab("Test Item") + ylab("Mean Fixation Count") +
  #ylim(-.5,7.5) +
  theme_bw() + 
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank()) +
  theme(plot.title = element_text(size = 14, hjust = 0.25)) +
  theme(legend.title=element_blank()) +
  scale_color_manual(values=c("firebrick", "dodgerblue")) +
  theme(axis.text=element_text(size=12), axis.title=element_text(size =12), legend.text = element_text(size =12))

##anova for fixation count
anova.TestFixbycond = ezANOVA(data = summary_testIA,
                              dv = MeanFix,
                              wid = ID,
                              within = .(listtype, condition, IA),
                              white.adjust = FALSE, 
                              type = 3,
                              return_aov = TRUE)
anova.TestFixbycond$ANOVA

##follow ups by trial type (if necessary)
##AB trials
test_AB <- subset(summary_testIA, condition == "AB")
anova.test_AB <- ezANOVA(data = test_AB,
                         dv = MeanFix,
                         wid = ID,
                         within = .(listtype, IA),
                         white.adjust = FALSE,
                         type = 3, 
                         return_aov = TRUE)
anova.test_AB$ANOVA

##follow-up t-tests (correct p-values for MC)
test_AB_sc <- subset(test_AB, listtype == "Scene")
test_AB_obj <- subset(test_AB, listtype == "Object")
t.test(test_AB_sc$MeanFix~test_AB_sc$IA, paired = TRUE)
t.test(test_AB_obj$MeanFix~test_AB_obj$IA, paired = TRUE)

##BC trials
test_BC <- subset(summary_testIA, condition == "BC")
anova.test_BC <- ezANOVA(data = test_BC,
                         dv = MeanFix,
                         wid = ID,
                         within = .(listtype, IA),
                         white.adjust = FALSE,
                         type = 3, 
                         return_aov = TRUE)
anova.test_BC$ANOVA

##follow-up t-tests (correct p-values for MC)
test_BC_sc <- subset(test_BC, listtype == "Scene")
test_BC_obj <- subset(test_BC, listtype == "Object")
t.test(test_BC_sc$MeanFix~test_BC_sc$IA, paired = TRUE)
t.test(test_BC_obj$MeanFix~test_BC_obj$IA, paired = TRUE)

##AC trials
test_AC <- subset(summary_testIA, condition == "AC")
anova.test_AC <- ezANOVA(data = test_AC,
                         dv = MeanFix,
                         wid = ID,
                         within = .(listtype, IA),
                         white.adjust = FALSE,
                         type = 3, 
                         return_aov = TRUE)
anova.test_AC$ANOVA

##follow-up t-tests (correct p-values for MC)
test_AC_sc <- subset(test_AC, listtype == "Scene")
test_AC_obj <- subset(test_AC, listtype == "Object")
t.test(test_AC_sc$MeanFix~test_AC_sc$IA, paired = TRUE)
t.test(test_AC_obj$MeanFix~test_AC_obj$IA, paired = TRUE)

detach("package:dplyr", unload = TRUE)
detach("package:Rmisc", unload = TRUE)
