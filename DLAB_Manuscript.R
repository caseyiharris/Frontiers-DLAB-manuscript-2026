library(dplyr)
install.packages("tidyverse")
library(tidyverse)
library(readxl)
library(tidyr)
library(ggplot2)
library(cowplot)
install.packages("glmm")
library(glmm)

###################################NURSERY######################################

setwd("C:/Users/charris/Desktop")
surv<-read_excel("DLAB Nursery Surv_Updated.xlsx",sheet="Master") #to read your file
str(surv) 

#SURVIVAL!!

#Determine the class of the Factors (not the covariates):
class(surv$Structure)
#character
surv$Structure <- factor(surv$Structure)
class(surv$Structure)
#[1] "factor"

class(surv$Size)
surv$Size <- factor(surv$Size)
class(surv$Size)
#[1] "factor"


attach(surv)

#Test the effect of the factor
library(survival)
levels(surv$Structure)
#"Module" "Table"  "Tree"  
levels(surv$Size)
#"Large" "Medium" Small"


#Test the effect of the factor
library(survival)
str(surv)
str(X)
summary(surv)


X <- cbind(surv$Structure,surv$Size)
cox <- coxph(Surv(Time, Status) ~ Structure + Size, data = surv)
summary(cox)

#this is ussing large size class as reference and modules as reference...

cox<- coxph (Surv(Time, Status)~ X, method='breslow', data = surv)
summary(cox)


cox <- coxph(Surv(Time, Status) ~ Size, data = surv)
summary(cox)


cox.zph(cox)

#change the reference when running cox proportional hazards model: 

#SMALL as reference
surv$Size <- relevel(surv$Size, ref = "Small")

coxph(Surv(Time, Status) ~ Size, data = surv) #data used in manuscript -> small as reference

#MEDIUM as reference

surv$Size <- relevel(surv$Size, ref = "Medium")

coxph(Surv(Time, Status) ~ Size, data = surv)

#Large as reference

surv$Size <- relevel(surv$Size, ref = "Large")

coxph(Surv(Time, Status) ~ Size, data = surv)


#ALL COMPARISONS 

survdiff(Surv(Time, Status) ~ Size, data = surv)

# Small vs Medium
survdiff(Surv(Time, Status) ~ Size, data = surv,
         subset = Size %in% c("Small","Medium"))

# Small vs Large
survdiff(Surv(Time, Status) ~ Size, data = surv,
         subset = Size %in% c("Small","Large"))

# Medium vs Large
survdiff(Surv(Time, Status) ~ Size, data = surv,
         subset = Size %in% c("Medium","Large"))

#combined structure and size

coxph(Surv(Time, Status) ~ Structure * Size, data = surv)


# run within each structure
coxph(Surv(Time, Status) ~ Size, data = surv[surv$Structure=="Tree", ])
coxph(Surv(Time, Status) ~ Size, data = surv[surv$Structure=="Table", ])
coxph(Surv(Time, Status) ~ Size, data = surv[surv$Structure=="Module", ])

#COMBINED FIGURE:

surv$Group <- interaction(surv$Structure, surv$Size)

fit <- survfit(Surv(Days, Status) ~ Group, data = surv)
plot(fit, col = 1:9)

#Figure in manuscript!

library(survival)
library(survminer)
library(ggplot2)

fit <- survfit(Surv(Days, Status) ~ Size, data = surv)

p <- ggsurvplot(
  fit,
  facet.by = "Structure",
  data = surv,
  xlab = "Time (days)",
  palette = c("Small" = "green",
              "Medium" = "red",
              "Large" = "blue"),
  linetype = "Size",
  legend.title = "Size",
  conf.int = FALSE,
  ggtheme = theme_classic(base_size = 18)+
    theme(
      strip.background = element_rect(fill = "white", color = "black"),
      strip.text = element_text(size = 14, face = "bold")
    ),
  xlim = c(0, 270),
  break.time.by = 30,
  censor.shape = 16,
  censor.size = 3
  
) + 
  scale_color_manual(
    values = c("Small" = "green",
               "Medium" = "red",
               "Large" = "blue"),
    breaks = c("Small", "Medium", "Large")   #controls legend order ONLY
  ) +
  scale_linetype_manual(
    values = c("Small" = "longdash",
               "Medium" = "dotdash",
               "Large" = "solid"),
    breaks = c("Small", "Medium", "Large")   #keeps linetype legend aligned
  )

p

ggsave("SurvSizeStructure.png", p, width = 14, height = 7, units = "in", dpi = 300)


library(dplyr)
subset(surv, Status == 1) %>% count() #333 recruits died
subset(surv, Status == 0 & Time ==(0)) %>% count() #601 recruits deployed in nursery
subset(surv, Status == 0 & Time ==(5)) %>% count() #268 recruits still alive by final timepoint

#Structure survival

subset(surv, Status == 1 & Structure == "Table") %>% count() #19 recruits died on table 
subset(surv, Status == 0 & Structure == "Table" & Time ==(5)) %>% count() #40 recruits alive
(19/(19+40))*100
(40/(40+19))*100
#Table = 67% survival after nine months

subset(surv, Status == 1 & Structure == "Module") %>% count() #104 recruits died 
subset(surv, Status == 0 & Structure == "Module" & Time ==(5)) %>% count() #194 recruits alive
(194/(194+104))*100
#Module = 65% survival after nine months

subset(surv, Status == 1 & Structure == "Tree") %>% count() #210 recruits died 
subset(surv, Status == 0 & Structure == "Tree" & Time ==(5)) %>% count() #34 recruits alive
(34/(34+210))*100
#Tree = 13% survival after nine months


#size class survival

subset(surv, Status == 1 & Size == "Small") %>% count() #131 small recruits died 
subset(surv, Status == 0 & Size == "Small" & Time ==(5)) %>% count() #71 recruits alive
(71/(131+71))*100
#small = 35% survival after nine months

subset(surv, Status == 1 & Size == "Medium") %>% count() #111 medium recruits died 
subset(surv, Status == 0 & Size == "Medium" & Time ==(5)) %>% count() #88 recruits alive
(88/(111+88))*100

#Medium = 44% survival after nine months

subset(surv, Status == 1 & Size == "Large") %>% count() #91 large recruits died 
subset(surv, Status == 0 & Size == "Large" & Time ==(5)) %>% count() #109 recruits alive
(109/(91+109))*100
#Large = 55% survival after nine months


km <-survfit(Surv(Time,Status)~ Structure) #just for plot
# do not include interactions
summary(cox)

km$strata
plot(km, lwd=2, xlab = 'Time', ylab = '% Survival', lty=c(1,2,3,4,5),
     col=c(1,2,3,4,5,6,7,8),conf.int=F) # does not include 95% confidence interval for each curve

legend("bottomleft", legend=c("Module","Table","Tree"), lty=c(1,2,3,4,5), col=c(1,2,3,4,5)) 


km2 <-survfit(Surv(Time,Status)~ Size) #just for plot
# do not include interactions
summary(cox)

km2$strata
plot(km2, lwd=2, xlab = 'Time', ylab = '% Survival', lty=c(1,2,3,4,5),
     col=c(1,2,3,4,5,6,7,8),conf.int=F) # does not include 95% confidence interval for each curve

legend("bottomleft", legend=c("Large", "Medium", "Small"), lty=c(1,2,3,4,5), col=c(1,2,3,4,5)) 


#GROWTH!!

Growth_Alive<-read_excel("DLAB Absolute Change_Updated.xlsx",sheet="Alive_Only") 
str(Growth) 
Growth<-read_excel("DLAB Absolute Change_Updated.xlsx",sheet="Absolute Change") 



class(Growth_Alive$Size) 
class(Growth_Alive$Structure)

Growth_Alive$Size <- factor(Growth_Alive$Size)
Growth_Alive$Structure <- factor(Growth_Alive$Structure)


attach(Growth_Alive)

install.packages("car")
library(car)

#two way anova 
anova3<-aov(Absolute_Change~as.factor(Size)*as.factor(Structure),data=Growth_Alive)
leveneTest(Absolute_Change~as.factor(Size)*as.factor(Structure),data=Growth_Alive)    

TukeyHSD(anova3)

#COPIOLOT changes

anova3 <- aov(Absolute_Change ~ Size * Structure, data = Growth_Alive)
TukeyHSD(anova3)

#Levenes test for homogeneity of variance
leveneTest(Absolute_Change ~ Size * Structure)
leveneTest(Absolute_Change ~ interaction(Size, Structure), data = Growth_Alive)

leveneTest(Absolute_Change ~ Size, data = Growth_Alive)
leveneTest(Absolute_Change ~ Structure, data = Growth_Alive)

#Results show that I would reject the null = variances differ!
#groups have unequal variances -> due to this it may not be best to use ANOVA
           

# check normality of residuals
plot(anova3, which = 2)   # QQ plot
shapiro.test(residuals(anova3))

#Shapiro-Wilk normality test
#data:  residuals(anova3)
#W = 0.95902, p-value = 2.974e-10
#RESIDUALS ARE NOT NORMAL -> Reject the null 
#Deviations present but sample size is large and QQ plot shows points are roughly on the line 

#Suggests using a linear model 

install.packages("sandwich") #need to update R
install.packages("lmtest")

library(sandwich)
library(lmtest)

model <- lm(Absolute_Change ~ Size * Structure, data = Growth_Alive)

coeftest(model, vcov = vcovHC(model, type = "HC3"))
Anova(model, white.adjust = TRUE)


install.packages("emmeans")
library(emmeans)

emm <- emmeans(model, ~ Size)
pairs(emm)

emm <- emmeans(model, ~ Structure)
pairs(emm)
      

#Averages

#overall alive
mean(Growth_Alive$Absolute_Change, na.rm = TRUE)
sd(Growth_Alive$Absolute_Change, na.rm = TRUE)

se <- sd(Growth_Alive$Absolute_Change, na.rm = TRUE) / 
  sqrt(sum(!is.na(Growth_Alive$Absolute_Change)))



x <- Growth$`Relative Growth`

mean_val <- mean(x, na.rm = TRUE)
se_val   <- sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))

mean_val
se_val



mean(Growth_Alive$`Relative Growth`, na.rm = TRUE)
sd(Growth_Alive$`Relative Growth`, na.rm = TRUE)
se <- sd(Growth_Alive$`Relative Growth`, na.rm = TRUE) / 
  sqrt(sum(!is.na(Growth_Alive$`Relative Growth`)))

#Linear extension to compare with Chamberland Paper 

mean(Growth_Alive$`Linear Extension (cm/month)`, na.rm = TRUE)
sd(Growth_Alive$`Linear Extension (cm/month)`, na.rm = TRUE)

se <- sd(Growth_Alive$`Linear Extension (cm/month)`, na.rm = TRUE) / 
  sqrt(sum(!is.na(Growth_Alive$`Linear Extension (cm/month)`)))

#overall dead
mean(Growth$Absolute_Change, na.rm = TRUE)
sd(Growth$Absolute_Change, na.rm = TRUE)
se <- sd(Growth$Absolute_Change, na.rm = TRUE) / 
  sqrt(sum(!is.na(Growth$Absolute_Change)))



#size class alive
aggregate(Absolute_Change ~ Size, data = Growth_Alive,
          FUN = function(x) c(
            mean = mean(x, na.rm = TRUE),
            sd   = sd(x, na.rm = TRUE),
            se   = sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
          ))


#structure alive 
aggregate(Absolute_Change ~ Structure, data = Growth_Alive,
          FUN = function(x) c(
            mean = mean(x, na.rm = TRUE),
            sd   = sd(x, na.rm = TRUE),
            se   = sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
          ))

#Manuscript figure

Growth_Alive$Size <- factor(Growth_Alive$Size,
                            levels = c("Small", "Medium", "Large"))


p_size <- ggplot(Growth_Alive, aes(x = Size, y = Absolute_Change, fill = Size)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 2) +
  
  # Dotted zero line
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) +
  
  # Color palette (green, red, blue)
  scale_fill_manual(values = c(
    "Small" = "green",
    "Medium" = "red",
    "Large" = "blue"
  )) +
  
  annotate("text", x = 1, y = max(Growth_Alive$Absolute_Change) + 0.05, label = "a", size = 5) +
  annotate("text", x = 2, y = max(Growth_Alive$Absolute_Change) + 0.05, label = "b", size = 5) +
  annotate("text", x = 3, y = max(Growth_Alive$Absolute_Change) + 0.05, label = "b", size = 5) +
  
  labs(
    x = "Size class",
    y = expression("Absolute growth (cm"^2*")"),
  ) +
  
  theme_classic(base_size = 14) +
  theme(legend.position = "none")

p_size

#Structure


# Set factor order
Growth_Alive$Structure <- factor(Growth_Alive$Structure,
                                 levels = c("Module", "Table", "Tree"))

# Set y-position dynamically
y_pos <- max(Growth_Alive$Absolute_Change) * 1.05

p_struct <- ggplot(Growth_Alive, aes(x = Structure, y = Absolute_Change, fill = Structure)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 2) +
  
# Dotted zero line
geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) +
  
  # NEW color palette (different from size plot)
  scale_fill_manual(values = c(
    "Module" = "#FFFFFF",   
    "Table"  = "#BDBDBD",   
    "Tree"   = "#4D4D4D"    
  )) +
  
# SIGNIFICANCE LETTERS
# Based on your results: no significant differences → all "a"
annotate("text", x = 1, y = y_pos, label = "a", size = 5) +
  annotate("text", x = 2, y = y_pos, label = "a", size = 5) +
  annotate("text", x = 3, y = y_pos, label = "a", size = 5) +
  
  labs(
    x = "Nursery structure",
    y = expression("Absolute growth (cm"^2*")"),
  ) +
  
  theme_classic(base_size = 14) +
  theme(legend.position = "none")

p_struct


library(cowplot)


combined_plot <- plot_grid(
  p_size,
  p_struct,
  labels = c("A", "B"),
  ncol = 2,
  align = "hv",
  label_size = 16
)

combined_plot


ggsave("Growth_panels.png",
       combined_plot,
       width = 10,
       height = 5,
       dpi = 300)

#INITIAL SIZE AND 9 MONTH SURVIVAL 

Nine_Mon<-read_excel("DLAB Nursery Surv_Area.xlsx",sheet="Nine") 
str(Growth) 


surv_no_tree <- Nine_Mon %>%
  filter(Structure != "Tree")



library(ggplot2)

ggplot(surv_no_tree, aes(x = Initial_Size, y = Nine_Surv)) +
  
  # jitter points so they don't overlap
  geom_jitter(height = 0.05, width = 0, alpha = 0.4, size = 2) +
  
  # logistic regression curve
  geom_smooth(method = "glm",
              method.args = list(family = "binomial"),
              color = "#FF2D95",
              size = 1.2) +
  
  scale_y_continuous(
    
    breaks = c(0, 1),
    labels = c("Alive", "Dead")
  ) +
  
  labs(
    x = expression("Initial Recruit Area (cm"^2*")"),
    y = "Survival Status at 9 Months",
  ) +
  
  theme_classic(base_size = 14)


##################################OUTPLANT#############################


sizes <- c(
  0.345,0.07,0.31,0.849,0.523,1.108,0.18,0.586,0.176,1.42,0.133,0.221,
  1.724,0.988,0.48,1.009,0.492,1.397,0.523,0.952,0.246,0.282,0.228,0.109,
  0.774,0.403,0.58,1.107,1.156,1.866,1.227,1.192,0.559,1.227,1.494,0.316,
  0.802,1.176,0.819,1.442,0.297,0.414,0.105,1.549,0.998,0.143,0.236,0.774,
  0.473,0.818,0.814,1.073,1.061,2.465,0.733,2.15,0.17,0.257,0.796,0.47,
  0.644,0.689,0.213,0.225,1.166,1.179,1.06,1.685,1.46,0.513,0.68,0.452,
  2.315,1.411,1.068,0.702,3.829,0.537,0.1,0.211,0.987,0.587,0.66,0.423, 
  2.271,1.335,0.579,2.051,0.953,0.315,0.486,0.422,1.052,1.938,1.118,
  1.582,2.103,0.445,1.09,0.271,1.865,1.219,1.235,1.859,2.275,1.501,
  3.742,3.308,1.752,1.643,0.73,0.868,1.671,0.593,0.842,1.313,2.714,
  0.443,3.267,1.947,0.841,0.801,0.278,1.308,1.217,0.9,4.46,1.921,
  0.537,0.484,0,0.222,0.746,0.578,0.91,0.151,1.346,1.666,0.605,
  1.85,2.035,1.888,0.981,3.679,0.387,0.95,0.385,1.371,0.875,2.182,
  0.409,0.321,1.287,1.186,0.944,0.97,0.096,0.603,0.509,0.205,0.431,
  0.506,1.531,1.02,0.41,0.96,0.173,0.339,1.546,0.54,2.235,0.729,
  0.7,0.48,0.829,0.516,1.059,1.92,0.795,1.451,0.735,0.524,0.872,
  0.465,2,1.124,0.894,3.98,1.201,2.375,1.245,2.496,0.546,0.531,
  0.233,0.003,0.423,0.138,0.535,0.749,0.675,0.423,0.743,0.428,
  0.052,0.114,0.355,0.142,1.251,1.621,1.396,0.071,0.874,0.818,
  1.103,0.515,0.327,0,0.091,0.925,1.27,0.624,0.795,0.849,0.583,
  0.886,1.367,0.413,0.969,1.084,0.501,0.536,0.069,0.068,0.509,
  0.503,0.756,0.122,0.138,0.358,0.548,0.962,0.36,0.449,0.393,
  0.372,0.82,0.43,1.109,1.006,0.046,0.273,0.03,0.372,0.418,0.405
)

quantile(sizes, probs = c(1/3, 2/3))


size_class <- cut(
  sizes,
  breaks = c(-Inf,
             quantile(sizes, probs = 1/3),
             quantile(sizes, probs = 2/3),
             Inf),
  labels = c("Small", "Medium", "Large")
)


outplant_data <- data.frame(
  Initial_Area = sizes,
  Size_Class = size_class
)

head(outplant_data)

table(outplant_data$Size_Class)

#Figure showing corals alive from nursery and their initial size compares to initial outplant size

NurstoOut<-read_excel("DLAB Absolute Change_Nursery to Outplant.xlsx",sheet="Absolute Change") #to read your file

library(tidyr)

library(dplyr)

#Averages for nursery initial

mean(NurstoOut$Nursery_Initial, na.rm = TRUE)
sd(NurstoOut$Nursery_Initial, na.rm = TRUE)
se <- sd(NurstoOut$Nursery_Initial, na.rm = TRUE) / 
  sqrt(sum(!is.na(NurstoOut$Nursery_Initial)))

#Average for outplant initial 

mean(NurstoOut$Outplant_Initial, na.rm = TRUE)
sd(NurstoOut$Outplant_Initial, na.rm = TRUE)
se <- sd(NurstoOut$Outplant_Initial, na.rm = TRUE) / 
  sqrt(sum(!is.na(NurstoOut$Outplant_Initial)))

long_data <- NurstoOut %>%
  select(Nursery_Initial, Outplant_Initial) %>%
  pivot_longer(
    cols = c(Nursery_Initial, Outplant_Initial),
    names_to = "Stage",
    values_to = "Size"
  )


sig_labels <- long_data %>%
  group_by(Stage) %>%
  summarise(y = max(Size, na.rm = TRUE)) %>%
  mutate(
    label = c("a", "b"),   # adjust order if needed
    y = y * 1.3            # push labels above violins (important for log scale)
  )



#Another VIOLIN PLOT --> MANUSCRIPT FIGURE


Gr <- ggplot(long_data, aes(x = Stage, y = Size, fill = Stage)) +
  
  geom_violin(
    trim = FALSE,
    alpha = 0.5,
    color = "black"
  ) +
  
  geom_boxplot(
    width = 0.12,
    outlier.shape = NA,
    alpha = 0.7
  ) +
  
  geom_jitter(
    width = 0.08,
    alpha = 0.35,
    size = 1
  ) +
  
  geom_text(
    data = sig_labels,
    aes(x = Stage, y = y * 2.3, label = label),
    size = 5,
    inherit.aes = FALSE
  )+

  scale_fill_manual(
    values = c(
      "Nursery_Initial" = "#F4A6A6",   # light salmon
      "Outplant_Initial" = "#C7A6FF"   # light purple
    ),
    labels = c(
      "Nursery deployment",
      "Outplant deployment"
    )
  ) +
  
  scale_x_discrete(
    labels = c(
      "Nursery_Initial" = "Nursery deployment",
      "Outplant_Initial" = "Outplant deployment"
    )
  ) +
  
  scale_y_log10(
    breaks = c(0.001, 0.01, 0.1, 1, 10),
    labels = c("0.001", "0.01", "0.1", "1", "10")
  ) +
  
  theme_classic() +
  
  labs(
    x = "",
    y = expression("Log"[10]*" live tissue planar area (cm"^2*")"),
    fill = ""
  ) +
  
  theme(
    legend.position = "none",
    axis.text.x = element_text(size = 12),
    axis.title.y = element_text(size = 14)
  )

Gr

ggsave("Growth_Comparison.png",
       Gr,
       width = 8,
       height = 5,
       dpi = 300)

 
#Welch's T-test on log transformed planar area 


t.test(
  log10(Size) ~ Stage,
  data = long_data
)

mean(NurstoOut$Nursery_Initial, na.rm = TRUE)
sd(NurstoOut$Nursery_Initial, na.rm = TRUE)

mean(NurstoOut$Outplant_Initial, na.rm = TRUE)
sd(NurstoOut$Outplant_Initial, na.rm = TRUE)

#OUTPLANT SURVIVAL!!
  
OutSurv<-read_excel("DLAB Outplant Surv.xlsx",sheet="Master") #to read your file


#Determine the class of the Factors (not the covariates):
class(OutSurv$Reef)
OutSurv$Reef <- factor(OutSurv$Reef)
class(OutSurv$Reef)

class(OutSurv$Size)
OutSurv$Size <- factor(OutSurv$Size)
class(OutSurv$Size)

class(OutSurv$Position)
OutSurv$Position <- factor(OutSurv$Position)
class(OutSurv$Position)

class(OutSurv$Array)
OutSurv$Array <- factor(OutSurv$Array)
class(OutSurv$Array)

class(OutSurv$Nursery)
OutSurv$Nursery <- factor(OutSurv$Nursery)
class(OutSurv$Nursery)



attach(OutSurv)

#Test the effect of the factor
library(survival)
levels(OutSurv$Reef)
#"Artificial Reef" "Inner Reef"   "Nearshore"    

OutSurv$Reef <- relevel(OutSurv$Reef, ref = "Nearshore")  

levels(OutSurv$Size)
#"Small"  "Medium" "Large" 
levels(OutSurv$Position)
#"High" "Low" 
levels(OutSurv$Array)
#"CI2" "G24" "G37" "H24" "X17" "XC8"

levels(OutSurv$Nursery)
# Mod, Table, Tree

#Test the effect of the factor
library(survival)
str(OutSurv)
str(X)
summary(OutSurv)


X <- cbind(OutSurv$Reef,OutSurv$Size, OutSurv$Position)
cox <- coxph(Surv(Time, Status) ~ Reef + Size + Position, data = OutSurv)
summary(cox)

cox<- coxph (Surv(Time, Status)~ X, method='breslow', data = OutSurv)
summary(cox)

#Reef: 0.9369
#Size: 1.8416
#Position: 0.9759

#ALL COMPARISONS REEF

survdiff(Surv(Time, Status) ~ Reef, data = OutSurv)#*Significant
survdiff(Surv(Time, Status) ~ Position, data = OutSurv)#NS
survdiff(Surv(Time, Status) ~ Size, data = OutSurv)#*Significant
survdiff(Surv(Time, Status) ~ Array, data = OutSurv)#*Significant
survdiff(Surv(Time, Status) ~ Nursery, data = OutSurv)#NS

#REEF SITES

# Nearshore vs Inner - NS
survdiff(Surv(Time, Status) ~ Reef, data = OutSurv,
         subset = Reef %in% c("Nearshore","Inner Reef"))

# Nearshore vs Artificial Reef *Significant
survdiff(Surv(Time, Status) ~ Reef, data = OutSurv,
         subset = Reef %in% c("Nearshore","Artificial Reef"))

#Inner Reef vs Artificial Reef *Significant
survdiff(Surv(Time, Status) ~ Reef, data = OutSurv,
         subset = Reef %in% c("Inner Reef","Artificial Reef"))

#SIZE

# Small & Medium - NS (p=0.2)
survdiff(Surv(Time, Status) ~ Size, data = OutSurv,
         subset = Size %in% c("Small","Medium"))

# Small & Large - *Significant (p= 2e-05)
survdiff(Surv(Time, Status) ~ Size, data = OutSurv,
         subset = Size %in% c("Small","Large"))

# Medium & Large - *Significant (p= 0.002)
survdiff(Surv(Time, Status) ~ Size, data = OutSurv,
         subset = Size %in% c("Medium","Large"))

#SUBSTRATE

# High & Low - NS
survdiff(Surv(Time, Status) ~ Position, data = OutSurv,
         subset = Position %in% c("High","Low"))

#High and Low with Clipper REMOVED - NS

survdiff(Surv(Time, Status) ~ Position,
         data = OutSurv,
         subset = Position %in% c("High","Low") & Site != "Clip")

#Nursery structure ORIGIN - Clipper Removed - NS
survdiff(Surv(Time, Status) ~ Nursery,
         data = OutSurv,
         subset = Nursery %in% c("Mod","Table", "Tree") & Site != "Clip")

#Combined reef, position, and size ALL --> additive cox, independent model 
#assess how each variable affect survival independently
#USING IN MANUSCRIPT

coxph(Surv(Time, Status) ~ Reef + Size + Position, data = OutSurv)

#Run additive cox with NO CLIPPER 

OutSurv_no_clipper <- subset(OutSurv, Site != "Clip")

cox_no_clipper <- coxph(
  Surv(Time, Status) ~ Reef + Size + Position,
  data = OutSurv_no_clipper
)

summary(cox_no_clipper)


#combined reef, Position, and size ALL -> Interaction model -> NOT USING IN MANUSCRIPT

coxph(Surv(Time, Status) ~ Reef * Size * Position, data = OutSurv)

#Interaction model without Clipper 

#check references
levels(OutSurv$Reef)
levels(OutSurv$Size)
levels(OutSurv$Position)


OutSurv$Reef <- relevel(OutSurv$Reef, ref = "Nearshore") #change the reference site


OutSurv_no_clip <- OutSurv %>%
  filter(Site != "Clip")

cox_no_clip <- coxph(
  Surv(Time, Status) ~ Reef * Size * Position,
  data = OutSurv_no_clip
)

summary(cox_no_clip)


# Compare size within each reef site
coxph(Surv(Time, Status) ~ Size, data = OutSurv[OutSurv$Reef=="Nearshore", ]) #*Significant
coxph(Surv(Time, Status) ~ Size, data = OutSurv[OutSurv$Reef=="Inner Reef", ]) #*Significant
coxph(Surv(Time, Status) ~ Size, data = OutSurv[OutSurv$Reef=="Artificial Reef", ]) #NS

#Compare Position within each reef site 
coxph(Surv(Time, Status) ~ Position, data = OutSurv[OutSurv$Reef=="Nearshore", ]) #NS
coxph(Surv(Time, Status) ~ Position, data = OutSurv[OutSurv$Reef=="Inner Reef", ]) #NS
coxph(Surv(Time, Status) ~ Position, data = OutSurv[OutSurv$Reef=="Artificial Reef", ]) #*Significant

#Array:
coxph(Surv(Time, Status) ~ Position, data = OutSurv[OutSurv$Array=="X17", ]) #NS


#Compare Position within each ARRAY
#Nearshore
coxph(Surv(Time, Status) ~ Position, data = OutSurv[OutSurv$Array=="G24", ]) #NS
coxph(Surv(Time, Status) ~ Position, data = OutSurv[OutSurv$Array=="X17", ]) #NS
#Inner reef
coxph(Surv(Time, Status) ~ Position, data = OutSurv[OutSurv$Array=="G37", ]) #NS
coxph(Surv(Time, Status) ~ Position, data = OutSurv[OutSurv$Array=="H24", ]) #NS

#GLM 

#ALL SITES
OutSurv$Survived <- 1 - OutSurv$Status #Flips zeros and ones

glm_model <- glm(
  Survived ~ Reef * Size * Position,
  data = OutSurv,
  family = binomial
)

summary(glm_model)


#CLIPPER REMOVED
OutSurv_no_clip$Survived <- 1 - OutSurv_no_clip$Status

glm_model <- glm(
  Survived ~ Reef * Size * Position,
  data = OutSurv_no_clip,
  family = binomial
)

summary(glm_model)



#Survival PERCENTAGES

library(dplyr)
subset(OutSurv, Status == 1) %>% count() #119 juveniles died
subset(OutSurv, Status == 0 & Time ==(0)) %>% count() #254 juveniles outplanted
subset(OutSurv, Status == 0 & Time ==(6)) %>% count() #135 recruits still alive by final timepoint
(135/(254))*100

#OVERALL Survival was 53% after two years!

#Survival at one year 
subset(OutSurv, Status == 0 & Time ==(0)) %>% count() #254 juveniles outplanted
subset(OutSurv, Status == 0 & Time ==(5)) %>% count() #135 recruits still alive by final timepoint
(187/(254))*100

#Survival was 74% at the one year time period 

#Reef survival

subset(OutSurv, Status == 1 & Reef == "Inner Reef") %>% count() #39 juveniles died 
subset(OutSurv, Status == 0 & Reef == "Inner Reef" & Time ==(6)) %>% count() #57 alive
(57/(57+39))*100
#Inner Reef (JA4 = H24 & G37) = 59% survival after 2 years

subset(OutSurv, Status == 1 & Reef == "Nearshore") %>% count() #38 juveniles died 
subset(OutSurv, Status == 0 & Reef == "Nearshore" & Time ==(6)) %>% count() #57 alive
(57/(57+38))*100
#Nearshore (JA6 = X17 and G24) = 60% survival after 2 years!

subset(OutSurv, Status == 1 & Reef == "Artificial Reef") %>% count() #42 Juveniles died 
subset(OutSurv, Status == 0 & Reef == "Artificial Reef" & Time ==(6)) %>% count() #21 alive
(21/(21+42))*100
#Artificial Reef (Clipper = CI2 and XC8) = 33% survival after 2 years!


#Size class survival

subset(OutSurv, Status == 1 & Size == "Small") %>% count() #50 small juveniles died 
subset(OutSurv, Status == 0 & Size == "Small" & Time ==(6)) %>% count() #32 alive
(32/(50+32))*100
#small = 39% survival after 2 years!

subset(OutSurv, Status == 1 & Size == "Medium") %>% count() #43 medium recruits died 
subset(OutSurv, Status == 0 & Size == "Medium" & Time ==(6)) %>% count() #40 recruits alive
(40/(43+40))*100

#Medium = 48% survival after 2 years!

subset(OutSurv, Status == 1 & Size == "Large") %>% count() #26 large juveniles died 
subset(OutSurv, Status == 0 & Size == "Large" & Time ==(6)) %>% count() #63 alive
(63/(63+26))*100
#Large = 70% survival after 2 years!

#Substrate Survival 

subset(OutSurv, Status == 1 & Position == "High") %>% count() #66 juveniles on high died 
subset(OutSurv, Status == 0 & Position == "High" & Time ==(6)) %>% count() #62 alive
(62/(66+62))*100

#High = 48% survival after 2 years!

subset(OutSurv, Status == 1 & Position == "Low") %>% count() #53 juveniles on low died 
subset(OutSurv, Status == 0 & Position == "Low" & Time ==(6)) %>% count() #73 alive
(73/(63+73))*100
#Low = 53% survival after 2 years!

#Survival of clipper high and low 

subset(OutSurv, Status == 1 & Position == "Low" & Site == "Clip") %>% count() #11 juveniles on high died 
subset(OutSurv, Status == 0 & Position == "Low" & Site == "Clip" & Time ==(6)) %>% count() #20 alive
(20/(11+20))*100

#65% still alive after 2 years

subset(OutSurv, Status == 1 & Position == "High" & Site == "Clip") %>% count() #31 juveniles on high died 
subset(OutSurv, Status == 0 & Position == "High" & Site == "Clip" & Time ==(6)) %>% count() #1 alive
(1/(31+1))*100

# 3% survival after 2 years on Clipper high

#High and Low with Clipper REMOVED 

subset(OutSurv, Status == 1 & Position == "High" & Site != "Clip") %>% count() #35 juveniles on high died
subset(OutSurv, Status == 0 & Position == "High" & Time == 6 & Site != "Clip") %>% count() #61 juveniles on high alive
(61/(61+35))*100

#High = 64% survival after 2 years!

subset(OutSurv, Status == 1 & Position == "Low" & Site != "Clip") %>% count() #42 juveniles on low died
subset(OutSurv, Status == 0 & Position == "Low" & Time == 6 & Site != "Clip") %>% count() #53 juveniles on low alive
(53/(42+53))*100

#Low = 56% survival after 2 years!


km3 <-survfit(Surv(Time,Status)~ Reef) #just for plot
# do not include interactions
summary(cox)

km3$strata
plot(km3, lwd=2, xlab = 'Time', ylab = '% Survival', lty=c(1,2,3,4,5),
     col=c(1,2,3,4,5,6,7,8),conf.int=F) # does not include 95% confidence interval for each curve

legend("bottomleft", legend=c("Artificial Reef","Inner Reef","Nearshore"), lty=c(1,2,3,4,5), col=c(1,2,3,4,5)) 


km4 <-survfit(Surv(Time,Status)~ Size) #just for plot
# do not include interactions
summary(cox)

km4$strata
plot(km4, lwd=2, xlab = 'Time', ylab = '% Survival', lty=c(1,2,3,4,5),
     col=c(1,2,3,4,5,6,7,8),conf.int=F) # does not include 95% confidence interval for each curve

legend("bottomleft", legend=c("Large", "Medium", "Small"), lty=c(1,2,3,4,5), col=c(1,2,3,4,5)) 


ggsurvplot(survfit(Surv(Time, Status) ~ Size, data = OutSurv))



#MANUSCRIPT FIGURE


OutSurv$Size <- factor(as.character(OutSurv$Size),
                       levels = c("Small", "Medium", "Large"))

fit2 <- survfit(Surv(Days, Status) ~ Size, data = OutSurv)
summary(fit2, times = 730)


c <- ggsurvplot(
  fit2,
  facet.by = "Reef",
  data = OutSurv,
  xlab = "Time post-outplant (days)",
  palette = c("Small" = "green",
              "Medium" = "red",
              "Large" = "blue"),
  linetype = "Size",
  legend.title = "Size",
  conf.int = FALSE,
  ggtheme = theme_classic(base_size = 18)+
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 14, face = "bold")
  ),
  xlim = c(0, 730),
  break.time.by = 90,
  censor.shape = 16,
  censor.size = 3
  
) + 
  scale_color_manual(
    values = c("Small" = "green",
               "Medium" = "red",
               "Large" = "blue"),
    breaks = c("Small", "Medium", "Large")   #controls legend order ONLY
  ) +
  scale_linetype_manual(
    values = c("Small" = "longdash",
               "Medium" = "dotdash",
               "Large" = "solid"),
    breaks = c("Small", "Medium", "Large")   #keeps linetype legend aligned
  )

c


#HIGH AND LOW SURV PLOT

fit3 <- survfit(Surv(Days, Status) ~ Position, data = OutSurv)
summary(fit3, times = 730)


b <- ggsurvplot(
  fit3,
  facet.by = "Reef",
  data = OutSurv,
  xlab = "Time post-outplant (days)",
  palette = c("#FF1493", "#FFA347"),
  linetype = "Position",
  legend.title = "Position",
  legend.labs = c("High", "Low"),
  conf.int = FALSE,
  ggtheme = theme_classic(base_size = 18) +
    theme(
      strip.background = element_rect(fill = "white", color = "black"),
      strip.text = element_text(size = 14, face = "bold")
    ),
  xlim = c(0, 730),
  break.time.by = 90,
  censor.shape = 16,
  censor.size = 3
)

b


#FINAL MANUSCRIPT FIGURE 

library(cowplot)

combined_plot2 <- plot_grid(
  b,
  c,
  labels = c("A", "B"),
  ncol = 1,
  label_size = 16
)

combined_plot2

ggsave("SurvOutplantSizePosition.png", combined_plot2, width = 14, height = 10, units = "in", dpi = 300)

subset(OutSurv, Status == 1 & Reef == "Nearshore" & Size == "Small") %>% count() #9 small juveniles died 
subset(OutSurv, Status == 0 & Reef == "Nearshore" & Size == "Small" & Time ==(6)) %>% count() #10 alive
(10/(9+10))*100

#53%

subset(OutSurv, Status == 1 & Reef == "Nearshore" & Size == "Medium") %>% count() #17 Med juveniles died 
subset(OutSurv, Status == 0 & Reef == "Nearshore" & Size == "Medium" & Time ==(6)) %>% count() #14 alive
(14/(14+17))*100

#45%

subset(OutSurv, Status == 1 & Reef == "Nearshore" & Size == "Large") %>% count() #12 Large juveniles died 
subset(OutSurv, Status == 0 & Reef == "Nearshore" & Size == "Large" & Time ==(6)) %>% count() #33 alive
(33/(12+33))*100

#73%


#OUTPLANT GROWTH!!! TWO YEAR DATA

Out_Growth<-read_excel("DLAB Absolute Change_Outplant.xlsx",sheet="Two_Year") 
str(Out_Growth) 


Out_Growth_live <- Out_Growth %>% #remove dead colonies from the analysis. Dead colonies at clipper skew results
  filter(Interval_3 != 0)


class(Out_Growth_live$Size) 
class(Out_Growth_live$Position)
class(Out_Growth_live$Reef)


Out_Growth_live$Size <- factor(Out_Growth_live$Size)
Out_Growth_live$Position <- factor(Out_Growth_live$Position)
Out_Growth_live$Reef <- factor(Out_Growth_live$Reef)


attach(Out_Growth_live)

install.packages("car")
library(car)

#COPIOLOT changes -> three-way anova -> NOT RECOMMENDED due to smaller sample size
#Many parameters relative to sample size

anova4 <- aov(Absolute_Change ~ Size * Reef * Position, data = Out_Growth_live)
TukeyHSD(anova4)

#Levenes test for homogeneity of variance
leveneTest(Absolute_Change ~ Size * Reef * Position)
leveneTest(Absolute_Change ~ interaction(Size, Reef, Position), data = Out_Growth_live)

leveneTest(Absolute_Change ~ Size, data = Out_Growth_live)
leveneTest(Absolute_Change ~ Reef, data = Out_Growth_live)
leveneTest(Absolute_Change ~ Position, data = Out_Growth_live)


#Results show that I would fail to reject the null = variances are NOT significantly different!
#Data meets the homogeneity of variance assumption -> Can use ANOVA, linear models, and parametric tests


# check normality of residuals
plot(anova4, which = 2)   # QQ plot
shapiro.test(residuals(anova4))

#Shapiro-Wilk normality test
#W = 0.99274, p-value = 0.7206
#RESIDUALS ARE NORMAL -> FAIL to Reject the null 

#Suggests to use anova additive model 


anova_add <- aov(Absolute_Change ~ Size + Reef + Position, data = Out_Growth_live)
summary(anova_add)
TukeyHSD(anova_add)

#Test one interation at a time: 
aov(Absolute_Change ~ Size * Reef, data = Out_Growth_live)



#Averages

#overall alive

mean(Out_Growth_live$Absolute_Change, na.rm = TRUE)
sd(Out_Growth_live$Absolute_Change, na.rm = TRUE)
se <- sd(Out_Growth_live$Absolute_Change, na.rm = TRUE) / 
  sqrt(sum(!is.na(Out_Growth_live$Absolute_Change)))

mean(Out_Growth_live$`Relative Growth`, na.rm = TRUE)
sd(Out_Growth_live$`Relative Growth`, na.rm = TRUE)
se <- sd(Out_Growth_live$`Relative Growth`, na.rm = TRUE) / 
  sqrt(sum(!is.na(Out_Growth_live$`Relative Growth`)))

#overall dead
mean(Out_Growth$Absolute_Change, na.rm = TRUE)
sd(Out_Growth$Absolute_Change, na.rm = TRUE)
se <- sd(Out_Growth$Absolute_Change, na.rm = TRUE) / 
  sqrt(sum(!is.na(Out_Growth$Absolute_Change)))


#size class alive
aggregate(Absolute_Change ~ Size, data = Out_Growth_live,
          FUN = function(x) c(
            mean = mean(x, na.rm = TRUE),
            sd   = sd(x, na.rm = TRUE),
            se   = sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
          ))

#Reef alive 
aggregate(Absolute_Change ~ Reef, data = Out_Growth_live,
          FUN = function(x) c(
            mean = mean(x, na.rm = TRUE),
            sd   = sd(x, na.rm = TRUE),
            se   = sd(x, na.rm = TRUE) / sqrt(sum(!is.na(x)))
          ))

#Manuscript figures

#SIZE CLASS 

Out_Growth_live$Size <- factor(Out_Growth_live$Size,
                            levels = c("Small", "Medium", "Large"))


gw2 <- ggplot(Out_Growth_live, aes(x = Size, y = Absolute_Change, fill = Size)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 2) +
  
  # Dotted zero line
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) +
  
  # Color palette (green, red, blue)
  scale_fill_manual(values = c(
    "Small" = "green",
    "Medium" = "red",
    "Large" = "blue"
  )) +
  
 # annotate("text", x = 1, y = max(Growth_Alive$Absolute_Change) + 0.05, label = "a", size = 5) +
  #annotate("text", x = 2, y = max(Growth_Alive$Absolute_Change) + 0.05, label = "b", size = 5) +
  #annotate("text", x = 3, y = max(Growth_Alive$Absolute_Change) + 0.05, label = "b", size = 5) +
  
  labs(
    x = "Initial Juvenile Size Class at Outplant",
    y = expression("Absolute Growth (cm"^2*")"),
  ) +
  
  theme_classic(base_size = 14) +
  theme(legend.position = "none")

gw2

#REEFS

greef2 <- ggplot(Out_Growth_live, aes(x = Reef, y = Absolute_Change, fill = Reef)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 2) +
  
  # Dotted zero line
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) +
  
  # Color palette (green, red, blue)
  scale_fill_manual(values = c(
    "Artificial Reef" = "pink",
    "Inner Reef" = "lightblue",
    "Nearshore" = "purple"
  )) +
  
  # annotate("text", x = 1, y = max(Growth_Alive$Absolute_Change) + 0.05, label = "a", size = 5) +
  #annotate("text", x = 2, y = max(Growth_Alive$Absolute_Change) + 0.05, label = "b", size = 5) +
  #annotate("text", x = 3, y = max(Growth_Alive$Absolute_Change) + 0.05, label = "b", size = 5) +
  
  labs(
    x = "Reef Site",
    y = expression("Absolute Growth (cm"^2*")"),
  ) +
  
  theme_classic(base_size = 14) +
  theme(legend.position = "none")

greef2


#GROWTH ONE YEAR DATA!!

Out_Growth1yr<-read_excel("DLAB Absolute Change_Outplant.xlsx",sheet="Absolute Change") 
str(Out_Growth) 


Out_Growth_live1yr <- Out_Growth1yr %>% #remove dead colonies from the analysis. Dead colonies at clipper skew results
  filter(Interval_2 != 0)


class(Out_Growth_live1yr$Size) 
class(Out_Growth_live1yr$Position)
class(Out_Growth_live1yr$Reef)


Out_Growth_live1yr$Size <- factor(Out_Growth_live1yr$Size)
Out_Growth_live1yr$Position <- factor(Out_Growth_live1yr$Position)
Out_Growth_live1yr$Reef <- factor(Out_Growth_live1yr$Reef)


attach(Out_Growth_live1yr)

install.packages("car")
library(car)

#COPIOLOT changes -> three-way anova -> NOT RECOMMENDED due to smaller sample size
#Many parameters relative to sample size

anova5 <- aov(Absolute_Change ~ Size * Reef * Position, data = Out_Growth_live1yr)
TukeyHSD(anova5)

#Levenes test for homogeneity of variance
leveneTest(Absolute_Change ~ Size * Reef * Position)
leveneTest(Absolute_Change ~ interaction(Size, Reef, Position), data = Out_Growth_live1yr)

leveneTest(Absolute_Change ~ Size, data = Out_Growth_live1yr)
leveneTest(Absolute_Change ~ Reef, data = Out_Growth_live1yr)
leveneTest(Absolute_Change ~ Position, data = Out_Growth_live1yr)


#Results show that I would fail to reject the null = variances are NOT significantly different!
#Data meets the homogeneity of variance assumption -> Can use ANOVA, linear models, and parametric tests


# check normality of residuals
plot(anova5, which = 2)   # QQ plot
shapiro.test(residuals(anova5))

#Shapiro-Wilk normality test
#W = 0.97887, p-value = 0.006
#RESIDUALS ARE NOT NORMAL -> Reject the null 
#might not be best to use ANOVA with one year data

oneway.test(Absolute_Change ~ Size, data = Out_Growth_live1yr)


anova_add1yr <- aov(Absolute_Change ~ Size + Reef + Position, data = Out_Growth_live1yr)
summary(anova_add1yr)
TukeyHSD(anova_add1yr)

#SIZE CLASS

Out_Growth_live1yr$Size <- factor(Out_Growth_live1yr$Size,
                               levels = c("Small", "Medium", "Large"))


gw1 <- ggplot(Out_Growth_live1yr, aes(x = Size, y = Absolute_Change, fill = Size)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 2) +
  
  # Dotted zero line
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) +
  
  # Color palette (green, red, blue)
  scale_fill_manual(values = c(
    "Small" = "green",
    "Medium" = "red",
    "Large" = "blue"
  )) +
  
  # annotate("text", x = 1, y = max(Growth_Alive$Absolute_Change) + 0.05, label = "a", size = 5) +
  #annotate("text", x = 2, y = max(Growth_Alive$Absolute_Change) + 0.05, label = "b", size = 5) +
  #annotate("text", x = 3, y = max(Growth_Alive$Absolute_Change) + 0.05, label = "b", size = 5) +
  
  labs(
    x = "Initial Juvenile Size Class at Outplant",
    y = expression("Absolute Growth (cm"^2*")"),
  ) +
  
  theme_classic(base_size = 14) +
  theme(legend.position = "none")

gw1

#REEFS

greef1 <- ggplot(Out_Growth_live1yr, aes(x = Reef, y = Absolute_Change, fill = Reef)) +
  geom_boxplot(alpha = 0.7, outlier.shape = NA) +
  geom_jitter(width = 0.15, alpha = 0.4, size = 2) +
  
  # Dotted zero line
  geom_hline(yintercept = 0, linetype = "dotted", color = "black", size = 0.8) +
  
  # Color palette (green, red, blue)
  scale_fill_manual(values = c(
    "Artificial Reef" = "pink",
    "Inner Reef" = "lightblue",
    "Nearshore" = "purple"
  )) +
  
  # annotate("text", x = 1, y = max(Growth_Alive$Absolute_Change) + 0.05, label = "a", size = 5) +
  #annotate("text", x = 2, y = max(Growth_Alive$Absolute_Change) + 0.05, label = "b", size = 5) +
  #annotate("text", x = 3, y = max(Growth_Alive$Absolute_Change) + 0.05, label = "b", size = 5) +
  
  labs(
    x = "Reef Site",
    y = expression("Absolute Growth (cm"^2*")"),
  ) +
  
  theme_classic(base_size = 14) +
  theme(legend.position = "none")

greef1

#Combined GROWTH figure OVERTIME


Comb_Out_Growth<-read_excel("DLAB Absolute Change_Outplant.xlsx",sheet="Combined") 
str(Out_Growth) 

#Convert to LONG format 


long_data2 <- Comb_Out_Growth %>%
  mutate(
    Interval_1 = as.numeric(Interval_1),
    Interval_2 = as.numeric(Interval_2),
    Interval_3 = as.numeric(Interval_3)
  ) %>%
  pivot_longer(
    cols = starts_with("Interval_"),
    names_to = "Time",
    values_to = "Area"
  ) %>%
  mutate(
    # convert zeros to NA (dead colonies)
    Area = ifelse(Area == 0, NA, Area),
    # fix time labels
    Time = case_when(
      Time == "Interval_1" ~ "0",
      Time == "Interval_2" ~ "365",
      Time == "Interval_3" ~ "730"
    ),
Time = factor(Time, levels = c("0", "365", "730")),
    # create ID
    ID = paste(Site, Tag, Position, sep = "_")
  )

#Line plot through time with dead colonies dropped from the figure


com1 <- ggplot(long_data2, aes(x = Time, y = Area, group = ID)) +
  
  # faint individual trajectories
  geom_line(alpha = 0.3, color = "gray60") +
  
  # jittered points (fix vertical stacking)
  geom_point(
    alpha = 0.25,
    size = 1, 
    position = position_jitter(width = 0.05)
  ) +
  
  facet_wrap(~ Reef)+
  
  # mean trajectory (IMPORTANT)
  stat_summary(
    aes(group = 1),
    fun = mean,
    geom = "line",
    linewidth = 1.6,
    color = "black"
  ) +
  
  #stat_summary(
   # fun = mean,
    #geom = "point",
    #size = 1,
   # color = "black"
 # ) +
  
  coord_cartesian(ylim = c(0, 10.5))+
  
  theme_classic() +
  
  theme(
    strip.background = element_rect(fill = "white", color = "black"),
    strip.text = element_text(size = 12, face = "bold"),
    
    # axis titles (x and y labels)
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    
    # axis tick labels (numbers on axes)
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    
  )+
  
  labs(
    x = "Time post-outplant (days)",
    y = expression("Live tissue planar area (cm"^2*")")
  )

com1    
    
    
#BOXPLOT --> SIZE CLASS 


#Removing dead colonies -> making them NA

Comb_Out_Growth_clean <- Comb_Out_Growth %>%
  mutate(
    AbsChange_1yr = ifelse(Interval_2 == 0, NA, Absolute_Change_2),
    AbsChange_2yr = ifelse(Interval_3 == 0, NA, Absolute_Change_3)
  )

#Long format:

long_growth_comb <- Comb_Out_Growth_clean %>%
  pivot_longer(
    cols = c(AbsChange_1yr, AbsChange_2yr),
    names_to = "Time",
    values_to = "Absolute_Change"
  ) %>%
  mutate(
    Time = case_when(
      Time == "AbsChange_1yr" ~ "365",
      Time == "AbsChange_2yr" ~ "730"
    ),
    Time = factor(Time, levels = c("365", "730"))
  )

#Prep for significance letters:

sig_labels2 <- data.frame(
  Time = rep(c("365", "730"), each = 3),
  Size = rep(c("Small", "Medium", "Large"), 2),
  label = c(
    "b", "a", "a",   # 1 Year (Small, Medium same; Large different)
    "a", "a", "a"    # 2 Years (all same)
  )
)

sig_labels2$y <- max(long_growth_comb$Absolute_Change, na.rm = TRUE) * 1.1

#order of size class

long_growth_comb$Size <- factor(
  long_growth_comb$Size,
  levels = c("Small", "Medium", "Large")
)

#Boxplot of long format 

sz <- ggplot(long_growth_comb, aes(x = Time, y = Absolute_Change, fill = Size)) +
  
  # dotted zero line
  geom_hline(
    yintercept = 0,
    linetype = "dotted",
    color = "black",
    linewidth = 0.8
  ) +
  
  # boxplots
  geom_boxplot(
    position = position_dodge(width = 0.8),
    width = 0.6,
    alpha = 0.7,
    outlier.shape = NA
  ) +
  
  # jittered raw data
  geom_jitter(
    aes(color = "black"),
    position = position_jitterdodge(
      jitter.width = 0.1,
      dodge.width = 0.8
    ),
    size = 1.2,
    alpha = 0.4,
    show.legend = FALSE
  ) +
  
  # significance letters
  geom_text(
    data = sig_labels2,
    aes(x = Time, y = y, label = label, fill = Size),  # ✅ map Size
    position = position_dodge(width = 0.8),
    size = 5,
    #fontface = "bold",
    inherit.aes = FALSE
  )+

  # colors
  scale_fill_manual(
    values = c(
      "Small" = "green",
      "Medium" = "red",
      "Large" = "blue"
    ),
    breaks = c("Small", "Medium", "Large")
  ) +
  
  scale_color_manual(
    values = c(
      "Small" = "green",
      "Medium" = "red",
      "Large" = "blue"
    ),
    breaks = c("Small", "Medium", "Large")
  ) +
  
  theme_classic() +
  
  theme(
    # axis titles (x and y labels)
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 16),
    
    # axis tick labels (numbers on axes)
    axis.text.x = element_text(size = 14),
    axis.text.y = element_text(size = 14),
    
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12)
  )+
    
  labs(
    x = "Time post-outplant (days)",
    y = expression("Absolute change (cm"^2*")"),
    fill = "Size class"
  )


sz

#MANUSCRIPT FIGURE: 

combined_plot3 <- plot_grid(
  com1,
  sz,
  labels = c("A", "B", "C"),
  ncol = 2,
  align = "h",
  label_size = 16
)

combined_plot3

ggsave("Out_Growth_panels.png",
       combined_plot3,
       width = 12,
       height = 6,
       dpi = 300)


#INITIAL SIZE AT OUTPLANT AND 2 YEAR SURVIVAL

TwoYr<-read_excel("DLAB Outplant_Surv_Area.xlsx",sheet="TwoYr") 

TwoYr$Status_alive <- ifelse(TwoYr$Status == 0, 1, 0)


logr <- ggplot(TwoYr, aes(x = Area, y = Status_alive)) +
  
  geom_jitter(height = 0.05, width = 0, alpha = 0.4, size = 2) +
  
  geom_smooth(
    method = "glm",
    method.args = list(family = "binomial"),
    color = "#C7A6FF",
    size = 1.2
  ) +
  
  # ✅ proportion scale (0–1)
  scale_y_continuous(
    limits = c(0, 1),
    breaks = seq(0, 1, by = 0.25)
  ) +
  
  labs(
    x = expression("Initial outplant live tissue planar area (cm"^2*")"),
    y = "Probability of survival"
  ) +
  
  theme_classic(base_size = 14)

logr

ggsave("Out_Logistics_Regression.png",
       logr,
       width = 8,
       height = 5,
       dpi = 300)


TwoYr$Status_alive <- ifelse(TwoYr$Status == 0, 1, 0)

model_surv <- glm(Status_alive ~ Area, 
                  data = TwoYr, 
                  family = binomial)

summary(model_surv)

exp(coef(model_surv))

table(TwoYr$Status_alive)

#Averages for the results section 

#overall alive 2 year
mean(Out_Growth_live$Absolute_Change, na.rm = TRUE)
sd(Out_Growth_live$Absolute_Change, na.rm = TRUE)

mean(Out_Growth_live$`Relative Growth`, na.rm = TRUE)
sd(Out_Growth_live$`Relative Growth`, na.rm = TRUE)

#overall dead
mean(Out_Growth$Absolute_Change, na.rm = TRUE)
sd(Out_Growth$Absolute_Change, na.rm = TRUE)


mean(Growth$`Relative Growth`, na.rm = TRUE)






         
##################################SEDIMENT################################

Sed<-read_excel("DLAB_Sed.xlsx",sheet="Sheet1") #to read your file


library(dplyr)

Sed_Avg <- Sed %>%
  mutate(RowMeanSed = rowMeans(select(., starts_with("Sed")), na.rm = TRUE))


Sed_Tag <- Sed_Avg %>% #Average per TAG
  group_by(Event, Days, Subs, Tag, Reef) %>%
  summarise(MeanSed = mean(RowMeanSed), .groups = "drop")

#Average per TAG
plot_sed <- Sed_Tag %>%
  group_by(Days, Subs, Reef) %>%
  summarise(
    mean = mean(MeanSed),
    se = sd(MeanSed)/sqrt(n()),
    .groups = "drop"
  )


# Convert Days to factor for even spacing
plot_sed$Days <- factor(plot_sed$Days)


library(ggplot2)

s <- ggplot(plot_sed, aes(x = Days, y = mean, color = Subs, group = Subs, linetype = Subs)) +
  geom_point(size = 3) +
  geom_errorbar(aes(ymin = mean - se, ymax = mean + se), width = 0.15, linetype = "solid") +
  geom_line(alpha = 0.7, linewidth = 0.8) +
  
  facet_wrap(~ Reef) +
  
  scale_color_manual(values = c(
    "High" = "#FF1493",   
    "Low"  = "#FFA347"    
  )) +
  
  scale_linetype_manual(values = c(
    "High" = "solid", 
    "Low"  = "dashed"
  )) +
  
  
  labs(
    x = "Time post-outplant (days)",
    y = " Mean LSAT depth (mm)",
    color = "Position",
    linetype = "Position"
  )+
  
  scale_y_continuous(
    limits = c(0, 4),                     
    breaks = seq(0, 4, by = 0.5)
  )+

  theme_classic(base_size = 14) +
  theme(
    axis.title = element_text(size = 18),     # X and Y labels
    axis.text = element_text(size = 16),      # tick labels (numbers)
    legend.title = element_text(size = 18),   # legend title
    legend.text = element_text(size = 16),
    strip.background = element_rect(fill = "white", color = "black"),  # ✅ FIXED
    strip.text = element_text(size = 15, face = "bold"),                           # keeps bold labels
    legend.position = "right"
  )

s

ggsave("Sed.png", s, width = 14, height = 7, units = "in", dpi = 300)

#Stats

wilcox.test(MeanSed ~ Subs, data = Sed_Tag) #Significant

kruskal.test(MeanSed ~ Reef, data = Sed_Tag) #NS


#Test position within each reef 
wilcox.test(MeanSed ~ Subs, data = subset(Sed_Tag, Reef == "Inner Reef")) #*Significant
wilcox.test(MeanSed ~ Subs, data = subset(Sed_Tag, Reef == "Nearshore")) #*Significant
wilcox.test(MeanSed ~ Subs, data = subset(Sed_Tag, Reef == "Artificial Reef")) #*Significant



sed_long <- Sed %>%
  pivot_longer(
    cols = starts_with("Sed"),
    names_to = "Sed_Measure",
    values_to = "Sed_Value"
  )


sed_long %>%
  group_by(Subs) %>%
  summarise(
    mean_sed = mean(Sed_Value, na.rm = TRUE),
    se = sd(Sed_Value, na.rm = TRUE) / sqrt(n()),
    n = n()
  )


sed_long %>%
  group_by(Reef, Subs) %>%
  summarise(
    mean_sed = mean(Sed_Value, na.rm = TRUE),
    se = sd(Sed_Value, na.rm = TRUE) / sqrt(n()),
    n = n()
  )

sed_long %>%
  group_by(Days, Subs) %>%
  summarise(mean_sed = mean(Sed_Value, na.rm = TRUE),
  se = sd(Sed_Value, na.rm = TRUE) / sqrt(n()),
  n = n()
  )


#Sed depth with height of substrate: 

library(dplyr)

all <- Sed_Tag %>%
  filter(Days == 730) %>%
  group_by(Tag, Subs, Reef) %>%
  summarise(
    mean_sed = mean(MeanSed, na.rm = TRUE),
    .groups = "drop"
  )

print(all, n = Inf)

write.csv(all, "Sed_730.csv", row.names = FALSE)


Elev<-read_excel("DLAB_Elev.xlsx",sheet="Sheet1") #to read your file

library(dplyr)
library(ggplot2)


# Plot

e <- ggplot(Elev, aes(x = Height_m, y = Sed)) +
  geom_point(aes(color = Substrate), size = 3) +
  
  # Linear regression line
  geom_smooth(method = "lm", se = TRUE, color = "black", linewidth = 1) +
  
  labs(
    x = "Substrate elevation (m)",
    y = "Mean LSAT depth (mm) at 730 day post-outplant",
    color = "Position"
  ) +
  scale_color_manual(values = c(
    "High" = "#FF1493",   
    "Low"  = "#FFA347"    
  )) +
  scale_x_continuous(
    limits = c(0.00, 0.9),                     
    breaks = seq(0.00, 0.9, by = 0.15)
  )+
  
  theme_classic(base_size = 14)+
theme (
  axis.title = element_text(size = 18),     # X and Y labels
  axis.text = element_text(size = 16),      # tick labels (numbers)
  legend.title = element_text(size = 18),   # legend title
  legend.text = element_text(size = 18),
)

e

library(cowplot)

library(cowplot)

legend <- get_legend(
 e + theme(legend.position = "right")
)

s_clean <- s + theme(legend.position = "none")
e_clean <- e + theme(legend.position = "none")


#Below legend different placement --> FINAL MANUSCRIPT FIGURE BELOW

e <- e +
  theme(
    legend.position = "bottom",
    legend.direction = "horizontal"
  ) +
  guides(
    color = guide_legend(nrow = 1),
    linetype = guide_legend(nrow = 1)
  )

legend <- get_legend(e)

Comb <- plot_grid(
  plot_grid(s_clean, e_clean, labels = c("A", "B"), ncol = 2),
  legend,
  ncol = 1,
  rel_heights = c(1, 0.12)
)

Comb

ggsave("Sed_2.png", Comb, width = 20, height = 10, units = "in", dpi = 300)

#Stats on linear regression

model <- lm(Sed ~ Height_m, data = Elev) #*Significant
summary(model)






