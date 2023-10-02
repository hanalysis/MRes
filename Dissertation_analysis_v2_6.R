
#_______________________________________________________________________________
# Reading in libraries and data ----

## Reading in libraries ----

library(tidyverse)
library(matrixStats)
library(visdat)
library(corrplot)
library(afex)
library(openxlsx)
library(emmeans)
library(performance)
library(car) # For vif() function
library(lme4) # For mixed model
library(lmerTest) # For p values
library(WebPower) # For power testing
library(patchwork) # For figures
library(ggdist) # For data visualisations
library(fitdistrplus) # For describing the distribution of the data
library(glmmTMB) # For running a beta distribution GLMM

## Reading in data ----

midus_data <- read_csv("MIDUS_Data.csv")


#_______________________________________________________________________________

# Tidying data ----

# Renaming variables for ease

midus_tidy <- midus_data %>%
  rename(age = 'RA1PRAGE',
         sex = 'RA1PRSEX',
         CTQ_1 = 'RA4Q9A',
         CTQ_2 = 'RA4Q9B',
         CTQ_3 = 'RA4Q9C',
         CTQ_4 = 'RA4Q9D',
         CTQ_5 = 'RA4Q9E',
         CTQ_6 = 'RA4Q9F',
         CTQ_7 = 'RA4Q9G',
         CTQ_8 = 'RA4Q9H',
         CTQ_9 = 'RA4Q9I',
         CTQ_10 = 'RA4Q9J',
         CTQ_11 = 'RA4Q9K',
         CTQ_12 = 'RA4Q9L',
         CTQ_13 = 'RA4Q9M',
         CTQ_14 = 'RA4Q9N', 
         CTQ_15 = 'RA4Q9O',
         CTQ_16 = 'RA4Q9P', 
         CTQ_17 = 'RA4Q9Q',
         CTQ_18 = 'RA4Q9R',
         CTQ_19 = 'RA4Q9S', 
         CTQ_20 = 'RA4Q9T',
         CTQ_21 = 'RA4Q9U',
         CTQ_22 = 'RA4Q9V',
         CTQ_23 = 'RA4Q9W',
         CTQ_24 = 'RA4Q9X',
         CTQ_25 = 'RA4Q9Y',
         CTQ_26 = 'RA4Q9Z',
         CTQ_27 ='RA4Q9AA',
         CTQ_28 = 'RA4Q9BB',
         CTQ_EmotionalAbuse = 'RA4QCT_EA',
         CTQ_PhysicalAbuse = 'RA4QCT_PA',
         CTQ_SexualAbuse = 'RA4QCT_SA',
         CTQ_EmotionalNeglect = 'RA4QCT_EN',
         CTQ_PhysicalNeglect = 'RA4QCT_PN',
         SGST_NormalCorrect = 'RA3TSTN',
         SGST_NormalCorrectPercentage = 'RA3TSPN',
         SGST_NormalMedianRT = 'RA3TSMN',
         SGST_ReverseCorrect = 'RA3TSTR',
         SGST_ReverseMedianRT = 'RA3TSMR',
         SGST_ReverseCorrectPercentage = 'RA3TSPR',
         SGST_MeanPhoneLag = 'RA3TSMMM',
         IL6 = 'RA4BIL6',
         TNFa = 'RA4BMSDTNFa',
         CRP = 'RA4BCRP',
         Num_Rx = 'RA4XPM',
         Smoke = 'RA1PA39',
         Rx = 'RA4XPMD',
         OTC = 'RA4XOMD',
         BMI = 'RA4PBMI',
         MASQ_GenDistressDepress = 'RA4QMA_D',
         MASQ_GenDistressAnxious = 'RA4QMA_A',
         MASQ_LossOfInterest = 'RA4QMA_LI',
         MASQ_AnxiousArousal = 'RA4QMA_AA',
         MASQ_HighPosAffect = 'RA4QMA_PA')

## Renaming MASQ

# Creating list with correct DF names in

MASQ_names <- c()

for (i in 1:64){
  MASQ_names[[i]] <- str_c("MASQ",i, sep = "_")
}

# Renaming MASQ col 83 -> 146

for (x in 83:146){
  
  colnames(midus_tidy)[x] = MASQ_names[x-82]
  
}


## Word recall

# Removing 98 (NA) values from calculated variables in the word recall

midus_tidy[,34:35][midus_tidy[,34:35] == 98] <- NA

# Making dataframe with uniquely remembered values to add to clean data

WR_miduscalc <- midus_tidy %>%
  dplyr:: select(MIDUSID, 34:35) %>%
  rename(WR_TotalUnique = 'RA3TWLDTU',
         WR_TotalRepeat = 'RA3TWLDTR') %>%
  mutate(WR_TU_percent = ((WR_TotalUnique/15)*100),
         num = c(1:3819))



#______________________________________________________________________________

## Calculating the CTQ score ----

# Reverse coding the appropriate columns

midus_tidy <- midus_tidy %>%
  mutate(CTQ_2 = recode(CTQ_2, 1 == 5,
                        2 == 4,
                        4 == 2, 
                        5 == 1),
         CTQ_5 = recode(CTQ_5, 1 == 5,
                        2 == 4,
                        4 == 2, 
                        5 == 1),
         CTQ_7 = recode(CTQ_7, 1 == 5,
                        2 == 4,
                        4 == 2, 
                        5 == 1),
         CTQ_13 = recode(CTQ_13, 1 == 5,
                        2 == 4,
                        4 == 2, 
                        5 == 1),
         CTQ_19 = recode(CTQ_19, 1 == 5,
                        2 == 4,
                        4 == 2, 
                        5 == 1),
         CTQ_26 = recode(CTQ_26, 1 == 5,
                        2 == 4,
                        4 == 2, 
                        5 == 1),
         CTQ_28 = recode(CTQ_28, 1 == 5,
                        2 == 4,
                        4 == 2, 
                        5 == 1),
         CTQ_Scored = CTQ_1 + 
           CTQ_2 + CTQ_3 + CTQ_4 + 
           CTQ_5 + CTQ_6 + CTQ_7 + 
           CTQ_8 + CTQ_9 + CTQ_10 +
           CTQ_11 + CTQ_12 + CTQ_13 + 
           CTQ_14 + CTQ_15 + CTQ_16 + 
           CTQ_17 + CTQ_18 + CTQ_19 + 
           CTQ_20 + CTQ_21 + CTQ_22 + 
           CTQ_23 + CTQ_24 + CTQ_25 + 
           CTQ_26 + CTQ_27 + CTQ_28)

# Joining the calculated WR score

midus_tidy <- full_join(midus_tidy, WR_miduscalc, by = 'MIDUSID')

## Removing lag from Stop and Go RT ----

# Replacing 99 with 0 as this represents landline use to fill in the survey,
# Which was not adjusted for due to negligible time delays

midus_tidy$SGST_MeanPhoneLag[midus_tidy$SGST_MeanPhoneLag==99] <- 0

# Changing mean phone lag to numeric (from character)

midus_tidy <- midus_tidy %>%
  mutate(SGST_MeanPhoneLag = as.numeric(SGST_MeanPhoneLag)) %>%
  filter(SGST_MeanPhoneLag < 98)

midus_tidy <- midus_tidy %>%
  mutate(SGST_NormalMedianRT = (SGST_NormalMedianRT - SGST_MeanPhoneLag),
         SGST_ReverseMedianRT = (SGST_ReverseMedianRT - SGST_MeanPhoneLag))



## Anhedonic depression subscale from MASQ ----


# Anhedonic subscale, 22 item

midus_tidy <- midus_tidy %>%
  mutate(MASQ_AD = (MASQ_18 + 
                    MASQ_25 + 
                    MASQ_33 + 
                    MASQ_41 + 
                    MASQ_50 + 
                    MASQ_51 + 
                    MASQ_57 + 
                    MASQ_61 + 
                    84 - 
                      (MASQ_3 + MASQ_7 + MASQ_10 + 
                      MASQ_15 + MASQ_22 + MASQ_27 + 
                      MASQ_39 + MASQ_43 + MASQ_47 + MASQ_49 + 
                      MASQ_53 + MASQ_56 + MASQ_58 + MASQ_60)))


#_______________________________________________________________________________

## Sub-setting data for analysis sample -----

# Removing smokers, participants younger than 18 or older than 65 and people on 
# prescription medication

midus_clean <- midus_tidy %>%
  filter(Smoke != 1,
         age >= 18, 
         age <= 65,
         Rx != 1)


# Removing missing data 

midus_clean <- na.omit(midus_clean) %>% # Removing data already coded as missing
  filter(CRP < 99) %>% # Removes data missing from inflammatory markers
  filter(SGST_NormalMedianRT < 98, 
         SGST_ReverseMedianRT < 98) %>% # Removes missing data from SGST
  filter(CTQ_SexualAbuse < 98,
         SGST_NormalCorrectPercentage < 8)
  

write.xlsx(midus_clean,file = "Midus_clean.xlsx",colNames = TRUE)


### Grouping BDI score

# Group 1 = Low depression and low CTQ
# Group 2 = Low depression and high CTQ
# Group 3 = High depression and low CTQ
# Group 4 = high depression and high CTQ


MIDUS_grouped <- midus_clean %>%
  mutate(MASQ_AD_Group = 
           case_when(MASQ_AD < 59 ~ "Low_AD",
                     MASQ_AD >= 59 ~ "High_AD"),
         CTQ_Group = 
           case_when(CTQ_Scored < 48 ~ "Low_CTQ",
                     CTQ_Scored >= 48 ~ "High_CTQ"), 
         BMI_Group = 
           case_when(BMI < 18.5 ~ "Underweight",
                     BMI >= 18.5 & BMI < 24.9 ~ "Normal",
                     BMI >= 25 & BMI < 29.9 ~ "Overweight",
                     BMI >= 30 & BMI < 39.9 ~ "Obesity",
                     BMI >= 40 ~ "Severe obesity"))


# Grouping data based on low mood and childhood adversity status

# Group 1 = Low depression and low CTQ
# Group 2 = Low depression and high CTQ
# Group 3 = High depression and low CTQ
# Group 4 = high depression and high CTQ


MIDUS_grouped <- MIDUS_grouped %>%
  mutate(Group = 
           case_when(MASQ_AD < 59 & CTQ_Scored < 48 ~ "1",
                     MASQ_AD < 59 & CTQ_Scored >= 48 ~ "2",
                     MASQ_AD >= 59 & CTQ_Scored < 48 ~ "3",
                     MASQ_AD >= 59 & CTQ_Scored >= 48 ~ "4"))


#_______________________________________________________________________________

# Analysis----

## Summary statistics -----

SummaryStats <- midus_clean %>%
  summarise("Age; mean" = mean(age),
            SDAge = sd(age),
            "BMI; mean" = mean(BMI),
            SDBMI = sd(BMI),
            "CTQ, mean" = mean(CTQ_Scored),
            SDCTQ = sd(CTQ_Scored),
            "MASQ; general distress depression, mean score" = mean(MASQ_GenDistressDepress),
            SDDepress = sd(MASQ_GenDistressDepress),
            "MASQ; anhedonic depression subscale, mean" = mean(MASQ_AD),
            sd_AD = sd(MASQ_AD),
            "IL6, mean" = mean(IL6),
            SDIL6 = sd(IL6),
            "TNFa, mean" = mean(TNFa),
            SDTNFa = sd(TNFa),
            "CRP, mean" = mean(CRP),
            SDCRP = sd(CRP),
            "Word recall, mean" = mean(WR_TU_percent),
            SDWordRecall = sd(WR_TU_percent))

SummaryStats

df <- SummaryStats            

min(midus_clean$SGST_ReverseCorrectPercentage)
max(midus_clean$SGST_ReverseCorrectPercentage)

            
write.xlsx(df,file = "SecondaryData_SummaryStats.xlsx",colNames = TRUE)

men <- midus_clean %>%
  filter(sex == 1)

woman <- midus_clean %>%
  filter(sex ==2)

other <- midus_clean %>%
  filter(sex == 3)

98/167
69/167

# Stop and Go summary statistics

SGST_SummaryStats <- midus_clean %>%
  summarise("Stop and Go, percentage of correct responses in congruent condition, Mean" = 
              mean(SGST_NormalCorrectPercentage),
            "Stop and Go, percentage of correct responses in congruent condition, SD" = 
              sd(SGST_NormalCorrectPercentage),
            "Stop and Go, percentage of correct responses in incongruent condition, Mean" = 
              mean(SGST_ReverseCorrectPercentage),
            "Stop and Go, percentage of correct responses in incongruent condition, SD" = 
              sd(SGST_NormalCorrectPercentage),
            "Stop and Go, median reaction time in congruent condition, Mean" = 
              mean(SGST_NormalMedianRT),
            "Stop and Go, median reaction time in congruent condition, SD" = 
              sd(SGST_NormalMedianRT),
            "Stop and Go, median reaction time in incongruent condition, Mean" = 
              mean(SGST_ReverseMedianRT),
            "Stop and Go, median reaction time in incongruent condition, SD" = 
              sd(SGST_ReverseMedianRT))


SGST_SummaryStats


#_______________________________________________________________________________

## QQ plot for normality for mixed model

# QQ plot for low mood score (anhedonic)

x = rnorm(50, 50 , 20)
y = MIDUS_grouped$MASQ_AD


qqplot_MASQ_AD <- qqplot(x, y, xlab = "Normal Distribution", ylab = "MASQ Anhedonia Subscale Score", 
       main = "Q-Q Plot for MASQ AD Subscale")


# QQ plot for CTQ Score 

x = rnorm(50, 50 , 20)
y = MIDUS_grouped$CTQ_Scored


qqplot_CTQ <- qqplot(x, y, xlab = "Normal Distribution", ylab = "CTQ Score", 
                         main = "Q-Q Plot for CTQ Score")


z = poly(y, 2)

qqplot_CTQ_Transformed <- qqplot(x, z, xlab = "Normal Distribution", ylab = "CTQ Score", 
                                     main = "Q-Q Plot for CTQ Score")


# QQ plot for IL6 score

x = rnorm(50, 50 , 20)
y = MIDUS_grouped$IL6


qqplot_IL6 <- qqplot(x, y, xlab = "Normal Distribution", ylab = "IL6 Score", 
                         main = "Q-Q Plot for IL6 Score")


z = poly(y, 2)

qqplot_IL6_Transformed <- qqplot(x, z, xlab = "Normal Distribution", ylab = "CTQ Score", 
                                     main = "Q-Q Plot for CTQ Score")


descdist(MIDUS_grouped$IL6)



#### IL6 and CTQ have non-normally distributed residuals


## Visualisations ----

vis_1 <- midus_clean %>%
  ggplot(aes(x = MASQ_GenDistressDepress, y = CTQ_Scored)) + 
  geom_point() + 
  geom_smooth(se = FALSE) + 
  theme_classic()

vis_1


# Correlation plot for CTQ subscales

corr_CTQ <- cor(midus_clean %>%
                   dplyr::select(CTQ_PhysicalAbuse, CTQ_EmotionalAbuse, 
                                 CTQ_EmotionalNeglect, CTQ_PhysicalNeglect, CTQ_SexualAbuse) %>%
                   rename(EmotionalAbuse = "CTQ_EmotionalAbuse",
                          EmotionalNeglect = "CTQ_EmotionalNeglect",
                          PhysicalAbuse = "CTQ_PhysicalAbuse",
                          PhysicalNeglect = "CTQ_PhysicalNeglect",
                          SexualAbuse = "CTQ_SexualAbuse"))

correlation_vis <- corrplot(corr_CTQ, method = "number")

correlation_vis




# Visualisation for CTQ subscales

colors <- c("Physical abuse" = "#8FBE81",
            "Emotional abuse" = "#77A1C1",
            "Physical neglect" = "#D0A875",
            "Emotional neglect" = "#C36E6E",
            "Sexual abuse" = "#DACB87")

CTQ_MASQGDD_subscale_vis <- midus_clean %>%
  ggplot(aes(x = MASQ_GenDistressDepress)) + 
  geom_smooth(aes(y = CTQ_PhysicalAbuse, color = "Physical abuse"), linewidth = 1.5, linetype = "dotted", se = FALSE, show.legend = FALSE) + 
  geom_smooth(aes(y = CTQ_EmotionalAbuse, color = "Emotional abuse"), linewidth = 1.5, se = FALSE, show.legend = FALSE) + 
  geom_smooth(aes(y = CTQ_PhysicalNeglect, color = "Physical neglect"), linewidth = 1.5, linetype = "dotted", se = FALSE, show.legend = FALSE) + 
  geom_smooth(aes(y = CTQ_EmotionalNeglect, color = "Emotional neglect"), linewidth = 1.5, linetype = "dotted", se = FALSE, show.legend = FALSE) + 
  geom_smooth(aes(y = CTQ_SexualAbuse, color = "Sexual abuse"), linewidth = 1.5, se = FALSE, show.legend = FALSE) + 
  labs(x = "MASQ GDD score", 
       y = "CTQ subscale score",
       color = "Legend") + 
 scale_color_manual(values = colors) + 
  theme_minimal()

CTQ_MASQGDD_subscale_vis

CTQ_MASQAD_subscale_vis <- midus_clean %>%
  ggplot(aes(x = MASQ_AD)) + 
  geom_smooth(aes(y = CTQ_PhysicalAbuse, color = "Physical abuse"), linewidth = 1.5, linetype = "dotted", se = FALSE) + 
  geom_smooth(aes(y = CTQ_EmotionalAbuse, color = "Emotional abuse"), linewidth = 1.5, linetype = "dotted", se = FALSE) + 
  geom_smooth(aes(y = CTQ_PhysicalNeglect, color = "Physical neglect"), linewidth = 1.5, linetype = "dotted", se = FALSE) + 
  geom_smooth(aes(y = CTQ_EmotionalNeglect, color = "Emotional neglect"), linewidth = 1.5,  se = FALSE) + 
  geom_smooth(aes(y = CTQ_SexualAbuse, color = "Sexual abuse"), linewidth = 1.5, linetype = "dotted", se = FALSE) + 
  labs(x = "MASQ AD score", 
       y = "CTQ subscale score",
       color = "Legend") + 
  scale_color_manual(values = colors) + 
  theme_minimal()

CTQ_MASQAD_subscale_vis


MASQ_CTQ <- CTQ_MASQGDD_subscale_vis + CTQ_MASQAD_subscale_vis

MASQ_CTQ


# Visualisation for inflammation

colors <- c("IL6" = "sienna1",
            "CRP" = "pink3",
            "TNFa" = "firebrick1")

Inflammation_MASQ_vis <- midus_clean %>%
  ggplot(aes(x = MASQ_GenDistressDepress)) + 
  geom_point(aes(y = IL6, color = "IL6"), shape = 16) + 
  geom_smooth(aes(y = IL6, color = "IL6"), se = FALSE) + 
  geom_point(aes(y = CRP, color = "CRP"), shape = 15) + 
  geom_smooth(aes(y = CRP, color = "CRP"), se = FALSE) + 
  geom_point(aes(y = TNFa, color = "TNFa"), shape = 17) +
  geom_smooth(aes(y = TNFa, color = "TNFa"), se = FALSE) + 
  labs(x = "MASQ General distress: Depression subscale score", 
       y = "Inflammatory marker concentration",
       color = "Legend") + 
  scale_color_manual(values = colors) + 
  theme_minimal()

Inflammation_MASQ_vis


# Visualisation of inflammation, low mood and CTQ


inf_LM_CTQ <- MIDUS_grouped %>%
  ggplot(aes(x = IL6, y = MASQ_AD_Group)) + 
  geom_dotsinterval(side = "left") + 
  stat_halfeye(side = "right") + 
  facet_wrap(~CTQ_Group) + 
  theme_minimal() + 
  labs(x = "IL6 concentration", 
       y = "MASQ AD Group")

inf_LM_CTQ


# Visualisation of reaction times in SGST vs CTQ Subscales


SGST_CTQSub_Vis <- ggplot(aes(x = SGST_NormalMedianRT),
                          data = midus_clean) + 
  geom_smooth(aes(y = CTQ_PhysicalAbuse, color = "Physical abuse"), linewidth = 1.5, show.legend = FALSE, se = FALSE) + 
  geom_smooth(aes(y = CTQ_EmotionalAbuse, color = "Emotional abuse"), linewidth = 1.5, show.legend = FALSE, linetype = "dotted", se = FALSE) + 
  geom_smooth(aes(y = CTQ_PhysicalNeglect, color = "Physical neglect"), linewidth = 1.5, show.legend = FALSE, linetype = "dotted", se = FALSE) + 
  geom_smooth(aes(y = CTQ_EmotionalNeglect, color = "Emotional neglect"), linewidth = 1.5,  show.legend = FALSE, linetype = "dotted", se = FALSE) + 
  geom_smooth(aes(y = CTQ_SexualAbuse, color = "Sexual abuse"), linewidth = 1.5, linetype = "dotted", show.legend = FALSE, se = FALSE) + 
  labs(x = "Median RT to Stop and Go Task, normal condition (s)", 
       y = "CTQ subscale score",
       color = "Legend") + 
  scale_color_manual(values = colors) + 
  theme_minimal() + 
  theme(text = element_text(size = 11))


SGST_CTQSub_Vis


# Visualisation of IL6 and CTQ Physical Abuse subscale

IL6_CTQSub_Vis <- ggplot(aes(x = IL6),
                          data = midus_clean) + 
  geom_smooth(aes(y = CTQ_PhysicalAbuse, color = "Physical abuse"), linewidth = 1.5, se = FALSE) + 
  geom_smooth(aes(y = CTQ_EmotionalAbuse, color = "Emotional abuse"), linewidth = 1.5, linetype = "dotted", se = FALSE) + 
  geom_smooth(aes(y = CTQ_PhysicalNeglect, color = "Physical neglect"), linewidth = 1.5, linetype = "dotted", se = FALSE) + 
  geom_smooth(aes(y = CTQ_EmotionalNeglect, color = "Emotional neglect"), linewidth = 1.5, linetype = "dotted", se = FALSE) + 
  geom_smooth(aes(y = CTQ_SexualAbuse, color = "Sexual abuse"), linewidth = 1.5, linetype = "dotted", se = FALSE) + 
  labs(x = "Peripheral IL6 concentration (pg/mL)", 
       y = "CTQ subscale score",
       color = "Legend") + 
  scale_color_manual(values = colors) + 
  theme_minimal() + 
  theme(text = element_text(size = 11))
  


IL6_CTQSub_Vis


## CTQ subscales and MASQ AD vis

CTQMASQAD_vis_data <- midus_clean %>%
  dplyr:: select(CTQ_PhysicalAbuse, CTQ_PhysicalNeglect, 
                 CTQ_EmotionalNeglect, CTQ_EmotionalAbuse, CTQ_SexualAbuse,
                 MASQ_AD) %>%
  rename('Physical Abuse' = CTQ_PhysicalAbuse,
         'Physical Neglect' = CTQ_PhysicalNeglect,
         'Emotional Abuse' = CTQ_EmotionalAbuse,
         'Emotional Neglect' = CTQ_EmotionalNeglect,
         'Sexual Abuse' = CTQ_SexualAbuse) %>%
  pivot_longer(cols = c('Physical Abuse',
                        'Physical Neglect',
                        'Emotional Abuse',
                        'Emotional Neglect',
                        'Sexual Abuse'),
               names_to = "Subscale",
               values_to = "Subscale_Score")

CTQMASQAD_vis_data$Subscale <- factor(CTQMASQAD_vis_data$Subscale , levels=c("Emotional Neglect", 
                                                                             "Emotional Abuse", 
                                                                             "Physical Abuse", 
                                                                             "Physical Neglect", 
                                                                             "Sexual Abuse") )

CTQMASQAD_vis <- ggplot(CTQMASQAD_vis_data, aes(x = MASQ_AD, y = Subscale_Score,
                                          fill = Subscale)) + 
  stat_smooth(
    geom = 'area', method = 'loess', span = 1/5,
    alpha = 0.65) + 
 # geom_area() +
  scale_fill_brewer(palette = "Accent") + 
  theme_minimal() + 
  labs(x = "Score on the MASQ Anhedonic Subscale",
       y = "Score on the CTQ",
       fill = "CTQ Subscale") + 
  theme(text = element_text(size = 12))

CTQMASQAD_vis





 #_______________________________________________________________________________

## CTQ and cognition regressions ----

## Recall_______________________________________________________________________

Cog_CTQ_Recall <- lm(WR_TU_percent ~ CTQ_Scored, 
                     data = midus_clean)

Cog_CTQ_Sub_Recall <- lm(WR_TU_percent ~ CTQ_EmotionalAbuse + 
                           CTQ_EmotionalNeglect + 
                           CTQ_PhysicalAbuse+ 
                           CTQ_PhysicalNeglect + 
                           CTQ_SexualAbuse, 
                         data = midus_clean)

summary(Cog_CTQ_Recall)
# p = 0.4325, F = 0.6194 on 1 and 153, adj R = -0.0024

summary(Cog_CTQ_Sub_Recall)
# p = 0.8091, F = 0.455 on 5 and 149, adju = -0.01801

vif(Cog_CTQ_Sub_Recall)
### All VIF below 3


## Stop and go task_____________________________________________________________

Cog_CTQ_SGST_Reverse <- lm(SGST_ReverseCorrect ~ CTQ_Scored, 
                           data = midus_clean)


Cog_CTQ_Sub_SGST_Reverse <- lm(SGST_ReverseCorrect ~ CTQ_EmotionalAbuse + 
                           CTQ_EmotionalNeglect + 
                           CTQ_PhysicalAbuse + 
                           CTQ_PhysicalNeglect + 
                           CTQ_SexualAbuse, 
                         data = midus_clean)

summary(Cog_CTQ_SGST_Reverse)
# p = 0.5704, F = 0.3234 on 1 and 155 df, adj R squared = -0.004356

summary(Cog_CTQ_Sub_SGST_Reverse)
# p = 0.2809, F = 1.268 5 and 151 df, adj R squared = 0.008502
# Emotional Abuse; p = 0.0393

vif(Cog_CTQ_Sub_SGST_Reverse) # Checking variance inflation score
### All VIF below 3


SGST_null <- lm(SGST_ReverseCorrect ~ 1, data = midus_clean) # Null model for stop and go reverse correct


# Linear regression just with emotional abuse

Cog_CTQ_SGST_Reverse_EA <- lm(SGST_ReverseCorrect ~ CTQ_EmotionalAbuse, 
                              data = midus_clean)

summary(Cog_CTQ_SGST_Reverse_EA) 
# p = 0.375, F = 0.7909, on 1 and 165, R sqr = 0.005077





## Reaction times_______________________________________________________________


# REVERSE

Cog_CTQ_SGST_ReverseRT <- lm(SGST_ReverseMedianRT ~ CTQ_Scored, 
                           data = midus_clean)

summary(Cog_CTQ_SGST_ReverseRT)
# p = 0.0556, F = 3.72 on 1 and 155, adj R squared = 0.01713


Cog_CTQ_Sub_SGST_ReverseRT <- lm(SGST_ReverseMedianRT ~ CTQ_EmotionalAbuse + 
                                 CTQ_EmotionalNeglect + 
                                 CTQ_PhysicalAbuse+ 
                                 CTQ_PhysicalNeglect + 
                                 CTQ_SexualAbuse, 
                               data = midus_clean)


summary(Cog_CTQ_Sub_SGST_ReverseRT)
# p = 0.3552, R squared = 0.003, F = 1.114 on 5 and 151


vif(Cog_CTQ_Sub_SGST_ReverseRT)
# All <3


# NORMAL

Cog_CTQ_SGST_NormalRT <- lm(SGST_NormalMedianRT ~ CTQ_Scored, 
                             data = midus_clean)

summary(Cog_CTQ_SGST_NormalRT)
# p = 0.0389, R squared = 0.02095, F = 4.339 on 1 and 155

ggplot(aes(x = CTQ_Scored,
           y = SGST_ReverseMedianRT),
       data = midus_clean) + 
  geom_point() + 
  geom_smooth(se = FALSE) + 
  theme_minimal() 

Cog_CTQ_Sub_SGST_NormalRT <- lm(SGST_NormalMedianRT ~ CTQ_EmotionalAbuse + 
                                   CTQ_EmotionalNeglect + 
                                   CTQ_PhysicalAbuse + 
                                   CTQ_PhysicalNeglect + 
                                   CTQ_SexualAbuse, 
                                 data = midus_clean)

summary(Cog_CTQ_Sub_SGST_NormalRT)
# p = 0.1697, F = 1.577 on 5 and 151, adj R squared = 0.01817
# p for PA = 0.0403


# Simple linear regression for just PA

SGST_NormalRT_PA <- lm(SGST_NormalMedianRT ~ CTQ_PhysicalAbuse,
                       data = midus_clean)

summary(SGST_NormalRT_PA)
# p = 0.007003, F = 7.47 on 1 and 155 df, adj R squared = 0.03982

# Null model

SGST_NormalRT_null <- lm(SGST_NormalMedianRT ~ 1,
                         data = midus_clean)


# ANOVA comparing null model to PA

anova(SGST_NormalRT_PA, SGST_NormalRT_null)
# p = 0.007003, RSS first = 8.9139, RSS second = 9.3435


## MASQ and cognition regressions ----

## ADNHEDONIC SUBSCALE _________________________________________________________

## Recall

Cog_MASQ_Recall <- lm(WR_TU_percent ~ MASQ_AD, 
                     data = midus_clean)


summary(Cog_MASQ_Recall)
# p = 0.8762, F = 0.02436 on 1 and 153, adj R = -0.006376


## Stop and go task

Cog_MASQ_SGST_Reverse <- lm(SGST_ReverseCorrectPercentage ~ MASQ_AD, 
                           data = midus_clean)


summary(Cog_MASQ_SGST_Reverse)
# p = 0.575, F = 0.3157 on 1 and 155 df, adj r squared = -0.004406


## Reaction times

# Reverse condition

Cog_MASQAD_SGST_ReverseRT <- lm(SGST_ReverseMedianRT ~ MASQ_AD, 
                             data = midus_clean)


summary(Cog_MASQAD_SGST_ReverseRT)
# p = 0.875, F = 0.02485 on 155 df, adj -0.00629


# Normal condition

Cog_MASQAD_SGST_NormalRT <- lm(SGST_NormalMedianRT ~ MASQ_AD, 
                                data = midus_clean)


summary(Cog_MASQAD_SGST_NormalRT)
# p = 0.7296, F = 0.1199 on 155 df, adj -0.005674




## GENERAL DISTRESS AND DEPRESSION  SUBSCALE ___________________________________

## Recall

Cog_MASQDD_Recall <- lm(WR_TU_percent ~ MASQ_GenDistressDepress, 
                      data = midus_clean)


summary(Cog_MASQDD_Recall)
# p = 0.4264, F = 0.636 on 1 and 153, adj R = -0.00236


## Stop and go task

Cog_MASQDD_SGST_Reverse <- lm(SGST_ReverseCorrectPercentage ~ MASQ_GenDistressDepress, 
                            data = midus_clean)


summary(Cog_MASQDD_SGST_Reverse)
# p = 0.7022, F = 0.1468 on 1 and 155 df, adj r squared = -0.0055


## Reaction times

# Reverse

Cog_MASQ_DDSGST_ReverseRT <- lm(SGST_ReverseMedianRT ~ MASQ_GenDistressDepress, 
                              data = midus_clean)


summary(Cog_MASQ_DDSGST_ReverseRT)
# p = 0.8727, F = 0.02576 1 and 155 df, adj = -0.006284


# Normal

Cog_MASQ_DDSGST_NormalRT <- lm(SGST_NormalMedianRT ~ MASQ_GenDistressDepress, 
                                data = midus_clean)


summary(Cog_MASQ_DDSGST_NormalRT)
# p = 0.6951, F = 0.1542 1 and 155 df, adj = -0.005451




## CTQ and MASQ regression ----

CTQ_MASQ_AD <- lm(MASQ_AD ~ CTQ_Scored, data = midus_clean)

summary(CTQ_MASQ_AD)


CTQ_MASQ_GDD <- lm(MASQ_GenDistressDepress ~ CTQ_Scored, data = midus_clean)
summary(CTQ_MASQ_GDD)


### CTQ MASQ CTQ subscale regression

MASQ_AD_CTQ_Sub <- lm(MASQ_AD ~ CTQ_EmotionalAbuse + 
                                   CTQ_EmotionalNeglect + 
                                   CTQ_PhysicalAbuse + 
                                   CTQ_PhysicalNeglect + 
                                   CTQ_SexualAbuse, 
                                 data = midus_clean)

summary(MASQ_AD_CTQ_Sub)

vif(MASQ_AD_CTQ_Sub)
### All variables are below 3


MASQ_AD_CTQ_EN <- lm(MASQ_AD ~ CTQ_EmotionalNeglect, data = midus_clean)

summary(MASQ_AD_CTQ_EN)

MASQ_AD_null <- lm(MASQ_AD ~ 1, data = midus_clean)

anova(MASQ_AD_CTQ_EN, MASQ_AD_null)


MASQ_GDD_CTQ_Sub <- lm(MASQ_GenDistressDepress ~ CTQ_EmotionalAbuse + 
                        CTQ_EmotionalNeglect + 
                        CTQ_PhysicalAbuse + 
                        CTQ_PhysicalNeglect + 
                        CTQ_SexualAbuse, 
                      data = midus_clean)

summary(MASQ_GDD_CTQ_Sub)

vif(MASQ_GDD_CTQ_Sub)
### All variables below 3

MASQ_GDD_EA_SA <- lm(MASQ_GenDistressDepress ~ CTQ_EmotionalAbuse + 
                        CTQ_SexualAbuse, data = midus_clean)

summary(MASQ_GDD_EA_SA)

MASQ_GDD_null <- lm(MASQ_GenDistressDepress ~ 1, data = midus_clean)

anova(MASQ_GDD_EA_SA, MASQ_GDD_null)



## Inflammation ----


# CRP and IL-6

CRP_IL6 <- lm(CRP ~ IL6, data = midus_clean)

summary(CRP_IL6)


# Inflammation and CTQ

## IL6 and CTQ/Low mood

IL6_CTQ <- lm(IL6 ~ CTQ_Scored, data = midus_clean) 
IL6_CTQ_Sub <- lm(IL6 ~ CTQ_EmotionalAbuse + 
                         CTQ_EmotionalNeglect + 
                         CTQ_PhysicalAbuse+ 
                         CTQ_PhysicalNeglect + 
                         CTQ_SexualAbuse, 
                       data = midus_clean)

summary(IL6_CTQ_Sub)

vif(IL6_CTQ_Sub)
### All below 3

summary(IL6_CTQ)
# Physical abuse sig

IL6_PA <- lm(IL6 ~ CTQ_PhysicalAbuse, data = midus_clean)

summary(IL6_PA)


IL6_null <- lm(IL6 ~ 1, data = midus_clean)

anova(IL6_PA, IL6_null)

summary(IL6_CTQ)
# Sig


IL6_MASQ_MDD <- lm(IL6 ~ MASQ_GenDistressDepress, data = midus_clean) 

summary(IL6_MASQ_MDD)

IL6_MASQ_AD <- lm(IL6 ~ MASQ_AD, data = midus_clean) 

summary(IL6_MASQ_AD)


## TNFa and CTQ/Low mood

TNFa_CTQ <- lm(TNFa ~ CTQ_Scored, data = midus_clean) 
TNFa_CTQ_Sub <- lm(TNFa ~ CTQ_EmotionalAbuse + 
                    CTQ_EmotionalNeglect + 
                    CTQ_PhysicalAbuse+ 
                    CTQ_PhysicalNeglect + 
                    CTQ_SexualAbuse, 
                  data = midus_clean)

summary(TNFa_CTQ_Sub)

vif(TNFa_CTQ_Sub)


# Physical abuse sig
summary(TNFa_CTQ)
# Sig


TNFa_MASQ_GDD <- lm(TNFa ~ MASQ_GenDistressDepress, data = midus_clean) 

summary(TNFa_MASQ_GDD)

TNFa_MASQ_AD <- lm(TNFa ~ MASQ_AD, data = midus_clean) 

summary(TNFa_MASQ_AD)


# Inflammation with stop and go

inflammation_SGST <- lm(SGST_ReverseCorrect ~ IL6, data = midus_clean)

summary(inflammation_SGST)


# Adding into stop and go model

CTQ_SGST_ReverseCorrect_IL6 <- lm(SGST_ReverseCorrect ~ CTQ_PhysicalNeglect + 
                                    CTQ_PhysicalAbuse + IL6, data = midus_clean)


summary(CTQ_SGST_ReverseCorrect_IL6)



#_______________________________________________________________________________
## Effects of BMI----


# Stop and Go Task (reaction time)______________________________________________

SGST_NormalRT_PA_BMI <- lm(SGST_NormalMedianRT ~ CTQ_PhysicalAbuse + BMI, 
                           data = midus_clean)

summary(SGST_NormalRT_PA_BMI)
# p = 0.00934, F = 4.848 on 2 and 154, adjusted R squared = 0.047
# BMI = 0.148

anova(SGST_NormalRT_PA_BMI, SGST_NormalRT_PA)
# p = 0.148

# MASQ AD and CTQ EN subscale___________________________________________________

MASQ_AD_CTQ_EN_BMI <- lm(MASQ_AD ~ CTQ_EmotionalNeglect + BMI,
                         data = midus_clean)

summary(MASQ_AD_CTQ_EN_BMI)
# p = 0.009072, for BMI = 0.86997, F = 4,84 on 2 and 164 df,  
# R sqr = 0.5573 which is lower than R sqr for model without BMI, adj = 0.04422


# MASQ GDD + Emotional Abuse and Sexual Abuse subscales_________________________

MASQ_GDD_CTQ_EA_SA_BMI <- lm(MASQ_GenDistressDepress ~ CTQ_EmotionalAbuse + 
                               CTQ_SexualAbuse + BMI,
                             data = midus_clean)

summary(MASQ_GDD_CTQ_EA_SA_BMI)
# p = 0.0003483, p of BMI = 0.4493, F = 6.506 on 3 and 163 df
# r sqr = 0.1069 which is fractionally above model without BMI, 
# adjusted R squared = 0.0905


# IL6 and CTQ___________________________________________________________________

IL6_CTQ_BMI <- lm(IL6 ~ CTQ_Scored + BMI, data = midus_clean)

summary(IL6_CTQ_BMI)
# p = 1.45e-12, BMI p = 1.23e-11***, F = 32.33 on 2 and 164
# multiple R squared = 0.2828, adjusted R squared = 0.2741 


# Regression of just IL6 and BMI
IL6_BMI <- lm(IL6 ~ BMI, data = midus_clean)
summary(IL6_BMI)
# p = 2.473e-12, F = 57.34 on 1 and 165, multiple R squared = 0.2579, adjusted 
# R squared = 0.2534

# Model accounting for interaction effects of IL6 and BMI

IL6_CTQ_BMI_v2 <- lm(IL6 ~ CTQ_Scored * BMI, data = midus_clean)

summary(IL6_CTQ_BMI_v2)
# p = 2.624e-12, p for interaction effect = 0.10688, F = 22.65 on 3 and 163, 
# Adjusted R squared = 0.2812 


# ANOVA to determine if either the two models (with and without interaction terms)
# is significantly better 

anova(IL6_CTQ_BMI, IL6_CTQ_BMI_v2)
# p = 0.1069
# RSS of first model = 318.23, RSS of second model = 313.18, F = 2.6287


# ANOVA to determine if including BMI significantly improves fit of model

anova(IL6_CTQ, IL6_CTQ_BMI_v2)
# p = 3.102e-11***, RSS for first model = 421.43, RSS for second model = 313.18


# IL6 and physical abuse subscale_______________________________________________

IL6_PA_BMI <- lm(IL6 ~ CTQ_PhysicalAbuse + BMI, data = midus_clean)

summary(IL6_PA_BMI)
# p = 6.033-13, p for BMI = 1.26-11, F = 33.57 on 2 and 164 df
# R squared = 0.2905, adjusted R squared = 0.2818

IL6_PA_BMI_v2 <- lm(IL6 ~ CTQ_PhysicalAbuse * BMI, data = midus_clean)

summary(IL6_PA_BMI_v2)
# p = 7.31e-13, F = 23.88 on 3 and 163, R squared = 0.3053,
# adjusted r squared = 0.2925, p for interaction = 0.06361, p for PA = 0.02171

vif(IL6_PA_BMI)
# both <2 


# ANOVA to see if adding BMI into the model sig improves fit

anova(IL6_PA, IL6_PA_BMI_v2)
# p = 2.087e-11, RSS for first model = 416.81, RSS for second model = 308.24
# F = 28.707


# BMI and TNFa__________________________________________________________________

BMI_TNFa <- lm(BMI ~ TNFa, data = midus_clean)

summary(BMI_TNFa)
# p = 0.009177, F = 6.951 on 1 and 165 df, multiple R sqr = 0.04042, 
# adjusted r sqr = 0.03461 

BMI_TNFa_IL6 <- lm(BMI ~ TNFa + IL6, data = midus_clean)

summary(BMI_TNFa_IL6)
# p = 3.215e-12, p for TNFa coefficient = 0.0455*, 
# p for IL6 coefficient = 1.17e-11 ***
# F = 31.23 on 2 and 164, R squared = 0.2758, adju r sqr = 0.267


#_______________________________________________________________________________
## ANOVA and MM in different groups ----

# Finding percentiles for childhood adversity / depression

CTQ_quant <- midus_clean$CTQ_Scored

quantile(CTQ_quant, 0.75)
### = 48

MASQ_AD_quant <- midus_clean$MASQ_AD

quantile(MASQ_AD_quant, 0.75)
### = 59


# Seeing how many of each value there is


group1 <- MIDUS_grouped %>%
  filter(Group == 1)
# 93 obs

group2 <- MIDUS_grouped %>%
  filter(Group == 2)
# 31 obs

group3 <- MIDUS_grouped %>%
  filter(Group == 3)
# 29 obs

group4 <- MIDUS_grouped %>%
  filter(Group == 4)
# 14 obs


# Word recall

anova_WordRecall <- aov_4(WR_TU_percent ~ Group + (1 | num), data = MIDUS_grouped)

summary(anova_WordRecall)
### F = 0.97, p-value = 0.4068

emmeans(anova_WordRecall, pairwise ~ Group)
### No pairwise comparisons are sig


# Stop and go; reverse

anova_SGSTReverse <- aov_4(SGST_ReverseCorrectPercentage ~ Group + (1 | num), data = MIDUS_grouped)

summary(anova_SGSTReverse)
### F = 2.6285, p-value = 0.05207

emmeans(anova_SGSTReverse, pairwise ~ Group)
### No pairwise differences


# Stop and go; reaction time

anova_SGSTReverseRT <- aov_4(SGST_ReverseMedianRT ~ Group + (1 | num), data = MIDUS_grouped)

summary(anova_SGSTReverseRT)

emmeans(anova_SGSTReverseRT, pairwise ~ Group)


# Inflammation

# IL - 6

anova_IL6 <- aov_4(IL6 ~ Group + (1 | num), data = MIDUS_grouped)

summary(anova_IL6)
### F value = 3.727, p = 0.0125

emmeans(anova_IL6, pairwise ~ Group)
### Group 1 and group 4, p = 0.0061
### Group 2 and group 4 = 0.0363
### Group 3 and group 4 = 0.0307


# CRP

anova_CRP <- aov_4(CRP ~ Group + (1 | num), data = MIDUS_grouped)

summary(anova_CRP)

emmeans(anova_CRP, pairwise ~ Group)


# TNF-alpha


anova_TNFa <- aov_4(TNFa ~ Group + (1 | num), data = MIDUS_grouped)

summary(anova_TNFa)

emmeans(anova_TNFa, pairwise ~ Group)







## Mixed models in different groups


## Mixed model with inflammation as output

#IL6

glmm_data <- MIDUS_grouped %>%
  mutate(IL6_trans = IL6/100)

mixed_model_IL6 <- glmmTMB(IL6_trans ~ MASQ_AD_Group * CTQ_Group + 
                           (1 | age) + 
                           (1 | BMI_Group),
                           family = beta_family(),
                         data = glmm_data)

summary(mixed_model_IL6)

emmeans(mixed_model_IL6,pairwise ~ MASQ_AD_Group*CTQ_Group)
## Significance is gone now


# Stop and go task

mixed_model_SGST <- lmer(SGST_ReverseCorrectPercentage ~ MASQ_AD_Group * CTQ_Group + 
                           (1  | age) + 
                           (1 | BMI_Group), 
                         data = MIDUS_grouped)

summary(mixed_model_SGST)
emmeans(mixed_model_SGST, pairwise ~ MASQ_AD_Group*CTQ_Group)

## Power analysis ----

x <- c(1, 2, 3, 4, 5)

# Power

power_2 <- wp.regression(n=167, p1 = x, p2 = 0, f2 = 0.2)

power_2

