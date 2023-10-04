
#_______________________________________________________________________________
# Reading in libraries and data ----

## Reading in libraries ----

library(tidyverse)
library(stringr)
library(mma)
library(performance)
library(openxlsx)
library(WebPower)
library(car) # For VIF() function

## Reading in data ----

BDI <- read_csv("BDI_SF.csv") # Beck's depression inventory data
CTQ <- read_csv("CTQ_SF.csv") # Childhood trauma questionnaire data
Ultimatum <- read_csv("Ultimatum_SF.csv") # Ultimatum 
Word_recall <- read_csv("Word_Recall_LF.csv") # Word recall
GNG <- read_csv("GNG_SF.csv") # Go no go
Demos <- read_csv("Demos_SF.csv") # Demographics data


#_______________________________________________________________________________
# Data cleaning ----


### Cleaning demographics data

Demos_tidy <- dplyr::select(Demos, "Response", "Object Name", "Participant Private ID", "Response Type") %>% # Only selecting columns needed
  rename(Question = 'Object Name',
         ID = 'Participant Private ID',
         ResponseType = 'Response Type') %>% # Renamimg columns appropriately
  filter(!is.na(Question),
         ResponseType != "action") # Filtering out missing values (additional rows put in by Gorilla)

Demos_age <- Demos_tidy %>% 
  filter(Question == "Age") %>%
  mutate(Response = as.numeric(Response)) %>%
  dplyr::select(Response, ID) %>%
  rename(Age = "Response")

Demos_gender <- filter(Demos_tidy, Question == "Gender")

Demos_gender <- Demos_gender %>%
  dplyr::select(Response, ID) %>%
  rename(Gender = "Response")

# Removing duplicate data (position of answer from Gorilla)
Demos_gender <- filter(Demos_gender, Gender != 2)
Demos_gender <- filter(Demos_gender, Gender != 3)
Demos_gender <- filter(Demos_gender, Gender != 1)

# Joining demographics together and removing ID - don't want to be able to identify participants
# these data were recorded to assess representativeness of sample only

Demographics <- full_join(Demos_gender, Demos_age, by = "ID") %>%
  dplyr::select(Age, Gender)


### Cleaning BDI data

BDI_tidy <- dplyr::select(BDI, "Participant Private ID", "Question", "Key", "Response") %>% # Only selecting columns we need
  rename(ID = 'Participant Private ID') %>% # Renaming columns for R
  filter(!is.na(Question),
         Key == "value") %>% # Filtering out missing values (additional rows put in by Gorilla)
mutate(Response = as.numeric(Response)) # Changing response column to be numeric



### Cleaning CTQ data

CTQ_tidy <- CTQ %>%
  dplyr::select("Participant Private ID", "Key", "Response") %>% # Selecting only needed columns
  mutate(quantised = str_detect(CTQ$Key, 'quantised'), # Creating a new variable 
         #to indicate which of the statements was "quantised" by Gorilla
         QNumber = str_sub(CTQ$Key,1,2)) %>% # Making a new column with the question number
  rename(ID = 'Participant Private ID') %>% # Renaming columns for R
  filter(!is.na(Key),
         quantised != "TRUE") # Filtering out missing values (additional rows put in by Gorilla)



### Cleaning GNG

GNG_tidy <- dplyr::select(GNG, "Participant Private ID", 
                   "Trial Number", 
                   "Display",
                   "Screen", 
                   "Screen ID", 
                   "Response",
                   "Reaction Time",
                   "Correct",
                   "Spreadsheet: ans") %>% # Selecting only needed columns
  rename(ID = 'Participant Private ID',
         TrialNumber = 'Trial Number',
         ScreenID = 'Screen ID',
         ReactionTime = 'Reaction Time',
         CorrectAnswer = 'Spreadsheet: ans') %>% # Renaming columns to be more appropriate
  filter(Screen != 'Interstim_1',
         Screen != 'Interstim_2',
         Screen != 'Screen 2',
         !is.na(Response)) # Filtering out missing values (additional rows put in by Gorilla)



### Cleaning ultimatum 
Ultimatum_tidy <- dplyr::select(Ultimatum, "Participant Private ID", 
                         "Trial Number", 
                         "Display",
                         "Screen", 
                         "Screen ID", 
                         "Response",
                         "Response Type",
                         "Reaction Time",
                         "Object Name",
                         "Object ID") %>% # Only selecting the needed columns
  rename(ID = 'Participant Private ID',
         TrialNumber = 'Trial Number',
         ScreenID = 'Screen ID',
         ReactionTime = 'Reaction Time',
         ResponseType = 'Response Type',
         ObjectName = 'Object Name',
         ObjectID = 'Object ID') %>% # Renaming columns to more appropriate names
  mutate(Screen = factor(Screen)) %>%
  filter(!is.na(Response),
         Screen == "A" | Screen == "B" | Screen == "C" |Screen == "D") # Filtering out missing values (additional rows put in by Gorilla)

Ultimatum_tidy <- Ultimatum_tidy %>%
  mutate(Ultimatum_sum_accepted = as.numeric(str_count(Ultimatum_tidy$Response, "Accept")))

  
Ultimatum_calc <- Ultimatum_tidy %>%
  group_by(ID) %>%
  summarise(Ultimatum_percent = sum((Ultimatum_sum_accepted)/51)*100)



### Cleaning word recall

Word_recall_tidy <- dplyr::select(Word_recall, "Participant Private ID",
                           "Trial Number",
                           "Display",
                           "Screen ID",
                           "Response",
                           "Spreadsheet: Word",
                           "Correct",
                           "Spreadsheet: Ans") %>%
  rename(ID = 'Participant Private ID',
         TrialNumber = 'Trial Number',
         ScreenID = 'Screen ID',
         Word = 'Spreadsheet: Word',
         Familiarity = 'Spreadsheet: Ans') %>%
  filter(!is.na(Response),
         !is.na(Display)) # Filtering out missing values (additional rows put in by Gorilla)


#_______________________________________________________________________________

## Calculating scores from answers----



### Calculating the BDI 

BDI_calc <- BDI_tidy %>% 
  group_by(ID) %>%
  summarise(BDI_Score = sum(Response))

BDI_calc



### Calculating word recall

# Working out % correctly remembered for word recall

# Removing one participants' data (as they said they did not understand the 
# instructions and did not accurately complete the task)

WordRecall_calc <- Word_recall_tidy %>% 
  group_by(ID) %>%
  summarise(WordRecall_Num_Correct = sum(Correct)) %>%
  filter(ID != "8778640")


# Calculating the percentage of words each participant got correct

WordRecall_percent <- WordRecall_calc %>%
  mutate(WordRecall_Per_Correct = (WordRecall_Num_Correct/60)*100)


# Grouping words by +ve and -ve valence

positive_words = c("success",
                   "victory",
                   "glee",
                   "overjoyed",
                   "fun",
                   "proud",
                   "splendid",
                   "thrill",
                   "wonderful",
                   "bliss",
                   "excitement",
                   "triumph",
                   "elated",
                   "surprised",
                   "ecstatic",
                   "passion",
                   "miracle",
                   "energy",
                   "lively",
                   "alive") # List of positive words
                   

negative_words <- c("threatening",
                    "tragic",
                    "death",
                    "outraged",
                    "assault",
                    "aggressive",
                    "stress",
                    "traumatising",
                    "panic",
                    "disappoint",
                    "sharp",
                    "storm",
                    "roaring",
                    "disappear",
                    "blizzard",
                    "extreme",
                    "bang",
                    "startled",
                    "chaos",
                    "gun") # List of negative words

Word_Valences <- Word_recall_tidy %>%
  mutate(Word_valence = 
           case_when(Word %in% positive_words ~ "Positive",
                     Word %in% negative_words ~ "Negative",
                     TRUE ~ "Neutral"))

# Number of positive words correctly identified as having been seen before
Word_poscorrect <- Word_Valences %>%
  group_by(ID) %>%
  filter(ID != "8778640",
         Word_valence == "Positive",
         Familiarity == "Yes") %>%
  summarise(WordRecall_NumCorrect_POS = sum(Correct)) %>%
  mutate(WordRecall_PerCorrect_POS = (WordRecall_NumCorrect_POS/20)*100)

# Number of negative words correctly identified as having been seen before
Word_negcorrect <- Word_Valences %>%
  group_by(ID) %>%
  filter(ID != "8778640",
         Word_valence == "Negative",
         Familiarity == "Yes") %>%
  summarise(WordRecall_NumCorrect_NEG = sum(Correct)) %>%
  mutate(WordRecall_PerCorrect_NEG = (WordRecall_NumCorrect_NEG/20)*100)


# Number of positive words incorrectly identified as having been seen before

Word_falsepos_POS <- Word_Valences %>%
  group_by(ID) %>%
  filter(ID != "8778640",
         Word_valence == "Positive",
         Familiarity == "No",
         Correct == 0) %>%
  summarise(WordRecall_NumCorrect_POS = n()) 


Word_falsepos_POS <- Word_falsepos_POS %>%
  mutate(WordRecall_PerFalsePos_POS = (WordRecall_NumCorrect_POS/20)*100) %>%
  dplyr:: select(ID, WordRecall_PerFalsePos_POS)


# Number of negative words incorrectly identified as having been seen before

Word_falsepos_NEG <- Word_Valences %>%
  group_by(ID) %>%
  filter(ID != "8778640",
         Word_valence == "Negative",
         Familiarity == "No",
         Correct == 0) %>%
  summarise(WordRecall_NumCorrect_NEG = n()) 

Word_falsepos_NEG <- Word_falsepos_NEG %>%
  mutate(WordRecall_PerFalsePos_NEG = (WordRecall_NumCorrect_NEG/20)*100) %>%
  dplyr:: select(ID, WordRecall_PerFalsePos_NEG)




# Joining datasets together

WR_1 <- full_join(WordRecall_percent, Word_poscorrect, by = "ID")
WR_2 <- full_join(Word_negcorrect, Word_falsepos_POS, by = "ID")
WR_3 <- full_join(WR_1, WR_2, by = "ID")

WordRecall_complete <- full_join(WR_3, Word_falsepos_NEG, by = "ID")


# ### Calculating score for GNG 

GNG_calc <- GNG_tidy %>% 
  group_by(ID) %>%
  summarise(GNG_Num_Correct = sum(Correct), 
            GNG_MeanRT = mean(ReactionTime))


# Mean reaction time for correct response

GNG_calc_cor <- GNG_tidy %>%
  group_by(ID) %>%
  filter(Correct == "1",
         CorrectAnswer == "pos") %>%
  summarise(GNG_MeanRTcorrect = mean(ReactionTime))


# Mean reaction time for incorrect response

GNG_calc_incor <- GNG_tidy %>%
  group_by(ID) %>%
  filter(Correct == "0",
         CorrectAnswer == "pos") %>%
  summarise(GNG_MeanRTIncorrect = mean(ReactionTime))


# Working out percent correct

GNG_calc <- GNG_calc %>%
  mutate(GNG_per_Correct = (GNG_Num_Correct/180)*100)

# Adding correct ans

CorrectAns <- GNG_tidy %>%
  dplyr::select("ID", "CorrectAnswer")



### Scoring the CTQ

new_col <- c(1:560)

# Loop to reverse code appropriate questions

for (i in 1:560){
  
# If any of the following questions have been answered 
  
  if (CTQ_tidy$QNumber[i] == "2." | 
      CTQ_tidy$QNumber[i] == "5." | 
      CTQ_tidy$QNumber[i] == "7." | 
      CTQ_tidy$QNumber[i] == "13"|
      CTQ_tidy$QNumber[i] == "19"| 
      CTQ_tidy$QNumber[i] == "26" |
      CTQ_tidy$QNumber[i] == "28"){
    
    # Look for the responses, and amend with the new column 
    
    # Response is 1
    
      if (CTQ_tidy$Response[i] == "1"){
      
      new_col[[i]] <- 5
      }
    
    # Response is 2
    
    if (CTQ_tidy$Response[i] == "2"){
      
      new_col[[i]] <- 4
    }
    
    # Response is 3
    
    if (CTQ_tidy$Response[i] == "3"){
      
      new_col[[i]] <- 3
    }
    
    # Response is 4
    
    if (CTQ_tidy$Response[i] == "4"){
      
      new_col[[i]] <- 2
    }
    
    # Response is 5
    
    if (CTQ_tidy$Response[i] == "5"){
      
      new_col[[i]] <- 1
    }
    
  }
  
  else{
    
    new_col[[i]] <- CTQ_tidy$Response[i]
    
  }
  
}

CTQ_tidy <- CTQ_tidy %>%
  mutate(scored = as.numeric(new_col))

CTQ_tidy


# Calculated score for each participant

CTQ_calc <- CTQ_tidy %>% 
  group_by(ID) %>%
  summarise(CTQ_Score = sum(scored))

CTQ_calc

### Scoring CTQ subcales

# Emotional abuse subscale 

CTQ_EmotionalAbuse <- CTQ_tidy %>% 
  filter(QNumber == "3." |
         QNumber == "8." |
          QNumber == "14" |
           QNumber == "18" |
           QNumber == "25") %>%
  group_by(ID) %>%
  summarise(CTQ_EA_Score = sum(scored))


# Physical abuse subscale 

CTQ_PhysicalAbuse <- CTQ_tidy %>% 
  filter(QNumber == "9." |
           QNumber == "11" |
           QNumber == "12" |
           QNumber == "15" |
           QNumber == "17") %>%
  group_by(ID) %>%
  summarise(CTQ_PA_Score = sum(scored))


# Sexual abuse subscale

CTQ_SexualAbuse <- CTQ_tidy %>% 
  filter(QNumber == "20" |
           QNumber == "21" |
           QNumber == "23" |
           QNumber == "24" |
           QNumber == "27") %>%
  group_by(ID) %>%
  summarise(CTQ_SA_Score = sum(scored))


# Emotional neglect subscale

CTQ_EmotionalNeglect <- CTQ_tidy %>% 
  filter(QNumber == "5." |
           QNumber == "7." |
           QNumber == "13" |
           QNumber == "19" |
           QNumber == "28") %>%
  group_by(ID) %>%
  summarise(CTQ_EN_Score = sum(scored))


# Physical neglect subscale 

CTQ_PhysicalNeglect <- CTQ_tidy %>% 
  filter(QNumber == "1." |
           QNumber == "2." |
           QNumber == "4." |
           QNumber == "6." |
           QNumber == "26") %>%
  group_by(ID) %>%
  summarise(CTQ_PN_Score = sum(scored))


## Joining datasets together ----

Primary_data <- full_join(CTQ_calc, BDI_calc,
                          by = "ID")

Primary_data <- full_join(Primary_data,                          
                          CTQ_PhysicalAbuse,
                          by = "ID")

Primary_data <- full_join(Primary_data,                          
                          CTQ_EmotionalAbuse,
                          by = "ID")

Primary_data <- full_join(Primary_data,                          
                          CTQ_PhysicalNeglect,
                          by = "ID")

Primary_data <- full_join(Primary_data,                          
                          CTQ_EmotionalNeglect,
                          by = "ID")

Primary_data <- full_join(Primary_data,                          
                          CTQ_SexualAbuse,
                          by = "ID")

Primary_data <- full_join(Primary_data,                          
                          GNG_calc,
                          by = "ID")

Primary_data <- full_join(Primary_data,                          
                          GNG_calc_cor,
                          by = "ID")

Primary_data <- full_join(Primary_data,                          
                          GNG_calc_incor,
                          by = "ID")

Primary_data <- full_join(Primary_data,                          
                          WordRecall_complete,
                          by = "ID")

Primary_data <- full_join(Primary_data,                          
                          Ultimatum_calc,
                          by = "ID")

#_______________________________________________________________________________


#  Summary statistics -----

SummaryStats <- Primary_data %>%
  summarise("CTQ, mean" = mean(CTQ_Score),
            "CTQ, SD" = sd(CTQ_Score),
            "CTQ: Physical Abuse subscale, mean" = mean(CTQ_PA_Score),
            "CTQ: Physical Abuse subscale, SD" = sd(CTQ_PA_Score),
            "CTQ: Emotional Abuse subscale, mean" = mean(CTQ_EA_Score),
            "CTQ: Emotional Abuse subscale, SD" = sd(CTQ_EA_Score),
            "CTQ: Physical Neglect subscale, mean" = mean(CTQ_PN_Score),
            "CTQ: Physical Neglect subscale, SD" = sd(CTQ_PN_Score),
            "CTQ: Emotional Neglect subscale, mean" = mean(CTQ_EN_Score),
            "CTQ: Emotional Neglect subscale, SD" = sd(CTQ_EN_Score),
            "CTQ: Sexual Abuse subscale, mean" = mean(CTQ_SA_Score),
            "CTQ: Sexual Abuse subscale, SD" = sd(CTQ_SA_Score),
            "BDI Score, mean" = mean(BDI_Score),
            "BDI Score, SD" = sd(BDI_Score),
            "Words correctly recalled, mean (percentage)" = mean(WordRecall_Per_Correct, na.rm = TRUE),
            "Words correctly recalled, SD (percentage)" = sd(WordRecall_Per_Correct, na.rm = TRUE), 
            "Go/No go correct responses, mean (percentage)" = mean(GNG_per_Correct),
            "Go/No go correct responses, SD (percentage)" = sd(GNG_per_Correct),
            "Go/No go reaction time to correct responses, mean" = mean(GNG_MeanRTcorrect, na.rm = TRUE),
            "Go/No go reaction time to correct responses, SD" = sd(GNG_MeanRTcorrect, na.rm = TRUE),
            "Go/No go reaction time to incorrect responses, mean" = mean(GNG_MeanRTIncorrect),
            "Go/No go reaction time to incorrect responses, SD" = sd(GNG_MeanRTIncorrect),
            "Ultimatm Task; Percentage of offers rejected, mean" = mean(Ultimatum_percent),
            "Ultimatm Task; Percentage of offers rejected, SD" = sd(Ultimatum_percent))

SummaryStats   

df <- SummaryStats

# Demographic statistics

Demographics_Stats <- Demographics %>%
  summarise("Age, mean" = mean(Age),
            "Age, SD" = sd(Age))

Demographics_Gender <- Demographics %>%
  group_by(Gender) %>%
  summarise(Percent = (((n())/18)*100), 
            Count = n())

Demographics_Stats
Demographics_Gender

# Exporting the summary statistics

write.xlsx(df,file = "PrimaryData_SummaryStats2.xlsx",colNames = TRUE)

# Visualisations ----


# CTQ visualisation 

CTQ_vis_1 <- Primary_data %>%
  dplyr::select(CTQ_PA_Score, CTQ_EA_Score, CTQ_PN_Score, CTQ_EN_Score, 
         CTQ_SA_Score)  %>% # Only selecting relevant components
  pivot_longer(., cols = c(CTQ_PA_Score, CTQ_EA_Score, CTQ_PN_Score, CTQ_EN_Score, 
                           CTQ_SA_Score), # Pivoting the data longer to plot multiple components on the x axis
               names_to = "CTQ_Subscale", 
               values_to = "Score") %>%
  ggplot(aes(x = CTQ_Subscale, y = Score)) + 
  geom_boxplot(colour = "pink") + 
  theme_minimal()

CTQ_vis_1


# BDI and CTQ visualisation

CTQ_BDI_vis <- Primary_data %>%
  ggplot(aes(x = BDI_Score, y = CTQ_Score)) + 
  geom_point(colour = "#3C5493", alpha = 4) + 
  geom_smooth(se = FALSE, colour = "#060C23") + 
  theme_minimal() + 
  labs(x = "BDI Score",
         y = "CTQ Score", 
         title = "Childhood adversity and low mood association")

CTQ_BDI_vis

# CTQ and BDI visualisation with subscales

colors <- c("Physical abuse" = "#8FBE81",
            "Emotional abuse" = "#77A1C1",
            "Physical neglect" = "#D0A875",
            "Emotional neglect" = "#C36E6E",
            "Sexual abuse" = "#DACB87")

CTQ_BDI_subscale_vis <- Primary_data %>%
  ggplot(aes(x = BDI_Score)) + 
  geom_smooth(aes(y = CTQ_PA_Score, color = "Physical abuse"), linewidth = 1.5, linetype = "dotted", se = FALSE) + 
  geom_smooth(aes(y = CTQ_EA_Score, color = "Emotional abuse"), linewidth = 1.5, linetype = "dotted",se = FALSE) + 
  geom_smooth(aes(y = CTQ_PN_Score, color = "Physical neglect"), linewidth = 1.5, linetype = "dotted",se = FALSE) + 
  geom_smooth(aes(y = CTQ_EN_Score, color = "Emotional neglect"), linewidth = 1.5, linetype = "dotted",se = FALSE) + 
  geom_smooth(aes(y = CTQ_SA_Score, color = "Sexual abuse"), linewidth = 1.5, se = FALSE) + 
  labs(x = "BDI score", 
       y = "CTQ subscale score",
       color = "Legend") + 
  scale_color_manual(values = colors) + 
  theme_minimal() + 
  theme(text = element_text(size = 15))

CTQ_BDI_subscale_vis



# BDI and cognitive measures

colors <- c("Go no go" = "#985456",
            "Ultimatum" = "#28421B",
            "Word recall" = "#3C5493")

BDI_cog_vis <- Primary_data %>%
  ggplot(aes(x = BDI_Score)) + 
  geom_smooth(aes(y = GNG_Num_Correct, color = "Go no go"), se = FALSE) + 
  geom_point(aes(y = GNG_Num_Correct, color = "Go no go")) + 
  geom_smooth(aes(y = Ultimatum_percent, color = "Ultimatum"),  se = FALSE) + 
  geom_point(aes(y = Ultimatum_percent, color = "Ultimatum")) + 
  geom_smooth(aes(y = WordRecall_Per_Correct, color = "Word recall"),  se = FALSE) +
  geom_point(aes(y = WordRecall_Per_Correct, color = "Word recall")) + 
  labs(x = "BDI score", 
       y = "Cognitive measure",
       color = "Legend", 
       title = "Cognitive measures and low mood") + 
  scale_color_manual(values = colors) + 
  theme_minimal()

BDI_cog_vis

# CTQ relationship

CTQ_cog_vis <- Primary_data %>%
  ggplot(aes(x = CTQ_Score)) + 
  geom_smooth(aes(y = GNG_Num_Correct, color = "Go no go"), se = FALSE) + 
  geom_point(aes(y = GNG_Num_Correct, color = "Go no go")) + 
  geom_smooth(aes(y = Ultimatum_percent, color = "Ultimatum"),  se = FALSE) + 
  geom_point(aes(y = Ultimatum_percent, color = "Ultimatum")) + 
  geom_smooth(aes(y = WordRecall_Per_Correct, color = "Word recall"),  se = FALSE) +
  geom_point(aes(y = WordRecall_Per_Correct, color = "Word recall")) + 
  labs(x = "CTQ score", 
       y = "Cognitive measure",
       color = "Legend", 
       title = "Cognitive measures and childhood adversity") + 
  scale_color_manual(values = colors) + 
  theme_minimal()

CTQ_cog_vis

# Go / no go and sexual assault visualisation

GNG_SAVis <- Primary_data %>%
  ggplot(aes(x = CTQ_SA_Score, y = GNG_per_Correct)) + 
  geom_point(colour = "#8F9E9B") + 
  geom_smooth(colour = "#060C23", se = FALSE) + 
  labs(x = "CTQ; Sexual Abuse subscale score",
       y = "Go / No Go; Percentage of correct responses") + 
  theme_minimal()

GNG_SAVis



# Reaction time and CTQ SA subscale

colors <- c("Physical abuse" = "#8FBE81",
            "Emotional abuse" = "#77A1C1",
            "Physical neglect" = "#D0A875",
            "Emotional neglect" = "#C36E6E",
            "Sexual abuse" = "#DACB87")


CTQ_GNGRT_subscale_vis <- Primary_data %>%
  ggplot(aes(x = GNG_MeanRTIncorrect)) + 
  geom_smooth(aes(y = CTQ_PA_Score, color = "Physical abuse"), linewidth = 1.5, linetype = "dotted", se = FALSE) + 
  geom_smooth(aes(y = CTQ_EA_Score, color = "Emotional abuse"), linewidth = 1.5, linetype = "dotted", se = FALSE) + 
  geom_smooth(aes(y = CTQ_PN_Score, color = "Physical neglect"), linewidth = 1.5, linetype = "dotted", se = FALSE) + 
  geom_smooth(aes(y = CTQ_EN_Score, color = "Emotional neglect"), linewidth = 1.5, linetype = "dotted", se = FALSE) + 
  geom_smooth(aes(y = CTQ_SA_Score, color = "Sexual abuse"), linewidth = 1.5, se = FALSE) + 
  labs(x = "Mean RT in the Go/No Go task for incorrect responses (ms)", 
       y = "CTQ subscale score",
       color = "Legend") + 
  scale_color_manual(values = colors) + 
  theme_minimal() +
  theme(text = element_text(size = 11))

CTQ_GNGRT_subscale_vis


## Word recall valences * BDI visualisation


WR_vis_data <- Primary_data %>%
  dplyr:: select(ID, WordRecall_PerCorrect_POS, WordRecall_PerCorrect_NEG,
         BDI_Score) %>%
  rename(Positive = WordRecall_PerCorrect_POS,
         Negative = WordRecall_PerCorrect_NEG) %>%
  pivot_longer(cols = c(Positive,
                        Negative),
               names_to = "WR_Valence",
               values_to = "WR_PerCorrect")

WR_vis <- ggplot(WR_vis_data, aes(x = BDI_Score, y = WR_PerCorrect,
                                 fill = WR_Valence)) + 
  geom_area() + 
  scale_fill_brewer(palette = "Accent") + 
  theme_minimal() + 
  labs(x = "Score on the BDI",
       y = "Percentage of words correctly recalled",
       fill = "Valence of words recalled") + 
  theme(text = element_text(size = 12))

WR_vis


## CTQ subscale and BDI visualisation with geom_area

CTQBDI_vis_data <- Primary_data %>%
  dplyr:: select(ID, CTQ_PA_Score, CTQ_PN_Score, CTQ_EN_Score, CTQ_EA_Score, CTQ_SA_Score,
                 BDI_Score) %>%
  rename('Physical Abuse' = CTQ_PA_Score,
         'Physical Neglect' = CTQ_PN_Score,
         'Emotional Abuse' = CTQ_EA_Score,
         'Emotional Neglect' = CTQ_EN_Score,
         'Sexual Abuse' = CTQ_SA_Score) %>%
  pivot_longer(cols = c('Physical Abuse',
                        'Physical Neglect',
                        'Emotional Abuse',
                        'Emotional Neglect',
                        'Sexual Abuse'),
               names_to = "Subscale",
               values_to = "Subscale_Score")

CTQBDI_vis <- ggplot(CTQBDI_vis_data, aes(x = BDI_Score, y = Subscale_Score,
                                  fill = Subscale)) + 
  geom_area() + 
  scale_fill_brewer(palette = "Accent") + 
  theme_minimal() + 
  labs(x = "Score on the BDI",
       y = "Score on the CTQ",
       fill = "CTQ Subscale") + 
  theme(text = element_text(size = 12))

CTQBDI_vis



#_______________________________________________________________________________

# Modelling ----

## Cog and CTQ ----

## Word recall 

Cog_CTQ_Recall <- lm(WordRecall_Per_Correct ~ CTQ_Score, 
                         data = Primary_data)

Cog_CTQ_Sub_Recall <- lm(WordRecall_Per_Correct ~ CTQ_EA_Score + 
                           CTQ_EN_Score + 
                           CTQ_PA_Score + 
                           CTQ_PN_Score+ 
                           CTQ_SA_Score, 
                         data = Primary_data)

### Summary
summary(Cog_CTQ_Recall)
summary(Cog_CTQ_Sub_Recall)

vif(Cog_CTQ_Sub_Recall)
### Emotional neglect = 5.48
### Physical abuse = 13.68
### Physical neglect = 15.94

## Removing PN from the model

Cog_CTQ_Sub_Recall_v2 <- lm(WordRecall_Per_Correct ~ CTQ_EA_Score + 
                           CTQ_EN_Score + 
                           CTQ_PA_Score + 
                           CTQ_SA_Score, 
                         data = Primary_data)

summary(Cog_CTQ_Sub_Recall_v2)

vif(Cog_CTQ_Sub_Recall_v2)
### Physical abuse = 3.902
### Emotional neglect = 5.447

## Removing EN from the model

Cog_CTQ_Sub_Recall_v3 <- lm(WordRecall_Per_Correct ~ CTQ_EA_Score + 
                              CTQ_PA_Score + 
                              CTQ_SA_Score, 
                            data = Primary_data)

summary(Cog_CTQ_Sub_Recall_v3)

vif(Cog_CTQ_Sub_Recall_v3)

### Still nothing significant, all VIF <2


## Recall looking at specific valences

# POSITIVE

CTQ_WR_pos <- lm(WordRecall_PerCorrect_POS ~ CTQ_Score,
                 data = Primary_data)

summary(CTQ_WR_pos)
# p = 0.9991, F = 1.33e-06 on 1 and 15 df, adj R = -0.0667

CTQ_WR_pos_sub <- lm(WordRecall_PerCorrect_POS ~ CTQ_EA_Score + 
                              CTQ_EN_Score + 
                              CTQ_PA_Score + 
                              CTQ_SA_Score + 
                       CTQ_PN_Score, 
                            data = Primary_data)

vif(CTQ_WR_pos_sub)
# PN = 15.94

# Removing PN

CTQ_WR_pos_sub_v2 <- lm(WordRecall_PerCorrect_POS ~ CTQ_EA_Score + 
                       CTQ_EN_Score + 
                       CTQ_PA_Score + 
                       CTQ_SA_Score,
                     data = Primary_data)

vif(CTQ_WR_pos_sub_v2)
# EN = 5.44

# Removing EN 

CTQ_WR_pos_sub_v3 <- lm(WordRecall_PerCorrect_POS ~ CTQ_EA_Score + 
                          CTQ_PA_Score + 
                          CTQ_SA_Score,
                        data = Primary_data)

vif(CTQ_WR_pos_sub_v3)
# All under 2

summary(CTQ_WR_pos_sub_v3)
# p = 0.732, F = 0.4338 on 3 and 13 DF, adj R squared = - 0.1188


# NEGATIVE

CTQ_WR_neg <- lm(WordRecall_PerCorrect_NEG ~ CTQ_Score,
                 data = Primary_data)

summary(CTQ_WR_neg)
# p = 0.1995, F = 1.801 on 1 and 15 df, adj R = 0.0477


# Removed subscales we know are too high from previous analysis

CTQ_WR_neg_sub <- lm(WordRecall_PerCorrect_NEG ~ CTQ_EA_Score + 
                       CTQ_PA_Score + 
                       CTQ_SA_Score, 
                     data = Primary_data)

vif(CTQ_WR_neg_sub)

summary(CTQ_WR_neg_sub)
# p = 0.4873, F = 0.8577 on 3 and 13, adju R squared = -0.0274



## GNG 

Cog_CTQ_GNG <- lm(GNG_per_Correct ~ CTQ_Score, 
                     data = Primary_data)

Cog_CTQ_Sub_GNG <- lm(GNG_per_Correct ~ CTQ_EA_Score + 
                           CTQ_EN_Score + 
                           CTQ_PA_Score + 
                           CTQ_PN_Score+ 
                           CTQ_SA_Score, 
                         data = Primary_data)
### Summary
summary(Cog_CTQ_GNG)
summary(Cog_CTQ_Sub_GNG)
# Sig for SA

vif(Cog_CTQ_Sub_GNG)
### PA = 9.31
### PN = 13.7

## Removing PN

Cog_CTQ_Sub_GNG_v2 <- lm(GNG_per_Correct ~ CTQ_EA_Score + 
                        CTQ_EN_Score + 
                        CTQ_PA_Score + 
                        CTQ_SA_Score, 
                      data = Primary_data)

summary(Cog_CTQ_Sub_GNG_v2)
# Sig for SA

vif(Cog_CTQ_Sub_GNG_v2)
### Now none of the subscales are significant


## Ultimatum

Cog_CTQ_Ultimatum <- lm(Ultimatum_percent ~ CTQ_Score, 
                  data = Primary_data)

Cog_CTQ_Sub_Ultimatum <- lm(Ultimatum_percent ~ CTQ_EA_Score + 
                        CTQ_EN_Score + 
                        CTQ_PA_Score + 
                        CTQ_PN_Score+ 
                        CTQ_SA_Score, 
                      data = Primary_data)
### Summary
summary(Cog_CTQ_Ultimatum)
summary(Cog_CTQ_Sub_Ultimatum)

vif(Cog_CTQ_Sub_Ultimatum)
### PN = 13.7
### PA = 9.3

## Removing PN

Cog_CTQ_Sub_Ultimatum_v2 <- lm(Ultimatum_percent ~ CTQ_EA_Score + 
                              CTQ_EN_Score + 
                              CTQ_PA_Score + 
                              CTQ_SA_Score, 
                            data = Primary_data)

summary(Cog_CTQ_Sub_Ultimatum_v2)

vif(Cog_CTQ_Sub_Ultimatum_v2)
# Nothing significant, all VIF <5

## Reaction times ----

# BDI 

BDI_GNGRT_Correct <- lm(BDI_Score ~ GNG_MeanRTcorrect, data = Primary_data)

summary(BDI_GNGRT_Correct)
# p = 0.4338, F = 0.647 on 1 and 15 df, adj R square = -0.02256


BDI_GNGRT_Incorrect <- lm(BDI_Score ~ GNG_MeanRTIncorrect, data = Primary_data)

summary(BDI_GNGRT_Incorrect)
# p = 0.1378, F = 2.44 on 1 and 16 df, adj R square = -0.07811


# CTQ

CTQ_GNGRT_Correct <- lm(CTQ_Score ~ GNG_MeanRTcorrect, data = Primary_data)

summary(CTQ_GNGRT_Correct)
# p = 0.5342, F = 0.4048 on 1 and 15 df, adj R square = -0.03864

CTQ_Sub_GNGRT_Correct <- lm(GNG_MeanRTcorrect ~ CTQ_PA_Score + 
                                CTQ_EA_Score + 
                                CTQ_PN_Score + 
                                CTQ_EN_Score + 
                                CTQ_SA_Score, data = Primary_data)

summary(CTQ_Sub_GNGRT_Correct)
# p = 0.277, F = 1.465 on 5 and 11 df, adj R squared = 0.127

vif(CTQ_Sub_GNGRT_Correct)


CTQ_Sub_GNGRT_Correct_v2 <- lm(GNG_MeanRTcorrect ~ CTQ_PA_Score + 
                              CTQ_EA_Score + 
                              CTQ_EN_Score + 
                              CTQ_SA_Score, data = Primary_data)

vif(CTQ_Sub_GNGRT_Correct_v2)

summary(CTQ_Sub_GNGRT_Correct_v2)
# p = 0.3628, F = 1.194, adj = 0.04624


CTQ_GNGRT_Incorrect <- lm(CTQ_Score ~ GNG_MeanRTIncorrect, data = Primary_data)

summary(CTQ_GNGRT_Incorrect)
# p = 0.02806, F = 5.833 on 1 and 16 df, adj R square = 0.2214


CTQ_Sub_GNGRT_Incorrect <- lm(GNG_MeanRTIncorrect ~ CTQ_PA_Score + 
                             CTQ_EA_Score + 
                             CTQ_PN_Score + 
                             CTQ_EN_Score + 
                             CTQ_SA_Score, data = Primary_data)

summary(CTQ_Sub_GNGRT_Incorrect)
# p = 0.07653, F = 2.661 on 5 and 12, adj r squared = 0.3282

vif(CTQ_Sub_GNGRT_Incorrect)

# Removing PN

CTQ_Sub_GNGRT_Incorrect_v2 <- lm(GNG_MeanRTIncorrect ~ CTQ_PA_Score + 
                                CTQ_EA_Score + 
                                CTQ_EN_Score + 
                                CTQ_SA_Score, data = Primary_data)

summary(CTQ_Sub_GNGRT_Incorrect_v2)
# p = 0.03462, F = 3.603 on 4 and 13, adj r squared = 0.3798
# p for SA = 0.00979

vif(CTQ_Sub_GNGRT_Incorrect_v2)
# All <4


# Simple with just SA

CTQ_SA_GNGRT_Incorrect <- lm(GNG_MeanRTIncorrect ~ CTQ_SA_Score,
                                data = Primary_data)

summary(CTQ_SA_GNGRT_Incorrect)
# p = 0.001209, F = 15.4 on 1 and 16 df, adj R squared = 0.4586

GNG_MeanRTIncorrect_null <- lm(GNG_MeanRTIncorrect ~ 1, 
                               data = Primary_data) # Null model

# ANOVA to determine if SA predicts mean RT for incorrect responses better
# then null model

anova(CTQ_SA_GNGRT_Incorrect, GNG_MeanRTIncorrect_null)
# p = 0.001209**, F = 15.403, RSS for first model = 0.14674, RSS for second
# model = 0.28800


## Cog and BDI ----

## Word recall 

Cog_BDI_Recall <- lm(WordRecall_Per_Correct ~ BDI_Score, 
                     data = Primary_data)


### Summary
summary(Cog_BDI_Recall)


## Valenced

## POSITIVE

BDI_WR_pos <- lm(WordRecall_PerCorrect_POS ~ BDI_Score, 
                 data = Primary_data)

summary(BDI_WR_pos)
# p-value = 0.5915, F = 0.3007 on 1 and 15 df, adju R squared = -0.0457


## NEGATIVE

BDI_WR_neg <- lm(WordRecall_PerCorrect_NEG ~ BDI_Score, 
                 data = Primary_data)

summary(BDI_WR_neg)
# p-value = 0.02866*, F = 5.858 on 1 and 15 df, adju R squared = 0.233



## GNG 

Cog_BDI_GNG <- lm(GNG_per_Correct ~ BDI_Score, 
                  data = Primary_data)
### Summary
summary(Cog_BDI_GNG)



## Ultimatum

Cog_BDI_Ultimatum <- lm(Ultimatum_percent ~ BDI_Score, 
                        data = Primary_data)
### Summary
summary(Cog_BDI_Ultimatum)


#_______________________________________________________________________________

## BDI * CTQ cog ----

Cog_GNG_null <- lm(GNG_per_Correct ~ 1,
                      data = Primary_data)

anova(Cog_GNG_null, Cog_CTQ_Sub_GNG_v2)
# Not significant


# Stepwise regression

cog_CTQ_GNG_stepwise <- step(Cog_GNG_null, scope = list (lower = Cog_GNG_null, upper = Cog_CTQ_Sub_GNG_v2), direction = "both")


summary(cog_CTQ_GNG_stepwise)
# Stepwise model now includes SA and EA
# R sqr = 0.2312, F = 2.255, p-value = 0.1392


vif(cog_CTQ_GNG_stepwise)
### SA = 1.28
### EA = 1.28


anova(Cog_GNG_null, cog_CTQ_GNG_stepwise)
# Stepwise model is not significant either


# Including BDI to see if it improves the amount of variance explained by the model

cog_CTQ_Stepwise_BDI <- lm(GNG_per_Correct ~ CTQ_SA_Score + CTQ_EA_Score + BDI_Score,
                           data = Primary_data)

summary(cog_CTQ_Stepwise_BDI)
# R sqr = 0.2357, F = 1.429, p-value = 0.2734

anova(cog_CTQ_GNG_stepwise, cog_CTQ_Stepwise_BDI)
# p = 0.7764

## Power analysis ----

x <- c(1, 2, 3, 4, 5)

# Power

power_pred <- wp.regression(n=18, p1 = x, p2 = 0, f2 = 0.2)

power_pred


?power.anova.test

# For regression with 1 participant removed 

power_pred_removed <- wp.regression(n=17, p1 = x, p2 = 0, f2 = 0.2)

power_pred_removed


# ANOVA power

# GNG RT Incorrect responses and SA subscale

summary(CTQ_SA_GNGRT_Incorrect)

summary(GNG_MeanRTIncorrect_null)

anova(CTQ_SA_GNGRT_Incorrect, GNG_MeanRTIncorrect_null)

groupRSS<- c(0.14574, 0.28800)

between.var = var(groupRSS)
within.var = 

## BDI And CTQ ----

BDI_CTQ <- lm(BDI_Score ~ CTQ_Score, data = Primary_data)

summary(BDI_CTQ)

BDI_CTQ_sub <- lm(BDI_Score ~ CTQ_PA_Score + 
                  CTQ_EA_Score + 
                  CTQ_PN_Score + 
                  CTQ_EN_Score + 
                    CTQ_SA_Score, data = Primary_data)

summary(BDI_CTQ_sub)
### Only SA is significant (SA p value = 0.00218),
### R sqr = 0.8208, F = 11, p-vaue = 0.0003777

vif(BDI_CTQ_sub)
### PN = 13.695769

## Removing PN

BDI_CTQ_sub_v2 <- lm(BDI_Score ~ CTQ_PA_Score + 
                    CTQ_EA_Score + 
                    CTQ_EN_Score + 
                    CTQ_SA_Score, data = Primary_data)

summary(BDI_CTQ_sub_v2)
### Only SA is significant (SA p value = 0.000224),
### R sqr = 0.8039, F = 13.32, p-value= 0.0001569

vif(BDI_CTQ_sub_v2)
### All variables VIF <4

BDI_CTQ_subSA_v2 <- lm(BDI_Score ~
                       CTQ_SA_Score, data = Primary_data)

summary(BDI_CTQ_subSA_v2)


## Running BDI * CTQ with outlier participant removed

Primary_data_removed <- Primary_data %>%
  filter(BDI_Score < 46)

BDI_CTQ_removed <- lm(BDI_Score ~ CTQ_Score, data = Primary_data_removed)

summary(BDI_CTQ_removed)

BDI_CTQ_sub_rem <- lm(BDI_Score ~ CTQ_PA_Score + 
                    CTQ_EA_Score + 
                    CTQ_PN_Score + 
                    CTQ_EN_Score + 
                    CTQ_SA_Score, data = Primary_data_removed)

summary(BDI_CTQ_sub_rem)

vif(BDI_CTQ_sub)

BDI_CTQ_sub_rem_v2 <- lm(BDI_Score ~ CTQ_PA_Score + 
                        CTQ_EA_Score + 
                        CTQ_EN_Score + 
                        CTQ_SA_Score, data = Primary_data_removed)

summary(BDI_CTQ_sub_rem_v2)

vif(BDI_CTQ_sub_v2)



