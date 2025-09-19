#################
### MSBA 1 HULT 2021/2022
### Created by: Team 7
### Team Names: Diksha Chandra, Saidakmal Yosinov and Maria Paula Lopez
### Case of Study: Air France Internet Marketing
### Date: 01.11.2021
### Version 1.0
################

# 4Ms Process to Analyze Air France Case
# MOTIVATION
# 1. Analyze the business success of get a booking based the variables of influence
# 2. Analyze the keywords by campaign and give a recommendation to improve the search resources
# 3. Analyze the data over the costs and returns by publisher and give recommendations

# METHOD
# 1. Create a regression model where we can analyze volume of bookings as a Boolean variable
# 2. Run a code that give us the frequency of the keywords and analyze it against the campaigns
# 3. Analyze the results per publisher, summarizing the financial statements that can help us to understand the financial flow.

# MECHANICS
# 1. MODEL
# Importing information
library(readxl)
Air_France_Case_Spreadsheet_Supplement <- read_excel("Air France Case Spreadsheet Supplement.xls", 
                                                     sheet = "DoubleClick (2)")
View(Air_France_Case_Spreadsheet_Supplement)

# Analyzing and understanding my data
summary(Air_France_Case_Spreadsheet_Supplement)

# PUBLISHERS: We have different publishers and the amount of observations for each is different
library("RColorBrewer")
Publishers_Frequency_Chart <- barplot(table(Air_France_Case_Spreadsheet_Supplement$`Publisher Name`),
                                      main = "Publishers' Frequency",
                                      xlab = "Publisher Name",
                                      ylab = "Frequency",
                                      col = brewer.pal(n = 7, name = "Spectral"))
text(Publishers_Frequency_Chart, 0, 
     labels = table(Air_France_Case_Spreadsheet_Supplement$`Publisher Name`), 
     cex=1, pos=3)

# Converting Volume of Bookings into a Boolean Variable - Our dependent variable of the model
# If the volume is higher or equal to 25, value = 1; otherwise = 0 - Because having a level of different from 0 get a 1 is going to give us high coefficients that are going to disrupt our results 
# Using ifelse, we also can use a For Loop with an If

Air_France_Case_Spreadsheet_Supplement$`Booking Success` <- ifelse(Air_France_Case_Spreadsheet_Supplement$`Total Volume of Bookings` >= 25, 1, 0)

# Selecting the variables that are important to explain our new variable Booking Success
# Using descriptive statistics to explain
# Correlations between numerical variables
Correlation_Matrix <- round(cor(Air_France_Case_Spreadsheet_Supplement[sapply(Air_France_Case_Spreadsheet_Supplement, is.numeric)]), 2)
Correlation_D.F <- as.data.frame(Correlation_Matrix)

# Filtering my matrix to get the ones that have correlation bigger than 0.5 with my Business Success Variable
library(dplyr)
Outstanding_Variables <- select(filter(Correlation_D.F, Correlation_D.F$`Booking Success` > 0.5),`Booking Success`)

# We select: Clicks, Click Charges and Impressions.
# We decide to use Impressions because as a business insight it represents the Publishers effect in our variable, without impression I cannot have clicks.
# Total Cost is related to Click Charges and Clicks so we decided no to use it.
# Total Amount it's correlated because it's the result of the bookings, so we can get the amount based on the bookings not the other way around
# Total Volume of Bookings is the variable that we're explaining with the original data - no boolean

# Standardizing our variables - why? they have some extreme values that are going to affect our results
# Creating our standardizing function
tstandard <- function(var1){
  tstandard <- (((var1 - mean(var1))/sd(var1))*10) + 50
  return (tstandard)
} #Closing tstandard UDF

# Standardizing our selected variables
Air_France_Case_Spreadsheet_Supplement$tClicks <- tstandard(Air_France_Case_Spreadsheet_Supplement$Clicks)
Air_France_Case_Spreadsheet_Supplement$tClicksCharges <- tstandard(Air_France_Case_Spreadsheet_Supplement$`Click Charges`)
Air_France_Case_Spreadsheet_Supplement$tImpressions <- tstandard(Air_France_Case_Spreadsheet_Supplement$Impressions)

# Normalizing our variables - why? to be able to compare them
# Creating our normalizing function
normal <- function(var1){
  my_normal <- (var1 - min(var1))/(max(var1)-min(var1))
  return(my_normal)
} #closing the normal UDF

# Normalizing our selected variables
Air_France_Case_Spreadsheet_Supplement$Clicks_Norm <- normal(Air_France_Case_Spreadsheet_Supplement$Clicks)
Air_France_Case_Spreadsheet_Supplement$Clicks_Charges_Norm <- normal(Air_France_Case_Spreadsheet_Supplement$`Click Charges`)
Air_France_Case_Spreadsheet_Supplement$Impressions_Norm <- normal(Air_France_Case_Spreadsheet_Supplement$Impressions)


# Based in our Publisher Frequency Chart, for us is important to stratify our data by publisher in order to analyze the information
# We want to be sure that every publisher is on the model and has a representation of its data proportionally to its size

# Stratifying my data frame by publisher - filter() function
Google_Global_Data <- filter(Air_France_Case_Spreadsheet_Supplement,
                             Air_France_Case_Spreadsheet_Supplement$`Publisher Name` == "Google - Global")
Google_US_Data <- filter(Air_France_Case_Spreadsheet_Supplement,
                         Air_France_Case_Spreadsheet_Supplement$`Publisher Name` == "Google - US")
MSN_Global_Data <- filter(Air_France_Case_Spreadsheet_Supplement,
                          Air_France_Case_Spreadsheet_Supplement$`Publisher Name` == "MSN - Global")
MSN_US_Data <- filter(Air_France_Case_Spreadsheet_Supplement,
                      Air_France_Case_Spreadsheet_Supplement$`Publisher Name` == "MSN - US")
Overture_Global_Data <- filter(Air_France_Case_Spreadsheet_Supplement,
                               Air_France_Case_Spreadsheet_Supplement$`Publisher Name` == "Overture - Global")
Overture_US_Data <- filter(Air_France_Case_Spreadsheet_Supplement,
                           Air_France_Case_Spreadsheet_Supplement$`Publisher Name` == "Overture - US")
Yahoo_US_Data <- filter(Air_France_Case_Spreadsheet_Supplement,
                        Air_France_Case_Spreadsheet_Supplement$`Publisher Name` == "Yahoo - US")

# Clustering my data - Random Index per group of publisher
index_GG <- sample(1:nrow(Google_Global_Data), size = 0.8 * nrow(Google_Global_Data))
index_GUs <- sample(1:nrow(Google_US_Data), size = 0.8 * nrow(Google_US_Data))
index_MG <- sample(1:nrow(MSN_Global_Data), size = 0.8 * nrow(MSN_Global_Data))
index_MUs <- sample(1:nrow(MSN_US_Data), size = 0.8 * nrow(MSN_US_Data))
index_OG <- sample(1:nrow(Overture_Global_Data), size = 0.8 * nrow(Overture_Global_Data))
index_OUs <- sample(1:nrow(Overture_US_Data), size = 0.8 * nrow(Overture_US_Data))
index_YUs <- sample(1:nrow(Yahoo_US_Data), size = 0.8 * nrow(Yahoo_US_Data))

# Creating training and testing data frames - rbind function to combine the data frames again
training_sample <- rbind(Google_Global_Data[index_GG,],Google_US_Data[index_GUs,],MSN_Global_Data[index_MG,],MSN_US_Data[index_MUs,],Overture_Global_Data[index_OG,],Overture_US_Data[index_OUs,],Yahoo_US_Data[index_YUs,])
testing_sample <- rbind(Google_Global_Data[-index_GG,],Google_US_Data[-index_GUs,],MSN_Global_Data[-index_MG,],MSN_US_Data[-index_MUs,],Overture_Global_Data[-index_OG,],Overture_US_Data[-index_OUs,],Yahoo_US_Data[-index_YUs,])

# Creating the model with the Outstanding Variables and Impressions - with units
Booking_Model <- glm(`Booking Success` ~ Clicks + `Click Charges` + Impressions , data = training_sample)
summary(Booking_Model)

# Cleaning the model with the pvalue - with units
Booking_Model_Better <- glm(`Booking Success` ~ Clicks + Impressions , data = training_sample)
summary(Booking_Model_Better)

exp(Booking_Model_Better$coefficients[2])-1
exp(Booking_Model_Better$coefficients[3])-1

# Creating the model with the standardizing variables - with units
Booking_tModel <- glm(`Booking Success` ~ tClicks + tImpressions , data = training_sample)
summary(Booking_tModel)

exp(Booking_tModel$coefficients[2])-1
exp(Booking_tModel$coefficients[3])-1

plot(Booking_tModel$residuals)

# Our results are really small due to the extreme values of our variables, but the standardized model give us the opportunity to explain them a little bit

# Creating the model with the Outstanding Variables and Impressions - unitless
Booking_Model_Norm <- glm(`Booking Success` ~ Clicks_Norm + Impressions_Norm , data = training_sample)
summary(Booking_Model_Norm)

# Testing our model and creating a Confusion Matrix
library(caret)
Predicted_Testing <- predict(Booking_tModel, testing_sample, type = "response")
confusionMatrix(data = as.factor(as.numeric(Predicted_Testing > 0.5)),
                reference = as.factor(as.numeric(testing_sample$`Booking Success`)))

# AUC ROC
library(ROCR)
Booking_Prediction_Curve <- Predicted_Testing
Booking_Pred_Val <- prediction(Booking_Prediction_Curve, testing_sample$`Booking Success`)
Booking_Perf <- performance(Booking_Pred_Val, "tpr", "fpr")
plot(Booking_Perf)


# 2. KEYWORDS
# Let's rename our data frame for working convenience 
my_air_france <- Air_France_Case_Spreadsheet_Supplement
# Since we are going to work with Keywords, let's clean our observations for a variable Keyword from special characters, punctuation, double spaces between words, single spaces at the beginning and end of each observation
my_air_france$Keyword <- gsub("[^[:alnum:]]", " ", str_trim(my_air_france$Keyword))
my_air_france$Keyword <- gsub("\\s+", " ", str_trim(my_air_france$Keyword))
library(stringr)
library(ggplot2)
library(lattice)
# install.packages("wordcloud")
library(wordcloud)

my_air_france %>% group_by(`Publisher Name`) %>% count() %>% arrange(desc(n))
my_air_france %>% group_by(`Publisher Name`) %>% count() %>% ggplot() + geom_bar(aes(`Publisher Name`,n), stat = 'identity')

# UDPipe-R package provides language-agnostic tokenization, tagging, lemmatization and dependency parsing of raw text, which is an essential part in natural language processing.
# install.packages("udpipe")
library(udpipe)

# Udpipe Package provides pretrained language models for respective languages (not programming - but spoken) and we can download the required model using udpipe_download_model()
model <- udpipe_download_model(language = "english")
View(model)

# getting the path of the model
model[,2]
udmodel_english <- udpipe_load_model(file = "D:/Documentos/HULT/UNIVERSITY/DATA SCIENCE - R/TEAM DOCUMENTS/AIR FRANCE/english-ewt-ud-2.5-191206.udpipe")

# This is the very first function that we'd use in udpipe to get started with our Text Analysis journey. udpipe_annotate() takes the language model and annoates the given text data
s <- udpipe_annotate(udmodel_english, my_air_france$Keyword)
x <- data.frame(s)

# Getting the most frequent keywords
sentence_frequency <- x %>% count(sentence, sort = TRUE)

# Illustrating bar chart with added parameters
barchart(sentence ~ n, data = head(sentence_frequency, 20),
         main = "The most frequent keywords",
         xlab = "Frequency",
         names.arg = sentence_frequency$sentence,
         col = "#BA5564",
         horiz = TRUE)

# Plotting Part-of-speech tags from the given keywords
stats <- txt_freq(x$upos)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = stats, col = "green3", 
         main = "UPOS (Universal Parts of Speech)\n frequency of occurrence", 
         xlab = "Frequency")

#Since nouns are the most annotated part of speech, let's understand the most common words of nouns used in Keywords.
stats <- subset(x, upos %in% "NOUN")
stats <- txt_freq(x = stats$lemma)
stats$key <- factor(stats$key, levels = rev(stats$key))
barchart(key ~ freq, data = head(stats, 10), col = "light blue2", main = "Most occurring nouns", xlab = "Frequency")


## Using a sequence of POS tags, let's bring out top phrases (noun phrases / verb phrases)
x$phrase_tag <- as_phrasemachine(x$upos, type = "upos")
stats <- keywords_phrases(x = x$phrase_tag, term = tolower(x$token), 
                          pattern = "(A|N)*N(P+D*(A|N)*N)*", 
                          is_regex = TRUE, detailed = FALSE)
stats <- subset(stats, ngram > 1 & freq > 3)
stats$key <- factor(stats$keyword, levels = rev(stats$keyword))
barchart(key ~ freq, data = head(stats, 20), col = "magenta", 
         main = "Keywords - simple noun phrases", xlab = "Frequency")

# Let's visualize our findings in wordcloud() library.
wordcloud(words = stats$key, freq = stats$freq, min.freq = 10, max.words = 50,
          random.order = FALSE, scale = c(2, .1),rot.per=.5,vfont=c("sans serif","bold"),colors=brewer.pal(12,"Paired"),)

# Since Google - US has the highest number of observations, let's check the most occuring keywords for Google - US Publisher
my_air_france_Google_US <- my_air_france %>% filter(`Publisher Name` == "Google - US")
s_Google_US <- udpipe_annotate(udmodel_english, my_air_france_Google_US$Keyword)
x_Google_US <- data.frame(s_Google_US)

# Getting the most frequent keywords
sentence_frequency_Google_US <- x_Google_US %>% count(sentence, sort = TRUE)

# Illustrating bar chart with added parameters
barchart(sentence ~ n, data = head(sentence_frequency_Google_US, 20),
         main = "The most frequent keywords",
         xlab = "Frequency",
         names.arg = sentence_frequency_Google_US$sentence,
         col = "#BA5564",
         horiz = TRUE)


# 3. FINANCIAL ANALYSIS
#profit calculation by assuming amount as the revenue generated
Air_France_Case_Spreadsheet_Supplement$Profits <- Air_France_Case_Spreadsheet_Supplement$Amount - Air_France_Case_Spreadsheet_Supplement$`Total Cost`
summary(Air_France_Case_Spreadsheet_Supplement$Profits)
#
#plotting average cost per click and average profit per publisher
avg_publisher <- tapply(Air_France_Case_Spreadsheet_Supplement$Profits, Air_France_Case_Spreadsheet_Supplement$'Publisher Name', FUN = mean)
avg_publisher <- cbind.data.frame(avg_publisher, unlist(tapply(Air_France_Case_Spreadsheet_Supplement$`Total Cost`, Air_France_Case_Spreadsheet_Supplement$`Publisher Name`, FUN = mean)))
names(avg_publisher) <-c("Average Profit", "Average Cost per Click")
label_names <- row.names(avg_publisher)

#install.packages("plotly")
library(plotly)
myavg_pub <- ggplot()+
  geom_point(data = avg_publisher, aes(x = `Average Profit`, y = `Average Cost per Click` , color=label_names, size = 7))
ggplotly(myavg_pub)

##plotting Profit vs impressions
im_profit <- tapply(Air_France_Case_Spreadsheet_Supplement$Profits, Air_France_Case_Spreadsheet_Supplement$'Publisher Name', FUN = mean)
im_profit <- cbind.data.frame(im_profit, unlist(tapply(Air_France_Case_Spreadsheet_Supplement$Impressions, Air_France_Case_Spreadsheet_Supplement$`Publisher Name`, FUN = mean)))
names(im_profit) <- c("Average Profit","Impressions")

im_profit_plot <- ggplot() +
  geom_point(data = Air_France_Case_Spreadsheet_Supplement, aes(x = `Impressions`, y = `Average Profit`))
my_label <- row.names(im_profit)
improfit_avg <- ggplot() +
  geom_point(data = im_profit, aes(x = `Impressions`, y = `Average Profit` ,color = my_label),size = 7)
ggplotly(improfit_avg) 

#Calculating Revenue/ Clicks  to compare it with Average cost per click
#This will show the efficiency of each Search engine with respect to
#clicks

Air_France_Case_Spreadsheet_Supplement$`Avg(Revenue/Click)` <- Air_France_Case_Spreadsheet_Supplement$Amount/Air_France_Case_Spreadsheet_Supplement$Clicks
for(x in 1:nrow(Air_France_Case_Spreadsheet_Supplement)){
  if(Air_France_Case_Spreadsheet_Supplement$Clicks[x] == 0){
    Air_France_Case_Spreadsheet_Supplement$`Avg(Revenue/Click)`[x] <- c(0)
  }
}
summary(Air_France_Case_Spreadsheet_Supplement$`Avg(Revenue/Click)`)

#Comparing profit with cost per click for every search engine
#Average cost per click for Yahoo - US (publisher)
rc_avgcost <- tapply(Air_France_Case_Spreadsheet_Supplement$`Avg(Revenue/Click)`, Air_France_Case_Spreadsheet_Supplement$`Publisher Name`, FUN=mean)
rc_avgcost <- cbind.data.frame(rc_avgcost, unlist(tapply(Air_France_Case_Spreadsheet_Supplement$`Avg. Cost per Click`, Air_France_Case_Spreadsheet_Supplement$`Publisher Name`, FUN=mean)))
names(rc_avgcost) <- c("Avg(Revenue/Click)", "Average Cost Per Clicks") 

##Plotting Average Revenue/Clicks vs average cost per click
af_label <- ggplot() + 
  geom_point(data = Air_France_Case_Spreadsheet_Supplement, aes(x = `Avg(Revenue/Click)`, y = `Average Cost Per Clicks`))
af_label <- row.names(rc_avgcost)
rc_avgcost_plt <- ggplot() +
  geom_point(data = rc_avgcost, aes(x =`Avg(Revenue/Click)`, y =`Average Cost Per Clicks`, color = af_label), size = 7)
ggplotly(rc_avgcost_plt)

### To know if Kayak is a viable option as part of 
### the SEM strategy of Air France, we will compare the performance of each publisher 
### to Kayak's performance using return on advertising (ROA) spend.

# Creating another variable to compute for the ROA of all observations
Air_France_Case_Spreadsheet_Supplement$RoA <- Air_France_Case_Spreadsheet_Supplement$Amount / Air_France_Case_Spreadsheet_Supplement$`Total Cost`

# Remove an observation with 0 total cost to avoid an error
Air_France_Case_Spreadsheet_Supplement <- Air_France_Case_Spreadsheet_Supplement[Air_France_Case_Spreadsheet_Supplement$`Total Cost` != 0, ]

# Creating a pivot table to analyze the ROA performance of each advertising platform
pub_roa <- Air_France_Case_Spreadsheet_Supplement%>%group_by(`Publisher Name`)%>% summarize(avg_roa = mean(RoA))
#View(pub_roa)

# Putting Kayak's advertising performance in a dataframe
df_kay <- data.frame("Clicks" = 2839, "Media Cost" = 3567, "Total Bookings" = 208, "Avg Ticket" = 1124, "Total Revenue" = 233694, "Net Revenue" = 230127)

# Computing for ROA of Kayak based on the created dataframe
roa_kayak <- df_kay[1,5] / df_kay[1,2]

# Creating a dataframe for kayak_ROA
roa_df_kay <- data.frame("Kayak", roa_kayak)

# Adding variable names to the kayak_ROA_df dataframe
names(roa_df_kay) <- c("Publishers", "RoA")

# Adding names to the publishers_ROA_df dataframe
names(pub_roa) <- c("Publishers", "RoA")

# Combining kayak dataframe to publishers dataframe
df_kayak <- rbind(pub_roa, roa_df_kay)

# Rounding ROA variable up to 2 decimal places
df_kayak[,-1] <- round(df_kayak[,-1],2)

#View(df_kayak)
# Visualizing the ROA comparisons between other publishers and creating base plot
ggplot(df_kayak, aes(Publishers, RoA)) + 
  geom_bar(position = "dodge", stat = "identity", fill = "orange", color = "white") +
  geom_text(aes(label=RoA),position=position_dodge(width=1.5), vjust=-0.25) +
  ggtitle("Publisher ROAs") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.subtitle = element_text(hjust = 0.5))

Air_France_Case_Spreadsheet_Supplement$Probability <- ( Air_France_Case_Spreadsheet_Supplement$`Trans. Conv. %` * Air_France_Case_Spreadsheet_Supplement$`Engine Click Thru %`) /100
Air_France_Case_Spreadsheet_Supplement_plot <- Air_France_Case_Spreadsheet_Supplement %>% group_by(`Publisher Name`) %>% summarize(
  total_records = n(),
  total_amount = sum(`Total Cost`),
  avg_cpc = mean(`Avg. Cost per Click`),
  avg_prob = mean(Probability),
  avg_roa = mean(RoA)
)
summary(Air_France_Case_Spreadsheet_Supplement_plot)

prob_plt <- plot_ly(Air_France_Case_Spreadsheet_Supplement_plot, x = ~avg_prob, y = ~avg_roa,
                    textposition = "auto",
                    type = 'scatter', 
                    mode = 'markers', 
                    size = ~avg_cpc, 
                    color = ~`Publisher Name`, 
                    colors = 'Paired',
                    marker = list(opacity = 0.5, sizemode = 'diameter')) %>%
  layout(title = 'Publisher Strategy',
         xaxis = list(title = "Booking Probability", showgrid = TRUE),
         yaxis = list(title = "Average RoA", showgrid = TRUE),
         showlegend = TRUE)
prob_plt
summary(Air_France_Case_Spreadsheet_Supplement_plot)

# MESSAGE
# As we pointed in our motivation, improving the performance of booking success trough your marketing strategies can be achieved: 
# 1. Generating internal KPIs to measure the performance of clicks and impressions per publisher and campaign.
# 2. Focusing on develop strategies to create special keywords per engine and per campaign. 
# 3. Creating alliances and new campaigns with engine platforms similar to Kayak
# 4. Reevaluating the alliances with publishers with a high cost per click and a low return of revenue such as MSN US and Overture Global and US.
# 5. Reinforcing the performance and campaigns with engines with a good Return over Advertisement and a high probability of booking success as Yahoo and MSN Global. 
