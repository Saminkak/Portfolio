library(tidyverse)
library(tokenizers)
library(quanteda)
library(quanteda.textplots)
library(devtools)
library(quanteda.corpora)
library(irr)
library(quanteda.textmodels)


# reading csv
maindata_unfiltered <- read_csv("fbpac-ads-en-US.csv")

# filter data frame to include only rows where political > not_political and with empty images
maindata <- maindata_unfiltered[maindata_unfiltered$political > maindata_unfiltered$not_political & maindata_unfiltered$images == "{}",  ]

# Delete prominent HTML code
maindata$message <- gsub("v_12zlaim5ia|x_d7cjj5-cm|e_1bjy3vuv88|s_sotqp4nrd|h_16pturn0un|f_ma6suteoj|#f|g_mh3mdxv3k|e_1bjy3vuv8|<|span|class|=|>|b_pc9c3vlbv|h_1o3zymnu7_|a__8pnribg|c_1hvunkzkv3|r_114lsp76q3|i_xzmo-zqj3", "", maindata$message)

check <- grepl("v_12zlaim5ia|x_d7cjj5-cm|e_1bjy3vuv88|s_sotqp4nrd|h_16pturn0un|f_ma6suteoj|#f|g_mh3mdxv3k|e_1bjy3vuv8|<|span|class|=|>|b_pc9c3vlbv|h_1o3zymnu7_|a__8pnribg|c_1hvunkzkv3|r_114lsp76q3|i_xzmo-zqj3", maindata$message)

is_any_true <- any(check)

print(is_any_true)

### HANDCODING DATASET 


# Random sample for hand coding
#set.seed(92073)
#prehandcoding <- maindata[sample(1:nrow(maindata),57),]

# create a new data frame of all the ids
#prehandcoding_ids <- handcoding[, c("id", "message", "title")]

# export handcode_ids to a CSV file for jack
#write.csv(prehandcoding_ids, file = "prehandcoding_ids.csv", row.names = FALSE)

# import handcoded csv for jack
handcoded_import <- read.csv("handcoded_ids.csv")

# Filter rows where neither partisan_j nor type_j are N/A
handcoded_import_filtered <- handcoded_import[!is.na(handcoded_import$partisan_j) & !is.na(handcoded_import$type_j), ]

# Merge CSV of jack and saminka hand coding results


handcoded <- merge( handcoded_import_filtered,  maindata, by = c("id"))

#handcoded$rep_likelihood_hc = handcoded$sam_dummy/handcoded$jack_dummy

# Remove "message.y" column and rename "message.x" to "message"
handcoded <- handcoded %>%
  select(-message.y) %>%
  rename(message = message.x)

# Confusion Matrix 
table(handcoded$partisan_j, handcoded$partisan_s)
table(handcoded$type_j, handcoded$type_s)

# Create Sam/Jack handcoding variable
# Select only partisan columns
partisan_cols <- c("partisan_j", "partisan_s")
partisan_df <- handcoded[, partisan_cols]

# Create a new column with proportions
handcoded$partisan_proportion <- apply(partisan_df, 1, function(row) {
  prop <- sum(row == row[1]) / length(row)
  ifelse(is.na(prop), 0, prop)
})

type_cols <- c("type_j", "type_s")
type_df <- handcoded[, type_cols]

handcoded$type_proportion <- apply(type_df, 1, function(row) {
  prop <- sum(row == row[1]) / length(row)
  ifelse(is.na(prop), 0, prop)
})

# Create dummys
#handcoded$dem_dummy_j <- ifelse(handcoded$partisan_j == 1, 1, 0)

#handcoded$rep_dummy_j <- ifelse(handcoded$partisan_j == 2, 1, 0)

#handcoded$not_partisan_j <- ifelse(handcoded$partisan_j == 3, 1, 0)

#handcoded$fundraising_j <- ifelse(handcoded$type_j == 0, 1, 0)

#handcoded$opposition_j <- ifelse(handcoded$type_j == 1, 1, 0)

#handcoded$support_j <- ifelse(handcoded$type_j == 2, 1, 0)

#handcoded$issue_j <- ifelse(handcoded$type_j == 3, 1, 0)

#handcoded$other_j <- ifelse(handcoded$type_j == 4, 1, 0)

# Krippendorff's alpha
# The amount of observed disagreement over the amount of expected disagreement given 
# that documents were randomly assigned:
# Its values range from -1 to 1, with 1 representing unanimous agreement between 
# the coders, 0 indicating they are guessing randomly, and negative values suggesting 
# the coder are systematically disagreeing.

kripp.alpha(t(handcoded[,c("partisan_j", "partisan_s")]))

kripp.alpha(t(handcoded[,c("type_j", "type_s")]))



# Preprocess the data
corpus_metadata <- corpus(maindata, text_field = "message")

# Create a document feature matrix (dfm)
# Some common pre-processing
toks <- tokens(corpus_metadata, remove_punct = TRUE, remove_numbers=TRUE)
toks <- tokens_wordstem(toks)
toks <- tokens_select(toks,  stopwords("en"), selection = "remove")
dfm <- dfm(toks)


# Creating training set corpus
corpus_handcoding <- corpus(handcoded, text_field = "message")

# Create a document feature matrix (dfm)
# Some common pre-processing
toks <- tokens(corpus_handcoding, remove_punct = TRUE, remove_numbers=TRUE)
toks <- tokens_wordstem(toks)
toks <- tokens_select(toks,  stopwords("en"), selection = "remove")
dfm2 <- dfm(toks)

# Split into training and validation
set.seed(92073)
docvars(corpus_handcoding, "id") <- 1:ndoc(corpus_handcoding)
alldocs <- 1:ndoc(corpus_handcoding)
training <- sample(alldocs, round(length(alldocs)*.75))
validation <- alldocs[!alldocs%in%training]

# Create separate dfm's for each
dfmat_train <- dfm_subset(dfm2, docvars(corpus_handcoding, "id")%in%training)
dfmat_val <- dfm_subset(dfm2, docvars(corpus_handcoding, "id")%in%validation)

# Naive Bayes
# Train
type_nb <- textmodel_nb(dfmat_train, docvars(dfmat_train, "type_j"))
summary(type_nb)

partisan_nb <- textmodel_nb(dfmat_train, docvars(dfmat_train, "partisan_s"))
summary(partisan_nb)

# Probability of a word given a category
coef_nb_type <- coef(type_nb)
head(coef_nb_type)

coef_nb_partisan <- coef(partisan_nb)
head(coef_nb_partisan)

# Words associated with opposition type:
sort(coef_nb_type[,2]/coef_nb_type[,3], decreasing=T)[1:20]
# Words not associated with opposition type:
sort(coef_nb_type[,2]/coef_nb_type[,3], decreasing=F)[1:20]

# Words associated with Democratic targeted ads:
sort(coef_nb_partisan[,1]/coef_nb_partisan[,2], decreasing=T)[1:20]
# Words not associated with Democratic targeted ads:
sort(coef_nb_partisan[,1]/coef_nb_partisan[,2], decreasing=F)[1:20]

# How well does it do in sample?
# Check Types
# Check Sams

predict.train <- predict(type_nb, dfmat_train)

tab_train_type_s <- table(docvars(dfmat_train, "type_s"), predict.train)
tab_train_type_s

# precision
diag(tab_train_type_s)/colSums(tab_train_type_s)
# recall
diag(tab_train_type_s)/rowSums(tab_train_type_s)

predict.train <- predict(type_nb, dfmat_train)

#Check jacks

tab_train_type_j <- table(docvars(dfmat_train, "type_j"), predict.train)
tab_train_type_j

# precision
diag(tab_train_type_j)/colSums(tab_train_type_s)
# recall
diag(tab_train_type_j)/rowSums(tab_train_type_s)

# Check partisans
# Check Sams

predict.train <- predict(partisan_nb, dfmat_train)

tab_train_partisan_s <- table(docvars(dfmat_train, "partisan_s"), predict.train)
tab_train_partisan_s

# precision
diag(tab_train_partisan_s)/colSums(tab_train_partisan_s)
# recall
diag(tab_train_partisan_s)/rowSums(tab_train_partisan_s)

predict.train <- predict(partisan_nb, dfmat_train)

# Check Jacks

predict.train <- predict(partisan_nb, dfmat_train)

tab_train_partisan_j <- table(docvars(dfmat_train, "partisan_j"), predict.train)
tab_train_partisan_j

# precision
diag(tab_train_partisan_j)/colSums(tab_train_partisan_j)
# recall
diag(tab_train_partisan_j)/rowSums(tab_train_partisan_j)

predict.train <- predict(partisan_nb, dfmat_train)



### VISUALIZATIONS

library(ggplot2)


# Convert "datetime" column to date format
maindata_unfiltered$date <- as.Date(maindata_unfiltered$created_at)

counts <- maindata_unfiltered %>%
  group_by(date) %>%
  summarise(count = n())


ggplot(counts, aes(x = date, y = count)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 3) +
  labs(x = "Date", y = "Count", title = "Posts per Day") +
  theme_minimal()

# Count the occurrences of each advertiser
counts <- maindata %>%
  filter(!is.na(advertiser)) %>%
  group_by(advertiser) %>%
  summarise(count = n()) %>%
  top_n(10, count) %>%
  arrange(desc(count))

# Create a bar plot of the top 10 advertisers
ggplot(counts, aes(x = reorder(advertiser, -count), y = count)) +
  geom_bar(stat = "identity", fill = "steelblue", color = "white") +
  labs(x = "Advertiser", y = "Count", title = "Top 10 Political Advertisers") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Calculate average targetness per post per day
averages <- maindata_unfiltered %>%
  group_by(date) %>%
  summarise(average_targetedness = mean(targetedness))

# Create a line plot
ggplot(averages, aes(x = date, y = average_targetedness)) +
  geom_line(color = "steelblue", size = 1) +
  geom_point(color = "steelblue", size = 3) +
  labs(x = "Date", y = "Average Targetedness", title = "Average Targedtness per Post per Day") +
  theme_minimal()

# Word Cloud

# Packages
install.packages("wordcloud")
library(wordcloud)

# Get the term frequency
term_freq <- colSums(dfm)

# Generate the word cloud
wordcloud(names(term_freq), term_freq, random.order = FALSE)





