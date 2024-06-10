
#Code for the decision trees

df <- df %>%
  select(bill_id, year, cycle, Attribute, Value,Top_Topic,
         Top_Topic_Weight, `sum(Vote)_Democrat`, `sum(Vote)_Republican`,
         `n()_Democrat`, `n()_Republican`, Maj.Vote_Democrat, Maj.Vote_Republican,
         bonica_rid, Sponsor, Cosponsor, Vote, Party, State, Seat,
         Incumbent_Status, Gender, N_Givers, Primary_Winner,
         General_Election_Vote_Percent, District_Pres_VS, Parties,
         Vote.Against.Majority, ticker, Total_Donations, Sector, Industry,
         Total_Democrat, Total_Republican, Number_of_Contributions,
         Total_Donation, Democrat_Republican_Ratio)

#[1] 7,231,383     37
dim(df)

df <- df %>% filter(Attribute == Top_Topic)

#[1] 1,400,898     37
dim(df)

#####Data Preparation##################################################

df %>% head()


#Energy Decision tree (Change the topic and the sector to create each tree and lasso regression)
topic <- c("tw.energy")
top.weight <- 0.25
sector <- c("Energy")

filtered.df <- df %>%
  filter(
    Top_Topic %in% topic,
    Top_Topic_Weight >=top.weight,Sector %in% sector,
    Total_Donation > 0) %>% group_by(
      bill_id,bonica_rid,Vote,Vote.Against.Majority,Industry,Incumbent_Status) %>% summarize(
        TD = log(sum(Total_Donation)),
        bill.year = max(year),
        sponsor.cosponsor = as.integer(any(Sponsor == 1 | Cosponsor == 1)),
        political.party = max(Party),
        seat = max(Seat) == "federal:senate",
        gender = max(Gender),
        n.givers = max(N_Givers),
        general.election.vote.percent = max(General_Election_Vote_Percent),
        mean.democrat.republican.ratio.for.companies = mean(Democrat_Republican_Ratio)) %>% ungroup()

filtered.df <- filtered.df %>% mutate(
  Vote = as.factor(Vote),
  Vote.Against.Majority = as.factor(Vote.Against.Majority),
  Industry = as.factor(Industry),
  Incumbent_Status = as.factor(Incumbent_Status),
  bill.year = as.factor(bill.year),
  political.party = as.factor(political.party),
  gender = as.factor(gender),
  sponsor.cosponsor = as.factor(sponsor.cosponsor)
)

# Create the model matrix excluding the response variable and other non-predictors
x <- model.matrix(~ . -1 - Vote.Against.Majority - bill_id - bonica_rid - bill.year,data = filtered.df)  # -1 to omit intercept

# Ensure the response variable is also appropriately factored and numeric
y <- as.numeric(as.factor(filtered.df$Vote.Against.Majority)) - 1  # subtract 1 to make it 0-based for glmnet

# Checking for NAs and handle them if necessary
complete_cases <- complete.cases(x, y)
x <- x[complete_cases, ]
y <- y[complete_cases]
###Modeling#############################################

# Lasso regression

lasso_model <- glmnet(x, y, family = "binomial", alpha = 1)


dev.new()
plot(lasso_model, xvar = "lambda", label = TRUE)

# Memory Usage Reached! Following code won't work
cv_lasso <- cv.glmnet(x, y, family = "binomial", alpha = 1)
plot(cv_lasso)
best_lambda_lasso <- cv_lasso$lambda.min

# Decision Tree

filtered.df$Vote.Against.Majority<-factor(filtered.df$Vote.Against.Majority)

sample.data<-sample.int(nrow(filtered.df), floor(0.8*nrow(filtered.df)),
                        replace = F)
train<-filtered.df[sample.data, ]
test<-filtered.df[-sample.data, ]

y.test<-test[,"Vote.Against.Majority"]

train<-filtered.df[sample.data, ] %>% select(-bonica_rid,-bill_id)
test<-filtered.df[-sample.data,] %>% select(-bonica_rid,-bill_id)

library(rpart)
library(rpart.plot)

tree <- rpart(Vote.Against.Majority ~ ., data = train, method = "class")
rpart.plot(tree, main = "Decision Tree for Energy Sector")

topic <- c("tw.healthcare")
top.weight <- 0.25
sector <- c("Healthcare")

filtered.df <- df %>%
  filter(
    Top_Topic %in% topic,
    Top_Topic_Weight >=top.weight,Sector %in% sector,
    Total_Donation > 0) %>% group_by(
      bill_id,bonica_rid,Vote,Vote.Against.Majority,Industry,Incumbent_Status) %>% summarize(
        TD = log(sum(Total_Donation)),
        bill.year = max(year),
        sponsor.cosponsor = as.integer(any(Sponsor == 1 | Cosponsor == 1)),
        political.party = max(Party),
        seat = max(Seat) == "federal:senate",
        gender = max(Gender),
        n.givers = max(N_Givers),
        general.election.vote.percent = max(General_Election_Vote_Percent),
        mean.democrat.republican.ratio.for.companies = mean(Democrat_Republican_Ratio)) %>% ungroup()

filtered.df <- filtered.df %>% mutate(
  Vote = as.factor(Vote),
  Vote.Against.Majority = as.factor(Vote.Against.Majority),
  Industry = as.factor(Industry),
  Incumbent_Status = as.factor(Incumbent_Status),
  bill.year = as.factor(bill.year),
  political.party = as.factor(political.party),
  gender = as.factor(gender),
  sponsor.cosponsor = as.factor(sponsor.cosponsor)
)

# Create the model matrix excluding the response variable and other non-predictors
x <- model.matrix(~ . -1 - Vote.Against.Majority - bill_id - bonica_rid - bill.year,data = filtered.df)  # -1 to omit intercept

# Ensure the response variable is also appropriately factored and numeric
y <- as.numeric(as.factor(filtered.df$Vote.Against.Majority)) - 1  # subtract 1 to make it 0-based for glmnet

# Checking for NAs and handle them if necessary
complete_cases <- complete.cases(x, y)
x <- x[complete_cases, ]
y <- y[complete_cases]
###Modeling#############################################

# Lasso regression

lasso_model <- glmnet(x, y, family = "binomial", alpha = 1)


dev.new()
plot(lasso_model, xvar = "lambda", label = TRUE)

# Memory Usage Reached! Following code won't work
cv_lasso <- cv.glmnet(x, y, family = "binomial", alpha = 1)
plot(cv_lasso)
best_lambda_lasso <- cv_lasso$lambda.min

# Decision Tree

filtered.df$Vote.Against.Majority<-factor(filtered.df$Vote.Against.Majority)

sample.data<-sample.int(nrow(filtered.df), floor(0.8*nrow(filtered.df)),
                        replace = F)
train<-filtered.df[sample.data, ]
test<-filtered.df[-sample.data, ]

y.test<-test[,"Vote.Against.Majority"]

train<-filtered.df[sample.data, ] %>% select(-bonica_rid,-bill_id)
test<-filtered.df[-sample.data,] %>% select(-bonica_rid,-bill_id)

tree <- rpart(Vote.Against.Majority ~ ., data = train, method = "class")
rpart.plot(tree, main = "Decision Tree for Healthcare Sector")


###
###
### Decision Tree for Transportation Sector


topic <- c("tw.transportation")
top.weight <- 0.25
sector <- c("Transportation")

filtered.df <- df %>%
  filter(
    Top_Topic %in% topic,
    Top_Topic_Weight >=top.weight,Sector %in% sector,
    Total_Donation > 0) %>% group_by(
      bill_id,bonica_rid,Vote,Vote.Against.Majority,Industry,Incumbent_Status) %>% summarize(
        TD = log(sum(Total_Donation)),
        bill.year = max(year),
        sponsor.cosponsor = as.integer(any(Sponsor == 1 | Cosponsor == 1)),
        political.party = max(Party),
        seat = max(Seat) == "federal:senate",
        gender = max(Gender),
        n.givers = max(N_Givers),
        general.election.vote.percent = max(General_Election_Vote_Percent),
        mean.democrat.republican.ratio.for.companies = mean(Democrat_Republican_Ratio)) %>% ungroup()

filtered.df <- filtered.df %>% mutate(
  Vote = as.factor(Vote),
  Vote.Against.Majority = as.factor(Vote.Against.Majority),
  Industry = as.factor(Industry),
  Incumbent_Status = as.factor(Incumbent_Status),
  bill.year = as.factor(bill.year),
  political.party = as.factor(political.party),
  gender = as.factor(gender),
  sponsor.cosponsor = as.factor(sponsor.cosponsor)
)

# Create the model matrix excluding the response variable and other non-predictors
x <- model.matrix(~ . -1 - Vote.Against.Majority - bill_id - bonica_rid - bill.year,data = filtered.df)  # -1 to omit intercept

# Ensure the response variable is also appropriately factored and numeric
y <- as.numeric(as.factor(filtered.df$Vote.Against.Majority)) - 1  # subtract 1 to make it 0-based for glmnet

# Checking for NAs and handle them if necessary
complete_cases <- complete.cases(x, y)
x <- x[complete_cases, ]
y <- y[complete_cases]
###Modeling#############################################

# Lasso regression

lasso_model <- glmnet(x, y, family = "binomial", alpha = 1)


dev.new()
plot(lasso_model, xvar = "lambda", label = TRUE)

# Memory Usage Reached! Following code won't work
cv_lasso <- cv.glmnet(x, y, family = "binomial", alpha = 1)
plot(cv_lasso)
best_lambda_lasso <- cv_lasso$lambda.min

# Decision Tree

filtered.df$Vote.Against.Majority<-factor(filtered.df$Vote.Against.Majority)

sample.data<-sample.int(nrow(filtered.df), floor(0.8*nrow(filtered.df)),
                        replace = F)
train<-filtered.df[sample.data, ]
test<-filtered.df[-sample.data, ]

y.test<-test[,"Vote.Against.Majority"]

train<-filtered.df[sample.data, ] %>% select(-bonica_rid,-bill_id)
test<-filtered.df[-sample.data,] %>% select(-bonica_rid,-bill_id)

tree <- rpart(Vote.Against.Majority ~ ., data = train, method = "class")
rpart.plot(tree, main = "Decision Tree for Transportation Sector")


### Decision Tree for Financial Sector


topic <- c("tw.banking.and.finance")
top.weight <- 0.25
sector <- c("Financial")

filtered.df <- df %>%
  filter(
    Top_Topic %in% topic,
    Top_Topic_Weight >=top.weight,Sector %in% sector,
    Total_Donation > 0) %>% group_by(
      bill_id,bonica_rid,Vote,Vote.Against.Majority,Industry,Incumbent_Status) %>% summarize(
        TD = log(sum(Total_Donation)),
        bill.year = max(year),
        sponsor.cosponsor = as.integer(any(Sponsor == 1 | Cosponsor == 1)),
        political.party = max(Party),
        seat = max(Seat) == "federal:senate",
        gender = max(Gender),
        n.givers = max(N_Givers),
        general.election.vote.percent = max(General_Election_Vote_Percent),
        mean.democrat.republican.ratio.for.companies = mean(Democrat_Republican_Ratio)) %>% ungroup()

filtered.df <- filtered.df %>% mutate(
  Vote = as.factor(Vote),
  Vote.Against.Majority = as.factor(Vote.Against.Majority),
  Industry = as.factor(Industry),
  Incumbent_Status = as.factor(Incumbent_Status),
  bill.year = as.factor(bill.year),
  political.party = as.factor(political.party),
  gender = as.factor(gender),
  sponsor.cosponsor = as.factor(sponsor.cosponsor)
)

# Create the model matrix excluding the response variable and other non-predictors
x <- model.matrix(~ . -1 - Vote.Against.Majority - bill_id - bonica_rid - bill.year,data = filtered.df)  # -1 to omit intercept

# Ensure the response variable is also appropriately factored and numeric
y <- as.numeric(as.factor(filtered.df$Vote.Against.Majority)) - 1  # subtract 1 to make it 0-based for glmnet

# Checking for NAs and handle them if necessary
complete_cases <- complete.cases(x, y)
x <- x[complete_cases, ]
y <- y[complete_cases]
###Modeling#############################################

# Lasso regression

lasso_model <- glmnet(x, y, family = "binomial", alpha = 1)


dev.new()
plot(lasso_model, xvar = "lambda", label = TRUE)

# Memory Usage Reached! Following code won't work
cv_lasso <- cv.glmnet(x, y, family = "binomial", alpha = 1)
plot(cv_lasso)
best_lambda_lasso <- cv_lasso$lambda.min

# Decision Tree

filtered.df$Vote.Against.Majority<-factor(filtered.df$Vote.Against.Majority)

sample.data<-sample.int(nrow(filtered.df), floor(0.8*nrow(filtered.df)),
                        replace = F)
train<-filtered.df[sample.data, ]
test<-filtered.df[-sample.data, ]

y.test<-test[,"Vote.Against.Majority"]

train<-filtered.df[sample.data, ] %>% select(-bonica_rid,-bill_id)
test<-filtered.df[-sample.data,] %>% select(-bonica_rid,-bill_id)

tree <- rpart(Vote.Against.Majority ~ ., data = train, method = "class")
rpart.plot(tree, main = "Decision Tree for Financial Sector")


### Decision Tree for Environment Sector


topic <- c("tw.environment")
top.weight <- 0.25
sector <- c("Energy","Utilities")

filtered.df <- df %>%
  filter(
    Top_Topic %in% topic,
    Top_Topic_Weight >=top.weight,Sector %in% sector,
    Total_Donation > 0) %>% group_by(
      bill_id,bonica_rid,Vote,Vote.Against.Majority,Industry,Incumbent_Status) %>% summarize(
        TD = log(sum(Total_Donation)),
        bill.year = max(year),
        sponsor.cosponsor = as.integer(any(Sponsor == 1 | Cosponsor == 1)),
        political.party = max(Party),
        seat = max(Seat) == "federal:senate",
        gender = max(Gender),
        n.givers = max(N_Givers),
        general.election.vote.percent = max(General_Election_Vote_Percent),
        mean.democrat.republican.ratio.for.companies = mean(Democrat_Republican_Ratio)) %>% ungroup()

filtered.df <- filtered.df %>% mutate(
  Vote = as.factor(Vote),
  Vote.Against.Majority = as.factor(Vote.Against.Majority),
  Industry = as.factor(Industry),
  Incumbent_Status = as.factor(Incumbent_Status),
  bill.year = as.factor(bill.year),
  political.party = as.factor(political.party),
  gender = as.factor(gender),
  sponsor.cosponsor = as.factor(sponsor.cosponsor)
)

# Create the model matrix excluding the response variable and other non-predictors
x <- model.matrix(~ . -1 - Vote.Against.Majority - bill_id - bonica_rid - bill.year,data = filtered.df)  # -1 to omit intercept

# Ensure the response variable is also appropriately factored and numeric
y <- as.numeric(as.factor(filtered.df$Vote.Against.Majority)) - 1  # subtract 1 to make it 0-based for glmnet

# Checking for NAs and handle them if necessary
complete_cases <- complete.cases(x, y)
x <- x[complete_cases, ]
y <- y[complete_cases]
###Modeling#############################################

# Lasso regression

lasso_model <- glmnet(x, y, family = "binomial", alpha = 1)


dev.new()
plot(lasso_model, xvar = "lambda", label = TRUE)

# Memory Usage Reached! Following code won't work
cv_lasso <- cv.glmnet(x, y, family = "binomial", alpha = 1)
plot(cv_lasso)
best_lambda_lasso <- cv_lasso$lambda.min

# Decision Tree

filtered.df$Vote.Against.Majority<-factor(filtered.df$Vote.Against.Majority)

sample.data<-sample.int(nrow(filtered.df), floor(0.8*nrow(filtered.df)),
                        replace = F)
train<-filtered.df[sample.data, ]
test<-filtered.df[-sample.data, ]

y.test<-test[,"Vote.Against.Majority"]

train<-filtered.df[sample.data, ] %>% select(-bonica_rid,-bill_id)
test<-filtered.df[-sample.data,] %>% select(-bonica_rid,-bill_id)

tree <- rpart(Vote.Against.Majority ~ ., data = train, method = "class")
rpart.plot(tree, main = "Decision Tree for Environment Sector")


