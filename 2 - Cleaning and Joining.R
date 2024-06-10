library(tidyr)
library(dplyr)

####READ IN INITIAL FILES##########################
company_donations <- read.csv("companies_ind.csv")
company_info <- read.csv("companies.csv")
candidate_info <- read.csv("recipients.csv")
bills <- read.csv("bills.csv")
votes <- read.csv("votes.csv")

####ADJUST THE COLUMN NAMES###############################
names(company_donations) <- gsub("FINAL_COMPANY_DONATIONS_", "", names(company_donations))
names(company_info) <- gsub("FINAL_COMPANY_INFO_", "", names(company_info))
names(candidate_info) <- gsub("FINAL_RECIPIENTS_", "", names(candidate_info))
names(bills) <- gsub("FINAL_BILLS_", "", names(bills))
names(votes) <- gsub("FINAL_VOTE_", "", names(votes))

names(company_donations) <- gsub("^ï..", "", names(company_donations))
names(company_info) <- gsub("^ï..", "", names(company_info))
names(candidate_info) <- gsub("^ï..", "", names(candidate_info))
names(bills) <- gsub("^ï..", "", names(bills))
names(votes) <- gsub("^ï..", "", names(votes))

#####INITIAL MANIPULATIONS###############################

#Company Party Alignment. Change variable to name
company_info$Company_Party <- ifelse(company_info$Total_Democrat > company_info$Total_Republican, "Democrat", "Republican")

#Fix the column types for donations to be numeric
company_info <- company_info %>% mutate(
  Total_Donation = as.numeric(gsub(",", "", gsub("\\$", "", Total_Donation))),
  Donation_Standard_Deviation = as.numeric(gsub(",", "", gsub("\\$", "", Donation_Standard_Deviation))),
  Mean_Donor_Amount = as.numeric(gsub(",", "", gsub("\\$", "", Mean_Donor_Amount))),
)


#Candidate Party Alignment
candidate_info$Parties <- ifelse(candidate_info$Party == 100, 1, ifelse(candidate_info$Party == 200, 0, 2))
candidate_info$Party <- ifelse(candidate_info$Party == 100, "Democrat", ifelse(candidate_info$Party == 200, "Republican", "Other"))

#Vote is Yay (originally 6, now 1) or Nay (0)
votes$Vote <- ifelse(votes$Vote == 6, 0, 1)

#Only include hr/hjres
bills <- bills %>% filter(
  grepl("(hr|hjres)\\d+",bill_id)
)

#######STARTING JOINS########################################

#Bills, Votes
bills.votes <- left_join(x=bills,y=votes,by = "bill_id")

#Bills and Votes, and Recipients
bills.votes.recipients <- left_join(x = bills.votes, y = candidate_info, by = c("cycle","bonica_rid"))


#Breakdown the majority vote by party to get the party dissenters variable

#Groupby bill_id, Party
#Sum Votes to get total yesses
#Count rows to get total votes
#Find the majority vote by party

temp.3.18.24.grouping <- bills.votes.recipients %>%
  filter(Party == "Democrat" | Party == "Republican") %>%
  group_by(bill_id,Party,Attribute) %>%
  summarize(sum(Vote),n())

#Whether the majority is yes or no
temp.3.18.24.grouping$Maj.Vote <- ifelse(
  temp.3.18.24.grouping$`sum(Vote)`/temp.3.18.24.grouping$`n()` > 0.5,
  1,
  0
)

temp.3.18.24.grouping.lookup.table <- temp.3.18.24.grouping %>%
  pivot_wider(
    names_from = Party,
    values_from = c(`sum(Vote)`,`n()`,Maj.Vote)
  ) %>% select(
    -Attribute
  ) %>% 
  distinct(bill_id,.keep_all = TRUE)

# Get majority vote by party
lookup.majority.votes <- temp.3.18.24.grouping.lookup.table %>% select(
  bill_id,Maj.Vote_Democrat,Maj.Vote_Republican)




#Bills, Party Majority Votes
bills.majority.votes <- left_join(x=bills,y=temp.3.18.24.grouping.lookup.table,by="bill_id")
#Bills and Party Majority Votes, Votes
bills.votes <- left_join(x=bills.majority.votes,y=votes,by = "bill_id")

#Bills and All Votes, and Recipients
bills.votes.recipients <- left_join(
  x = bills.votes, y = candidate_info, by = c("cycle","bonica_rid"))

#Vote_Differently
#temp.bills.votes.recipients <- left_join(
#  bills.votes.recipients,lookup.majority.votes,by='bill_id')

#Calculate the votes against their party's majority vote
#bills.votes.recipients <- bills.votes.recipients %>% mutate(
#  Vote.Against.Majority = case_when(Party == 'Democrat' ~ ifelse(Vote == Maj.Vote_Democrat,0,1),
#            Party == 'Republican' ~ ifelse(Vote == Maj.Vote_Republican,0,1))
#)

bills.votes.recipients <- bills.votes.recipients %>% mutate(
  Vote.Against.Majority = case_when(
    Party == 'Democrat' ~ ifelse(Vote == Maj.Vote_Democrat,0,1),
    Party == 'Republican' ~ ifelse(Vote == Maj.Vote_Republican,0,1)
  )
)

all.company.info <- left_join(x = company_donations, y = company_info, by = "ticker")
all.company.info$Democrat_Republican_Ratio <- all.company.info$Total_Democrat / all.company.info$Total_Republican
names(all.company.info) <- gsub("dime_rid", "bonica_rid", names(all.company.info))

## VERY IMPORTANT
## SINCE WE JOIN ON THE CYCLE COLUMN, THIS MEANS THAT OUR DONATION COLUMNS ACTUALLY ONLY MEAN
## DONATIONS FROM THE PREVIOUS ELECTION FOR THAT CONGRESSIONAL SESSION
## FOR EXAMPLE:
## IF A CYCLE SAYS 2006, THEN IT WILL MATCH TO THE BILLS THAT HAPPEN IMMEDIATELY AFTER THE 2006 
## ELECTION CYCLE, WHICH MEANS THE SESSION OF CONGRESS THAT WENT FROM JANUARY 2007 TO DECEMBER 2008
## IN THIS EXAMPLE, TOTAL DONATIONS MEANS DONATIONS FROM ONLY THE 2006 ELECTION CYCLE (NO PREVIOUS ONES)
df <- left_join(x = bills.votes.recipients, y = all.company.info, by = c("cycle","bonica_rid"))
df <- na.omit(df)

dim(df)
# [1] 7231383      46


