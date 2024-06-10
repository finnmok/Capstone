library(dplyr)
library(tidyr)
library(stringr)


companies <- read.csv("dataverse_files/bod_fortune_500_DIME.csv")

companies <- select(
  companies,
  -c("corp.person.id", "middle.name", "age", "gender", "ceo", "privatefirm", 
     "chairman", "dime.cfscore", "self.funded", "pct.to.dems", "to.incumbs", 
     "to.open.seat", "to.challs", "to.winner", "to.losers"))

companies <- companies %>%
  mutate(
    total2 = ifelse(total == "NA", total.dem + total.rep, as.numeric(total))) %>%
  select(-total) %>%
  rename(total = total2) %>%
  filter(total > 0) %>%
  mutate(num.conts = ifelse(is.na(num.conts), 1, num.conts))


companies.ind <- read.csv("dataverse_files/bod_fortune_500_DIME_cont_records.csv")

companies.ind <- companies.ind %>%
  filter(cycle >= 2006, cycle <= 2014) %>%
  select(-c(latitude, longitude, cfscore, date, contributor.mname, 
            contributor.suffix, contributor.title, contributor.occupation, 
            contributor.employer, contributor.address, contributor.city, 
            contributor.state, contributor.zipcode, recipient.name, recipient.party)) %>%
  filter(!str_starts(dime.rid, "comm")) %>%
  select(-c(contributor.lname, contributor.fname, corpname))


recipients <- read.csv("fixed_recipients.csv")

recipients <- recipients %>%
  select(-fecyear) %>%
  filter(
    cycle %in% c(2006, 2008, 2010, 2012, 2014),
    str_starts(seat, "federal"),
    gwinner == "W"
    )


votes <- read.csv("vote_db.csv")

bills <- read.csv("bills_db.csv")

bills$date <- as.Date(bills$date)

bills <- bills %>%
  filter(
    date >= as.Date("2005-01-03") & date <= as.Date("2013-01-02"),
    congno > 109,
    tw.latent1 <= 0.95
    ) %>%
  select(-c(date, bill.str, sponsors, cosponsors))

bills.long <- bills %>%
  pivot_longer(
    cols = -c(bill.id, year, bill.desc, congno), # Columns to keep fixed
    names_to = "Attribute", # Name of the new column for the attribute names
    values_to = "Value" # Name of the new column for the values
  )

bills <- bills.long %>%
  filter(
    !Attribute %in% c("tw.abortion.and.social.conservatism", "tw.civil.rights", 
                           "tw.congress.and.procedural", "tw.crime", "tw.education", 
                           "tw.fair.elections", "tw.higher.education", "tw.indian.affairs", 
                           "tw.latent1", "tw.veterans.affairs", "tw.womens.issues", 
                           "tw.law.courts.and.judges"),
    Value != 0
         )


write.csv(companies, "companies.csv")
write.csv(companies.ind, "companies_ind.csv")
write.csv(recipients, "recipients.csv")
write.csv(votes, "votes.csv")
write.csv(bills, "bills.csv")