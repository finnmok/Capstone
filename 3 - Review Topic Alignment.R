#Look at the individual sectors and check which ones align with the bill topics
df$Sector %>% unique()

#The sectors that line up to a topic weight
#Group Energy and Utilities
#Try Technology and Services Grouped, then without
c("Technology", "Services", 
  "Financial",
  "Healthcare","Energy","Utilities","Transportation")


df %>% select(Top_Topic) %>% unique()

# The bill topics that align with a sector
#Group economy and banking/finance
#Group energy and environment
c(
  'tw.labor','tw.economy','tw.banking.and.finance',
  'tw.healthcare','tw.energy','tw.environment','tw.transportation')
