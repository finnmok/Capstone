library(ggplot2)

get.model <- function(topic,sector,log.donations = FALSE,top.weight=0.4,include.industries=FALSE){
  
  # topic = bill topic
  # sector = company sector
  # log.donations = apply log() transformation on donations
  # top.weight = minimum threshold for bill top topic weight (default at 0.4 for bills highly related to 
  #          topic, but some sectors have no bills at weight = 0.4, so there's a need to lower this
  # include.industries = look at interactions with industries instead of only sector in model
  
  if(length(topic) == 1){
    filtered.df <- df %>% 
      filter(
        Top_Topic == topic,
        Top_Topic_Weight >=top.weight,Sector==sector,
        Total_Donation > 0) %>% group_by(
          bill_id,bonica_rid,Vote,Vote.Against.Majority,Industry,Incumbent_Status) %>% summarize(
            TD = sum(Total_Donation))
  } else {
    filtered.df <- df %>% 
      filter(
        Top_Topic %in% topic,
        Top_Topic_Weight >=top.weight,Sector==sector,
        Total_Donation > 0) %>% group_by(
          bill_id,bonica_rid,Vote,Vote.Against.Majority,Industry,Incumbent_Status) %>% summarize(
            TD = sum(Total_Donation))
  }
  
  # Three summary statistics to understand input data
  cat("Number of Bills: ", filtered.df$bill_id %>% unique() %>% length(),"\n")
  cat("Median Donation Amount: ",filtered.df$TD %>% median(),"\n")
  cat("Donations at 0th,50th,80th percentiles: ",quantile(filtered.df$TD,c(0,0.5,0.8)))
  
  if(include.industries == FALSE){
    if(log.donations == FALSE){
      glm_result <- glm(Vote.Against.Majority ~ TD,
                        family = binomial, data = filtered.df)
      print(ggplot(data=filtered.df,aes(x=TD,y=factor(Vote.Against.Majority))) + geom_boxplot())
    }else{
      glm_result <- glm(Vote.Against.Majority ~ log(TD),
                        family = binomial, data = filtered.df)
      print(ggplot(data=filtered.df,aes(x=log(TD),y=factor(Vote.Against.Majority))) + geom_boxplot())
    }
  } else {
    if(log.donations == FALSE){
      glm_result <- glm(Vote.Against.Majority ~ TD*Industry,
                        family = binomial, data = filtered.df)
      print(ggplot(data=filtered.df,aes(x=TD,y=factor(Vote.Against.Majority))) + geom_boxplot())
    } else{
      glm_result <- glm(Vote.Against.Majority ~ log(TD)*Industry,
                        family = binomial, data = filtered.df)
      print(ggplot(data=filtered.df,aes(x=log(TD),y=factor(Vote.Against.Majority))) + geom_boxplot())
      
    }
  }
  # Return summary of logistic regression model
  return(summary(glm_result))
}



##################Linear Model Breakdown by Topic########################
#Equations represent example probability predictions for                #
# either 0th,50th, or 80th percentile for the best model for that topic #
#########################################################################


#Energy
get.model("tw.energy","Energy",top.weight=0.25,log.donations = TRUE) #Better model

exp(-4.3769 + log(20351410)*0.1484) / (1+exp(-4.3769 + log(20351410)*0.1484))

#Healthcare
get.model("tw.healthcare","Healthcare",TRUE,top.weight = 0.25) #Better model

exp(-18.74362 + log(14048181)*1.04499) / (1+exp(-18.74362 + log(14048181)*1.04499))

#Transportation
get.model("tw.transportation","Transportation",TRUE,top.weight=0.25)

exp(-13.1939 + log(24849638)*0.6975) / (1+exp(-13.1939 + log(24849638)*0.6975))

#Financial
get.model('tw.banking.and.finance',"Financial",TRUE,top.weight=0.25) #Best model

exp(-8.54476 + log(12177782)*0.42195) / (1+exp(-8.54476 + log(12177782)*0.42195))

#Environment
get.model(c('tw.environment',"tw.energy"),"Energy",TRUE,top.weight=0.25)

exp(-4.59741 + log(16974213)*0.16236) / (1+exp(-4.59741 + log(16974213)*0.16236))


