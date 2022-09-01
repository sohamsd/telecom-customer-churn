library(smotefamily)
df = read.csv('data/Telco-Customer-Churn.csv')
df[df == 'No internet service'] = 'No'
df[df == 'No phone service'] = 'No'
df$SeniorCitizen[df$SeniorCitizen == 1] = 'Yes'
df$SeniorCitizen[df$SeniorCitizen == 0] = 'No'
df$Partner <- revalue(df$Partner, c('Yes' = 'Married', 'No' = 'Single'))
df$InternetService <- revalue(df$InternetService, c('No' = 'No Internet'))
df$PhoneServ <- paste(df$PhoneService, df$MultipleLines)
df$PhoneServ <- revalue(df$PhoneServ, c('Yes Yes' = 'Multiple Lines', 'Yes No' = 'One Line', 'No No' = 'No Phone'))
df$Streaming<- paste(df$StreamingTV, df$StreamingMovies)
df$Streaming <- revalue(df$Streaming, c('Yes Yes' = 'TV & Movies', 'Yes No' = 'Only TV', 'No Yes' = 'Only Movies', 'No No' = 'No Streaming'))


ggplot(df, aes(x = Churn, fill = Churn )) +
  geom_bar()

