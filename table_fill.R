text_count <- function(indicator)
{
  y<-rle(indicator)
  if(length(which(y$values!='NA'))==0)
  {return('NA')
  }
  else
  {
    q<- max(which(y$values!='NA'))
    if(is.na(q))
    {
      return('NA')
    }
    else
    {
      if (is.na(y$values[q]))
      {
        return('NA')
      }
      else
      {
      if (y$values[q]==0)
      {
        return("Not implemented")
      }
      else
      {
        return(paste(y$lengths[q],"days"))
      }
      }
    }
  }
  

}


table_data<-read_csv("data_processed.csv") 

table_data <-
  table_data %>% group_by(CountryName) %>% select(
    CountryName,
    Date,
    "S1_School closing",
    "S1_IsGeneral",
    "S2_Workplace closing",
    "S2_IsGeneral",
    "S3_Cancel public events",
    "S3_IsGeneral",
    "S4_Close public transport",
    "S4_IsGeneral",
    "S6_Restrictions on internal movement",
    "S6_IsGeneral",
    "S7_International travel controls"
  )

table_data <- table_data %>% mutate("S1_School closing" = if_else(`S1_School closing`<2,0,1),
                                    "S2_Workplace closing" = if_else(`S2_Workplace closing`<2,0,1),
                                    "S3_Cancel public events" = if_else(`S3_Cancel public events`<2,0,1),
                                    "S4_Close public transport" = if_else(`S4_Close public transport`<2,0,1),
                                    "S6_Restrictions on internal movement" = if_else(`S6_Restrictions on internal movement`<2,0,1),
                                    "S7_International travel controls" = if_else(`S7_International travel controls`<3,0,1)
                                    )

k=n_distinct(table_data$CountryName)

daycounts <- as.data.frame(matrix(NA,k,7))
colnames(daycounts)<-c("CountryName","S1c","S2c","S3c","S4c","S6c","S7c")
daycounts$CountryName=unique(table_data$CountryName)
daycounts<- daycounts %>% mutate_all(as.character)

for (i in 1:k)
{
  temp<-table_data %>% filter(CountryName==daycounts$CountryName[i])

  {
   daycounts$S1c[i] = text_count(temp$`S1_School closing`)
   daycounts$S2c[i] = text_count(temp$`S2_Workplace closing`)
   daycounts$S3c[i] = text_count(temp$`S3_Cancel public events`)
   daycounts$S4c[i] = text_count(temp$`S4_Close public transport`)
   daycounts$S6c[i] = text_count(temp$`S6_Restrictions on internal movement`)
   daycounts$S7c[i] = text_count(temp$`S7_International travel controls`)
  }
}

colnames(daycounts)<-c("Country","Schools closed","Workplaces closed","Public events cancelled","Public transport closed","Internal movement restricted","International travel bans")
