library(haven)
library(ggplot2)
library(car)
library(labelled)
library(usmap)
library(visreg)
library(olsrr)
library(stargazer)

# Uncomment, import data, then comment out again. It's 61,000 rows and 716 columns.
#data <- read_dta("CES20_Common_OUTPUT_vv.dta")
 
# Create variables
state_number <- data$inputstate
cares_act <- ifelse(data$CC20_351a == 2,0, data$CC20_351a)
health <- data$CC20_309e
health <- recode(health,'1=5;2=4;4=2;5=1')
expand_medicare <- ifelse(data$CC20_327a == 2,0, data$CC20_327a)

# Covid variables
covid_me <- ifelse(data$CC20_309a_1==1,1,0)
covid_no <- ifelse(data$CC20_309a_5==1,1,0)
covid_other <- ifelse(data$CC20_309a_1==2 &
                      (data$CC20_309a_2==1 | 
                       data$CC20_309a_3==1 |
                       data$CC20_309a_4==1),1,0)

# Covid death variables
family_death <- ifelse(data$CC20_309b_1==1,1,0)
friend_death <- ifelse(data$CC20_309b_2==1,1,0)
coworker_death <- ifelse(data$CC20_309b_3==1,1,0)
no_death <- ifelse(data$CC20_309b_4==1,1,0)

#ftable(covid_me, covid_other,covid_no)
#ftable(covid_me=data$CC20_309a_1,covid_family=data$CC20_309a_2,covid_friend=data$CC20_309a_3,covid_coworker=data$CC20_309a_4,covid_no=data$CC20_309a_5)


# Recode work status variables from 1,2 to 0,1
Hours_reduced <- ifelse(data$CC20_309c_1==1,1,0)
Hours_reduced_restored <- ifelse(data$CC20_309c_2==1,1,0)
Temp_laid_off <- ifelse(data$CC20_309c_3==1,1,0)
Temp_laid_off_rehired <- ifelse(data$CC20_309c_4==1,1,0)
Lost_one_job <- ifelse(data$CC20_309c_5==1,1,0)
Lost_only_job <- ifelse(data$CC20_309c_6==1,1,0)
Not_working_before <- ifelse(data$CC20_309c_7==1,1,0)
Hours_increased <- ifelse(data$CC20_309c_8==1,1,0)
Taken_additional_jobs <- ifelse(data$CC20_309c_9==1,1,0)
Nothing_changed <- ifelse(data$CC20_309c_10==1,1,0)

# Create data frame and change state numbers as state names
df <- data.frame(state_number, cares_act, expand_medicare,
                 health, covid_me, covid_other, covid_no,
                 Hours_reduced, Hours_reduced_restored,
                 Temp_laid_off,Temp_laid_off_rehired,
                 Lost_one_job, Lost_only_job,
                 Not_working_before, Hours_increased,
                 Taken_additional_jobs, Nothing_changed,
                 family_death, friend_death, coworker_death, no_death)
df$state <- as_factor(df$state_number)

# set state column as first column
columnNumber <- which(colnames(df)=="state")
df <- df[,c(columnNumber,1:ncol(df)-1)]

# Import US Governors csv, recode values, and rename column
us_governors <- read.csv("us-governors.csv")
us_governors$party <- ifelse(us_governors$party == "democrat",1,0) # Recode democrat as 1, republican as 0
names(us_governors)[names(us_governors)=='state_name']<- 'state'

# Import covid case data (# of days till 1000 total cases per state)
days_1000_cases <- read.csv("days_till_1000_cases.csv")
pct_population_cases <- read.csv("state_population_pct.csv")


# Merge original data frame with US Governors data frame and omit NA rows
df2 <- merge(df, us_governors, by="state")
#df2 <- na.omit(df2)

# Create data frame for states
states_cares_act<-table(df2$state,df2$cares_act)
state_df <- as.data.frame(round(prop.table(states_cares_act,1)*100,digits=1))
state_df$state <- rownames(states_cares_act)
state_df <- subset(state_df,select = -c(Var1))
state_df <- state_df[!(state_df$Var2==0),]
state_df <- subset(state_df,select = -c(Var2))
names(state_df)[names(state_df)=='Freq']<- 'pct_support_cares'
state_df <- state_df[,c("state","pct_support_cares")]
state_df <- merge(state_df, us_governors, by="state")

# Create data frame for states support of medicare for all
states_support_medicare_table<-table(df2$state,df2$expand_medicare)
medicare_by_state <- as.data.frame(round(prop.table(states_support_medicare_table,1)*100,digits=1))
medicare_by_state$state <- rownames(states_support_medicare_table)
medicare_by_state <- subset(medicare_by_state,select = -c(Var1))
medicare_by_state <- medicare_by_state[!(medicare_by_state$Var2==0),]
medicare_by_state <- subset(medicare_by_state,select = -c(Var2))
names(medicare_by_state)[names(medicare_by_state)=='Freq']<- 'pct_support_medicare'
medicare_by_state <- medicare_by_state[,c("state","pct_support_medicare")]

# Create table for health by state
states_health_table<-table(df2$state,df2$health)
health_by_state <- as.data.frame.matrix(round(prop.table(states_health_table,1)*100,digits=1))

# Create health data frame
health_by_state$state <- rownames(states_health_table)
health_df <- as.data.frame.matrix(states_health_table)
health_df <- health_df[health_df$`1` != 0,]
names(health_df) <- c("x1", "x2", "x3","x4", "x5")
health_df$total_response <- rowSums(health_df[,c('x1','x2','x3','x4','x5')])
health_df$weighted_sum <- (health_df$`x1` * 1)+(health_df$`x2` * 2)+(health_df$`x3` * 3)+
                          (health_df$`x4` * 4)+(health_df$`x5` * 5)
health_df$average_health <- health_df$weighted_sum/health_df$total_response
health_df$state <- rownames(health_df)

# Merge support for cares act data frame with original data frame
df2 <- merge(df2, state_df, by="state")
df2 <- merge(df2, medicare_by_state, by="state")
df2 <- subset(df2, select=-c(party.y))
names(df2)[names(df2)=='party.x']<- 'party'

# Merge data to create states only data frame
state_df <- merge(state_df, medicare_by_state, by="state")

state_df <- merge(state_df, pct_population_cases, by="state")
state_df <- merge(state_df, days_1000_cases, by="state")
state_df <- merge(state_df, health_df, by="state")
state_df <- subset(state_df, select=-c(state_code,X,total_response,weighted_sum))
state_df <- subset(state_df, select=-c(x1,x2,x3,x4,x5))

# Number of respondents that had covid
covid_me_ag <- aggregate(covid_me~state, df2, sum)
state_df <- merge(state_df,covid_me_ag)

# Number of respondents that know someone that had covid
covid_other_ag <- aggregate(covid_other~state, df2, sum)
state_df <- merge(state_df,covid_other_ag)

# Number of respondents did not have covid or know someone that had covid
covid_no_ag <- aggregate(covid_no~state, df2, sum)
state_df <- merge(state_df,covid_no_ag)


# Aggregate work status variables into states, then merge each with state data frame
hours_reduced_ag               <- aggregate(Hours_reduced~state, df2, sum)
state_df <- merge(state_df,hours_reduced_ag)

hours_reduced_restored_ag      <- aggregate(Hours_reduced_restored~state, df2, sum)
state_df <- merge(state_df,hours_reduced_restored_ag)

temp_laid_off_ag               <- aggregate(Temp_laid_off~state, df2, sum)
state_df <- merge(state_df,temp_laid_off_ag)

temp_laid_off_rehired_ag       <- aggregate(Temp_laid_off_rehired~state, df2, sum)
state_df <- merge(state_df,temp_laid_off_rehired_ag)

lost_one_job_ag                <- aggregate(Lost_one_job~state, df2, sum)
state_df <- merge(state_df,lost_one_job_ag)

lost_only_job_ag               <- aggregate(Lost_only_job~state, df2, sum)
state_df <- merge(state_df,lost_only_job_ag)

not_working_before_ag          <- aggregate(Not_working_before~state, df2, sum)
state_df <- merge(state_df,not_working_before_ag)

hours_increased_ag             <- aggregate(Hours_increased~state, df2, sum)
state_df <- merge(state_df,hours_increased_ag)

taken_additional_jobs_ag       <- aggregate(Taken_additional_jobs~state, df2, sum)
state_df <- merge(state_df,taken_additional_jobs_ag)

Nothing_changed_ag       <- aggregate(Nothing_changed~state, df2, sum)
state_df <- merge(state_df,Nothing_changed_ag)


# Aggregate covid death variables into states, then merge each with state data frame
family_death_ag       <- aggregate(family_death~state, df2, sum)
state_df <- merge(state_df,family_death_ag)

friend_death_ag       <- aggregate(friend_death~state, df2, sum)
state_df <- merge(state_df,friend_death_ag)

coworker_death_ag       <- aggregate(coworker_death~state, df2, sum)
state_df <- merge(state_df,coworker_death_ag)

no_death_ag       <- aggregate(no_death~state, df2, sum)
state_df <- merge(state_df,no_death_ag)

# Number of respondents from each state
state_count <- as.data.frame(table(df2$state))
state_df <- merge(state_df, state_count, by.x = "state", by.y="Var1")

# Calculate percent of each state with Covid data
state_df$pct_covid_me                   <- round((state_df$covid_me/state_df$Freq)*100,2)
state_df$pct_covid_other                <- round((state_df$covid_other/state_df$Freq)*100,2)
state_df$pct_covid_no                   <- round((state_df$covid_no/state_df$Freq)*100,2)

# Calculate percent of each state with work status data
state_df$pct_hours_reduced              <- round((state_df$Hours_reduced/state_df$Freq)*100,2)
state_df$pct_hours_reduced_restored     <- round((state_df$Hours_reduced_restored/state_df$Freq)*100,2)
state_df$pct_temp_laid_off              <- round((state_df$Temp_laid_off/state_df$Freq)*100,2)
state_df$pct_temp_laid_off_rehired      <- round((state_df$Temp_laid_off_rehired/state_df$Freq)*100,2)
state_df$pct_lost_one_job               <- round((state_df$Lost_one_job/state_df$Freq)*100,2)
state_df$pct_lost_only_job              <- round((state_df$Lost_only_job/state_df$Freq)*100,2)
state_df$pct_not_working_before         <- round((state_df$Not_working_before/state_df$Freq)*100,2)
state_df$pct_hours_increased            <- round((state_df$Hours_increased/state_df$Freq)*100,2)
state_df$pct_taken_additional_jobs      <- round((state_df$Taken_additional_jobs/state_df$Freq)*100,2)
state_df$pct_nothing_changed            <- round((state_df$Nothing_changed/state_df$Freq)*100,2)

# Calculate percent of each state with work status covid death data
state_df$pct_family_death              <- round((state_df$family_death/state_df$Freq)*100,2)
state_df$pct_friend_death     <- round((state_df$friend_death/state_df$Freq)*100,2)
state_df$pct_coworker_death              <- round((state_df$coworker_death/state_df$Freq)*100,2)
state_df$pct_no_death     <- round((state_df$no_death/state_df$Freq)*100,2)

# View state level data
# View(state_df)

# These plots require a data source that has the first column as "state"
# "Values" is the column that is plotted

# State map of CARES act
plot_usmap(data = state_df, values = "pct_support_cares", color = "black", labels=TRUE) +
  labs(title = "Support for CARES act",
       subtitle = "By percent support") +
  scale_fill_continuous(
    low = "red1", high = "yellow", name = "% Support", label = scales::comma,
    breaks=ceiling(seq(min(state_df$pct_support_cares),max(state_df$pct_support_cares),
                       (max(state_df$pct_support_cares)-min(state_df$pct_support_cares))/4))
  ) + theme(legend.position = "right",panel.background = element_rect(color = "black", fill = "lightblue"),
            plot.title = element_text(size=22, face="bold"), plot.subtitle = element_text(size=14),
            legend.text = element_text(size=12), legend.title = element_text(size=12),
            legend.key.size = unit(1.5,'cm'))

# State map of expanding Medicare
plot_usmap(data = state_df, values = "pct_support_medicare", color = "black", labels=TRUE) +
  labs(title = "Support for expanding Medicare",
       subtitle = "By percent support") +
  scale_fill_continuous(
    low = "red1", high = "yellow", name = "% Support", label = scales::comma,
    breaks=ceiling(seq(min(state_df$pct_support_medicare), max(state_df$pct_support_medicare),
                       (max(state_df$pct_support_medicare)-min(state_df$pct_support_medicare))/4))
  ) + theme(legend.position = "right", panel.background = element_rect(color = "black", fill = "lightblue"),
            plot.title = element_text(size=22, face="bold"), plot.subtitle = element_text(size=14),
            legend.text = element_text(size=12), legend.title = element_text(size=12),
            legend.key.size = unit(1.5,'cm'))

# State map of political party
party_map_data <- data.frame(state = state_df$state, party = as.factor(state_df$party))

plot_usmap(data = party_map_data, values = "party", color = "black", labels=TRUE) +
  labs(title = "Political party",
       subtitle = "by Governor") +
  scale_fill_manual(values = c(`0` = "firebrick1", `1` = "steelblue3"), name = "Party",
                    labels=c("Rep","Dem")) + 
  theme(legend.position = "right", panel.background = element_rect(color = "black", fill = "lightblue"),
        plot.title = element_text(size=22, face="bold"), plot.subtitle = element_text(size=14),
        legend.text = element_text(size=12), legend.title = element_text(size=12),
        legend.key.size = unit(1.5,'cm'))



# Create data frame of states health in percent of response, but has issues. Uncomment to view
# states_health_table<-table(df2$state,df2$health)
# states_health <- as.data.frame(round(prop.table(states_health_table,1)*100,digits=1))
# View(state_health)


# Model 1
m1 <- lm(pct_support_cares ~ party + pct_support_medicare +
               + average_health + days_tot_cases + pct_covid_me + pct_covid_other +
               pct_hours_reduced + pct_hours_reduced_restored + pct_temp_laid_off +
               pct_temp_laid_off_rehired + pct_lost_one_job + pct_lost_only_job +
               pct_not_working_before + pct_hours_increased + pct_taken_additional_jobs
               ,data = state_df)
summary(m1)
visreg(m1, "party")
visreg(m1, "pct_support_medicare")
visreg(m1, "average_health")
visreg(m1, "days_tot_cases")
visreg(m1, "pct_covid_me")
visreg(m1, "pct_covid_other")
visreg(m1, "pct_hours_reduced")
visreg(m1, "pct_hours_reduced_restored")
visreg(m1, "pct_temp_laid_off")
visreg(m1, "pct_temp_laid_off_rehired")
visreg(m1, "pct_lost_one_job")
visreg(m1, "pct_lost_only_job")
visreg(m1, "pct_not_working_before")
visreg(m1, "pct_hours_increased")
visreg(m1, "pct_taken_additional_jobs")


# Model 2
m2 <- lm(pct_support_cares ~ party + pct_support_medicare +
               + average_health + days_tot_cases +
               pct_hours_reduced + pct_hours_reduced_restored + pct_temp_laid_off +
               pct_temp_laid_off_rehired + pct_lost_one_job + pct_lost_only_job +
               pct_not_working_before + pct_hours_increased + pct_taken_additional_jobs
             ,data = state_df)
summary(m2)
visreg(m2, "party")
visreg(m2, "pct_support_medicare")
visreg(m2, "average_health")
visreg(m2, "days_tot_cases")
visreg(m2, "pct_hours_reduced")
visreg(m2, "pct_hours_reduced_restored")
visreg(m2, "pct_temp_laid_off")
visreg(m2, "pct_temp_laid_off_rehired")
visreg(m2, "pct_lost_one_job")
visreg(m2, "pct_lost_only_job")
visreg(m2, "pct_not_working_before")
visreg(m2, "pct_hours_increased")
visreg(m2, "pct_taken_additional_jobs")

# Model 3
m3 <- lm(pct_support_cares ~ party + pct_support_medicare +
               + average_health + days_tot_cases + pct_covid_me + pct_covid_other
             ,data = state_df)
summary(m3)
visreg(m3, "party")
visreg(m3, "pct_support_medicare")
visreg(m3, "average_health")
visreg(m3, "days_tot_cases")
visreg(m3, "pct_covid_me")
visreg(m3, "pct_covid_other")

# Step wise regression
#ols_step_best_subset(m1)

# Final model, recommended from ols_step_best_subset(m1) in addition to remaining work status dummy variables
final_model_df <-subset(state_df,select = c(pct_support_cares,pct_support_medicare,days_tot_cases,pct_hours_reduced,
                                            pct_hours_reduced_restored,pct_temp_laid_off,pct_temp_laid_off_rehired,pct_lost_one_job,
                                            pct_lost_only_job,pct_not_working_before,pct_hours_increased, pct_taken_additional_jobs))


final_model <- lm(pct_support_cares ~ pct_support_medicare + days_tot_cases + pct_hours_reduced +
                    pct_hours_reduced_restored + pct_temp_laid_off + pct_temp_laid_off_rehired +
                    pct_lost_one_job + pct_lost_only_job + pct_not_working_before + pct_hours_increased +
                    pct_taken_additional_jobs,
                    data=final_model_df)

# New model with covid deaths variable, made Adjust R2 worse
m4 <- lm(pct_support_cares ~ party + pct_support_medicare +
           + average_health + days_tot_cases + pct_covid_me + pct_covid_other +
           pct_hours_reduced + pct_hours_reduced_restored + pct_temp_laid_off +
           pct_temp_laid_off_rehired + pct_lost_one_job + pct_lost_only_job +
           pct_not_working_before + pct_hours_increased + pct_taken_additional_jobs +
           pct_family_death + pct_friend_death + pct_coworker_death
         ,data = state_df)

summary(m4)
visreg(m4, "pct_coworker_death")


summary(final_model)
visreg(final_model, "pct_support_medicare")
visreg(final_model, "pct_hours_reduced")
visreg(final_model, "pct_hours_reduced_restored")
visreg(final_model, "pct_temp_laid_off")
visreg(final_model, "pct_temp_laid_off_rehired")
visreg(final_model, "pct_lost_one_job")
visreg(final_model, "pct_lost_only_job")
visreg(final_model, "pct_not_working_before")
visreg(final_model, "pct_hours_increased")
visreg(final_model, "pct_taken_additional_jobs")



# Residual model
plot(m1$fitted.values, m1$residuals)
plot(m2$fitted.values, m1$residuals)
plot(m3$fitted.values, m1$residuals)
plot(final_model$fitted.values, final_model$residuals)

stargazer(final_model_df, type = "html", out="sumstats.html", nobs = TRUE, mean.sd = TRUE, median = TRUE,iqr = TRUE)

