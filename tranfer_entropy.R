# Set-up----
dels<-ls()
rm(list=dels)
pacman::p_unload("data.table")
pacman::p_load("tidyverse","readxl","lubridate",'RTransferEntropy','future')
read_excel("raw.xlsx",sheet = "new_values")->dat
dat<-dat[-c(14,20)] # exclude aggregate bonds due to high correlation with other
names(dat)<- str_replace(names(dat),"  ....","") 
read_excel("raw.xlsx",sheet ="more desc")[,1:2]->flow_names
new_names<-flow_names$NAME[match(names(dat),flow_names$Code)]
new_names<-str_replace(new_names,"US ETF FLO ","")
new_names[c(2:3,13,19,20)]<-c("USD3M",'SONIA','EUR3M',"USD1W","EUR1W")
names(dat)[-1]<-new_names[-1]

# Create data for analysis
dat %>% 
  select(-CONV,-SONIA) %>%
  arrange(Date) %>%
  drop_na() %>%
  mutate(d_USD3M=c(NA,diff(USD3M)),
         d_EUR3M=c(NA,diff(EUR3M)),
         d_USD1W=c(NA,diff(USD1W)),
         d_EUR1W=c(NA,diff(EUR1W)),
         CE_dummy=if_else(Date>=as.Date("23-3-2020","%d-%m-%Y") & 
                            Date<=as.Date("31-12-2020","%d-%m-%Y"),1,0)) %>%
  # select(-Date,-USD3M,-USD1W,-EUR3M,-EUR1W) %>%
  drop_na() ->y_endog

y_endog %>% 
  filter(Date>=as.Date("1-3-2020","%d-%m-%Y"))->y_endog_short

## Unit root testing using Elliot Rothenberg & Stock Test
library(urca)
summary(ur.ers(y_endog$CORP,model='const',lag.max = 10))

## Minimum embedding time series dimension
nonlinearTseries::estimateEmbeddingDim(y_endog$d_USD3M, 
                                                   time.lag=10, 
                                                   max.embedding.dim=25,
                                 threshold=0.9, do.plot=TRUE)

nl_dim<-function(ts){
  nonlinearTseries::estimateEmbeddingDim(ts, 
                                         time.lag=10, 
                                         max.embedding.dim=25,
                                         threshold=0.9, do.plot=TRUE)
}

y_endog_short %>% select(-Date,-CE_dummy) %>% map_df(~nl_dim(.x)) ->dims

library(data.table)
## Analysis
max_te <- function(d,lyx=2) {
  te <- transfer_entropy(d$Value, d$d_IR, 
                         shuffles =200 ,ly=lyx, lx=lyx, nboot =500)
  data.table(
    ticker = d$ETF[1],
    dir = c("X->Y", "Y->X"),
    coef(te)
  )
}
TE_algo <- function(yvar,data,lags) {
  
  require(dplyr)
  data %>% 
    rename(d_IR=yvar) %>%
    select(-excludes) %>%
    pivot_longer(cols=!starts_with(c("Date","d_IR")), 
                 names_to = "ETF",values_to = "Value") %>%
    group_split(ETF) %>%
    map(~max_te(.x)) %>%
    bind_rows()
}

plan(multisession)
y_endog_short %>% select(-excludes)
results<-vector("list",length = 4)
yvars<-c("USD3M","EUR3M","USD1W","EUR1W")
for (i in seq_along(yvars)) {
  excludes<-c(setdiff(yvars,yvars[i]),"d_EUR3M","d_USD3M","d_EUR1W","d_USD1W")
  results[[i]]<-TE_algo(yvars[i],y_endog_short,8)
}

## Quick plot
rename_data<-function(df){
  df[, ticker := factor(ticker, 
                        levels = unique(df$ticker)[order(df[dir == "X->Y"]$ete)])]
  df[, dir := factor(dir, levels = c("X->Y", "Y->X"),
                     labels = c("Flow towards Interest Rate Changes",
                                "Flow from Interest Rate Changes to .."))]
}

for (i in 1:length(results)) {
  plot_data<-rename_data(results[[i]])
  p<-ggplot(plot_data, aes(x = ticker, y = ete)) + 
    facet_wrap(~dir) +
    geom_hline(yintercept = 0, color = "gray") +
    theme(axis.text.x = element_text(angle = 90)) +
    labs(x = NULL, y = "Effective Transfer Entropy") +
    geom_errorbar(aes(ymin = ete - qnorm(0.95) * se,  
                      ymax = ete + qnorm(0.95) * se),  
                  width = 0.25, col = "blue") +
    geom_point()
  return(p)
}
rename(results[[4]])%>% 
  ggplot(aes(x = ticker, y = ete)) + 
  facet_wrap(~dir) +
  geom_hline(yintercept = 0, color = "gray") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = NULL, y = "Effective Transfer Entropy") +
  geom_errorbar(aes(ymin = ete - qnorm(0.95) * se,  
                    ymax = ete + qnorm(0.95) * se),  
                width = 0.25, col = "blue") +
  geom_point()


y_endog %>% 
  rename(d_IR="d_USD3M") %>%
  select(-d_USD1W,-d_EUR1W) %>%
  pivot_longer(cols=!starts_with(c("Date","d_IR")), 
               names_to = "ETF",values_to = "Value") %>%
  group_split(ETF) %>%
  map(~max_te(.x)) %>%
  bind_rows()->res_US3M

rename_data<-function(df){
  df[, ticker := factor(ticker, 
                        levels = unique(df$ticker)[order(df[dir == "X->Y"]$ete)])]
  df[, dir := factor(dir, levels = c("X->Y", "Y->X"),
                     labels = c("Flow towards Interest Rate Changes",
                                "Flow from Interest Rate Changes to .."))]
}
plot_data<-rename_data(res_US3M)
ggplot(plot_data, aes(x = ticker, y = ete)) + 
  facet_wrap(~dir) +
  geom_hline(yintercept = 0, color = "gray") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = NULL, y = "Effective Transfer Entropy") +
  geom_errorbar(aes(ymin = ete - qnorm(0.95) * se,  
                    ymax = ete + qnorm(0.95) * se),  
                width = 0.25, col = "blue") +
  geom_point()

y_endog %>% 
  rename(d_IR="d_EUR3M") %>%
  select(-d_USD1W,-d_EUR1W) %>%
  pivot_longer(cols=!starts_with(c("Date","d_IR")), 
               names_to = "ETF",values_to = "Value") %>%
  group_split(ETF) %>%
  map(~max_te(.x)) %>%
  bind_rows()->res_EUR3M
plot_data<-rename_data(res_EUR3M)
ggplot(plot_data, aes(x = ticker, y = ete)) + 
  facet_wrap(~dir) +
  geom_hline(yintercept = 0, color = "gray") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = NULL, y = "Effective Transfer Entropy") +
  geom_errorbar(aes(ymin = ete - qnorm(0.95) * se,  
                    ymax = ete + qnorm(0.95) * se),  
                width = 0.25, col = "blue") +
  geom_point()

df_list_US_1W %>% 
  group_split(ETF) %>%
  map(~max_te(.x)) %>%
  bind_rows()->res_US_1W

df_list_EU %>%
  group_split(ETF) %>%
  map(~max_te(.x)) %>%
  bind_rows()->res_EU

df_list_EU_1W %>%
  group_split(ETF) %>%
  map(~max_te(.x)) %>%
  bind_rows()->res_EU_1W

# Credit easing period

y_endog %>% 
  rename(d_IR=d_EUR3M) %>%
  filter(CE_dummy==1) %>%
  select(-CE_dummy) %>%
  pivot_longer(cols=!starts_with(c("Date","d_IR")), 
               names_to = "ETF",values_to = "Value") %>%
  group_split(ETF) %>%
  map(~max_te(.x)) %>%
  bind_rows()->res_EU_CE

y_endog %>% 
  rename(d_IR=d_EUR1W) %>%
  filter(CE_dummy==1) %>%
  select(-CE_dummy) %>%
  pivot_longer(cols=!starts_with(c("Date","d_IR")), 
               names_to = "ETF",values_to = "Value") %>%
  group_split(ETF) %>%
  map(~max_te(.x)) %>%
  bind_rows()->res_EU_CE_1W

y_endog %>% 
  rename(d_IR=d_USD1W) %>%
  filter(CE_dummy==1) %>%
  select(-CE_dummy) %>%
  pivot_longer(cols=!starts_with(c("Date","d_IR")), 
               names_to = "ETF",values_to = "Value") %>%
  group_split(ETF) %>%
  map(~max_te(.x)) %>%
  bind_rows()->res_US_CE_1W

y_endog %>% 
  rename(d_IR=d_USD3M) %>%
  filter(CE_dummy==1) %>%
  select(-CE_dummy) %>%
  pivot_longer(cols=!starts_with(c("Date","d_IR")), 
               names_to = "ETF",values_to = "Value") %>%
  group_split(ETF) %>%
  map(~max_te(.x)) %>%
  bind_rows()->res_US_CE

#iptables -I INPUT -p tcp --dport 1022 -j ACCEPT'

save.image("eteResults.RData")
write_csv(y_endog,"etf_flows.csv")
