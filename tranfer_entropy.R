# Set-up----
dels<-ls()
rm(list=dels)
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
  select(-Date,-USD3M,-USD1W,-EUR3M,-EUR1W) %>%
  drop_na() ->y_endog

# Shannon's transfer entropy
## full period
y_endog %>% 
  rename(d_IR=d_EUR3M) %>%
  pivot_longer(cols=!starts_with(c("Date","d_IR")), 
               names_to = "ETF",values_to = "Value")->df_list_EU

y_endog %>% 
  rename(d_IR=d_EUR1W) %>%
  pivot_longer(cols=!starts_with(c("Date","d_IR")), 
               names_to = "ETF",values_to = "Value")->df_list_EU_1W

y_endog %>% 
  rename(d_IR=d_USD3M) %>%
  pivot_longer(cols=!starts_with(c("Date","d_IR")), 
               names_to = "ETF",values_to = "Value")->df_list_US

y_endog %>% 
  rename(d_IR=d_USD1W) %>%
  pivot_longer(cols=!starts_with(c("Date","d_IR")), 
               names_to = "ETF",values_to = "Value")->df_list_US_1W

library(data.table)
## Analysis
max_te <- function(d) {
  te <- transfer_entropy(d$Value, d$d_IR, 
                         shuffles =500 ,ly=5, lx=5, nboot = 2000)
  data.table(
    ticker = d$ETF[1],
    dir = c("X->Y", "Y->X"),
    coef(te)
  )
}
plan(multisession)

df_list_US %>%
  group_split(ETF) %>%
  map(~max_te(.x)) %>%
  bind_rows()->res_US

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
