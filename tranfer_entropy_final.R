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
  # select(-Date,-USD3M,-USD1W,-EUR3M,-EUR1W) %>%
  drop_na() ->y_endog

y_endog %>% 
  filter(Date>=as.Date("1-3-2020","%d-%m-%Y"))->y_endog_short

## Unit root testing using Elliot Rothenberg & Stock Test
# library(urca)
# summary(ur.ers(y_endog$CORP,model='const',lag.max = 10))

## Minimum embedding time series dimension

nl_dim<-function(ts){
  nonlinearTseries::estimateEmbeddingDim(ts, 
                                         time.lag=1, 
                                         max.embedding.dim=10,
                                         threshold=0.9, do.plot=TRUE)
}

y_endog_short %>% select(-Date,-CE_dummy) %>% map_df(~nl_dim(.x)) ->dims
rowMeans(dims)

## Some functions for the analysis
max_te <- function(d,lyx=2,bootn) {
  te <- transfer_entropy(d$Value, d$d_IR, 
                         shuffles =200 ,
                         ly=lyx, lx=lyx, 
                         nboot =bootn)
  data.table::data.table(
    ticker = d$ETF[1],
    dir = c("X->Y", "Y->X"),
    coef(te))
}
TE_algo <- function(yvar,data,lags,boot=500) {
  require(dplyr)
  data %>% 
    rename(d_IR=yvar) %>%
    select(-excludes) %>%
    pivot_longer(cols=!starts_with(c("Date","d_IR")), 
                 names_to = "ETF",values_to = "Value") %>%
    group_split(ETF) %>%
    map(~max_te(.x,lyx = lags,bootn = boot)) %>%
    bind_rows()
}
rename_data<-function(df){
  df[, ticker := factor(ticker, 
                        levels = unique(df$ticker)[order(df[dir == "X->Y"]$ete)])]
  df[, dir := factor(dir, levels = c("X->Y", "Y->X"),
                     labels = c("Flow towards Interest Rate Changes",
                                "Flow from Interest Rate Changes to .."))]
}
# Analysis
excludes<-c("USD3M","EUR3M","USD1W","EUR1W","CE_dummy")
plan(multisession)

full_results<-vector("list",4)
yvars<-c("d_USD3M","d_EUR3M","d_USD1W","d_EUR1W")
names(full_results)<-yvars
for (i in yvars) {
  full_results[[i]]<-TE_algo(i,y_endog_short,lags = 2,boot = 500)
}
full_results %>% map(~rename_data(.x))

CEperiod_results<-vector("list",4)
names(CEperiod_results)<-yvars
y_endog_short %>% filter(CE_dummy==1)->CE_data

for (i in yvars) {
  CEperiod_results[[i]]<-TE_algo(i,CE_data,lags = 2,boot = 500)
}
CEperiod_results %>% map(~rename_data(.x))

save.image("eteResults.RData")
write_csv(y_endog,"etf_flows.csv")
