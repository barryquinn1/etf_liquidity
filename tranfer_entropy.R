# Set-up----
dels<-ls()
rm(list=dels)
pacman::p_load("tidyverse","readxl","lubridate",'RTransferEntropy','future',"data.table")
read_excel("raw.xlsx",
           sheet = "daily values",
           skip = 3,
           col_types = "text") %>% 
  dplyr::slice(-c(1:2)) %>%
  drop_na(`BFFUEIG Index`) %>%
  rename(Date="...1") %>%
  mutate(datE=as.Date(Date,"%m/%d/%Y"),
         datE=if_else(is.na(datE),
                      as.Date(as.numeric(Date),
                              origin = "1899-12-30"),datE),Date=NULL)->dat
dat<-bind_cols(Date=dat$datE,dat %>% 
                 dplyr::select(-datE) %>% 
                 map_df(as.numeric))
# drop all missing variables
dat<-dat[-c(17:19)]
excel_sheets("raw.xlsx")->sheet_names
read_excel("raw.xlsx",sheet =sheet_names[1])[,1:2]->flow_names
new_names<-flow_names$Description[match(names(dat),flow_names$Security)]
new_names<-str_replace(new_names,"US ETF Flows - ","")
new_names[17:19]<-c("US 3M LIBOR","SONIA","EU 3M LIBOR")
names(dat)[-1]<-new_names[-1]

xvar<-names(dat)[-c(1,4,17:19)]
yvar<-names(dat)[17]
dat %>% 
  arrange(Date) %>%
  mutate(d_US=c(NA,diff(`US 3M LIBOR`)),
         d_SONIA=c(NA,diff(SONIA)),
         d_EU=c(NA,diff(`EU 3M LIBOR`)),
         CE_dummy=if_else(Date>=as.Date("23-3-2020","%d-%m-%Y") & 
                            Date<=as.Date("31-12-2020","%d-%m-%Y"),1,0)) %>%
  ungroup() %>%
  dplyr::select(-Date,-`Convertible Bonds`,
                -`US 3M LIBOR`,-`EU 3M LIBOR`) %>%
  drop_na() ->y_endog

# Shannon's transfer entropy
## full period
y_endog %>% 
  rename(d_IR=d_EU) %>%
  pivot_longer(cols=!starts_with(c("Date","d_IR")), 
               names_to = "ETF",values_to = "Value") %>%
  group_split(ETF)->df_list_EU

y_endog %>% 
  rename(d_IR=d_SONIA) %>%
  pivot_longer(cols=!starts_with(c("Date","d_IR")), 
               names_to = "ETF",values_to = "Value") %>%
  group_split(ETF)->df_list_SONIA

y_endog %>% 
  rename(d_IR=d_US) %>%
  pivot_longer(cols=!starts_with(c("Date","d_IR")), 
               names_to = "ETF",values_to = "Value") %>%
  group_split(ETF)->df_list_US

## Credit Easing period
y_endog %>% 
  rename(d_IR=d_EU) %>%
  filter(CE_dummy==1) %>%
  pivot_longer(cols=!starts_with(c("Date","d_IR")), 
               names_to = "ETF",values_to = "Value") %>%
  group_split(ETF)->df_list_EU_CEperiod

y_endog %>%
  rename(d_IR=d_SONIA) %>%
  filter(CE_dummy==1) %>%
  pivot_longer(cols=!starts_with(c("Date","d_IR")), 
               names_to = "ETF",values_to = "Value") %>%
  group_split(ETF)->df_list_SONIA_CEperiod

y_endog %>% 
  rename(d_IR=d_US) %>%
  filter(CE_dummy==1) %>%
  pivot_longer(cols=!starts_with(c("Date","d_IR")), 
               names_to = "ETF",values_to = "Value") %>%
  group_split(ETF)->df_list_US_CEperiod

# effective transfer entropy for High Yield and Corporate Bonds for full sample
# transfer_entropy(dat_anal$d_int_rate,dat_anal$`High Yield`,
#                  shuffles = 200,ly=3, lx=3, nboot = 1000 )
# 
# plan(multiprocess)
# transfer_entropy(dat_anal$d_int_rate,dat_anal$`High Yield`,
#                  shuffles = 200,ly=3, lx=3, nboot = 1000 )
# transfer_entropy(dat_anal$d_int_rate,dat_anal$`Corporate Bonds`,
#                  shuffles = 200,ly=3, lx=3, nboot = 1000 )
# # For period of programme starting 23rd march
# df1<-dat_anal%>% filter(Date>as.Date("23-2-2020","%d-%m-%Y"))
# transfer_entropy(df1$d_int_rate,df1$`High Yield`,
#                  shuffles = 200,ly=3, lx=3, nboot = 1000 )
# transfer_entropy(df1$d_int_rate,df1$`Corporate Bonds`,
#                  shuffles = 200,ly=3, lx=3, nboot = 1000 )


max_te <- function(d) {
  te <- transfer_entropy(d$Value, d$d_IR, 
                         shuffles = 200,ly=3, lx=3, nboot = 1000)
  data.table(
    ticker = d$ETF[1],
    dir = c("X->Y", "Y->X"),
    coef(te)
  )
}
plan(multiprocess)

df_list_US %>% 
  map(~max_te(.x))->res_US
df_list_US_CEperiod %>% 
  map(~max_te(.x))->res_US_CE

df_list_SONIA %>% 
  map(~max_te(.x))->res_SONIA
df_list_SONIA_CEperiod %>% 
  map(~max_te(.x))->res_SONIA_CE

df_list_EU_CEperiod %>% 
  map(~max_te(.x))->res_EU_CE
df_list_EU %>% 
  map(~max_te(.x))->res_EU

save(y_endog,res_EU,
     res_EU_CE,res_SONIA,
     res_SONIA_CE,res_US,
     res_US_CE,file ="eteResults.RData")
write_csv(y_endog,"etf_flows.csv")

max_rte <- function(d) {
  te <- transfer_entropy(d$Value, d$d_IR, entropy = "Renyi",
                         shuffles = 50, nboot = 200, quiet = T)
  data.table(
    ticker = d$ETF[1],
    dir = c("X->Y", "Y->X"),
    coef(te)[1:2, 2:3]
  )
}

df_list %>% 
  map(~max_rte(.x))->res_renyi


df_full <- bind_rows(res_full)
df1 <- bind_rows(res_renyi)
# order the ticker by the ete of X->Y
df_full[, ticker := factor(ticker, 
                       levels = unique(df_full$ticker)[order(df_full[dir == "X->Y"]$ete)])]
# df_full[, ticker := factor(ticker, 
#                       levels = unique(df1$ticker)[order(df1[dir == "X->Y"]$ete)])]

# rename the variable (xy/yx)
df_full[, dir := factor(dir, levels = c("X->Y", "Y->X"),
                    labels = c("Flow towards Interest Rate Changes",
                               "Flow towards ETF"))]
# df1[, dir := factor(dir, levels = c("X->Y", "Y->X"),
#                    labels = c("Flow towards Interest Rate Changes",
#                               "Flow towards ETF"))]

ggplot(df_full, aes(x = ticker, y = ete)) + 
  facet_wrap(~dir) +
  geom_hline(yintercept = 0, color = "gray") +
  theme(axis.text.x = element_text(angle = 90)) +
  labs(x = NULL, y = "Effective Transfer Entropy") +
  geom_errorbar(aes(ymin = ete - qnorm(0.95) * se,  
                    ymax = ete + qnorm(0.95) * se),  
                width = 0.25, col = "blue") +
  geom_point()

# ggplot(df1, aes(x = ticker, y = ete)) + 
#   facet_wrap(~dir) +
#   geom_hline(yintercept = 0, color = "gray") +
#   theme(axis.text.x = element_text(angle = 90)) +
#   labs(x = NULL, y = "Effective Transfer Entropy") +
#   geom_errorbar(aes(ymin = ete - qnorm(0.95) * se,  
#                     ymax = ete + qnorm(0.95) * se),  
#                 width = 0.25, col = "blue") +
#   geom_point()



# Run all three interest rates
dat %>% 
  group_by(Date) %>%
