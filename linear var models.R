## VAR modelling ----
# Set-up----
dels<-ls()
rm(list=dels)
pacman::p_load("tidyverse","readxl","lubridate",'vars','corrplot','data.table')
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

dat %>% 
  arrange(Date) %>%
  mutate(
    d_US=c(NA,diff(`US 3M LIBOR`)),
    d_SONIA=c(NA,diff(`SONIA`)),
    d_EU=c(NA,diff(`EU 3M LIBOR`)),
    CE_dummy=if_else(Date>=as.Date("23-3-2020","%d-%m-%Y") & 
                       Date<=as.Date("31-12-2020","%d-%m-%Y"),1,0)) %>%
  ungroup() %>%
  dplyr::select(-`Convertible Bonds`,
                -`US 3M LIBOR`,-`EU 3M LIBOR`,-`SONIA`
  ) %>%
  drop_na() ->y_endog
y_endog %>% 
  dplyr::select(-Date,-CE_dummy) %>%
  map_df(scale) %>%
  bind_cols(y_endog %>% 
              dplyr::select(Date,CE_dummy)) ->y_endog_scaled

# Analysis ----


# Model one: linear model with exogeneous interactions for corporate, high yield and government bonds.
library(vars)
var_model_one<-VAR(y_endog_scaled %>% 
                 dplyr::select(-CE_dummy,-Date),
               exogen =y_endog_scaled %>% 
                 mutate(Corporate_i=`Corporate Bonds`*CE_dummy,
                        HighY_i=`High Yield`*CE_dummy,
                        Gov_i=`Government Bonds`*CE_dummy) %>% 
                 dplyr::select(Corporate_i,HighY_i,Gov_i,CE_dummy),
               lag.max = 4)
var_model_one %>% summary() -> var_model_results

var_model_results$varresult$d_US
var_model_results$varresult$d_SONIA
var_model_results$varresult$d_EU


var_model_two<-VAR(y_endog_scaled %>% 
                     mutate(Corporate_i=`Corporate Bonds`*CE_dummy,
                            HighY_i=`High Yield`*CE_dummy,
                            Gov_i=`Government Bonds`*CE_dummy) %>%
                     dplyr::select(-CE_dummy,-Date),
                   exogen =y_endog_scaled %>% 
                     dplyr::select(-CE_dummy),
                   lag.max = 4)
var_model_two %>% summary() -> var_model_results2

var_model_results2$varresult$d_US
var_model_results2$varresult$d_SONIA
var_model_results2$varresult$d_EU

# Only variables that had relevance in the Entropy exercise

var_model_three<-VAR(y_endog_scaled %>% 
                       dplyr::select(d_US,d_EU,d_SONIA,`Government Bonds`,`Inflation Protected`,`Ultra Short`,Preferred),
                     exogen =y_endog_scaled %>% 
                       transmute(Gov_i=`Government Bonds`*CE_dummy,
                                 Ip_i=`Inflation Protected`*CE_dummy,
                                 Us_i=`Ultra Short`*CE_dummy,
                                 Pref_i=Preferred*CE_dummy,
                                 CE_dummy=CE_dummy),
                     lag.max = 4)


var_model_three %>% summary() -> var_model_results3

var_model_results3$varresult$d_US
var_model_results3$varresult$d_SONIA
var_model_results3$varresult$d_EU

