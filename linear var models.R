## VAR modelling ----
# Set-up----
dels<-ls()
rm(list=dels)
pacman::p_load("tidyverse","readxl","vars")
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
                 select(-datE) %>% 
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
dat_anal<-dat %>%
  select(Date,yvar,xvar) %>%
  arrange(Date) %>%
  drop_na() %>%
  mutate(d_int_rate=c(NA,diff(`US 3M LIBOR`)))

# Analysis ----
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
var_model<-VAR(y_endog %>% dplyr::select(-CE_dummy),
               exogen =y_endog %>% dplyr::select(CE_dummy),
               lag.max = 4,type = "both")
var_model %>% summary()