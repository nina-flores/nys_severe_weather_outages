install.packages("dlnm")
#install.packages("mgcv")
library(dlnm)
library(mgcv)
require(splines)
require(glm)
require(fst)
require(dplyr)
require(lubridate)




# read in the data: 
setwd("~/sw outages")

overall_dta <- read_fst("weather_outages_hourly.fst")%>%
  dplyr::select(dps_id,temperature, total_precipitation, snowfall_hourly, abs_wind_speed_knots,customers, customers_out, utility, op_div, urn,
                datetime_eastern, prop_out) %>%
  mutate(hour = hour(datetime_eastern),
         year = year(datetime_eastern),
         dt = as.Date(datetime_eastern)) %>%
  mutate(snowfall_hourly = if_else(snowfall_hourly<0, 0, snowfall_hourly))



### set up the model ###

urbanicty <- c("n", "u", "r")
weather_var <- c("temperature", "total_precipitation","abs_wind_speed_knots","snowfall_hourly")

step_size = 1

lapply(urbanicty, function(urnyc){
  dta <- overall_dta %>%
    filter(urn == urnyc) %>%
    arrange(dt)
  
  
  

  lapply(weather_var, function(wv){
    
    if(wv == "snowfall_hourly"){
      dta = dta %>% tidyr::drop_na(snowfall_hourly)
    }
    
    # create crossbasis with natural spline
    cb.exposure <- crossbasis(
      dta[, wv],         
      lag = 24,    
      argvar = list(fun="ns", df = 5),  # use a natural spline for the exposure-response curve
      arglag = list(fun="ns", df = 5),  # use a natural spline for the lag-response curve
    )
    
    # run gam with crossbasis and automatic selection of df
    mod.ns <- gam(prop_out ~ cb.exposure + factor(utility) + ns(dt, df = 4*6),
                  family = "quasipoisson",
                  data = dta,
                  method = "REML",  # use REML to improve model fitting
                  select = TRUE,  # automatically select the best value for df
                  scale = 0) 
    
    # print summary of the model
    mod.ns.summary <- summary(mod.ns)
    df_table <- mod.ns.summary$pTerms.df   
    print(df_table)
    
    if(wv == "snowfall_hourly"){
      step_size = .0005
    }
    
    # predict the association using the crosspred function
    pred.mod <- crosspred(basis = cb.exposure,
                          model = mod.ns, 
                          at = seq(from = min(overall_dta[wv], na.rm = TRUE),
                                   to = max(overall_dta[wv], na.rm = TRUE),
                                   by = step_size),
                          bylag = 0.2,  
                          cen = median(overall_dta[,wv], na.rm = TRUE), 
                          cumul = TRUE)
    
    
    
    
    # 5f.i Extract coefficient fit  
    
    fit.table.ns <- as.data.frame(pred.mod$matRRfit)  

    fit.table.ns <- fit.table.ns %>%   mutate(variable = as.numeric(row.names(fit.table.ns))) %>%
      tidyr::pivot_longer(c(1:121), names_to = "lag", values_to = "fit.rr" ) %>%
      tidyr::separate(lag, into=c(NA, "lag"), sep = "lag")
    
    
    # 5f.ii Extract 95% CI  
    
    lci.table.ns <- as.data.frame(pred.mod$matRRlow)  %>%
      tidyr::pivot_longer(c(1:121), names_to = "lag", values_to = "lci.rr" ) %>%
      tidyr::separate(lag, into=c(NA, "lag"), sep = "lag")
    
    
    uci.table.ns <- as.data.frame(pred.mod$matRRhigh)  %>%
      tidyr::pivot_longer(c(1:121), names_to = "lag", values_to = "uci.rr" ) %>%
      tidyr::separate(lag, into=c(NA, "lag"), sep = "lag")
    
    # 5f.iii Combine fit and se 
    
    pred.table.ns <- fit.table.ns %>%
      cbind(lci.table.ns$lci.rr) %>%
      cbind(uci.table.ns$uci.rr) %>%
      rename("lci.rr"="lci.table.ns$lci.rr",
             "uci.rr"="uci.table.ns$uci.rr")%>%   
      mutate(sw_exp = as.numeric(row.names(fit.table.ns)))
   
    
    # 4i write out data
    
    # select just integer lags
    
    Mini.Pred.Table.ns <- pred.table.ns %>% 
      mutate(lag = as.numeric(lag))
    
    setwd("~/sw outages/data/output")    
    write.fst(Mini.Pred.Table.ns, paste0(urnyc,"_",wv,"_24.fst"))
    
    gc()
    
  })
})






