#install.packages("dlnm")
#install.packages("mgcv")
library(dlnm)
library(mgcv)
require(splines)
require(glm)
require(fst)
require(dplyr)
require(lubridate)

set.seed(4)

# read in the data: 
setwd("~/sw outages")

overall_dta <- read_fst("weather_outages_hourly.fst")%>%
  dplyr::select(dps_id,temperature, total_precipitation, snowfall_hourly, abs_wind_speed_knots,customers, customers_out, utility, op_div, urn,
                datetime_eastern, prop_out) %>%
  mutate(hour = hour(datetime_eastern),
         year = year(datetime_eastern),
         dt = as.Date(datetime_eastern)) %>%
  mutate(snowfall_hourly = if_else(snowfall_hourly<0, 0, snowfall_hourly))%>%
  sample_frac(.1)


urbanicty <- c("n", "u", "r")
weather_var <- c("temperature", "total_precipitation","abs_wind_speed_knots","snowfall_hourly")



# Initialize an empty data frame to store results
result_df <- data.frame(urbanicity = character(0), weather_var = character(0), best_aic = numeric(0), df_argvar = numeric(0), df_arglag = numeric(0))

result_df <- lapply(urbanicty, function(urnyc) {
  dta <- overall_dta %>%
    filter(urn == urnyc) %>%
    arrange(dt)
  
  result_list <- list()  # Create a list to store results for this urbanicity
  
  for (wv in weather_var) {
    if (wv == "snowfall_hourly") {
      dta = dta %>% tidyr::drop_na(snowfall_hourly)
    }
    
    # KNOTS GRID
    grid <- as.matrix(expand.grid(var = 2:5, lag = 2:5))
    
    system.time({
      best_converged <- FALSE
      best_aic_val <- 1e10  # Initialize with a high value
      best_model <- NULL
      df_argvar <- NA        # Initialize outside the loop
      df_arglag <- NA        # Initialize outside the loop
      
      for (i in seq(nrow(grid))) {
        cb <- crossbasis(dta[, wv], lag = 24, argvar = list(fun = "ns", df = grid[i, 1]),
                         arglag = list(fun = "ns", df = grid[i, 2]))
        m <- gam(round(customers_out) ~ cb + factor(utility) + ns(dt, df = 4 * 6),
                 family =  nb(theta = NULL, link = "log"),
                 offset = log(customers),
                 data = dta)
        
        # Check for model convergence 
        current_aic_val <- AIC(m)
        if (current_aic_val < best_aic_val & m$converged == TRUE) {
          best_aic_val <- current_aic_val
          best_model <- m
          best_converged <- TRUE
          df_argvar <- grid[i, 1]  # Update outside the loop
          df_arglag <- grid[i, 2]  # Update outside the loop
        }
        
        # Print progress
        cat("Processing model:", i, "/", nrow(grid), "for", urnyc, "-", wv, "\n")
      }
      
      # If a converged model was found, store the results in the result_list
      if (best_converged) {
        result_list <- append(result_list, data.frame(urbanicity = urnyc, weather_var = wv, best_aic = best_aic_val, df_argvar = df_argvar, df_arglag = df_arglag))
      }
      
    })
  }
  
  return(result_list)  # Return the list of results for this urbanicity
})

# Combine the results from the list into the result_df data frame
result_df <- as.data.frame(result_df)

setwd("~/sw outages/data/output")    
write.fst(result_df, "result_df_nb.fst")




