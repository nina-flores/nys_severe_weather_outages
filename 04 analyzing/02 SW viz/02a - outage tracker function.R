# define outage function

### define outage tracker function
f_outage_tracker_dps_id <- function(outage_threshold, data){
  
  dps_id <- eval(substitute(dps_id), data) #changed dps_id to dps_id
  # hour_collapser_final <- eval(substitute(hour_collapser_final), data)
  ind_outage <- eval(substitute(outage_threshold), data)
  
  j = 1
  out_track <- rep(0, nrow(data))
  for (i in 1:nrow(data)){
    
    #deal with the first row
    print(i)
    
    if (i == 1){
      if (ind_outage[[i]] == 0){
        out_track[[i]] = 0
      }
      else{
        out_track[[i]] = 1
      }
    }
    #deal with non-first row
    else{
      #if the consecutive row is 1-0 then set to 0 and increment the counter
      if (ind_outage[[i]] == 0 &
          ind_outage[[i - 1]] == 1){
        out_track[[i]] = 0
        j = j+1
      }
      #if the consecutive row is 0-1 then increment counter and set to new counter value
      else if (ind_outage[[i]] == 1 &
               ind_outage[[i - 1]] == 0){
        out_track[[i]] = j
      }
      #if the current row is 1-1 and the dps_id previously are the same then set to counter
      else if (ind_outage[[i]] == 1 &
               ind_outage[[i - 1]] == 1 &
               dps_id[[i]] == dps_id[[i - 1]] #&
               # hour_collapser_final[[i]] <= 48 &
               # hour_collapser_final[[i]] > 0
      ){
        out_track[[i]] = j
      }
      #if the consecutive row is 1-1 but dps_id is different or there is a long duration between outages, then increase counter
      else if (ind_outage[[i]] == 1 &
               ind_outage[[i - 1]] == 1 &
               (dps_id[[i]] != dps_id[[i - 1]] #|
                # hour_collapser_final[[i]] > 48 |
                # hour_collapser_final[[i]] < 0
               )){
        j = j+1
        out_track[[i]] = j
      }
      #all other situations should be 0
      else{
        out_track[[i]] = 0
      }
    }
  }
  
  out_track
}
