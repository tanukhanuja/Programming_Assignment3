
best <- function(state,outcome){
        #Read csv file 
        df<- read.csv("outcome-of-care-measures.csv") 
        outcomes <- c("heart attack", "heart failure", "pneumonia")
        if(!any(outcomes==outcome)&&!any(df$State==state)){
          return(print("inavalid state and outcome"))}
        if(!any(outcomes==outcome)){
          return(print("invalid outcome"))}
        if(!any(df$State==state)){
          return(print("invalid state"))}
          
  
        #extract rows for given state only
        state_data<- df[df$State==state,]
        
        #sort based on hospital name
        ordered_hosp_data <- state_data[order(state_data$Hospital.Name),]
        
        #apply conditions for "outcome" input
        if(outcome=="heart attack"){         
                #find value of min mortality rate
                min_mortality <- min(ordered_hosp_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
                
                #extract the rows with value of min mortality rate
                min_mortality_df <- ordered_hosp_data[ordered_hosp_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack==min_mortality,]
        }
        if(outcome=="heart failure"){
                min_mortality <- min(ordered_hosp_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
                min_mortality_tab <- ordered_hosp_data[ordered_hosp_data$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure==min_mortality,]
        }
        
        if(outcome=="pneumonia"){
                min_mortality <- min(ordered_hosp_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia)
                min_mortality_tab <- ordered_hosp_data[ordered_hosp_data$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia==min_mortality,]
        }
       
        #Return hospital name of the extracted row/rows
         return(min_mortality_tab$Hospital.Name)
        
}



