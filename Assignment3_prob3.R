rankhospital <- function(state, outcome, num){
        #Read csv file
        df <- read.csv("outcome-of-care-measures.csv")
        outcomes<-c("heart attack","heart failure","pneumonia")
        
        if (!any(outcomes==outcome) && !any(df$State==state)){
                return(print("Invalid state and outcome"))
                }
        if (!any(outcomes==outcome)){
                return(print("Invalid outcome"))
                }
        #check if state and outcome are valid
        if( !any(df$State==state)){
                return(print("invalid state"))
                }
        
        if (outcome=="heart attack") {
                column<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if(outcome=="heart failure") {
              column<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else {
                column<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        }
        
        #Extract rows for given state only
        state_data <- df[df$State==state,]
        state_data <- state_data[state_data[,column] != "Not Available", ]
        #sort based on hospital name(alphabetical order)
        ordered_data <- state_data[order(state_data$Hospital.Name),]
        
        #sort based on mortality rate values
        n_ordered_data <- ordered_data[order(as.numeric(ordered_data[,column])),]
        
        if(num=="best"){
                num <- 1
                }
        if(num=="worst"){
                num <- nrow(n_ordered_data)
                }

        return(n_ordered_data$Hospital.Name[num])
}