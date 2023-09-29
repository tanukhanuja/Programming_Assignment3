rankall <- function(outcome, num = "best"){
        #Read csv file
        df <- read.csv("outcome-of-care-measures.csv")
        
        outcomes<-c( "heart attack", "heart failure" , "pneumonia" )
        
        if (!any(outcomes==outcome)){
                return(print("Invalid outcome"))
                }
        
        if (outcome == "heart attack") {
                column<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
        } else if(outcome=="heart failure") {
                column<-"Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
        } else {
                column<-"Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
        }
                
        state<-unique(df$State)
        hospital<-c()
        for (value in state){
                #Extract rows for given state only
                state_data <- df[df$State==value,]
                state_data <- state_data[state_data[,column] != "Not Available", ]
                #sort based on hospital name(alphabetical order)
                ordered_data <- state_data[order(state_data$Hospital.Name),]
                
                #sort based on mortality rate values
                n_ordered_data <- ordered_data[order(as.numeric(ordered_data[,column])),]

                if(num=="best"){
                        hospital<-append(hospital, n_ordered_data$Hospital.Name[1])
                }else if(num=="worst"){
                        hospital<-append(hospital, n_ordered_data$Hospital.Name[nrow(n_ordered_data)])
                }else {
                        hospital<-append(hospital, n_ordered_data$Hospital.Name[num])
                }
        }
        result<-data.frame(hospital,state)
        return(result[order(result$state),])
}