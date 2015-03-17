rankall <- function(outcome, num = 'best') {
	data <- read.csv('outcome-of-care-measures.csv', colClasses = "character")

	if (outcome == 'heart attack') {
		col <- 3
	} else if (outcome == 'heart failure') {
		col <- 4
	} else if (outcome == 'pneumonia') {
		col <- 5
	} else {
		stop ('invalid outcome')
	}
	
	answer <- data.frame()
	states <- sort(unique(data[, 7]))
			
	data <- data[, c(2, 7, 11, 17, 23)]
	
	for (i in 1:length(states)) {
		
		data_state <- subset(data, data[, 2] == states[i])

		data_state[, col] <- as.numeric(data_state[, col])

		data_state <- subset(data_state, data_state[, col] != 'Not Available')	
	
		
		if (num == 'best')
			row <- 1
		else if (num == 'worst')	
			row <- nrow(data_state)
		else
			row <- num
			
		data_state <- data_state[ order(data_state[, col], data_state[, 1]), ]
		
		hospital_name <- data_state[row, 1]
		state_name <- states[i]
		answer <- rbind(answer, data.frame(hospital = hospital_name, state = state_name))
	}

	answer
}