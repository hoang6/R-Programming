rankhospital <- function(state, outcome, num = 'best') {
	data <- read.csv('outcome-of-care-measures.csv', colClasses = "character")

	if (!any(state == data$State)) {
		stop('invalid state')
	}
			
	data <- data[, c(2, 7, 11, 17, 23)]
	data <- subset(data, data[, 2] == state)
	
	if (outcome == 'heart attack') {
		col <- 3
	} else if (outcome == 'heart failure') {
		col <- 4
	} else if (outcome == 'pneumonia') {
		col <- 5
	} else {
		stop ('invalid outcome')
	}
	data <- subset(data, data[, col] != 'Not Available')	

	data[, col] <- as.numeric(data[, col])
	
	if (num == 'best')
		row <- 1
	else if (num == 'worst')	
		row <- nrow(data)
	
	data[ order(data[, col], data[, 1]), ][row, 1]
}