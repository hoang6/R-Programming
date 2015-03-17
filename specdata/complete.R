complete <- function(directory, id) {
	files_list <- list.files(directory, full.names = TRUE)
	dat <- data.frame()
	for (i in id) {
		data = read.csv(files_list[i])
		ok = complete.cases(data)		
		dat <- rbind(dat, data.frame(id = i, nobs = sum(ok)))
	}
	dat
}