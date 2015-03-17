corr <- function(directory, threshold = 0) {
	files_list <- list.files(directory, full.names = TRUE)
	correlation <- c()
	for (i in 1:length(files_list)) {
		raw_data = read.csv(files_list[i])
		ok = complete.cases(raw_data)
		clean_data = raw_data[ok,]
		nobs = sum(ok)
		if (nobs > threshold) {
			correlation <- c(correlation, cor(clean_data $nitrate, clean_data $sulfate))
		}
	}
	correlation
}