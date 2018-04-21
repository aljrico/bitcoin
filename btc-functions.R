
# Bitcoin Analysis Functions ----------------------------------------------

vol <- function(df,n = 112){
	vol <- c()
	for(i in 1:nrow(df)){
		vol[[i]] = sd(df$price[i:(i+n)], na.rm=TRUE)/mean(df$price[i:(i+n)], na.rm=TRUE)
	}
	return(vol)
}
