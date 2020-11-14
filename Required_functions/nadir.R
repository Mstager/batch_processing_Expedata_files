#this function identifies a period of specified length containing the lowest values and outputs the range of time and the mean value

nadir = function(X, nadir_length=10) {
	#X = channel
	#nadir_length = duration over which to calculate VO2min in minutes, defaults to 10
	n=NULL
	for (j in 1:(length(X)-nadir_length*60)){
		window_avg=mean(X[j:(j+nadir_length*60)], na.rm=T)
		if (!any(names(n)=="mean")) {
			n$period=range(j,j+nadir_length*60)
			n$mean=window_avg
		}
		if (window_avg < n$mean) {
			n$period=range(j,j+nadir_length*60)
			n$mean=window_avg
		}
		else{next}
	}
	return(n) #returns mean value and the range of values for which it was identified
}