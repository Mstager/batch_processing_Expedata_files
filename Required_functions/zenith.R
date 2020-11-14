#this function identifies a period of specified length containing the highest values and outputs the range of time and the mean value

zenith = function(X, zen_length=5) {
	#X = channel
	#zen_length = duration over which to calculate VO2max (or VCO2) in minutes, defaults to 5
	zen=NULL
	for (j in 1:(length(X)-zen_length*60)){
		if(length(which(is.na(X[j:(j+zen_length*60)])))>1){next}
		else{	
			window_avg=mean(X[j:(j+zen_length*60)], na.rm=TRUE)
			if (!any(names(zen)=="mean")) {
				zen$period=range(j,j+zen_length*60)
				zen$mean=window_avg
			}
			if (!is.na(window_avg) & window_avg > zen$mean) {
				zen$period=range(j,j+zen_length*60)
				zen$mean=window_avg
			}
			else{next}
		}
	}
	return(zen) #returns mean value and the range of values for which it was identified
}