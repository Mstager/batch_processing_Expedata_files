#this function allows the O2 channel to be corrected for lag

lag.correction=function(X, lag, input=FALSE){
	#X = O2 channel to be corrected
	#lag = period in seconds between peaks in O2 and CO2
	#input = if TRUE, prompts for user input of lag time (default FALSE)
	if (input==TRUE) {
	lag=as.numeric(readline(prompt="Enter lag time (in seconds) for O2 channel as compared to CO2 channel:"))
	}
	lagless=X[c(lag:length(X),rep(NA, lag-1))]
	return(lagless)
}

#this function identifies the lag between CO2 and O2 channels

#id.lag = function(X, Y, marker1=600, marker2=1000, period=5) {
	#X = channel to be corrected (e.g. O2)
	#Y = reference channel (e.g. CO2)
	#marker = time of marker denoting animal in/out
#	marker1=as.integer(marker1)
#	marker2=as.integer(marker2)
	
#	max_dxdt = NA
#	dx_dt = rep(NA, (marker_1-marker_2-1))
#	a=NA
#	b=NA
#		for (i in (marker1):(marker2)){
#		for(k in (i):(i+5)){
#			dx_dt[k-i]= abs(X[k]-X[k+1])
#		}	
#			if(is.na(max_dxdt)) {
#				max_dxdt = mean(dx_dt)
#				a=i
#				b=k
#			} else if(mean(dx_dt) > max_dxdt) {
#				max_dxdt = mean(dx_dt)
#				a=i
#				b=k
#			} else{}
#		}	
#		return(range(a:b))
#	}
#}
