#this function finds a relatively flat period in a specified region of the file

id.baseline = function(X, y=length(X), marker=600, time_range=6, period=5, beg=TRUE) {
	#X = object of interest
	#y = duration, defaults to length(X)
	#marker = time of marker denoting animal in/out
	#time_range = length of time before marker to search for beg=TRUE
	#period = length of the baseline period in minutes, defaults to 5 min
	#beg = logical, defaults to searching from beginning of file
	marker=as.integer(marker)
	time_range=as.numeric(time_range)*60
	period=as.numeric(period)*60
	min_dxdt = NA
	dx_dt = rep(NA, period-1)
	a=NA
	b=NA
		if (beg==TRUE){
			for (i in (marker-time_range):(marker-period)){
				for(k in (i):(period-1+i)){
					dx_dt[k-i]= abs(X[k]-X[k+1])
				}	
				if(is.na(min_dxdt)) {
					min_dxdt = mean(dx_dt, na.rm=TRUE)
					a=i
					b=k
				} else if(mean(dx_dt, na.rm=TRUE) < min_dxdt) {
					min_dxdt = mean(dx_dt, na.rm=TRUE)
					a=i
					b=k
				} else{}
			}	
			return(range(a:b))
		}
		if (beg==FALSE){
			for (i in marker:(y-period)){
				for(k in (i):(period-1+i)){
					dx_dt[k-i]= abs(X[k]-X[k+1])
				}	
				if(is.na(min_dxdt)) {
					min_dxdt = mean(dx_dt, na.rm=TRUE)
					a=i
					b=k
				} else if(mean(dx_dt, na.rm=TRUE) < min_dxdt) {
					min_dxdt = mean(dx_dt, na.rm=TRUE)
					a=i
					b=k
				} else{}
			}	
			return(range(a:b))
		}
}
