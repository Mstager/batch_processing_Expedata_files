##corrects O2 or CO2 baseline from specified periods at beginning and end of file and divides by 100

correct.baseline = function(X, begBaseline, endBaseline, breakpoint=NA, begValue=NA, endValue=NA, type="SMR"){
	#X = column or channel
	## begBaseline = range of observations covering the initial baseline
	## endBaseline = range of observation at the end of the file
	## breakpoint = point at which drift likely occurred (eg due to ascarite clog), for SMR only; default = NA
	## begValue = value to set beginning baseline at, to be used when you want to ignore erroneous values in file; default = NA
	## endValue = value to set end baseline at, to be used when you want to ignore erroneous values in file; default = NA
	##type = SMR (defualt) or RMR
	if (is.na(begValue)) {
		beginning = mean(X[begBaseline])
	}
	else {beginning = begValue}
	if (is.na(endValue)) {
		end = mean(X[endBaseline])
	}
	else {end = endValue}
	if (type=="SMR") {
		if (is.na(breakpoint)){
			firstDatum=begBaseline[2]+1
			lastDatum=endBaseline[1]-1
			s = firstDatum:lastDatum
			B = seq(from = beginning, to = end, length = length(s))
			correctedBase = c(X[1:(firstDatum - 1)] - beginning, X[s] - B, X[(lastDatum + 1):length(X)] - end)
		}
		else{
			firstDatum=breakpoint+1
			lastDatum=endBaseline[1]-1
			s = firstDatum:lastDatum
			B = seq(from = beginning, to = end, length = length(s))
			correctedBase = c(X[1:(firstDatum - 1)] - beginning, X[s] - B, X[(lastDatum + 1):length(X)] - end)
		}
	}
	if (type=="RMR") {
		firstDatum=begBaseline[1]+1
		lastDatum=endBaseline[2]-1
		s = firstDatum:lastDatum
		B = seq(from = beginning, to = end, length = length(s))
		correctedBase = c(X[begBaseline[1]] - beginning, X[s] - B, X[endBaseline[2]] - end)
	}
	correctedBase=-(correctedBase/100) 
	return(correctedBase)
}