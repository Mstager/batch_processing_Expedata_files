#this function identifies the period of time for which a specified % of a specified value is met in a given channel and outputs the length range of time

endurance = function(X, max = NA , percent = 90) {
	#X = channel
	#max = value
	#percent = percent of max maintained above this; defaults to 90
	dur=length(X[X > max * percent / 100])
	return(dur) #returns length
}