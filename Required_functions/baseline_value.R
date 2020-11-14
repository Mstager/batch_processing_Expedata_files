baseline.value=function(X, from, to) {
	#this function calculates the baseline value of a specified region of a channel; can take ouput from id.baseline() as input
	##X = object
	##from, to = range of values, as from id_baseline()
	return(mean(X[from:to])/100)
}