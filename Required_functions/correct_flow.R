##this fucntions corrects flow channel for special gas mixtures

correct.flow = function(X, time, heliox=750, input=FALSE){
	## X = channel with flow rate to correct
	##time = approximate timepoint at which Alicat reading was taken
	##heliox = flow rate as measured by Alicat, to be used when flow is constant among files
	##input = if TRUE, prompts for user input of Alicat flow rate (default FALSE)
	if (input==TRUE){
	heliox=as.numeric(readline(prompt="Enter heliox flow rate (as measured by Alicat) at start of trial:"))
	}
	correctedFlow = X * (heliox/X[time])
	return(correctedFlow)
}