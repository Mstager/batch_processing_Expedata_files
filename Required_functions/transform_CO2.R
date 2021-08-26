##this function transforms the CO2 channel according to Gwen Bachman's equation

transform.CO2=function(X, VO2.channel, flow.channel){
	##X = CO2 channel to transform (fractional)
	transformed = (X/100 * flow.channel - (X/100 * VO2.channel)) / (1 - X/100)
	return(transformed)
}
