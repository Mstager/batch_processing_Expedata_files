##this function transforms the CO2 channel according to Gwen Bachman's equation

transform.CO2=function(X, VO2.channel, flow.channel){
	##X = CO2 channel to transform
	transformed = (X * flow.channel - (X * VO2.channel)) / (1 - X)
	return(transformed)
}