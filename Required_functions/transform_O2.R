##this function transforms the O2 channel according to Gwen Bachman's equation

transform.O2=function(X, flow.channel, baseline_value){
	##X = O2 channel to transform
	transformed = (X * flow.channel)/(1-(baseline_value-X))
	return(transformed)
}