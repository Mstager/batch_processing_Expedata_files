#This script allows you to process Expedata files to generate VO2max statistics plus two pdf plots with the uncorrected and corrected O2 and CO2 traces

#Load required function from Sable to import Expedata files into R
library(SableBase)
setwd("./Required_functions") #directory where required functions are stored
sapply(list.files(pattern="[.]R$"), source) #source all required functions

#If you used compressed gas that differs in density from ambient air, flow rates recorded by a Sable Massflow Meter need to be corrected. We use an Alicat to measure these flow rates and usually record these at the beginning of a trial. This script performs a linear correction of the flow rate using this value at the time the trial began.
#Load file with alicat values for each file to be processed, must have corresponding file names. 
alicat = read.csv("./AlicatValues.csv")
alicat$Filename = paste("c_", alicat$Filename, sep="") #to match fixed filenames

#Identify folder with SMR files
setwd("DIRECTORY_NAME") #directory in which files are stored
files = dir(pattern="exp") #list all files in that folder

dir.create("Plots") #create directory for all plots to be stored in

#Create output file
output = data.frame(sub("c_", "", files)); names(output)="Filename"
output$VO2max = NA
output$endurance = NA
output$VCO2 = NA

n = min(which(is.na(output$VO2max)))
start=Sys.time()
for (i in n:length(files)) { 
	print(i)
	#Read file
	sscf = read.sscf(files[i])
	file = data.frame(sscf, check.names=TRUE)
	marker = attr(sscf, "marker")
	
	#Files now w/o any markers
	if(length(marker)==0){
		marker = data.frame(sample=NA, time=NA, code=NA, type=NA, text=NA)
		plot(file[,O2], type="l", col="blue")
		warning("No markers exist.", immediate.=TRUE)
		marker$sample[1]=as.integer(readline("Please enter sample number indicating animal in:"))
		abline(v=marker$sample[1])
		answer = as.character(readline("Are you satisfied with marker placement? y/n"))
		if (answer=="n") {
			marker$sample[1] = as.integer(readline("Well shit. Try again:"))
			abline(v=marker$sample[1])
		}
		marker$text[1]="a"
	}	
	
	#Remove extra markers: expedata commonly throws these letters in and we can ignore them here
	marker = marker[!is.na(marker$text),] 
	marker = marker[marker$text!="D",] #I don't know what this indicates
	marker = marker[marker$text!="p",] #pauses
	marker = marker[marker$text!="U",] #I don't know what this indicates
	
	#Files now w/o any markers
	if(nrow(marker)==0){
		marker[1,]=c(rep(NA,5))
		plot(file[,O2], type="l", col="blue")
		warning("No markers remaining.", immediate.=TRUE)
		marker$sample[1]=as.integer(readline("Please enter sample number indicating animal in:"))
		abline(v=marker$sample[1])
		answer = as.character(readline("Are you satisfied with marker placement? y/n"))
		if (answer=="n") {
			marker$sample[1] = as.integer(readline("Well shit. Try again:"))
			abline(v=marker$sample[1])
		}
		marker$text[1]="a"
	}	
		
	#Locate necessary channels (these may have been entered in the Expedata setup), if not located throw warning
		O2 = grep("^O2", names(file))
		if (length(O2)==0) {O2=1; warning("O2 channel is indistinguishable. Channel 1 assumed to be O2.")}
			
		CO2 = grep("CO2|C02", names(file))
		if (length(CO2)==0) {CO2=2; warning("CO2 channel is indistinguishable. Channel 2 assumbed to be CO2.")}
		
		Flow = 5 #assumes animal flow rate in channel 5
	
	#Locate or specify marker for animal in		
		Animal_in = marker$sample[marker$text=="a" | marker$text=="1" | marker$text=="3" | marker$text=="A"]
		if (length(Animal_in)==0) {
			plot(file[1:2000,O2], type="l", col="blue")
			warning("Marker denoting animal in is indistinguishable.", immediate.=TRUE)
			Animal_in=as.integer(readline("Please enter sample number indicating animal in:"))
			abline(v=Animal_in)
			answer = as.character(readline("Are you satisfied with marker placement? y/n"))
			if (answer=="n") {
				Animal_in = as.integer(readline("Well shit. Try again:"))
				abline(v=Animal_in)
			}
		}
		if (length(Animal_in)>1) {Animal_in=min(Animal_in, na.rm=TRUE)}
		
		
	#Locate or specify marker for animal out			
		Animal_out = marker$sample[marker$text=="b" | marker$text=="B" | marker$text=="2" | is.na(marker$text)]
		if (length(Animal_out)==0) {
			plot(file[,O2], type="l", col="blue")
			abline(v=marker$sample)
			warning("Marker denoting animal out is indistinguishable.", immediate.=TRUE)
			Animal_out=as.integer(readline("Please enter sample number indicating animal out:"))
			abline(v=Animal_out)
			answer = as.character(readline("Are you satisfied with marker placement? y/n"))
			if (answer=="n") {
				Animal_out = as.integer(readline("Well shit. Try again:"))
				abline(v=Animal_out)
			}
		}
		if (length(Animal_out)>1) {Animal_out=max(Animal_out, na.rm=TRUE)}
		
			
	#Flow correction using file with all alicat values
		if (is.na(alicat$Alicat[alicat$Filename==files[i]])){file$corrected.flow = correct.flow(X=file[,Flow], time=Animal_in, heliox=750, input=FALSE)}
		else {file$corrected.flow = correct.flow(X=file[,Flow], time=Animal_in, heliox=alicat$Alicat[alicat$Filename==files[i]], input=FALSE)}
		
		
					
	#O2 Baseline correction
	if (Animal_in>=360){tr=6}; if (Animal_in>=300 & Animal_in<360){tr=5}; if (Animal_in>=240 & Animal_in<300){tr=4}; if (Animal_in>=180 & Animal_in<240) {tr=3}; if (Animal_in>=120 & Animal_in<180) {tr=2}

	#Identify beginning baseline
		per = 3 #default is 3 minutes
		O2base_beg = id.baseline(X=file[,O2], marker=Animal_in, time_range=tr, period=per) 
				
	#Identify end baseline
		O2base_end = id.baseline(X=file[,O2], marker=Animal_out, period=per, beg=FALSE) 
		
	#Identify basline O2 value		
		base_O2 = baseline.value(file[,O2], O2base_beg[1], O2base_beg[2]) 
	
	#Plot O2
		pdf(paste("Plots/", sub("c_SMR_", "", sub(".exp", "", files[i])), "_O2.pdf"))
		plot(file[,O2], type="l", col="blue")
		lines(file[,CO2]+(base_O2*95), col="red")
		abline(v=Animal_in)
		abline(v=Animal_out)
		abline(v=O2base_end, col="grey")
		abline(v=O2base_beg, col="grey")
		dev.off()		
		
	breakpoint = NA
	#Manually identify clogs in ascarite by including if statements on file by file basis and IDing breakpoint eg: if (files[i]=="HL_F2_SMR.exp") {breakpoint = 1300} 
						
	#Baseline correction & flip	
		file$corrected.O2 = correct.baseline(file[,O2], O2base_beg, O2base_end, breakpoint)
		file$corrected.CO2 = -correct.baseline(file[,CO2], O2base_beg, O2base_end, breakpoint, 0, 0) 
		
	#Transform O2 channel to calculate VO2	
		file$VO2 = transform.O2(file$corrected.O2, file$corrected.flow, base_O2)
			
	#Identify zenith, skipping first 3 mins animal in to avoid spikes; defaults to 5 min period
		skip=180 
		VO2max = zenith(file$VO2[(Animal_in+skip):Animal_out]) 
		output$VO2max[i] = VO2max$mean
	
	#Calculate endurance (period that VO2 is > 90% VO2max) in minutes
		output$endurance[i] = endurance(file$VO2[(Animal_in+skip):Animal_out], max=VO2max$mean, percent=90)/60
		
	#Transform CO2 channel to calculate VCO2 (assuming no CO2 drift because using gas)	
		file$VCO2 = transform.CO2(file$corrected.CO2, file$VO2, file$corrected.flow)
		output$VCO2[i] = mean(file$VCO2[VO2max$period[1]:VO2max$period[2]+Animal_in+skip])	
							
	#Plot VO2 with VO2max period highlighted	
	pdf(paste("Plots/", sub("c_SMR_", "", sub(".exp", "", files[i])), "_VO2.pdf"))
		plot(file$VO2, type="l", col="blue")
		abline(v=VO2max$period[1]:VO2max$period[2] + Animal_in + skip, col="grey")
		lines(file$VO2, col="blue")
		abline(v=Animal_in)
		abline(v=Animal_out)
		lines(file[,CO2]*5, col="red")
		abline(h = VO2max$mean*.9, lty=2, col="dark green")
		text(x=length(file$VO2), y=.5+VO2max$mean*.9,"90%", col="dark green")
	dev.off()
	
}


Sys.time()-start

write.csv(output, "SMR_outputfilename.csv")	
