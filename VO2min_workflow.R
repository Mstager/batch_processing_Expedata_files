#This script process Expedata files and generates basal (or resting) metabolic rate statistics plus two pdf plots with corrected and uncorredted CO2 and O2 traces
#It assumes you have multiplexed three animals and can be used if baselining occurred in between each animal or in between each round of animals

#Load functions required to open Expedata files in R
library(SableBase) #this package is available from https://rdrr.io/github/hawkmoth/sablebase/

setwd("./Required_functions") #directory where required functions are stored
sapply(list.files(pattern="[.]R$"), source) #source all required functions

setwd("DIRECTORY NAME") #directory in which files are stored
files = dir(pattern="exp") #list all files in that folder

dir.create("Plots")

#Did you baseline between each animal or between each round?
base_freq = "ind" #frequency of baseline, for RMR only; "ind" if baseline performed between each individual animal (i.e. B, 1, B, 2, B...); "round" if baseline performed once per round of individuals (i.e. B, 1, 2, 3, B...)

#Create output file
output = data.frame(sub("c_", "", files)); names(output)="Filename"
output$VO2min_1 = NA
output$VO2min_2 = NA
output$VO2min_3 = NA

n = min(which(is.na(output$VO2min_1))) #start here if an error is thrown to avoid rerunning previously processed files
start=Sys.time() 

for (i in n:length(files)) { 
	#for each file in the directory
	print(i) #print file number as you go
	sscf = read.sscf(files[i]) #read file
	file = data.frame(sscf, check.names=TRUE) #convert file format to a data frame
	marker = attr(sscf, "marker") #make a data frame with the markers 
	
	#Locate necessary channels (as labeled in Expedata setup), if not located throw warning
	O2 = grep("^O2", names(file))
	if (length(O2)==0) {O2=1; warning("O2 channel is indistinguishable. Channel 1 assumed to be O2.")}
			
	CO2 = grep("CO2|C02", names(file))
	if (length(CO2)==0) {CO2=2; warning("CO2 channel is indistinguishable. Channel 2 assumbed to be CO2.")}
			
	Flow1 = grep("Channel.1|Ch1|CH1", names(file))
	if (length(Flow1)==0) {Flow1=5; warning("Animal 1 flow channel is indistinguishable. Channel 5 assumed to be Animal 1 flow.")}
		
	Flow2 = grep("Channel.2|Ch2|CH2", names(file))
	if (length(Flow2)==0) {Flow2=6; warning("Animal 2 flow channel is indistinguishable. Channel 6 assumed to be Animal 2 flow.")}
	
	Flow3 = grep("Channel.3|Ch3|CH3", names(file))
	if (length(Flow3)==0) {Flow3=7; warning("Animal 3 flow channel is indistinguishable. Channel 7 assumed to be Animal 3 flow.")}
	
		
	#Remove extra markers: expedata commonly throws these letters in
	marker = marker[!is.na(marker$text),] 
	marker = marker[marker$text!="D",]
	marker = marker[marker$text!="p",] #pauses
	marker = marker[marker$text!="U",]
	rownames(marker)=1:nrow(marker)
	
	#Change marker 4 to b
	marker$text[marker$text==4] = "b"
		
	#Remove markers placed in rapid succession
	if(exists("rem_marker")) {rm(rem_marker)} #remove any previous instance of this object 
	for (h in 1:(length(marker$text)-1)){
		if (marker$sample[h+1]-marker$sample[h]<61) {
			warning("Markers ", marker$text[h], " at ", marker$sample[h], " and ", marker$text[h+1], " at ", marker$sample[h+1], " are placed within 60 seconds of one another. Will delete first marker (", marker$text[h], " at ", marker$sample[h], ").", immediate.=TRUE)
			plot(file[,O2], type="l", col="blue")
			abline(v=marker$sample, col="red")
			text(marker$sample,  max(file[,O2]), marker$text, font=2, col="red")
			print(marker)
			if (!exists("rem_marker")) {rem_marker = h}
			else {rem_marker=c(rem_marker, h)}
		}
	}
	if(exists("rem_marker")) {marker = marker[-rem_marker,]} #markers must be removed after for loop closes
			
	#Checking for other extraneous markers: this throws a warning if something other than 1, 2, 3, 4, b, or B is included as a marker
	if(exists("rem_marker")) {rm(rem_marker)} #remove any previous instance of this object 
	for (f in 1:length(marker$text)){
		if (marker$text[f]!="1" & marker$text[f]!="2" & marker$text[f]!="3" & marker$text[f]!="4" & marker$text[f]!="B" & marker$text[f]!="b") {
			warning("Atypical marker ", marker$text[f], " occurs.", immediate.=TRUE)
			plot(file[,O2], type="l", col="blue")
			abline(v=marker$sample, col="red")
			text(marker$sample,  max(file[,O2]), marker$text, font=2, col="red")
			print(marker)
			answer1 = as.character(readline("Would you like to replace this letter/number? Otherwise marker will be removed entirely. (y/n)"))
			if (answer1 == "y") {
				answer2 = as.character(readline("What letter/number would you like to replace it with?"))
				marker$text[f]=answer2 #edit marker table to include new letter
				print(marker) #print updated marker table
			}
			else {  #else remove marker
				if (!exists("rem_marker")) {rem_marker = f}
				else {rem_marker=c(rem_marker, h)}
			}	
		}
	}
	if(exists("rem_marker")) {marker = marker[-rem_marker,]} #markers must be removed after for loop closes
		
	#Checking for repeated markers: this throws a warning allowing you to remove any/all repeated markers or replace marker text
	if(exists("rem_marker")) {rm(rem_marker)} #remove any previous instance of this object 
	m = length(marker$text)
	for (d in 2:m){
		if (marker$text[d]==marker$text[d-1]) {
			warning("Marker ", marker$text[d], " occurs consecutively.", immediate.=TRUE)
			plot(file[,O2], type="l", col="blue")
			abline(v=marker$sample, col="red")
			text(marker$sample,  max(file[,O2]), marker$text, font=2, col="red")
			print(marker)
			answer = as.character(readline("Would you like to remove a marker? y/n"))
			if (answer=="y") {
				if (!exists("rem_marker")){
					rem_marker=as.integer(readline("Which marker would you like to remove? (indicate row number)"))
				}
				else {
					rem_addmark=as.integer(readline("Which marker would you like to remove? (indicate row number)"))
					rem_marker=c(rem_marker,rem_addmark)
				}
			}
			answer2 = as.character(readline("Would you like to replace the marker text? y/n"))
			if (answer2 == "y") {
				marker$text[d] = as.character(readline("Which marker would you like to replace this second marker with? (e.g. 1, 2, 3, B)"))
				print(marker)
			}
			answer3 = as.character(readline("Would you like to add a marker here? y/n"))
			if (answer3 == "y") {
				answer4 = as.character(readline("Which marker would you like to add? (e.g. 1, 2, 3, B)"))
				answer5 = as.integer(readline("Please enter associated sample number:"))
				marker = rbind(marker[1:d-1,], c(answer5, NA, NA, NA, answer4), marker[(d):length(marker$text),])
				print(marker)
				abline(v=marker$sample)
				m = m+1
			}
		}
	}	
	if(exists("rem_marker")) {marker = marker[-rem_marker,]} #markers must be removed after for loop closes
	
	#Checking for missing first marker (only works if starting with animal 1)
	if (marker$text[1]!="1" & marker$text[2]!="1") {
		warning("First marker at sample ", marker$sample[1], " does not denote Animal 1. Perhaps there is a missing marker.", immediate.=TRUE)
		print(marker)
		plot(file[,O2], type="l", col="blue")
		abline(v=marker$sample, col="red")
		text(marker$sample,  max(file[,O2]), marker$text, font=2, col="red")
		answer1 = as.character(readline("Would you like to add marker 1 here? y/n"))
		if (answer1 == "y") {
			answer2 = as.integer(readline("Please enter associated sample number:"))
			abline(v=answer2, col="red")
			answer3 = as.character(readline("Are you satisfied with marker placement? y/n"))
			if (answer3=="n") {
				answer2 = as.integer(readline("Well shit. Try again:"))
				abline(v=answer2, col="red")
			}
			marker = rbind(c(answer2,NA,NA,NA, 1),marker)
			print(marker)
		}
	}
	
	#Checking for missing markers (assuming chronological progression of markers) and allow you to add markers
	if (base_freq == "ind"){
		for (g in 1:(length(marker$text)-1)) {	
			if (marker$text[g]=="1" & marker$text[g+1]!="B" & marker$text[g+1]!="b") {
			warning("Marker 1 at sample ", marker$sample[g], " is not followed by baseline marker.", immediate.=TRUE)
			print(marker)
			plot(file[,O2], type="l", col="blue")
			abline(v=marker$sample, col="red")
			text(marker$sample,  max(file[,O2]), marker$text, font=2, col="red")
			answer1 = as.character(readline("Would you like to add a marker here? y/n"))
			if (answer1 == "y") {
				answer2 = as.character(readline("Which marker would you like to add? (e.g. 1, 2, 3, B)"))
				answer3 = as.integer(readline("Please enter associated sample number:"))
				marker = rbind(marker[1:g,],c(answer3,NA,NA,NA, answer2),marker[(g+1):length(marker$text),])
				print(marker)
				abline(v=marker$sample, col="red")
			}
			}
			if (marker$text[g]=="2" & marker$text[g+1]!="B" & marker$text[g+1]!="b") {
			warning("Marker 2 at sample ", marker$sample[g], " is not followed by baseline marker.", immediate.=TRUE)
			print(marker)
			plot(file[,O2], type="l", col="blue")
			abline(v=marker$sample, col="red")
			text(marker$sample,  max(file[,O2]), marker$text, font=2, col="red")
			answer1 = as.character(readline("Would you like to add a marker here? y/n"))
			if (answer1 == "y") {
				answer2 = as.character(readline("Which marker would you like to add? (e.g. 1, 2, 3, B)"))
				answer3 = as.integer(readline("Please enter associated sample number:"))
				marker = rbind(marker[1:g,],c(answer3,NA,NA,NA, answer2),marker[(g+1):length(marker$text),])
				print(marker)
				abline(v=marker$sample, col="red")
			}
			}
			if (marker$text[g]=="3" & marker$text[g+1]!="B" & marker$text[g+1]!="b") {
			warning("Marker 3 at sample ", marker$sample[g], " is not followed by baseline marker.", immediate.=TRUE)
			print(marker)
			plot(file[,O2], type="l", col="blue")
			abline(v=marker$sample, col="red")
			text(marker$sample,  max(file[,O2]), marker$text, font=2, col="red")
			answer1 = as.character(readline("Would you like to add a marker here? y/n"))
			if (answer1 == "y") {
				answer2 = as.character(readline("Which marker would you like to add? (e.g. 1, 2, 3, B)"))
				answer3 = as.integer(readline("Please enter associated sample number:"))
				marker = rbind(marker[1:g,],c(answer3,NA,NA,NA, answer2),marker[(g+1):length(marker$text),])
				print(marker)
				abline(v=marker$sample, col="red")
			}
			}
			if ((marker$text[g]=="b" | marker$text[g]=="B") & (marker$text[g+1]=="b" | marker$text[g+1]=="B")) {
				warning("Baseline marker at sample ", marker$sample[g], " is followed by another baseline marker.", immediate.=TRUE)
				print(marker)
				plot(file[,O2], type="l", col="blue")
				abline(v=marker$sample, col="red")
				text(marker$sample,  max(file[,O2]), marker$text, font=2, col="red")
				answer1 = as.character(readline("Would you like to add a marker here? y/n"))
				if (answer1 == "y") {
					answer2 = as.character(readline("Which marker would you like to add? (e.g. 1, 2, 3, B)"))
					answer3 = as.integer(readline("Please enter associated sample number:"))
					marker = rbind(marker[1:g,],c(answer3,NA,NA,NA, answer2),marker[(g+1):length(marker$text),])
					print(marker)
					abline(v=marker$sample, col="red")
				}
			}
		}	
	}
	rownames(marker)=1:nrow(marker)
	
	#Add additional if statements to add/remove markers here:
	
	#Lag correction
	lag_factor = 20 #set lag correction in seconds
	file[,O2] = lag.correction(file[,O2], lag_factor) #remove first samples in O2 channel using period indicated above
	file = file [-c((nrow(file)-lag_factor):nrow(file)),] #cut end of file by period indicated (which is now missing O2)
	
	#Identify first marker: this can be anything
	First_marker = marker$text[1] 
	if (First_marker=="B" | First_marker=="b") #if its a baseline, get rid of it
		{marker = marker[-1,]}
	
	#Identify last marker: this should be a baseline or it will throw warning allowing you to add the marker	
	End_marker = marker$text[length(marker$text)]
	if (End_marker!="B" & End_marker!="b"){ 
		#if the end baseline marker is missing
		plot(file[,O2], type="l", col="blue") #plot the O2 channel
		abline(v=marker$sample, col="red") #plot the markers
		text(marker$sample, max(file[,O2]), marker$text, font=2, col="red")
		warning("Marker denoting end baseline is indistinguishable.", immediate.=TRUE) #throw warning
		#Gives you two chances to add baseline and plots your work
		End_marker = as.integer(readline("Please enter sample number indicating ending baseline:"))
		abline(v=End_marker)
		answer = as.character(readline("Are you satisfied with marker placement? y/n"))
		if (answer=="n") {
			End_marker = as.integer(readline("Well shit. Try again:"))
			abline(v=End_marker)
		}
		marker[length(marker$text)+1,] = c(End_marker, NA, NA, NA, "B") #add this new marker to the marker table
	}

	marker$sample = as.numeric(marker$sample)
	num_base = length(marker$text[marker$text=="B" | marker$text=="b"]) #number of baselines in file
	num_mark = length(marker$text) #number of total markers in file
		
	#Baseline Correction
	nadir_length = 5 #averaging over this period of time
	skip = 60 #period in seconds to skip after switching between animals (to de-spike)
	#Set null values for the output
	beg_O2 = NA
	VO2min_1=NULL
	VO2min_2=NULL
	VO2min_3=NULL
	
	#sampling scheme matters!
	if (base_freq=="ind"){ 
		#if you baselined between each individual then you can correct O2 for each sample
		for (j in 1:num_base) { 
			Animal_in = marker$sample[marker$text!="b" & marker$text!="B"][j] #identify when you switched to animal
			Animal_out = marker$sample[marker$text=="b" | marker$text=="B"][j] #identify when you switched to baseline
			if (j<num_base){ #if this isnt the last baseline
				in_next = marker$sample[marker$text!="b" & marker$text!="B"][j+1] #identify when you switchd to the next animal
			} 
			else {in_next=length(file[,O2])} #if it is the last baseline, identify the end of the file
			if (Animal_in < 300) {tr=4} else {tr = 5} #if initial baseline < 5 mins, assume its 4 mins
			O2base_end = id.baseline(X=file[,O2], y=in_next, marker=Animal_out, period=2, beg=FALSE) #identify 3 min base after	
			O2base_beg = id.baseline(X=file[,O2], marker=Animal_in, time_range=tr, period=2) #identify 3 min baseline before
			base_O2 = baseline.value(file[,O2], O2base_beg[1], O2base_beg[2]) #identify O2 value during initial baseline
			if (j==1) {beg_O2 = base_O2} #if this is the first baseline, this is the O2 value to use in the correction
			sub = file[(O2base_beg[1]:O2base_end[2]),] #take subset including only interval from beginning to ending baselines
			sub$corrected.O2 = correct.baseline(file[,O2], O2base_beg, O2base_end, type="RMR") #baseline correct the O2 in the subsetted period only
			
				
			#VO2 correction		
			id = marker$text[marker$text!="b" & marker$text!="B"][j] #which individual is this in this period?
			if (Animal_out-Animal_in-2*skip<nadir_length*60) { #if animal wasn't measured for the length of time over which to calculate the nadir (designated above) throw warning
				warning("Length of time over which to calculate nadir (", nadir_length, " mins) is larger than the length of time over which animal ", id, " was measured. Nadir will be calculated over period of 5 mins instead.", immediate.=TRUE)
				nadir_length = 5 #defaults to 5 min nadir length under this scenario
			}
			if (id=="1") {flow=Flow1}; if (id=="2") {flow=Flow2}; if (id=="3") {flow=Flow3}
			sub$VO2 = transform.O2(sub$corrected.O2, sub[,flow], base_O2) #transform O2 channel based on initial O2 value
			low = nadir(sub$VO2[(Animal_in-O2base_beg[1]+skip): (Animal_out-O2base_beg[1]-skip)], nadir_length) #find the nadir
			low$sample = Animal_in-O2base_beg[1]+skip #during which sample did the nadir occur?
			if (low$mean > 0) { #ignore negative values
				if (id=="1"){ #if this is individual 1
					if (!any(names(VO2min_1)=="mean")) {
					VO2min_1 = low #if this is the first instance of ind 1, then this nadir is the lowest value
					} 
					if (low$mean < VO2min_1$mean) {VO2min_1 = low} #otherwise, check if this is lower than  previous value
					output$VO2min_1[i] = VO2min_1$mean #store the lowest value in the output
				}
				if (id=="2"){  #if this is individual 2
					if (!any(names(VO2min_2)=="mean")) {
						VO2min_2 = low #if this is the first instance of ind 2, then this nadir is the lowest value
					} 
					if (low$mean < VO2min_2$mean) {VO2min_2 = low}	#otherwise, check if this is the lower than previous value	
					output$VO2min_2[i] = VO2min_2$mean #store the lowest value in the output
				}
				if (id=="3"){  #if this is individual 3
					if (!any(names(VO2min_3)=="mean")) {
						VO2min_3 = low #if this is the first instance of ind 3, then this nadir is the lowest value
					} 
					if (low$mean < VO2min_3$mean) {VO2min_3 = low}	#otherwise, check if this is the lower than previous value	
					output$VO2min_3[i] = VO2min_3$mean #store the lowest value in the output
				}
			#Save a plot for each individual and each round
			pdf(paste("Plots/", sub("c_RMR_", "", sub(".exp", "", files[i])), "_VO2_Animal", id,"_Round", j,".pdf")) #O2 shown in blue
				plot(sub$VO2, type="l", col="blue", xlab="Time (s)", ylab="Resting Metabolic Rate (ml O2/min)")
				abline(v=low$period[1]:low$period[2] + low$sample, col="grey") #lowest region highlighted in gray
				lines(sub$VO2, col="blue") #replot O2 over this region
				lines(sub[,CO2], col="red") #CO2 shown in red
				text(Animal_in-O2base_beg[1], 0, id, font=2, col="red") #print ID
			dev.off()
			}
		}
	}
	
	#Save plot with original O2 trace to file
	pdf(paste("Plots/", sub("c_RMR_", "", sub(".exp", "", files[i])), "_O2.pdf"))
		plot(file[,O2], type="l", col="blue", xlab="Time (s)", ylab="Oxygen Concentration (%)") #O2 shown in blue
		abline(v=marker$sample) #markers
		text(marker$sample, (beg_O2*100), marker$text, font=2, col="red") #marker labels
	dev.off()			
}

write.csv(output, "RMR_output.csv")
