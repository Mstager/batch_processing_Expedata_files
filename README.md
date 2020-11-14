# batch_processing_Expedata_files
*Because manually processing hundreds of files with the proprietary software is terrible*

This is a series of R scripts that I wrote to automatically process respirometry files generated by Sable Systems Expedata software. They can be used to process hundreds of files in seconds and offer high reproducibility among users. 

This repository includes two workflows specifically for calculating VO2max or basal/resting metabolic rate according to Lighton 2008. They assume that you've recorded flow rates, O2, and CO2 concentrations. It is also preferable to use markers to indicate when measurements began and ended on an animal (though prompts allow you to add these manually if missing). They perform automatic linear correction for baseline drift, as well as linear correction of flow rates if compressed gas with densities different than ambient air (e.g. heliox) were used. These scripts can be used to process files containing multiple individuals at once and can easily be modified to account for wonky stuff that happens during respirometry, like clogs in ascarite, etc. Each workflow produces a table containing the metric of choice, as well as two pdf plots illustrating both the raw data and the corrected VO2.

I have tried to make these accessible though they are written with the Cheviron lab's setup in mind. I am always happy to improve upon their utility if others require additional features.