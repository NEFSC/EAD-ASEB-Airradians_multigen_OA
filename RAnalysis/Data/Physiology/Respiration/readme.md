README


## Summary of folder contentst: **Respiration**


### About documents...


#### '<YYYMMDD>'

- each contains respiration data from that sampling date as yearmonthday

- summary of contents is as follows:

	- F1 Bay scallops
		- larvae/spat: NA
		- juveniles: 20220914, 20220930, 20221026
		- adults: 20220222, 20220301, 20220922, 20221026 (all text files with loligo system)

	- F2 Bay scallops
		- larvae/spat: 20220420 & 20220422 (failed spawn!), 20220824, 20220829, 20220830, 20220922 (SDR csv files)
		- juveniles: 20221116 (text files with loligo system)
		- adults: 20230131, 20230223 (text files with loligo system - note the vessels were remeasured and unique on 20230223 due to scrambled tubing!)

#### 'Reference_resp_ID'

- Date      - self-explanatory

- Fed_Unfed	- a food x OA expeimrent occured on dates 20220914, 20220930, 20221026, these two columns ONLY pertain to these dates, rest NA

- Food	    - same as Fed_Unfed, but not the later is abbreviated

- pH        - pH treatment, note this is simply the categorial value used to identify the treatment but the actual measured pH is not 8, 7.5 nor 7

- Replicate	- tanks where the animals derived, often as letters such as A, B, C, D

- Chamber_tank	- concatinated pH and Replicate for the tank ID (i.e. 7.5_A)

- Number	    - are there redundant samples by treatment + replicate for the measurements on that sampling day, for example, two individuals measured for 7.5 A in separate chambers, therefore the rows will be 1 and 2

- Channel	    - pertains to the respiration chamber ID, depending on the SDR sensor plate (.csv raw files) or the 8-channel loligo system (.txt raw files), the value here is a plate position (i.e. A1, A2, A3, etc.) or a channel number (i.e. CH1, CH2, etc.), respectively

- Plate	        - pertains only to data measured with the SDR 24-well plate system, as multiple plates can be tethered per run

- Run	        - number of the run, ordered by the time at which it was completed

- Num_indivs	- the number of individuals measured per well/channel - should only be >1 for early larval/spat trials with the SDR 24-well plate

- Filename      - the exact filename for the data - this will be called downstream in R as a unique identifier to merge information correctly



#### 'Reference_resp_size'

- the shell lenght, dry tissue weight, dry shell weight, and ash weight for each individuals

- note: these data only reference trails where a single individual was measured per well/channel, review 'Reference_resp_size_LARVAE' below

- Vessel_well_volume - the volume of the channel measured for respiration, loligo system refers to the 'vessel_volume' .csv file - call a categorical variable (i.e. x_small, small, large, etc.) to merge these rows with that of the 'vessel_volume.csv' file.  In cases with the SDR 24-well plate, all volumes are the same


#### 'Reference_resp_size_LARVAE'

- as opposed to 'Reference_resp_size', larval/spat were often measured with mutliple animals per well (always used SDR 24-well plate system for larvae/spat) - therefore we
need a separate format to track this data and call appropriately (calculating length mean for that specific well)


#### 'start.end_timestamps'

- in numerous trails, ammonia excretion was also measured with the respiration chamber water. In these cases, the start and end point are essential to calculate excression and total oxygen consumption- these can be used to examine O:N ratio!

#### 'vessel_volumes'

- note: pertains ONLY to runs with the 8-channel loligo system!

- contains the measured volume of the different vessels used for various life stages, contains the volume in mL for each vessel/channel and categorical variable (i.e. x_small, small, large, etc.) to merge these rows with that of the 'Reference_resp_size.csv' file.  In cases with the SDR 24-well plate, all volumes are the same and are note included in this file (added values to the 'Reference_resp_size.csv' file)
