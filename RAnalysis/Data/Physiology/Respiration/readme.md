README


## Summary of folder contentst: **Respiration**


### About documents...

### (i) 'raw' folder
#### folders as 'YYYMMDD'

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

  - F2 Bay scallops (larval full reciprocal OA to D-hinge)

	NOTE: these larvae are NOT representative of the F2 adults we have for genomics and were spawned for F3s, this was a full-reciprocal experiment with a 2023 spawn of F1s!  

    - D-hinge larvae day 2: 20230316

  - F3 Bay scallops (larval full reciprocal OA until settlement)
	NOTE: this full reciprocal experiment is overlapped with the F3 grow out of the low, mid and high pCO2 cohorts (unlike the F2 full reciprocal larvae experiment). Therefore, the larvae are representative of the same cohorts for genomics of F3s

	- D-hinge larvae day 2: 20230407 - no size data, based on individual counts - 80 ul plates

	- Larvae day 7: 20230412 - no size data, based on individual counts - 80 ul plates

	- Larvae day 16: 20230421 - no size data, based on individual counts - targeted a single individual per tank picked by hand (other timepoints measured volumetrically) - 80 ul plates

	- Spat: 20230518 - length data with imageJ (phots on google drive), single individual per tank picked by hand - 200 ul plates

### (ii) 'metadata' folder

#### 'Reference_resp_ID'

- Date      - self-explanatory

- pH_parents - only applied for the full-reciprocal larval experiments where parent x offspring pCO2 (pH) was assessed (dates: 20230316, 20230407, 20230412, 20230421)

- pH        - pH treatment, note this is simply the categorial value used to identify the treatment but the actual measured pH is not 8, 7.5 nor 7

- Replicate	- tanks where the animals derived, often as letters such as A, B, C, D

- Chamber_tank	- concatenated pH and Replicate for the tank ID (i.e. 7.5_A)

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

- as opposed to 'Reference_resp_size', larval/spat were often measured with multiple animals per well (always used SDR 24-well plate system for larvae/spat) - therefore we
need a separate format to track this data and call appropriately (calculating length mean for that specific well)

#### 'Reference_resp_count_LARVAE'

- as opposed to 'Reference_resp_size' and 'Reference_resp_size_LARVAE'. larval were too small to take photos prior or while loaded into the SDR wells. The 24-well plates were filled volumetrically (typically 10 ul each)
from a small volume (~50-60 mL) of all larvae contents per tank. three-Four measurements of 10 ul from the condensed larvae was measured to estimate how many larvae, on average, are in the 10ul sample loaded for respirometry.
This data sheet only pertains to the early larvae measurements on  20230316 (F2 full reciprocal OA), 20230407, 20230412, and 20230421 (F3 full reciprocal OA)

#### 'start.end_timestamps'

- in numerous trails, ammonia excretion was also measured with the respiration chamber water. In these cases, the start and end point are essential to calculate excression and total oxygen consumption- these can be used to examine O:N ratio!

#### 'vessel_volumes'

- note: pertains ONLY to runs with the 8-channel loligo system!

- contains the measured volume of the different vessels used for various life stages, contains the volume in mL for each vessel/channel and categorical variable (i.e. x_small, small, large, etc.) to merge these rows with that of the 'Reference_resp_size.csv' file.  In cases with the SDR 24-well plate, all volumes are the same and are note included in this file (added values to the 'Reference_resp_size.csv' file)
