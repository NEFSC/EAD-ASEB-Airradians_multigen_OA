README


## Summary of folder contentst: **Biodeposition**


### About documents...

* folders divided by generation **(F1, F2)**

**NOTE**: measurements were done at different temperatures, as follows:

- F1s
	- 20220302: 16C
	- 20220923: 20C
	- 20221027: 12C

- F2s
	- 20230201: 16C
	- 20230224: 16C
	- 20230328: 16C

### (i) 'cumulative_raw' folder

* contains the raw master file data and **only measured values**

	* Question: What is this *only measured values* nonsense?

 		Answer: Raw data aquired from our biodeposition method is a timeseries of filtered water samples - samples for experimental blanks, the input seawater (before flow to the system), and the manually picked feces and pseudofeces for each individual. All other data are calculated from these values, with the exclusion of a b-factor constant  

	* Question: Why begin with these data?

		Answer: These data are static. All calculations and data carpentry is dependent on these core 'raw' values remaining unchanged -- using **commented scripts** (such as R) allows us to **track every change** without inadvertent errors to the raw data (e.g. copy-paste, omissions, etc.)

### (ii) 'worksheets' folder


 * the **downloaded google sheets** sorted by date of data collection

 * contains the raw data and all data calculations in excel format

 	* Question: Is this not redundant? Why *also* use raw data and scripts?

	Answer: Ideally yes, this is redundant. However, evoking all excel calls on static raw data using R prevents an array of foreseeable challenges -- for example, when a particular data points or threshold criterion changes, we do not depend on memory or a commented spreadsheet cell, but a command and comment in a script.

### Notes:

* Important! The team decided to use the 'blank' seawater to standardize our input conditions.
	Though we still measured filters for both blanks and the input for F1s, the F2s, **do not use the data for 'input' (only blanks!)**

	* Why? the blank represented the seawater+algae conditions that the animals would experience, whereas the 'input' is the seawater before entering the head tank. Importantly, algae is introduced to the head tank from a peristaltic pump separate from the seawater inflow therfore 'input' also does not represent the added food. 'Blank' is the best representation to normalize our data.
