Started 3 September by Lizzie

These files were received to Lizzie in 2019 (maybe also 2018? I forget ...) from winegrowing companies in the Okanagan region. 

<><><><><><><><><><><><><><><><><><><>
<> Organizing the BC phenology data <>
<><><><><><><><><><><><><><><><><><><>

What we eventually want:
- We want eventually to create a PHENOLOGY file with the following columns:
	- company (e.g., Arterra or SebastianFarms)
	- vineyard
	- variety
	- block
	- year
	- month
	- day
	- event (budburst, flowering, veraison, brix ... we may also have other data such as yield)
	- value (e.g., for Brix give the value -- for flowering etc. give the % flowering ... you may need to review notes closely or ask Lizzie to confirm  with growers)	
	- notes
	- other columns you think of!

- We also want climate data in a separate file:
	- company (e.g., Arterra or SebastianFarms)
	- vineyard
	- variety
	- block
	- year
	- month
	- day
	- precip (daily)
	- min (daily)
	- max (daily)
	- mean (if given)

How do we want it:
- Unless impossible all data cleaning should occur in R (save files from Excel as CCV formats then clean in code). Cleaning code should go in a NEW folder called analyses/cleaning and CSV files should go in analyses/input (another new folder you need to create). You can write out new files to analyses/output. *I recommend you write one script for each company's phenology data and each company's weather data...) and one file to merge phenology data and one file to merge climate data.
- Do NOT use special characters, avoid spaces when possible. 
- You may need to transcribe data from maps etc. and then merge with other files to get all the data together (e.g., variety often is mapped, but not in data file)
- Make a notes folder (.txt) of progress, questions etc.!

