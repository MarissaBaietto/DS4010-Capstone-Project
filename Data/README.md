### The data files are stored on Google Drive and are organized as follows:

## data/csv: Contains the links to all csv files.
  Unique Crashes.csv: A cleaned dataset resulting from the merge of Crash_Data.csv and Vehicle_Data.csv. These two datasets were joined on the attributes "Iowa.DOT.Case.Number" (from Crash_Data) and "CASENUMBER" (from Vehicle_Data). After merging, the data was filtered to ensure that each unique vehicle crash is represented only once by selecting the first occurrence for each crash.
  Iowa Counties.csv: A dataset depicting the total number of crashes for each county in Iowa, by year. It was created using data from the "mapdata" and "maps" packages in R and the Unique_Crashes.csv data. 
  Crash_Data.csv: A raw dataset sourced from the Iowa Department of Transportation website, linked here: https://data.iowadot.gov/datasets/IowaDOT::crash-data-sor/about. This dataset contains information for every crash that occurred in Iowa from 2014 to 2025. 
  Vehicle_Data.csv: A raw dataset sourced from the Iowa Department of Transportation website, linked here: https://data.iowadot.gov/datasets/IowaDOT::crash-vehicle-data-sor/about. This dataset contains information for every vehicle involved in each crash that occurred in Iowa from 2014 to 2025. 

## data/rds: Contains the links to all rds files.
  Unique_Crashes.rds: The RDS file version of the Unique_Crashes.csv dataset.
  Iowa_Counties.rds: The RDS file version of the Iowa_Counties.csv dataset.
  state.rds: An RDS file containing geographic data for the state border of Iowa.
  iowa.rds: An RDS file containing geographic data for the county borders within the state of Iowa.
