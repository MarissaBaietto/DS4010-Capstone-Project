### The data files are stored in the data/ directory and are organized as follows:

data/crashes_raw/: Contains the original dataset sourced from the Iowa Department of Transportation website

Contains the original, unprocessed data files split into 13 parts due to upload size restrictions. Each part contains 50,000 rows of the original data file, with "Vehicle_Crashes_Part_1.csv" containing teh first 50,000 rows.

data/RDS/: Contains the clean and transformed data in the form of an RDS file. This data will potentially be split into parts as well depending on whether the data will be sized down throughout data processing. 
