## 2025-02-17: Acquire Data Milestone
### Project Goals
The goal for this project is to learn more about the factors of fatal and non-fatal car crashes, such as where they occur, when they occur, and what driving hazards may have been involved with the crash. Driving is a crucial part of American life, and it is important to maintain as much safety as possible when driving. I believe that much of the unsafe driving behavior seen on the roads stems from drivers not realizing how dangerous unsafe driving can be. By the end of the project, I hope to teach my audience, the driving population of Iowa, the dangers of driving they may not realize are there, and to encourage safe driving behavior. 

The scientific questions I will try to address include the following:

What conditions are most prominent in fatal car crashes?
Is it possible to predict whether a crash will be fatal using the factors recorded?
What, if any, are the ways to minimize the possibility of a fatal crash?
What are the main differences between fatal and non-fatal car crashes?

### Data Wrangling
The data that has been collected is a single csv that contains 60,000 rows and 37 columns. Each row contains information about an individual car crash that occurred in Iowa from 2014 to 2025. Considering the size of the file, I was expecting R to be slow or to crash when working with the data, but there have been zero problems. So far, I created bar graphs for the variables I was most interested in. I also looked at the unique values for some of the columns, which made me realize that there were almost too many unique values for some of the variables, such the variable "Major Cause" that has 74 unique values. In addition, some of the values of the columns are too long and are repetitive. For example, the months are written as "01-January", which is redundant. I refactored the columns that were related to dates so that the values would fit easier on a bar graph, but I have not yet worked with the issue of too many unique values. For this issue, I intend to wait to clean a variable until I am certain that I want to use it because I believe it will be a long process to clean. Lastly, there are some values of the variables that appear to mean the same thing. For example, "N/A", "None apparent", and "Unknown" may all mean the same thing for environmental conditions, and it might be a good idea to group all of these responses together. 
