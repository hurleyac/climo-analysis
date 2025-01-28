# climo-analysis

Intro:
These files are a means of utilizing the R program that I wrote to find any significant climotological information about a location. This README.md file will tell someone how to change the files to use them for other locations.

The files:
There are three files:
- climo-analysis.Rmd
- analysis-Plots.R
- analysis-Rcode.R

These three files are run in R Studio, and constitute the innerworkings of how to create this r-pubs page: https://rpubs.com/hurleya/climateAnalysis

Structure:
The R Markdown (Rmd) file is run, which calls the other two files.

Data:
The scripts are pre-programmed to recognize some specific column titles for data. Additionally, they are programmed to recognize specific column numbers. You can tweak this within the analysis-Rcode.R file to adjust which columns you want to analyze. I've uploaded a sample dataset to show the current structure of the data being ingested.

analysis-Rcode.R:
1. point your script to where you have your .csv file located. You can replace all instances of this file with your location: C:/Users/hurley/OneDrive/Documents/MSDA Capstone/Raw Data/by site/KRDM.csv
2. currently, this script runs the same analysis for 8 locations. if you want to only run 1 location, then you will need to delete all the duplicate sections. orignally, the script had multiple loops within loops to be able to make it shorter, but due to the computational intensity, it would regularly hang (i put print statements to find where it would hang and it varied, depending on other processes that were occuring). Thus, in order to ensure it would run, i needed to split apart each instance for each location to be able to run them sequentially.
3. after you cut out all extra locations, find all instances of "krdm" and replace with the ICAO (4-letter airport or station identifier).
4. NOTE: there are multiple sections to this script: it finds and corrects null values; it creates splines for 10, 20, and 30yr datasets; it then calculates trends in the data for each 10, 20, and 30yr dataset; it then calculates an R^2 and adjusted R^2 matrix for all values for each 10, 20, and 30yr dataset.

analysis-Plots.R:
1. Similar to the previous script, you need to find the occurances of this file and change the location to your local storage: C:/Users/hurley/OneDrive/Documents/MSDA Capstone/images/krdm/TEMP Composite.png - NOTE: there are multiple .png files created for each location. it would be best to create a directory for your location(s) and use that leading up to the .png name. Eg: .../kind/TEMP Composite.png, .../ksfo/TEMP Composite.png, etc.
2. Similarly, replace all instances of "krdm" with your desired ICAO.
3. This creates all the plots (56 for each location)

climo-analysis.Rmd:
1. There are many instances of hard-coding into this file to speed up the process (it took ~3hrs to compile/run due to the complexity/computational intensity of the dependant scripts). So, pick what you want, cut what you want. I made use of lots of tabs to make interacting with the data simpler, without as much scrolling.
2. Something to keep in mind, it references the locations of the .pngs you created in the previous, 'Plot' script. Make sure if you use this layout you are changing the directory path so it will pull the correct picture. Also, you may want to remove the 14th Weather Squadron emblem (line 80) otherwise it will probably kill it or hang it up (after running the long scripts).

The .R scripts can be run independent of the .Rmd file. However, the analysis-Rcode.R script needs to be run prior to the analysis-Plots.R script, as the prior is dependent upon the later.
