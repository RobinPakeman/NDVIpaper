READ.ME

If using the data from the Soay Sheep Project [meteorological data or sheep population data] it is advised that contact the authors as the project data is continually being added to and checked. The files here are only appropriate for use with this particular analysis. 


1. Prediction of Hirta weather prior to installation of weather stations

WeatherAnalysis.R
WeatherForAnalysis.csv


2. Accessing Google Earth Engine Landsat Data

GEE_code.txt


3. Analysis of NDVI change for each vegetation type

TimeSeries_slopes.R
AllHabitats_WithDates2022.csv [NDVI data per habitat]
AllDates.csv [All potential dates for Landsat image]
Traits.csv [Community Weighted Mean Traits for each vegetation type]


4. Calculation of summer maximum NDVI per vegetation type

TimeSeries_MaxMin2.R
AllHabitats_WithDates2022.csv [NDVI data per habitat]
AllDates.csv [All potential dates for Landsat image]
Traits.csv [Community Weighted Mean Traits for each vegetation type]


5. Linear modelling of drivers of population change

TimeSeries_maxmincorrelation_cleardays.R
MxDay1.csv [Output from TimeSeries_MaxMin2.R]
HirtaPop_simplified.csv [Sheep population data]
NAO.csv [North Atlantic Oscillation data]
PredictedMonthyl.csv [Output from WeatherAnalysis.R]
TrainGapsFilled.csv [Output from WeatherAnalysis.R]

