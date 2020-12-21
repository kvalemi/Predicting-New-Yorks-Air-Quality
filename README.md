## Project Description

In this project, I aimed to train and test numerous regression machines on the New York Air Quality dataset. My goal was to predict numeric Ozone count (ppb) from the provided features. By predicting Ozone, we can get a better idea about the air quality of New York city on any given day.

Please see below for details regarding the dataset:

**Format**
A data frame with 153 observations on 6 variables.

* Ozone	numeric	Ozone (ppb)
* Solar.R	numeric	Solar R (lang)
* Wind	numeric	Wind (mph)
* Temp	numeric	Temperature (degrees F)
* Month	numeric	Month (1--12)
* Day	numeric	Day of month (1--31)

**Details**
Daily readings of the following air quality values for May 1, 1973 (a Tuesday) to September 30, 1973.

* Ozone: Mean ozone in parts per billion from 1300 to 1500 hours at Roosevelt Island

* Solar.R: Solar radiation in Langleys in the frequency band 4000–7700 Angstroms from 0800 to 1200 hours at Central Park

* Wind: Average wind speed in miles per hour at 0700 and 1000 hours at LaGuardia Airport

* Temp: Maximum daily temperature in degrees Fahrenheit at La Guardia Airport.

**Source**
The data were obtained from the New York State Department of Conservation (ozone data) and the National Weather Service (meteorological data).

## Project Steps##

1) I fit several machines to this dataset and attempted to tune each in order to gain the best performance out of each machine.

2) After I tuned each machine, I then proceeded to benchmark and compare each fitted machine using repeated 10-fold cross validation.

* The benchmarking pipeline can be found in the benchmarking pipelinef ile.
