# ozone-web-map
R Script to create ozone web map application | By Mark A. Prettyman and Shane Cone (https://github.com/microscone) 

This script analyzes historic ambient ozone concentration data in order to estimate the current year's "year-to-date" 4th maximum 8-hour average ozone concentration for the year for each monitor in the US, as well as the 3-year design value for ozone, to include the current year.  The resulting data is displayed as a web map, created with Leaflet for R.

The script obtains the current year maximum 8-hour average ozone concentrations for all monitors in the US from the AirNow API [1], for each day of the year through "yesterday."  The highest daily 8-hour ozone concentrations are listed by AirNow and ranked for each monitor in the US to determine the 4th maximum ozone concentration. Historic 4th maximum 8-hour average ozone concentrations are obtained from EPA's AQS Data Mart via the RAQSAPI package. [2]  Together, a draft 3-year design value for ozone can be calculated. 

Pre-formatted json files are used for state outlines and the nonattainment areas for the 2015 ozone NAAQS, along with a data file of monitors and the nonattainment area they are in.

The resulting web map can be saved as an HTML file, for easy web hosting.

The web map includes 2 layers of data for the ambient ozone monitors, as well as two selectable polygon layers.

- The "4th Max" layer represent the year to date 4th maximum daily 8-hour ozone concentrations for each monitor in the US. The points can be selected to view data about the monitor, the four highest ozone concentrations, and the number of days that have exceeded the 2015 ozone NAAQS of 70ppb. The "year to date" value will be given in the popup for each data point.
- The "DV" layer represents the draft 3-year ozone design value, for the current year.  The points can be selected to view data about the monitor; the 4th maximum ozone concentrations for 2019, 2020, and 2021; the draft 3 year design value, and the number of days that have exceeded the 2015 ozone NAAQS of 70ppb. The "year to date" value will be given in the popup for each data point.
- The "State Outlines" layer helps to delineate the state boundaries.
- The "2015 NAA Classifications" layer shows the original classification under the 2015 ozone NAAQS. Each nonattainment area can be selected, which lists the current monitor with the highest 3 year design value within the nonattainment area.


[1] https://docs.airnowapi.org/

[2] https://github.com/USEPA/RAQSAPI 
