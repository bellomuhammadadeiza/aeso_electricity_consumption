# Impact of Temperature on Electricity Consumption Pattern in Alberta, Canada
###### Completion date (DD/MM/YYYY): 03/11/2024

## 1. Background

The connection between temperature and electricity consumption is a well-established concept, often described by a U-shaped pattern. When temperatures drop significantly during winter, electricity use rises as heating systems work harder to maintain warmth. Similarly, soaring temperatures increase reliance on air conditioning and cooling systems during summer, driving up electricity demand. However, this relationship isn’t uniform. Factors like local climate, urbanization, and industrial activity create variations in energy use. For example, urban areas with dense populations and industrial hubs tend to consume more electricity due to higher energy demands. Climate change adds another layer of complexity, as rising temperatures can exacerbate electricity needs, particularly in regions already struggling with energy shortages. This project, thus explores  these dynamics, by examining how seasonal temperatures  such as summer heatwaves and winter cold snaps affect electricity consumption across nine Alberta Electric System Operator (AESO) regions. 

### 1.1 Research Questions

1. What is the relationship between average summer and winter temperatures and per capita electricity consumption across different regions?

2. How do summer temperature spikes impact regional electricity consumption, and how does this vary across regions?

3. How do winter cold snaps impact regional electricity consumption, and how does this vary across regions?

## 2. Methodology

This study examines the relationship between temperature fluctuations and electricity consumption across nine Alberta Electric System Operator (AESO) regions from 2011 to 2018. The methodology involves the following key steps:

#### The Software package used for this project is "R"

- Data Preparation: 
   - Data Import and Cleaning
      - Imported datasets included population data, electricity consumption metrics, a dictionary linking municipalities to AESO regions, and geographic shapefiles.
      - Merged population data with the dictionary by Area ID and year to map population counts to specific AESO regions.
      - Aggregated population data by AESO Area ID and year, removing rows with NA values to ensure data completeness.
        
   - Temperature Data Preparation:
      - Imported and combined temperature data for all regions, converting it from wide to long format.
      - Standardized datetime columns and extracted time components (year, month) for consistent temporal analysis.

  - Electricity Data Transformation:
     - Renamed and reshaped electricity data to long format, extracting the year for consistent merging.
       
  - Merging Datasets:
     - Joined electricity data with aggregated population data using Area ID and year, calculating per capita electricity consumption.
     - Merged temperature data with the combined dataset using Area ID, year, and datetime as keys.
     - Conducted final data cleaning by removing rows with NA values to ensure data quality.
  
  - Visualization Strategy
      - Created spatial visualizations (maps) using sf and ggplot2 to show regional variations in electricity consumption and temperature.
      - Developed time series and clustered bar graphs to visualize the trends in temperature and electricity usage over time and assess seasonal impacts.

  - Econometric Analysis
     - Conducted regression analysis to evaluate the impact of seasonal temperature extremes (summer and winter) on electricity consumption.
     - Investigated the effects of July temperature spikes and December cold snaps on regional electricity usage.
     - Analyzed the overall impact of summer and winter temperatures on electricity consumption.
     - Applied log transformation to summer data to normalize distribution and stabilize variance, while avoiding log transformation for winter data due to negative values.


###### _For full details of the methodology, please read the report in this file attached_: [aeso_report.pdf](resources/aeso_report.pdf)

## 3. Results 

###### _The results can be found in the report attached: [aeso_report.pdf](resources/aeso_report.pdf)_


## 4. APPENDIX

- The data used for this project is available at  [aeso_data.zip](resources/data.zip)
- The R script, which includes step-by-step codes and comments for running all the analysis for the project, is available [here](resources/aeso_Rscript.R)
- The output of the analysis on R in ```.html``` is available [here](resources/aeso_output.html)
- All visual outputs from the analysis are also available at [here](resources/aeso_viz.zip)

