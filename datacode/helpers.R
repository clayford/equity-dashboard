# Text for helpers()

helpers <- list(counties = c("<b>Counties:</b> select one or more counties of interest to view. The localities selected appear on the map and as observations in the figures below. Use the Select/Deselect All button below to make many changes quickly."), 
                
               geo = c("View indicators at the county level, the census tract level, or the block group level. Some indicators are only available at the tract or locality level.",
               "<b>County:</b> The counties and cities within the region.", "",
               "<b>Census Tract:</b> The Census Tract is an area roughly equivalent to a neighborhood generally encompassing between 2,500 to 8,000 people. They are designed to be relatively homogeneous with respect to population characteristics, economic status, and living conditions.", "",
               "<b>Block Group:</b> The block group is a cluster of adjacent census blocks within a census tract. Block groups generally contain between 600 and 3,000 people. This is the smallest geographical unit for which the census publishes sample data."), 
               
               category = c("There are 30+ indicators currently available, with more in the works. You may find filtering the available indicators to a specific category helpful to view included measures or locate a measure of interest. Selecting a category will limit the choice of indicators in the Primary and Secondary Indicator fields.",
               "<b>Health:</b> Health insurance rates and life expectancy.", "",
               "<b>Housing:</b> Number and rates of vacant housing, home ownership rates, percent rent burdened.", "",
               "<b>Indices:</b> Measures derived from combined indicators including human development index, gini inequality index, and measures of residential dissimilarity.", "",
               "<b>Jobs, Wages & Income:</b> Median household income and personal earnings, poverty and unemployment rates.", "",
               "<b>People:</b> Basic demographics including total population, racial and ethnic populations, populations by age group.", "",
               "<b>Youth & Education:</b> Educational attainment, school enrollment, child poverty rates."),
               
               indicator = c("Select an indicator to view on the Primary map. The distribution of the selected indicator will also be plotted below. There are 30+ indicators currently available in the following categories:", "",
                             "<b>Health:</b> Health insurance rates and life expectancy.", "",
                             "<b>Housing:</b> Number and rates of vacant housing, home ownership rates, percent rent burdened.", "",
                             "<b>Indices:</b> Measures derived from combined indicators including human development index, gini inequality index, and measures of residential dissimilarity.", "",
                             "<b>Jobs, Wages & Income:</b> Median household income and personal earnings, poverty and unemployment rates.", "",
                             "<b>People:</b> Basic demographics including total population, racial and ethnic populations, populations by age group.", "",
                             "<b>Youth & Education:</b> Educational attainment, school enrollment, child poverty rates."), 
               
               indicator2 = c("Select an indicator to view on the Secondary map. The distribution of the selected indicator will also be plotted below, including a plot showing the correlation with the Primary indicator."), 
               
               time = c("<b>Under Development.</b> Some indicators are available for multiple years. Select the year you wish to view. Currently, over time data is only present for total population and population by race or ethnicity."), 
               
               map_geo = c("Select a base map. The detailed map will show roads and other features more clearly, but viewers may find the detail distracting."),
               
               distribution = c("The distribution, or histogram, shows how often each value of an indicator occurs in the selected data (defined by locality and geography level). The distribution quickly shows the minimum and maximum values as well as the range of the most common values of an indicator."),
               
               correlation = c("The correlation, or scatterplot, shows the relation between the primary and secondary indicators across geographic units; this helps us see if two indicators are associated and, if so, how -- e.g., as the value of the primary indicator increases, does the value of the secondary indicator notably increase or decrease. It can also help us locate geographic units that have extreme values on both measures."),
               
               race = c("<b>Under Development</b>. The comparison by race is intended to show, where available, how an indicator like household income differs by racial and ethnic populations. Currently only available when Median Household Income or Life Expectancy at Birth is selected as the primary indicator."))