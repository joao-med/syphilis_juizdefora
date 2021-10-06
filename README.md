# Descriptive Analysis of Syphilis in Juiz de Fora

The main objetctive of this project is to create vizualizations and descriptive analysis for discussion of the percieved results in scientific article.

# Fecthing Data

Data herein are available in http://indicadoressifilis.aids.gov.br/. Moreover this data was processed in Excel and futher introduced to a R project.

# Methodology

A quantitative, descriptive and retrospective study of time series analysis was carried out using secondary data of compulsory notifications of AS, SPW and CS made in the city of JF. These data are collected by the healthcare institutions of the municipality and forwarded to the Notifiable Diseases Information System (SINAN) that processes the information. Later, after being processed, this data is offered by the Department of Chronic Diseases and Sexually Transmitted Infections (DCCI) online. Furthermore, population data was collected from the MH information system, DATASUS.

Notifications follow specific diagnostic criteria and their frequencies were calculated as the ratio between reported cases and local population multiplied by 100,000 for SA and reported cases on live births in that location multiplied by 1,000 for SPW and SC. Ideally, the SA denominator should be individuals over 13 years old, since according to the Information Note No.2-SEI/2017, any case of syphilis in individuals under 13 years of age is considered SPW. However, the estimated population provided by DATASUS does not offer that specific range. Moreover, in other to have understand adult’s syphilis cases as a whole, one of our analysis merged SA and SPW, having in mind that pregnant women diagnosed with syphilis are notified only as SPW and not SA.

After data collection, an analysis of data completeness was performed, in which variables with unknown data greater than 20% were excluded from the analysis. The criteria used for this analysis were proposed by the Economic Commission for Latin America (ECLAC). ECLAC stratifies the data according to the following degrees: excellent, when there is less than 5% incomplete information, good 5% to 10%, fair 10% to 20%, poor 20% to 50% and very bad 50% or more (18, 19).

Following the completeness analysis, exploratory data analysis was performed using simple absolute and relative frequencies. To describe the variation of the annual rates of each type of notification, a linear model of the natural logarithm of the data was used in order to estimate the coefficients for an exponential regression. All files and scripts produced can be found in github.com/joao-med/syphilis_juizdefora.

A 95% confidence interval (95% CI) and statistical significance of p<0.05 were established. In addition, the value of the coefficient of determination (R²) was calculated to better understand how well the model fits the data. This value is represented from 0 to 1 and, the higher the R², the better the model explains the variability of the data around its mean.

Both data and graphic visualizations were analyzed and produced using Microsoft Excel and the software The R Project for Statistical Computing (R version 4.1.0).


