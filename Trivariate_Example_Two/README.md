The purpose of the exercise is to revise the code in Trivariate_Example_Two such that it points to a new ICEES+ endpoint and invokes new variables and new data sources. The second example is perhaps more interesting.

The parameters for two new examples are provided below:

**Query 1:** This query invokes UNC Health Care System data only.

*Endpoint*

icees.renci.org:16339/apidocs

*Parameters*

* v3.0.0
* Year 2010
* Patient table
* TotalEDInpatientVisits
* AvgDailyPM2.5Exposure_StudyAvg_qcut
* Race

**Query 2:** This query invokes linked data from UNC Health Care System and the NIEHS Environmental Polymorphisms Registry.

*Endpoint*

icees.renic.org:16339/apidocs

*Parameters*

* v3.0.0
* Year 2016
* Patient table
* TotalEDInpatientVisits
* PM25_ANNUAL_AVERAGE_qcut
* RESPONDER_STATUS (note that I ignored 'Neither' and collapsed "Hypo/Super Hypo')

**Feature Discovery**

The query below runs the ICEES functionality that I mentioned in a previous email and allows users to explore all of the available feature variables + bins for a cohort of interest. This can be parameterized as desired to specify, for instance, JSON output, etc.

curl -X GET "https://icees.renci.org:16339/patient/2010/cohort/COHORT%3A1/features" -H  "accept: text/tabular"


