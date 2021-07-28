# HDNG

The HDNG is the Historische Database Nederlandse Gemeentes (Historical Database Dutch Municipalities) and contains a wide range of historical indicators for Dutch municipalities for the 19th and 20th centuries. The HDNG dates from the floppy disk era and has been made available online by Onno Boonstra. Hitherto, there have been 4 versions of the database:

## Version history

**Version 1 (HED):** 
- Contains demographic indicators stored in wide format. 
- Using municipality name and CBS-code as unique identifyers.
- Information was stored on 7 floppy disks in .POR (SPSS portable) files.
- Variable names were coded as 8 digits, which were interpretable via a look-up table.

**Version 2:** 
- Was published online by Onno Boonstra as 11 separate csv files.
- Added the Amsterdam-codes for all Dutch municipalities.
- Dropped 15 variables.

**Version 3:** 
- Published the HDNG in one csv file. 
- Transformed the HDNG from a wide format into long format, so that look-up tables are no longer necessary.
- Added variables from the Historisch-Ecologische Database (HED) on religion.
- Added provincial and national aggregates from the Historisch-Ecologische Database (HED).
- Returned the 15 dropped variables.

**Version 4:** 
- Dropped redundant columns from previous versions.
- Adds descriptive fields to the HDNG to make the database more easily queryable.
- Adds visualisation descriptions to the HDNG, to enable hookups with NLGIS or other shapefiles.
- Redundant rows have been removed from the database. 


## Script overview

**1. transform HDNG from separate files into long format.R** transforms the HDNG v2 into long format.

**2. transform HDNG_long to HDNG+HED.R** adds information from the HED to the HDNG.

**3. provincietotalen.R** puts the provincial and national aggregates from the HED into long format.

**4. add missings and provincietotalen to HDNG+.R** adds provincial and national aggregates from the HED, as well as the 15 variables dropped in HDNG v2.

**5. filter existing municipalities.R** removes entries for non-existing and duplicated municipalities.
