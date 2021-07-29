# HDNG

The HDNG is the Historische Database Nederlandse Gemeentes (Historical Database Dutch Municipalities) and contains a wide range of historical indicators for Dutch municipalities for the 19th and 20th centuries. The HDNG dates from the floppy disk era and has been made available online by Onno Boonstra. Hitherto, there have been 4 versions of the database:

## Version history

**Version 1 (HED, 1970-medio 1990):** 
- Data gathered under supervision of Hans Knippenberg.
- Contains demographic indicators stored in wide format. 
- Using municipality name and CBS-code as unique identifyers.
- Information was stored on 7 floppy disks in .POR (SPSS portable) files.
- Variable names were coded as 8 digits, which were interpretable via a look-up table.

**Version 2 (HDNG, 2003):** 
- Was published online by Onno Boonstra as 11 separate csv files.
- Added the Amsterdam-codes for all Dutch municipalities.
- Dropped 14 variables.

**Version 3 (HDNG, 2020):** 
- Published the HDNG in one csv file. 
- Transformed the HDNG from a wide format into long format, so that look-up tables are no longer necessary.
- Added variables from the Historisch-Ecologische Database (HED) on religion.
- Added provincial and national aggregates from the Historisch-Ecologische Database (HED).
- Returned the 14 dropped variables.

**Version 4 (HDNG, 2021):** 
- Dropped redundant columns from previous versions.
- Adds descriptive fields to the HDNG to make the database more easily queryable.
- Adds visualisation descriptions to the HDNG, to enable hookups with NLGIS or other shapefiles.
- Redundant rows have been removed from the database. 


## Script overview

**1. transform HDNG from separate files into long format.R** transforms the HDNG v2 into long format.

**2. transform HDNG_long to HDNG+HED.R** adds information from the HED to the HDNG.

**3. provincietotalen.R** puts the provincial and national aggregates from the HED into long format.

**4. add missings and provincietotalen to HDNG+.R** adds provincial and national aggregates from the HED, as well as the 15 variables dropped in HDNG v2.

**5. filter existing municipalities.R** removes entries for non-existing municipalities.


## Variable description
- **amco**  provides the Amsterdamse code, a 5-digit number to refer to municipalities (https://nl.wikipedia.org/wiki/Amsterdamse_code)
- **name**  the name of the municipality as provided in the HDNG
- **variable**  the original 8-digit variable name consisting of a topic (A-H), year (809-997), and variable (3 letters).
- **description** queryable summary of the available variables in the HDNG.
- **information** actual variables in the HDNG.
- **sex** male (M), female (F), both sexes (-T), or not applicable ( ) 
- **year**  the year for which data is available
- **visualisation_year**  the year for which municipal borders have been used
- **value** score on description
- **sources**  reference to source in ... 
- **remark**  reference to remarks in ...
