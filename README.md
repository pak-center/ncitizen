# ncitizen
**Nakshatra Citizen** - COBOL PGM to extract dates of birth from BIG DATA in DB2 using SQL query in batch processing. 

Does simple Vedic Astrology calculations for 27 Nakshatras by grouping processing data into 27 collections. 

Afterwards, does the analytics and displays the statistics. At the end, writes them to the data set as a report.

---
## CODE STILL IN DEVELOPMENT PHASE

**TODO**
- optimaze SQL multi rows query for single row queries and move some PGM logic to SQL and advance queries for calculation on DB2 side (give choice to user in JCL PARAM to choose: adv sql queries or single simple query)
- clean code related with records and variables
- refactor multiplied code (more PARAGRAPHS)
- add WRITE RECORDS to QSAM dataset VB800 as backup and for future READ only FOR DISPLAY (no SQL Queries)
- DISPLAY TO SPOOL user frendly reports
- create sample JCL for SUB

--- 
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
