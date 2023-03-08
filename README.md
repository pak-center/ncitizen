# ncitizen
## CODE STILL IN ALPHA DEVELOPMENT PHASE
## Table of contents
* [General info](#general-info)
* [Technologies](#technologies)
* [Features](#features)
* [TOO](#todo)
* [License](#license)
## General info
**Nakshatra Citizen** - COBOL PGM to extract dates of birth from BIG DATA in DB2 using SQL query in batch processing.
Does simple Vedic Astrology calculations for 27 Nakshatras by grouping processing data into 27 collections. 
Afterwards, does the analytics and displays the statistics with horizontal bar view. At the end, writes them to the data set as a report.
## Technologies
Project is created with:
* COBOL 
* SQL 
* JCL
## Features
| Feature       | Status        |
| ------------- | ------------- |
| SLOW SQL SQ   | Completed     |
| FAST SQL MQ   | In Progress   |
| QSAM WRITE    | Completed     |
| QSAM READ     | In Progress   |
| PRINT STATS   | In Progress   |
| H BAR CHARTS  | In Progress   |
## TODO
* optimaze SQL multi rows query for single row queries and move some PGM logic to SQL and advance queries for calculation on DB2 side (giving choice to user in JCL PARM: FAST adv sql queries or SLOW single simple query)
* clean code related with records and variables
* refactor multiplied code (more PARAGRAPHS)
* add WRITE RECORDS to QSAM dataset VB800 as backup and for future READ only FOR DISPLAY (no SQL queries)
* DISPLAY TO SPOOL user frendly reports
* create sample JCL for SUB
## License
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
