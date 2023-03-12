# ncitizen
![GitHub top language](https://img.shields.io/github/languages/top/pak-center/ncitizen?style=plastic)
![Lines of code](https://img.shields.io/tokei/lines/github/pak-center/ncitizen?label=total%20lines%20of%20code&style=plastic)
![GitHub](https://img.shields.io/github/license/pak-center/ncitizen?style=plastic)
## CODE STILL IN ALPHA DEVELOPMENT PHASE
## Table of contents
* [General info](#general-info)
* [Technologies](#technologies)
* [Installation](#installation)
* [Features](#features)
* [Run the Project](#run-the-project)
* [TOO](#todo)
* [License](#license)
## General info
**Nakshatra Citizen** - COBOL PGM to extract dates of birth from BIG DATA in DB2 using SQL queries in batch processing.
Does simple Vedic Astrology calculations for 27 Nakshatras by grouping processing data into 27 collections. 
Afterwards, does the analytics and displays the statistics with horizontal bar view. At the end, writes them to the data set as a report.
## Technologies
Project is created with:
* COBOL 
* EMBEDDED SQL as DB2 CONNECTION 
* JCL
## Installation
* Available z/OS COBOL compiler PTF
* DB2 Database
* QSAM dataset access
## Features
SINGLE FILE CODE (NO INCLUDES, NO COPYBOOKS, NO CALLS)
| Feature       | % Progress | Status |
| ---           | ---        |--- |
| SLOW SQL SQ   | ########## | Completed |
| FAST SQL MQ   | ####       | In Progress |
| QSAM WRITE    | ########## | Completed |
| QSAM READ     | ########   | In Progress |
| PRINT STATS   | ## | In Progress |
| H-BAR CHARTS  | # | In Progress |
## Run the Project
* JCL configuration to run (when 120 years is divided by interval value with the rest interval value is increased by 1 as it gives mod 0):
> MULTIPLE ROWS SQL QUERY FOR ALL NAKSHATRAS IS NOT EFFICIENT but SIMPLE and fast in implementation in code!
> Default interval = 10
```jcl
PARM='SQ'
```
> For minimum interval = 1 year
```jcl
PARM='SQ,1'
```
> SINGLE - ONE ROW SQL QUERIES FOR EVERY NAKSHATRA GIVES BETTER PERFORMANCE BUT MAY NOT WORK FOR ALL DATABASES VERSIONS
```jcl
PARM='MQ,120'
```
> For maximum interval = 120 years
```jcl
PARM='MQ,120'
```
> READ QSAM DATASET with log data records from earlier PGM run and only PRINT TO SPOOL THE VIEW (no SQL queries)
```jcl
PARM='DS'
```
## TODO
* optimaze SQL multi rows query for single row queries and move some PGM logic to SQL and advance queries for calculation on DB2 side (giving choice to user in JCL PARM: FAST adv sql queries or SLOW single simple query)
* clean code related with records and variables
* refactor multiplied code (more PARAGRAPHS)
* add WRITE RECORDS to QSAM dataset VB800 as backup and for future READ only FOR DISPLAY (no SQL queries)
* DISPLAY TO SPOOL user frendly reports
* create sample JCL for SUB
## License
[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
