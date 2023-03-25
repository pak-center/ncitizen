# ncitizen

![GitHub top language](https://img.shields.io/github/languages/top/pak-center/ncitizen?style=plastic)
[![Scc Count Badge](https://sloc.xyz/github/pak-center/ncitizen/)](https://github.com/pak-center/ncitizen/)
[![Scc Count Badge](https://sloc.xyz/github/pak-center/ncitizen/?category=code)](https://github.com/pak-center/ncitizen/)
[![Scc Count Badge](https://sloc.xyz/github/pak-center/ncitizen/?category=comments)](https://github.com/pak-center/ncitizen/)
[![Scc Count Badge](https://sloc.xyz/github/pak-center/ncitizen/?category=blanks)](https://github.com/pak-center/ncitizen/)
![GitHub repo size](https://img.shields.io/github/repo-size/pak-center/ncitizen?style=plastic)
![GitHub](https://img.shields.io/github/license/pak-center/ncitizen?style=plastic)

## CODE STILL IN ALPHA DEVELOPMENT PHASE

## Table of contents

* [General info](#general-info)
* [`SQ` mode Sequence Diagram](#sq-mode-sequence-diagram)
* [`MQ` mode Sequence Diagram](#mq-mode-sequence-diagram)
* [Technologies](#technologies)
* [Installation](#installation)
* [Features](#features)
* [Run the Project](#run-the-project)
* [TODO](#todo)
* [License](#license)

## General info

**Nakshatra Citizen** - COBOL PGM to extract citizens birthdates from BIG DATA in DB2 using SQL queries in batch processing.
Does simple Vedic Astrology calculations for 27 Nakshatras by grouping processing data into 27 collections.

Afterwards, does the analytics and displays the statistics with horizontal bar view. At the end, writes them to QSAM dataset as a report.

**The software has three operating modes:**
| **Mode**| **Name** | **Efficiency** | **Calculations** | **Description** |
|:---:|:---:|:---:|:---|:---|
| `SQ` | Single SQL Query | worst | mostly APP side | use iterations ROWSET Processing = 3200 |
| `MQ` | Multi SQL Queries | best | mostly DB side | use single ROWSET Processing = 121 |
| `DS` | Dataset mode |n/a | only APP side | prints previously saved historical records to the SPOOL area |

By design, it works on country identity documents and is limited to the size of the country's population. For global calculations it is necessary to correct the size of variables and add a list of databases from which data will be retrieved in turn or redesign DS mode and merge multiple records and PRINTS them as a single data sheet.

```cobol
      ******************************************************************
      *                      NAKSHATRA CITIZEN
      *                      (VEDIC ASTROLOGY)
      ******************************************************************
      *    ABOUT
      *          PGM SELECTS SQL QUERIES OF DATA FROM DB2 LIKE
      *          GENDER, ALIVE STATUS AND AGE. RANGE IS BEING
      *          COUNTED IN VARIABLES AND MOVED TO PROPER BOUNDRIES
      *          AND GROUPS OF NAKSHATRAS, AGE
      *          LAST BUT NOT LEAST IT DOES PERCENTAGE STATISTICS
      *          SHOWS GRAPHS LIKE: 40% = |####      |
      *                             90% = |######### |
      *
      ******************************************************************
```

## `SQ` mode Sequence Diagram

```mermaid
sequenceDiagram
    PGM(SQ)->>+DB2: SELECT current date
    DB2->>-PGM(SQ): Sending data
    PGM(SQ)->>DB2: Single SELECT SQ query
    loop EVERY ROWSET
        DB2->>PGM(SQ): Sending data ROWSET=3200
    end
    Note over PGM(SQ),DB2:loop iterations = <br/> total citizens / 3200
```

## `MQ` mode Sequence Diagram

```mermaid
    sequenceDiagram
    PGM(MQ)->>+DB2: SELECT lower & upper date SQL
    DB2->>-PGM(MQ): Sending data
    PGM(MQ)->>+DB2: SELECT MQ query
    Note right of DB2: heavy workload...
    Note left of PGM(MQ):fast processing the result
    DB2->>-PGM(MQ): Sending data as single ROWSET=121
    Note over PGM(MQ),DB2:Multi Queries
    PGM(MQ)->>+DB2: SELECT MQ query
    Note right of DB2: heavy workload...
    Note left of PGM(MQ):fast processing the result
    DB2->>-PGM(MQ): Sending data as single ROWSET=121    
```

## Technologies

Project is created with:

* COBOL
* EMBEDDED SQL (SQLCA) as DB2 CONNECTION
* JCL

## Installation

* Available z/OS COBOL compiler PTF
* DB2 Database
* QSAM dataset

## Features

SINGLE FILE CODE (NO INCLUDES, NO COPYBOOKS, NO CALLS)

| Feature       | % Progress | Status      |
| :---          | :---       | :---:       |
| SLOW SQL SQ   | ########## | Completed   |
| ROWSET for SQ | ########## | Completed   |
| FAST SQL MQ   | #####      | In Progress |
| ROWSET for MQ | ####       | In Progress |
| QSAM WRITE    | ########## | Completed   |
| QSAM READ     | #########  | In Progress |
| PRINT STATS   | ###        | In Progress |
| H-BAR CHARTS  | ####       | In Progress |
| Beautify code | ###        | In Progress |

## Run the Project

* JCL configuration to run:

> MULTIPLE ROWS SQL QUERY FOR ALL NAKSHATRAS IS NOT EFFICIENT but SIMPLE and fast in implementation in code!
> Default interval = 10

```jcl
PARM='SQ'
```

> For minimum interval = 1 year (PGM PRINTS 120 TABLE RAPORTS + SUMMARY)

```jcl
PARM='SQ,1'
```

> SINGLE - ONE ROW SQL QUERIES FOR EVERY NAKSHATRA GIVES BETTER PERFORMANCE BUT MAY NOT WORK FOR ALL DATABASES VERSIONS

```jcl
PARM='MQ,40'
```

> For maximum interval = 120 years (PGM PRINTS 1 TABLE RAPORT + SUMMARY)

```jcl
PARM='MQ,120'
```

> READ QSAM DATASET with log data records from earlier PGM run and only PRINT TO SPOOL THE VIEW (no SQL queries)

```jcl
PARM='DS'
```

> (when 120 years is divided by interval value with the rest, the interval is increased by 1 inside loop as it gives mod 0)

```jcl
PARM='MQ,73' inserted in JCL by the user is corrected to PARM='MQ,120' internally
```

## TODO

* optimaze SQL multi rows query for single row queries and move some PGM logic to SQL and advance queries for calculation on DB2 side (giving choice to user in JCL PARM: FAST adv sql queries or SLOW single simple query)
* clean code related with records and variables
* refactor multiplied code (more PARAGRAPHS)
* add WRITE RECORDS to QSAM dataset VB800 as backup and for future READ only FOR DISPLAY (no SQL queries)
* DISPLAY TO SPOOL user frendly reports
* create sample JCL for SUB
* DCLGEN

## License

[![License: GPL v3](https://img.shields.io/badge/License-GPLv3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
[Lines of code](https://img.shields.io/tokei/lines/github/pak-center/ncitizen?label=total%20lines%20of%20code&style=plastic)
