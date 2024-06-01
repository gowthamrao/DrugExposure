DrugExposure
================


[![Build Status](https://github.com/OHDSI/DrugExposure/workflows/R-CMD-check/badge.svg)](https://github.com/OHDSI/DrugExposure/actions?query=workflow%3AR-CMD-check)
[![codecov.io](https://codecov.io/github/OHDSI/DrugExposure/coverage.svg?branch=main)](https://codecov.io/github/OHDSI/DrugExposure?branch=main)

Introduction
============

THIS PACKAGE IS UNDER ACTIVE DEVELOPMENT. IT IS NOT PART OF HADES.

DrugExposure is an R package designed to understand medication compliance and persistence in cohort studies. 

Features
========
- Calculate key adherence metrics such as medication possession ratio and proportional days covered, with and without stockpiling.
- Assess drug utilization from first exposure through user-defined persistence windows.
- Visualize adherence patterns and statistics through both tabular and graphical outputss.


Technology
============
DrugExposure is an R package.

System Requirements
============
Requires R (version 3.6.0 or higher). 

Installation
=============
1. See the instructions [here](https://ohdsi.github.io/Hades/rSetup.html) for configuring your R environment, including RTools and Java.

2. In R, use the following commands to download and install DrugExposure:

  ```r
  install.packages("remotes")
  remotes::install_github("ohdsi/DrugExposure")
  ```

User Documentation
==================
Documentation can be found on the [package website](https://ohdsi.github.io/DrugExposure).

PDF versions of the documentation are also available:
* Package manual: [DrugExposure.pdf](https://raw.githubusercontent.com/OHDSI/DrugExposure/main/extras/DrugExposure.pdf)

Support
=======
* Developer questions/comments/feedback: <a href="http://forums.ohdsi.org/c/developers">OHDSI Forum</a>
* We use the <a href="https://github.com/OHDSI/DrugExposure/issues">GitHub issue tracker</a> for all bugs/issues/enhancements

Contributing
============
Read [here](https://ohdsi.github.io/Hades/contribute.html) how you can contribute to this package.

License
=======
DrugExposure is licensed under Apache License 2.0

Development
===========
DrugExposure is being developed in R Studio.

### Development status

DrugExposure is under development.
