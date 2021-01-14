# GRRS
Global Ranavirus Reporting System

* Code for the interactive Shiny app at https://brunnerlab.shinyapps.io/GRRS_Interactive/
* Underlying data in GRRS_Map.csv 
    * Note that this file is created by Convert_GRRS_to_MapDat.R, from data pulled from a Google Sheet. 
      This file converts between dd mm ss and decimal degree, and 
      adds in columns for translated "Genus", "Family", and "Order" merged in from the Genera_good.csv file
* MetaData.csv contains the metadata describing the data
* GRRS_template.csv is a file to serve as a template for data entry for those who wish to add records
