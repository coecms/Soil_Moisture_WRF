Soil Moisture
=============

This program is meant to convert a NetCDF Soil Moisture file into the intermediary format for WRF.

It consists of three source files:

* **main.f90** contains the main program and the configuration. Yes, you have to configure in the source code for now.
* **mod_nc_reader.f90** contains the routine to read the soil moisture levels from the netCDF file. 
  It contains two type declarations:
   * **tp_nc_meta** contains the file name and the variable names for latitude, longitude, and the field
   * **tp_nc_data** contains all the variables that get read out of the netCDF file, including some meta-data like *units*.
* **mod_wrf_writer.f90** contains the routine to safely copy the data, and a routine to write all the data into a WRF intermediary file.
  The method of writing is basically a copy of http://www2.mmm.ucar.edu/wrf/users/docs/user_guide/users_guide_chap3.html#_Writing_Meteorological_Data.


