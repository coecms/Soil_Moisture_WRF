program main
    use mod_nc_reader
    use mod_wrf_writer
    implicit none
    real, dimension(20, 30) :: fake_data
    type(tp_wrf_data) :: wrf_data
    type(tp_nc_data) :: nc_data
    type(tp_nc_meta) :: nc_meta
    character(len=100) :: out_file_name

    nc_meta % file_name = "/short/public/sjj565/soil_moisture.nc"
    nc_meta % var_name = "SM000010"
    nc_meta % lat_name = "latitude"
    nc_meta % lon_name = "longitude"
    out_file_name = "Soil_Moisture.WRF"

    !wrf_data %

    call nc_read_data(nc_data, nc_meta)

    ! See http://www2.mmm.ucar.edu/wrf/users/docs/user_guide/users_guide_chap3.html#_Writing_Meteorological_Data
    wrf_data%version = 5
    call set_resolution(wrf_data, nc_data%nx, nc_data%ny)
    wrf_data%iproj = 0 ! cylindrical equidistant
    wrf_data%nlats = 0
    wrf_data%xfcst = 0.0
    wrf_data%xlvl = 0.0
    wrf_data%startlat = nc_data%startlat
    wrf_data%startlon = nc_data%startlon
    wrf_data%deltalat = nc_data%deltalat
    wrf_data%deltalon = nc_data%deltalon
    wrf_data%dx = 5.0
    wrf_data%dy = 5.0
    wrf_data%xlonc = 0.0
    wrf_data%truelat1 = 0.0
    wrf_data%truelat2 = 0.0
    wrf_data%earth_radius = 6371.0
    call set_slab(wrf_data, nc_data%slab)
    wrf_data%is_wind_grid_rel = .FALSE.
    wrf_data%startloc = "SWCORNER"
    wrf_data%field = "soilmoist"
    wrf_data%hdate = "2008:01:01_00:00:00"
    wrf_data%units = nc_data%units
    wrf_data%map_source = "CSIRO Marine and Atmospheric Research"
    wrf_data%desc = ""

    print *, "lats: ", wrf_data%startlat, wrf_data%deltalat
    print *, "lons: ", wrf_data%startlon, wrf_data%deltalon

    call wrf_write(out_file_name, wrf_data)

end program main

