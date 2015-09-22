module mod_nc_reader
    implicit none
    type tp_nc_meta
        character(len=100) :: file_name
        character(len=20) :: lon_name, lat_name, var_name
    end type tp_nc_meta
    type tp_nc_data
        integer :: nx, ny
        real, dimension(:, :), allocatable :: slab
        real :: startlat, startlon
        real :: deltalat, deltalon
        character(len=25) :: units
    end type tp_nc_data
contains

    subroutine nc_read_data(nc_data, nc_meta)
        use netcdf
        implicit none
        type(tp_nc_data), intent(inout) :: nc_data
        type(tp_nc_meta), intent(in) :: nc_meta
        integer :: ncid
        integer :: dimid_long, dimid_lat
        integer :: varid, varid_lon, varid_lat

        real, dimension(:), allocatable :: lats, lons
        real, dimension(:, :), allocatable :: slab

        call check(nf90_open(nc_meta % file_name, NF90_NOWRITE, ncid), "opening file")

        ! get the longitude size
        call check(nf90_inq_dimid(ncid, nc_meta % lon_name, dimid_long), "getting dimid_long")
        call check(nf90_inquire_dimension(ncid, dimid_long, len=nc_data%nx), "getting nx")

        ! get the latitude size
        call check(nf90_inq_dimid(ncid, nc_meta % lat_name, dimid_lat), "getting dimid_lat")
        call check(nf90_inquire_dimension(ncid, dimid_lat, len=nc_data%ny), "getting ny")

        if (allocated(nc_data%slab)) then
            deallocate(nc_data%slab)
        end if

        allocate(lons(nc_data%nx))
        allocate(lats(nc_data%ny))
        allocate(slab(nc_data%nx, nc_data%ny))
        allocate(nc_data%slab(nc_data%nx, nc_data%ny))

        ! get the first two longitudes
        call check(nf90_inq_varid(ncid, nc_meta%lon_name, varid_lon), "getting varid_lon")
        call check(nf90_get_var(ncid, varid_lon, lons), "getting first lons")

        ! get the first two latitudes
        call check(nf90_inq_varid(ncid, nc_meta%lat_name, varid_lat), "getting varid_lat")
        call check(nf90_get_var(ncid, varid_lat, lats), "getting first lats")

        ! get the data
        call check(nf90_inq_varid(ncid, nc_meta%var_name, varid), "getting varid")
        call check(nf90_get_var(ncid, varid, slab), "reading data")

        call check(nf90_get_att(ncid, varid, "units", nc_data%units), "getting units")

        call check(nf90_close(ncid), "closing file")

        if (lats(1) > lats(2)) then
            nc_data%startlat = lats(nc_data%ny)
            nc_data%deltalat = (lats(1) - lats(nc_data%ny)) / (nc_data%ny-1)
            nc_data%slab(:, :) = slab(:, nc_data%ny:1:-1)
        else
            nc_data%startlat = lats(1)
            nc_data%deltalat = (lats(nc_data%ny)-lats(1)) / (nc_data%ny-1)
            nc_data%slab = slab
        end if
        nc_data%startlon = lons(1)
        nc_data%deltalon = (lons(nc_data%nx) - lons(1)) / (nc_data%nx-1)

    end subroutine nc_read_data

    subroutine check(nc_error, operation)
        use netcdf, only: nf90_strerror, NF90_NOERR
        implicit none
        integer, intent(in) :: nc_error
        character(len=*), intent(in), optional :: operation
        if (nc_error == NF90_NOERR) return
        write(*, '(A, I0, A)') "NetCDF error ", nc_error," encountered"
        if (present(operation)) then
            write(*, '(2A)') "during operation: ", operation
        end if
        write(*, '(A)') nf90_strerror(nc_error)
        stop 1
    end subroutine check

end module mod_nc_reader
