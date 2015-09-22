module mod_wrf_writer
    implicit none

    type tp_wrf_data
        integer :: version = 5        ! Format version (must =5 for WPS format)
        integer :: nx, ny             ! x- and y-dimensions of 2-d array
        integer :: iproj              ! Code for projection of data in array:
                                      !     0 = cylindrical equidistant
                                      !     1 = Mercator
                                      !     3 = Lambert conformal conic
                                      !     4 = Gaussian
                                      !     5 = Polar stereographic
        integer :: nlats              ! Number of latitudes north of equator
                                      !     (for Gaussian grids)
        real :: xfcst                 ! Forecast hour of data
        real :: xlvl                  ! Vertical level of data in 2-d array
        real :: startlat, startlon    ! Lat/lon of point in array indicated by
                                      !     startloc string
        real :: deltalat, deltalon    ! Grid spacing, degrees
        real :: dx, dy                ! Grid spacing, km
        real :: xlonc                 ! Standard longitude of projection
        real :: truelat1, truelat2    ! True latitudes of projection
        real :: earth_radius          ! Earth radius, km
        real, dimension(:,:), allocatable :: slab      ! The 2-d array holding the data
        logical :: is_wind_grid_rel   ! Flag indicating whether winds are
                                      !     relative to source grid (TRUE) or
                                      !     relative to earth (FALSE)
        character (len=8)  :: startloc      ! Which point in array is given by
                                            !     startlat/startlon; set either
                                            !     to 'SWCORNER' or 'CENTER  '
        character (len=9)  :: field   ! Name of the field
        character (len=24) :: hdate   ! Valid date for data YYYY:MM:DD_HH:00:00
        character (len=25) :: units   ! Units of data
        character (len=32) :: map_source    ! Source model / originating center
        character (len=46) :: desc    ! Short description of data
    end type tp_wrf_data

contains

    subroutine set_resolution(wrf_data, nx, ny)
        implicit none
        type(tp_wrf_data), intent(inout) :: wrf_data
        integer, intent(in) :: nx, ny

        if (allocated(wrf_data%slab)) then
            print *, "Deallocating slab"
            deallocate(wrf_data%slab)
        end if
        wrf_data%nx = nx
        wrf_data%ny = ny
        allocate(wrf_data%slab(nx, ny))
    end subroutine set_resolution

    subroutine set_slab(wrf_data, slab)
        implicit none
        type(tp_wrf_data), intent(inout) :: wrf_data
        real, dimension(:, :), intent(in) :: slab
        if (.not. allocated(wrf_data%slab)) then
            print *, "call set_resolution before calling set_slab"
            stop 1
        end if
        if (.not. all(shape(wrf_data%slab) == shape(slab))) then
            print *, "slab must be of dimension (nx, ny)"
            stop 2
        end if
        wrf_data % slab = slab
    end subroutine set_slab

    subroutine wrf_write(filename, wrf_data)
        implicit none
        character(len=*), intent(in) :: filename
        type(tp_wrf_data), intent(in) :: wrf_data
        integer, parameter :: ounit = 924623 ! random number
        integer :: io_status
        open(ounit, file=filename, status="REPLACE", iostat=io_status, access="SEQUENTIAL",   &
            form="UNFORMATTED", action="WRITE", convert="BIG_ENDIAN")
        if (.not. io_status == 0) then
            print *, "Error opening " // trim(filename)
            stop 1
        end if

        write(unit=ounit) wrf_data%version
        SELECT CASE (wrf_data%iproj)

        	CASE (0)   ! Cylindrical equidistant
        		write(unit=ounit) wrf_data%hdate, wrf_data%xfcst, wrf_data%map_source, &
        		wrf_data%field, wrf_data%units, wrf_data%desc, wrf_data%xlvl, &
        		wrf_data%nx, wrf_data%ny, wrf_data%iproj

        		write(unit=ounit) wrf_data%startloc, wrf_data%startlat, wrf_data%startlon, &
        		wrf_data%deltalat, wrf_data%deltalon, wrf_data%earth_radius

        	CASE (1)   ! Mercator
        		write(unit=ounit) wrf_data%hdate, wrf_data%xfcst, wrf_data%map_source, &
        		wrf_data%field, wrf_data%units, wrf_data%desc, wrf_data%xlvl, &
        		wrf_data%nx, wrf_data%ny, wrf_data%iproj

        		write(unit=ounit) wrf_data%startloc, wrf_data%startlat, wrf_data%startlon,&
        		wrf_data%dx, wrf_data%dy, wrf_data%truelat1, wrf_data%earth_radius

        	CASE (3)   ! Lambert conformal
        		write(unit=ounit) wrf_data%hdate, wrf_data%xfcst, wrf_data%map_source, &
        		wrf_data%field, &
        		wrf_data%units, wrf_data%desc, wrf_data%xlvl, wrf_data%nx, wrf_data%ny, &
        		wrf_data%iproj

        		write(unit=ounit) wrf_data%startloc, wrf_data%startlat, wrf_data%startlon, &
        		wrf_data%dx, wrf_data%dy, &
        		wrf_data%xlonc, wrf_data%truelat1, wrf_data%truelat2, wrf_data%earth_radius

        	CASE (4)   ! Gaussian
        		write(unit=ounit) wrf_data%hdate, wrf_data%xfcst, wrf_data%map_source, &
        		wrf_data%field, &
        		wrf_data%units, wrf_data%desc, wrf_data%xlvl, wrf_data%nx, wrf_data%ny, &
        		wrf_data%iproj

        		write(unit=ounit) wrf_data%startloc, wrf_data%startlat, wrf_data%startlon, &
        		wrf_data%nlats, wrf_data%deltalon, wrf_data%earth_radius

        	CASE (5)   ! Polar stereographic
        		write(unit=ounit) wrf_data%hdate, wrf_data%xfcst, wrf_data%map_source, &
        		wrf_data%field, &
        		wrf_data%units, wrf_data%desc, wrf_data%xlvl, wrf_data%nx, wrf_data%ny, &
        		wrf_data%iproj

        		write(unit=ounit) wrf_data%startloc, wrf_data%startlat, wrf_data%startlon, &
        		wrf_data%dx, wrf_data%dy, &
        		wrf_data%xlonc, wrf_data%truelat1, wrf_data%earth_radius
        END SELECT

        !  3) WRITE WIND ROTATION FLAG
        write(unit=ounit) wrf_data%is_wind_grid_rel
        !  4) WRITE 2-D ARRAY OF DATA
        write(unit=ounit) wrf_data%slab
        close(ounit)
    end subroutine wrf_write
end module mod_wrf_writer
