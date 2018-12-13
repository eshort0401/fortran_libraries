module nc_tools

    use netcdf
    implicit none
    public
    contains
        subroutine read_cl_real_arg(argI, var)

            integer(kind=4) :: argI
            real :: var
            character(len=32) :: name

            call get_command_argument(argI, name)
            name = trim(adjustl(name))
            read(name, *) var

        end subroutine read_cl_real_arg

        function export_nc_3d( &
            filename, array, array_name, &
            x, y, z, &! Coord. variables.
            x_len, y_len, z_len, & ! Length of coord. variables
            x_name, y_name, z_name, & ! Names of coord. variables.
            x_units, y_units, z_units & ! Names of coord. variable units.
        ) &
            result(ncid)

            ! Inputs
            character(len = *) :: filename, array_name
            character(len = *) :: x_name, y_name, z_name
            character(len = *) :: x_units, y_units, z_units
            integer(kind=4) :: x_len, y_len, z_len
            real :: array(x_len, y_len, z_len)
            real :: x(x_len), y(y_len), z(z_len)

            ! Function variables
            integer(kind=4) :: varid
            integer(kind=4) :: dimid(3)
            integer(kind=4) :: dim_varid(3)
            character(len = *), parameter :: units = 'units'

            ! Outputs
            integer(kind=4) :: ncid

            ! Create nc file
            call check(nf90_create(filename, nf90_clobber, ncid))

            ! Create dimensions
            call check(nf90_def_dim(ncid, x_name, x_len, dimid(1)))
            call check(nf90_def_dim(ncid, y_name, y_len, dimid(2)))
            call check(nf90_def_dim(ncid, z_name, z_len, dimid(3)))

            ! Create coordinates
            call check( &
                nf90_def_var(ncid, x_name, nf90_real, dimid(1), dim_varid(1)) &
            )
            call check( &
                nf90_def_var(ncid, y_name, nf90_real, dimid(2), dim_varid(2)) &
            )
            call check( &
                nf90_def_var(ncid, z_name, nf90_real, dimid(3), dim_varid(3)) &
            )

            ! Specify coordinate units
            call check(nf90_put_att(ncid, dim_varid(1), units, x_units))
            call check(nf90_put_att(ncid, dim_varid(2), units, y_units))
            call check(nf90_put_att(ncid, dim_varid(3), units, z_units))

            ! Create variables
            call check( &
                nf90_def_var(ncid, array_name, nf90_float, dimid, varid) &
                )

            call check(nf90_enddef(ncid))

            ! Write the coordinate data.
            call check(nf90_put_var(ncid, dim_varid(1), x))
            call check(nf90_put_var(ncid, dim_varid(2), y))
            call check(nf90_put_var(ncid, dim_varid(3), z))

            call check(nf90_put_var(ncid, varid, array))

            ! Close the file
            call check(nf90_close(ncid))

            print *, 'Successfully created netcdf file!'

            contains
                subroutine check(status)
                    integer(kind=4), intent ( in) :: status
                    if(status/=nf90_noerr) then
                        print *, trim(nf90_strerror(status))
                        stop 'Stopped'
                    end if
                end subroutine check

        end function export_nc_3d

        function add_var_nc_3d( &
            filename, array, array_name, &
            x_len, y_len, z_len, & ! Length of coord. variables
            x_name, y_name, z_name & ! Names of coord. variables.
        ) &
            result(ncid)

            ! Inputs
            character(len = *) :: filename, array_name
            character(len = *) :: x_name, y_name, z_name
            integer(kind=4) :: x_len, y_len, z_len
            real :: array(x_len, y_len, z_len)

            ! Function variables
            integer(kind=4) :: varid
            integer(kind=4) :: dimid(3)

            ! Outputs
            integer(kind=4) :: ncid

            ! Open nc file
            call check(nf90_open(filename, nf90_write, ncid))

            ! Put in define module
            call check(nf90_redef(ncid))

            call check(nf90_inq_dimid(ncid, x_name, dimid(1)))
            call check(nf90_inq_dimid(ncid, y_name, dimid(2)))
            call check(nf90_inq_dimid(ncid, z_name, dimid(3)))

            ! Create variables
            call check( &
                nf90_def_var(ncid, array_name, nf90_float, dimid, varid) &
                )

            call check(nf90_enddef(ncid))

            call check(nf90_put_var(ncid, varid, array))

            ! Close the file
            call check(nf90_close(ncid))

            print *, 'Successfully added variable to netcdf file!'

            contains
                subroutine check(status)
                    integer(kind=4), intent ( in) :: status
                    if(status/=nf90_noerr) then
                        print *, trim(nf90_strerror(status))
                        stop 'Stopped'
                    end if
                end subroutine check

        end function add_var_nc_3d

        function add_global_attr_nc( &
            filename, attribute_name, attribute &
        ) &
            result(ncid)

            ! Inputs
            character(len = *) :: filename, attribute_name
            real :: attribute

            ! Function variables
            integer(kind=4) :: status

            ! Outputs
            integer(kind=4) :: ncid

            ! Open nc file
            call check(nf90_open(filename, nf90_write, ncid))

            ! Put in define module
            call check(nf90_redef(ncid))

            status = nf90_put_att(ncid, NF90_GLOBAL, attribute_name, attribute)

            ! Close the file
            call check(nf90_close(ncid))

            print *, 'Successfully added attribute to netcdf file!'

            contains
                subroutine check(status)
                    integer(kind=4), intent ( in) :: status
                    if(status/=nf90_noerr) then
                        print *, trim(nf90_strerror(status))
                        stop 'Stopped'
                    end if
                end subroutine check

        end function add_global_attr_nc

end module nc_tools
