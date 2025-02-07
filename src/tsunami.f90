program tsunami
    use m_config, only : tsunami_config
    use m_init, only : set_gaussian
    use m_diff, only : diff
    use precision, only : dp

    ! This version solves the linearized 1-d advection equation:
    !
    !     du/dt + c du/dx = 0

    implicit none

    integer :: n

    type(tsunami_config) :: config

    real(kind = dp), dimension(:), allocatable :: h

    character(*), parameter :: fmt = '(i0,*(1x,es15.8e2))'

    config = tsunami_config(100, 100, 1.0_dp, 1.0_dp, 1.0_dp)

    allocate(h(config%get_grid_size()))

    call set_gaussian(h, 25, 0.02_dp)

    ! write initial state to screen
    print fmt, 0.0_dp, h

    time_loop : do n = 1, config%get_num_time_steps()

        h = h - config%get_c() * diff(h) / config%get_dx() * config%get_dt()

        print fmt, n, h

    end do time_loop

end program tsunami
