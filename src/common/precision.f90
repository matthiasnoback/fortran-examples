module precision
    use iso_fortran_env, only : real64
    implicit none
    private
    public :: dp

    integer, parameter :: dp = real64
end module precision
