module precision
    implicit none
    private
    public :: dp

    integer, parameter :: dp = selected_real_kind(4)
end module precision
