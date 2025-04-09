module stopwatch
   use iso_fortran_env, only: wp => real64, int64

   implicit none(type, external)

   private
   public :: timer_t, timer_list_t

   type :: timer_t
      character(len=:), allocatable :: label
      integer(int64) :: system_clock_start_time
      integer(int64), allocatable :: system_clock_end_time
   contains
      procedure :: print => timer_print
      procedure :: end => timer_end
   end type timer_t

   interface timer_t
      module procedure :: timer_constructor
   end interface

   type :: timer_list_t
      type(timer_t), dimension(:), allocatable :: timers
   contains
      procedure :: add => timer_list_add
      procedure :: print_all => timer_list_print_all
   end type timer_list_t
contains

   function timer_constructor(label) result(timer)
      character(len=*), intent(in) :: label
      type(timer_t) :: timer

      integer(kind=int64) :: start_time

      call system_clock(start_time)

      timer = timer_t(label, start_time)
   end function timer_constructor

   subroutine timer_print(self)
      class(timer_t), intent(inout) :: self

      if (allocated(self%system_clock_end_time)) then
         print *, self%label, self%system_clock_start_time, self%system_clock_end_time
      else
         print *, self%label, self%system_clock_start_time
      end if
   end subroutine timer_print

   subroutine timer_end(self)
      class(timer_t), intent(inout) :: self

      integer(kind=int64) :: end_time

      call system_clock(end_time)

      self%system_clock_end_time = end_time
   end subroutine timer_end

   subroutine timer_list_add(self, timer)
      class(timer_list_t), intent(inout) :: self
      type(timer_t), intent(in) :: timer

      if (allocated(self%timers)) then
         self%timers = [timer, self%timers]
      else
         allocate (self%timers(1))
         self%timers(1) = timer
      end if
   end subroutine timer_list_add

   subroutine timer_list_print_all(self)
      class(timer_list_t), intent(inout) :: self

      integer :: i

      do i = 1, size(self%timers)
         call self%timers(i)%print()
      end do
   end subroutine timer_list_print_all

end module stopwatch

program example
   use stopwatch, only: timer_t, timer_list_t

   implicit none(type, external)

   type(timer_list_t) :: timer_list
   type(timer_t) :: timer1
   type(timer_t) :: timer2

   timer1 = timer_t('Column-first')
   timer2 = timer_t('Row-first')

   call timer_list%add(timer1)
   call timer1%end()
   call timer1%print()

   call timer_list%add(timer2)
   call timer2%end()
   call timer2%print()

   call timer_list%print_all()

end program example
