module m_stopwatch
   use iso_fortran_env, only: int64, real64
   use m_to_string, only: to_string

   implicit none

   private
   public :: t_started_timer, t_ended_timer, timer_start, timer_end

   type :: t_started_timer
      character(len=:), allocatable :: label
      integer(kind=int64) :: start_time
   end type t_started_timer

   type :: t_ended_timer
      character(len=:), allocatable :: label
      integer(kind=int64) :: start_time
      integer(kind=int64) :: end_time
   contains
      procedure :: to_string => ended_timer_to_string
   end type t_ended_timer

   interface t_ended_timer
      module procedure :: ended_timer_constructor
   end interface t_ended_timer

contains
   function ended_timer_constructor(started_timer, end_time) result(res)
      class(t_started_timer), intent(in) :: started_timer
      integer(kind=int64), intent(in) :: end_time
      type(t_ended_timer) :: res

      res%label = started_timer%label
      res%start_time = started_timer%start_time
      res%end_time = end_time
   end function ended_timer_constructor

   function ended_timer_to_string(ended_timer) result(res)
      class(t_ended_timer), intent(in) :: ended_timer
      character(len=:), allocatable :: res

      res = ended_timer%label//': '//trim(to_string(calculate_actual_time(ended_timer)))
   end function ended_timer_to_string

   function timer_start(label) result(res)
      character(len=*), intent(in) :: label
      type(t_started_timer) :: res

      integer(kind=int64) :: start_time

      call system_clock(count=start_time)

      res = t_started_timer(label, start_time)
   end function timer_start

   function timer_end(started_timer) result(res)
      class(t_started_timer), intent(in) :: started_timer
      type(t_ended_timer) :: res

      integer(kind=int64) :: end_time

      call system_clock(count=end_time)

      res = t_ended_timer(started_timer, end_time)
   end function timer_end

   function calculate_actual_time(ended_timer) result(res)
      class(t_ended_timer), intent(in) :: ended_timer
      real(kind=real64) :: res

      real(kind=int64) :: clock_rate

      call system_clock(count_rate=clock_rate)  ! # of clock ticks per second

      res = (ended_timer%end_time - ended_timer%start_time)/real(clock_rate, kind=real64)
   end function calculate_actual_time

end module m_stopwatch
