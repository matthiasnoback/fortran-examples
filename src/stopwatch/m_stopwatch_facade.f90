module m_stopwatch_facade
   use m_stopwatch, only: timer_start, timer_end
   use m_started_timer_map, only: t_started_timer_map => map_type, &
                                  started_timer_map_value_type => value_type
   use m_ended_timer_list, only: t_ended_timer_list => list_type, &
                                 ended_timer_list_value_type => value_type

   implicit none

   private

   public :: stopwatch_start, stopwatch_end, stopwatch_print_all

   type(t_started_timer_map) :: started_timers
   type(t_ended_timer_list) :: ended_timers

contains
   subroutine stopwatch_start(label)
      character(len=*), intent(in) :: label
      call started_timers%add(label, started_timer_map_value_type(timer_start(label)))
   end subroutine stopwatch_start

   subroutine stopwatch_end(label)
      character(len=*), intent(in) :: label

      class(started_timer_map_value_type), allocatable :: map_value
      logical :: found

      map_value = started_timers%get(label, found)

      if (found) then
         call ended_timers%add(ended_timer_list_value_type(timer_end(map_value%value)))
      end if
   end subroutine stopwatch_end

   subroutine stopwatch_print_all()
      call ended_timers%foreach(print_ended_timer)
   end subroutine stopwatch_print_all

   subroutine print_ended_timer(value)
      type(ended_timer_list_value_type), intent(inout) :: value
      print *, value%value%to_string()
   end subroutine print_ended_timer

end module m_stopwatch_facade
