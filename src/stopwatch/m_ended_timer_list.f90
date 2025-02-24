module m_ended_timer_list
   use m_stopwatch, only: t_ended_timer
   implicit none
   type :: value_type
      type(t_ended_timer) :: value
   end type value_type

   include '../common/list_template.inc'
end module m_ended_timer_list
