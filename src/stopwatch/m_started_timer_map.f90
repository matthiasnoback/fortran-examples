module m_started_timer_map
   use m_stopwatch, only: t_started_timer
   implicit none
   type :: value_type
      type(t_started_timer) :: value
   end type value_type

   include '../common/map_template.inc'
end module m_started_timer_map
