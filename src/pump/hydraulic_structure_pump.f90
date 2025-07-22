module hydraulic_structure_pump

   implicit none(type, external)

   private

   public :: next_pump_state
   public :: dp
   public :: pump_specification_t
   public :: pump_state_t

   integer, parameter :: dp = selected_real_kind(15)

   type :: pump_state_t
      real(kind=dp) :: discharge
      logical :: is_running
   end type pump_state_t

   type :: pump_specification_t
      real(kind=dp) :: capacity
      real(kind=dp) :: water_level_start
      real(kind=dp) :: water_level_stop
   end type pump_specification_t

contains

   pure function next_pump_state(pump_specification, previous_state, water_level_current &
                                 ) result(next_state)
      type(pump_specification_t), intent(in) :: pump_specification
      type(pump_state_t), intent(in) :: previous_state
      real(kind=dp), intent(in) :: water_level_current
      type(pump_state_t) :: next_state

      if (water_level_current >= pump_specification%water_level_start) then
         next_state%is_running = .true.
         next_state%discharge = pump_specification%capacity
         return
      end if

      if (previous_state%is_running .and. &
          water_level_current >= pump_specification%water_level_stop) then
         next_state%is_running = .true.
         next_state%discharge = pump_specification%capacity
         return
      end if

      next_state%is_running = .false.
      next_state%discharge = 0.0_dp
   end function next_pump_state

end module hydraulic_structure_pump
