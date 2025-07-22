module hydraulic_structure_pump

   implicit none(type, external)

   private

   public :: next_pump_state
   public :: dp
   public :: pump_specification_t
   public :: pump_state_t
   public :: switched_off
   public :: running_at_capacity

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

   pure function next_pump_state(pump_specification, previous_state, &
                                 water_level_at_suction_side, &
                                 water_level_at_delivery_side &
                                 ) result(next_state)
      type(pump_specification_t), intent(in) :: pump_specification
      type(pump_state_t), intent(in) :: previous_state
      real(kind=dp), intent(in) :: water_level_at_suction_side
      real(kind=dp), intent(in) :: water_level_at_delivery_side
      type(pump_state_t) :: next_state

      if (water_level_at_suction_side >= pump_specification%water_level_start) then
         next_state = running_at_capacity(pump_specification%capacity)
         return
      end if

      if (previous_state%is_running .and. &
          water_level_at_suction_side >= pump_specification%water_level_stop) then
         next_state = running_at_capacity(pump_specification%capacity)
         return
      end if

      next_state = switched_off()
   end function next_pump_state

   pure function running_at_capacity(capacity) result(state)
      real(kind=dp), intent(in) :: capacity
      type(pump_state_t) :: state

      state%discharge = capacity
      state%is_running = .true.
   end function running_at_capacity

   pure function switched_off() result(state)
      type(pump_state_t) :: state

      state%discharge = 0.0_dp
      state%is_running = .false.
   end function switched_off

end module hydraulic_structure_pump
