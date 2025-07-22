module hydraulic_structure_pump

   implicit none(type, external)

   private

   public :: calculate_pump_discharge
   public :: dp
   public :: pump_specification_t

   integer, parameter :: dp = selected_real_kind(15)

   type :: pump_specification_t
      real(kind=dp) :: capacity
      real(kind=dp) :: water_level_start
      real(kind=dp) :: water_level_stop
   end type pump_specification_t

contains

   pure function calculate_pump_discharge(pump_specification, water_level_current &
                                          ) result(discharge)
      type(pump_specification_t), intent(in) :: pump_specification
      real(kind=dp), intent(in) :: water_level_current
      real(kind=dp) :: discharge

      if (water_level_current >= pump_specification%water_level_start .and. water_level_current >= pump_specification%water_level_stop) then
         discharge = pump_specification%capacity
      else
         discharge = 0.0_dp
      end if
   end function calculate_pump_discharge

end module hydraulic_structure_pump
