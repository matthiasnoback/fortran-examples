module test_pump
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed
   use hydraulic_structure_pump, only: calculate_pump_discharge, dp, pump_specification_t

   implicit none(type, external)

   private
   public :: collect_pump_tests

contains

   !> Collect all exported unit tests
   subroutine collect_pump_tests(testsuite)
      !> Collection of tests
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_discharge_of_a_pump_that_is_turned_off", &
                               test_discharge_of_a_pump_that_is_turned_off), &
                  new_unittest("test_discharge_of_a_pump_that_is_always_on", &
                               test_discharge_of_a_pump_that_is_always_on), &
                  new_unittest("test_water_level_suction_side_is_at_start_level", &
                               test_water_level_suction_side_is_at_start_level), &
                  new_unittest("test_water_level_suction_side_is_below_start_level", &
                               test_water_level_suction_side_is_below_start_level), &
                  new_unittest("test_water_level_suction_side_is_above_start_level", &
                               test_water_level_suction_side_is_above_start_level), &
                  new_unittest("test_water_level_suction_side_is_at_stop_level", &
                               test_water_level_suction_side_is_at_stop_level), &
                  new_unittest("test_water_level_suction_side_is_below_stop_level", &
                               test_water_level_suction_side_is_below_stop_level), &
                  new_unittest("test_water_level_suction_side_is_above_stop_level", &
                               test_water_level_suction_side_is_above_stop_level) &
                  ]
   end subroutine collect_pump_tests

   subroutine test_discharge_of_a_pump_that_is_turned_off(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_t) :: pump_specification
      pump_specification = pump_specification_t(0.0_dp, 0.0_dp, 0.0_dp)

      call check(error, calculate_pump_discharge(pump_specification, 0.0_dp), 0.0_dp)
   end subroutine test_discharge_of_a_pump_that_is_turned_off

   subroutine test_discharge_of_a_pump_that_is_always_on(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_t) :: pump_specification
      pump_specification = pump_specification_t(10.0_dp, 0.0_dp, 0.0_dp)

      call check(error, calculate_pump_discharge(pump_specification, 0.0_dp), 10.0_dp)
   end subroutine test_discharge_of_a_pump_that_is_always_on

   subroutine test_water_level_suction_side_is_at_start_level(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_t) :: pump_specification
      pump_specification = pump_specification_t(10.0_dp, 2.0_dp, 0.0_dp)

      call check(error, calculate_pump_discharge(pump_specification, 2.0_dp), 10.0_dp)
   end subroutine test_water_level_suction_side_is_at_start_level

   pure function pump_with_capacity(capacity) result (pump)
      real(kind=wp) ::
      type(pump_specification_t) :: pump
      pump%capacity = capacity
end function pump_with_capacity


   subroutine test_water_level_suction_side_is_below_start_level(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_t) :: pump_specification
      pump_specification = pump_specification_t(10.0_dp, 2.0_dp, 0.0_dp)

      call check(error, calculate_pump_discharge(pump_specification, 1.0_dp), 0.0_dp)
   end subroutine test_water_level_suction_side_is_below_start_level

   subroutine test_water_level_suction_side_is_above_start_level(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_t) :: pump_specification
      pump_specification = pump_specification_t(10.0_dp, 2.0_dp, 0.0_dp)

      call check(error, calculate_pump_discharge(pump_specification, 3.0_dp), 10.0_dp)
   end subroutine test_water_level_suction_side_is_above_start_level

   subroutine test_water_level_suction_side_is_at_stop_level(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_t) :: pump_specification
      pump_specification = pump_specification_t(10.0_dp, 2.0_dp, 1.0_dp)

      call check(error, calculate_pump_discharge(pump_specification, 1.0_dp), 0.0_dp)
   end subroutine test_water_level_suction_side_is_at_stop_level

   subroutine test_water_level_suction_side_is_below_stop_level(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_t) :: pump_specification
      pump_specification = pump_specification_t(10.0_dp, 2.0_dp, 1.0_dp)

      call check(error, calculate_pump_discharge(pump_specification, 0.5_dp), 0.0_dp)
   end subroutine test_water_level_suction_side_is_below_stop_level

   subroutine test_water_level_suction_side_is_above_stop_level(error)
      type(error_type), allocatable, intent(out) :: error

      type(pump_specification_t) :: pump_specification
      pump_specification = pump_specification_t(10.0_dp, 2.0_dp, 1.0_dp)

      call check(error, calculate_pump_discharge(pump_specification, 2.0_dp), 10.0_dp)
   end subroutine test_water_level_suction_side_is_above_stop_level

end module test_pump
