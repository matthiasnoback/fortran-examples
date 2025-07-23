module test_pump
   use common_to_string, only: to_string
   use testdrive, only: new_unittest, unittest_type, error_type, check, test_failed, skip_test
   use hydraulic_structure_pump, only: next_pump_state, dp, pump_specification_t, pump_state_t, &
                                       switched_off, running_at_capacity

   implicit none(type, external)

   private
   public :: collect_pump_tests

   interface check
      procedure :: check_pump_state, check_pump_state_and_discharge
   end interface

contains

   subroutine check_pump_state(error, actual, expected)
      type(error_type), allocatable, intent(out) :: error
      type(pump_state_t), intent(in) :: actual
      type(pump_state_t), intent(in) :: expected

      call check(error, actual%discharge, expected%discharge)
      if (allocated(error)) then
         return
      end if

      call check(error, actual%is_running, expected%is_running)
      if (allocated(error)) then
         return ! optional, if it's the last call to `check`
      end if
   end subroutine check_pump_state

   subroutine check_pump_state_and_discharge(error, actual, expected)
      type(error_type), allocatable, intent(out) :: error
      type(pump_state_t), intent(in) :: actual
      real(kind=dp), intent(in) :: expected

      call check(error, actual%discharge, expected)
   end subroutine check_pump_state_and_discharge

   !> Collect all exported unit tests
   subroutine collect_pump_tests(testsuite)
      !> Collection of tests
      type(unittest_type), allocatable, intent(out) :: testsuite(:)

      testsuite = [ &
                  new_unittest("test_next_pump_state", &
                               test_next_pump_state) &
                  ]
   end subroutine collect_pump_tests

   subroutine test_next_pump_state(error)
      type(error_type), allocatable, intent(out) :: error

      integer, parameter :: cases = 6
      integer :: case
      type(pump_specification_t), dimension(cases) :: pump
      type(pump_state_t), dimension(cases) :: previous_state
      real(kind=dp), dimension(cases) :: suction_side_water_level
      real(kind=dp), dimension(cases) :: delivery_side_water_level
      type(pump_state_t), dimension(cases) :: expected_state
      suction_side_water_level = 0.0_dp
      delivery_side_water_level = 0.0_dp

      ! Water level at suction side is above the start level
      case = 1
      pump(case) = pump_specification_t(10.0_dp)
      call pump(case)%configure_suction_side_control(2.0_dp, 0.0_dp)
      suction_side_water_level(case) = 3.0_dp
      expected_state(case) = running_at_capacity(10.0_dp)

      ! Water level at suction side is at start level
      case = 2
      pump(case) = pump_specification_t(10.0_dp)
      call pump(case)%configure_suction_side_control(2.0_dp, 0.0_dp)
      suction_side_water_level(case) = 2.0_dp
      expected_state(case) = running_at_capacity(10.0_dp)

      ! Water level at suction side is below the start level
      case = 3
      pump(case) = pump_specification_t(10.0_dp)
      call pump(case)%configure_suction_side_control(2.0_dp, 0.0_dp)
      suction_side_water_level(case) = 1.0_dp
      delivery_side_water_level(case) = 0.0_dp
      expected_state(case) = switched_off()

      ! Water level at suction side is at the stop level
      case = 4
      pump(case) = pump_specification_t(10.0_dp)
      call pump(case)%configure_suction_side_control(2.0_dp, 1.0_dp)
      suction_side_water_level(case) = 1.0_dp
      expected_state(case) = switched_off()

      ! Water level at suction side is below stop level
      case = 5
      pump(case) = pump_specification_t(10.0_dp)
      call pump(case)%configure_suction_side_control(2.0_dp, 1.0_dp)
      suction_side_water_level(case) = 0.5_dp
      expected_state(case) = switched_off()

      ! Water level at suction side is between start and stop level
      case = 6
      pump(case) = pump_specification_t(10.0_dp)
      previous_state(case)%is_running = .true.
      call pump(case)%configure_suction_side_control(2.0_dp, 1.0_dp)
      suction_side_water_level(case) = 1.5_dp
      expected_state(case) = running_at_capacity(10.0_dp)

      call check(error, cases, case, 'Case number mismatch')
      if (allocated(error)) then
         return
      end if

      do case = 1, cases
         call check(error, next_pump_state(pump(case), &
                                           previous_state(case), &
                                           suction_side_water_level(case), &
                                           delivery_side_water_level(case)), &
                    expected_state(case))
         if (allocated(error)) then
            error%message = 'Test case '//to_string(case)//' failed. '//error%message
            return
         end if
      end do
   end subroutine test_next_pump_state

end module test_pump
