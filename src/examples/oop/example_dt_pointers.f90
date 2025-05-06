module example_module
   implicit none(type, external)
   public

   type counter_t
      character(len=:), allocatable :: name
      integer :: count = 0
   contains
      procedure :: increase => counter_increase
      final :: counter_destructor
   end type counter_t

   type(counter_t), allocatable, target :: module_counter

   type(counter_t), allocatable :: reusable_allocatable_counter

contains

   subroutine counter_increase(self)
      class(counter_t), intent(inout) :: self

      self%count = self%count + 1

   end subroutine counter_increase

   function return_new_counter() result(new_counter)
      type(counter_t) :: new_counter
      new_counter%name = 'Factory returns local variable'
      call new_counter%increase()
   end function return_new_counter

   function return_previously_allocated_counter() result(counter)
      type(counter_t), allocatable :: counter
      if (.not. allocated(reusable_allocatable_counter)) then
         allocate (reusable_allocatable_counter)
         reusable_allocatable_counter%name = 'Factory reuses allocatable module counter'
      end if
      counter = reusable_allocatable_counter
   end function return_previously_allocated_counter

   function return_pointer_to_new_counter() result(pointer_to_new_counter)
      type(counter_t), pointer :: pointer_to_new_counter
      type(counter_t), target :: new_counter
      new_counter%name = 'Factory returns pointer to local variable'
      pointer_to_new_counter => new_counter
   end function return_pointer_to_new_counter

   function return_pointer_to_module_state() result(pointer_to_module_state)
      type(counter_t), pointer :: pointer_to_module_state

      if (.not. allocated(module_counter)) then
         module_counter = counter_t('Factory returns pointer to local variable')
         call module_counter%increase()
         call module_counter%increase()
      end if

      pointer_to_module_state => module_counter
   end function return_pointer_to_module_state

   function return_allocated_pointer() result(allocated_pointer)
      type(counter_t), pointer :: allocated_pointer

      allocate (allocated_pointer)
      allocated_pointer%name = 'Factory returns allocated pointer'
      call allocated_pointer%increase()
      call allocated_pointer%increase()
      call allocated_pointer%increase()
   end function return_allocated_pointer

   subroutine counter_destructor(self)
      type(counter_t), intent(in) :: self
      if (allocated(self%name)) then
         print *, 'Destructing counter: ', self%name, self%count
      else
         print *, 'Destructing unallocated counter'
      end if
   end subroutine counter_destructor

end module example_module

program example
   use example_module, only: return_pointer_to_new_counter, counter_t, &
                             return_new_counter, return_pointer_to_module_state, &
                             return_allocated_pointer, return_previously_allocated_counter

   implicit none(type, external)

   type(counter_t), allocatable :: counter_1
   type(counter_t), pointer :: counter_2
   type(counter_t), pointer :: counter_3
   type(counter_t), pointer :: counter_4
   type(counter_t), allocatable :: counter_5, counter_6

   print *, 'Example 1: return a new counter'
   counter_1 = return_new_counter()
   call counter_1%increase()
   print *, counter_1%name, counter_1%count
   deallocate (counter_1)
   print *, ''

   print *, 'Example 2: return a pointer to a new counter'
   counter_2 => return_pointer_to_new_counter()
   call counter_2%increase()
   print *, 'Pointer is associated?', associated(counter_2)
   print *, 'Name is allocated? ', allocated(counter_2%name)
   print *, ''

   ! We can't do this because the pointer's target has gone out of scope
   ! print *, counter_2%name, counter_2%count

   print *, 'Example 3: return a pointer to a counter stored in a module variable'
   counter_3 => return_pointer_to_module_state()
   call counter_3%increase()
   print *, counter_3%name, counter_3%count
   deallocate (counter_3)
   print *, ''

   print *, 'Example 4: return a pointer that has been allocated in the factory'
   counter_4 => return_allocated_pointer()
   call counter_4%increase()
   print *, counter_4%name, counter_4%count
   deallocate (counter_4)
   print *, ''

   print *, 'Example 5: Reusing an allocatable counter stored as a module variable'
   counter_5 = return_previously_allocated_counter()
   call counter_5%increase()
   print *, counter_5%name, counter_5%count

   counter_6 = return_previously_allocated_counter()
   print *, counter_6%name, counter_6%count
   deallocate (counter_5, counter_6)

   counter_5 = return_previously_allocated_counter()
   print *, counter_5%name, counter_5%count
   print *, ''
end program example
