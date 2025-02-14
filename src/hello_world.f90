program helloworld
   use m_hello, only: hello, anyone
   use m_world, only: world

   implicit none

   character(len=10) :: name_input

   print *, hello(anyone('Liesbeth'))
   print *, hello(world())

   print *, 'Now, provide a custom name: '
   read *, name_input

   print *, hello(anyone(name_input))

end program helloworld
