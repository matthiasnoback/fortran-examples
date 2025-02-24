! map_template.inc
type :: kv_pair
    character(len=:), allocatable :: key
    type(value_type) :: value
end type kv_pair

type :: map_type
    type(kv_pair), allocatable :: pairs(:)
contains
    procedure :: add => map_add
    procedure :: get => map_get
    procedure :: remove => map_remove
end type map_type

contains

    subroutine map_add(this, key, value)
        class(map_type), intent(inout) :: this
        character(len=*), intent(in) :: key
        type(value_type), intent(in) :: value
        integer :: i

        do i = 1, size(this%pairs)
            if (this%pairs(i)%key == key) then
                this%pairs(i)%value = value
                return
            end if
        end do

        if (allocated(this%pairs)) then
            this%pairs = [this%pairs, kv_pair(key, value)]
        else
            allocate(this%pairs(1))
            this%pairs(1) = kv_pair(key, value)
        end if
    end subroutine map_add

    function map_get(this, key, found) result(value)
        class(map_type), intent(in) :: this
        type(value_type), allocatable :: value
        character(len=*), intent(in) :: key
        logical, intent(out) :: found
        integer :: i

        found = .false.
        do i = 1, size(this%pairs)
            if (this%pairs(i)%key == key) then
                value = this%pairs(i)%value
                found = .true.
                return
            end if
        end do
    end function map_get

    subroutine map_remove(this, key)
        class(map_type), intent(inout) :: this
        character(len=*), intent(in) :: key
        integer :: i

        do i = 1, size(this%pairs)
            if (this%pairs(i)%key == key) then
                this%pairs(i:) = this%pairs(i+1:)
                if (size(this%pairs) > 1) then
                    allocate(this%pairs(size(this%pairs)-1))
                else
                    deallocate(this%pairs)
                end if
                return
            end if
        end do
    end subroutine map_remove
