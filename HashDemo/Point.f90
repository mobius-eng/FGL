module point_m
    
    use hashtable_m
    
    implicit none
    ! Actual value to be stored
    type point_t
        real :: x, y
    end type
    ! Adapter for hashtable
    type, extends(hashtable_entry_t) :: point_entry_t
        type(point_t) :: p
    contains
        procedure, pass(self) :: copy => point_entry_copy
    end type
    ! Key to set the length to constant
    type, extends(hashtable_key_t) :: point_key_t
        character(len=64) :: key
    contains
        procedure, pass(self) :: get => point_key_get
        procedure, pass(self) :: store => point_key_store
    end type
    
contains

    subroutine point_entry_copy(self, other)
        class(point_entry_t), intent(inout) :: self
        class(hashtable_entry_t), intent(in) :: other
        select type(y => other)
            type is (point_entry_t)
                self%p = y%p
        end select
    end subroutine
    
    function point_entry_create(p) result(pe)
        type(point_t), intent(in) :: p
        type(point_entry_t) :: pe
        pe%p = p
    end function
    
    function point_key_get(self) result(s)
        class(point_key_t), intent(in) :: self
        character(len=:), allocatable :: s
        s = trim(self%key)
    end function
    
    subroutine point_key_store(self, key)
        class(point_key_t), intent(inout) :: self
        character(len=*), intent(in) :: key
        self%key = trim(key)
    end subroutine
    
    
end module