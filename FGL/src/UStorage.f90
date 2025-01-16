! USTORAGE_M
!
! (c) 2024 Alexey V. Cherkaev
!
! Universal storage: low-level structure to help generic programming
!
module ustorage_m
    
    use iso_c_binding, only: c_int8_t, c_loc, c_f_pointer, c_ptr
    
    implicit none
    
    private
    
    public :: get_raw_pointer, ustorage_t
    
    ! USTORAGE_T
    !   Type incopsulating ability to store any Fortran object
    !
    !   Interface
    !
    !       USTORAGE_T(SIZE_ITEM) :: X
    !
    !       CALL X%STORE(OBJ)
    !       CALL X%RETREIVE(OBJ)
    type ustorage_t(len)
        integer, len :: len
        integer(c_int8_t), dimension(len) :: data
    contains
        procedure, pass(self) :: store => ustorage_store
        procedure, pass(self) :: retrieve => ustorage_retrieve
    end type
    
contains
    
    ! GET_RAW_POINTER
    !   Gets Fortran pointer (to INTEGER(C_INT8_T) array) from any Fortran type
    !
    !   Parameters:
    !       ITEM        (IN)    Any Fortran object
    !       ITEM_SIZE   (IN)    Byte-size of ITEM 
    !       PITEM       (INOUT) Resulting pointer
    subroutine get_raw_pointer(item, item_size, pitem)
    
        type(*), intent(in), target :: item
        integer, intent(in) :: item_size
        integer(c_int8_t), dimension(:), pointer, intent(inout) :: pitem
        ! Cannot convert directly. Use C-pointer as an intermediate buffer
        type(c_ptr) :: cp
        ! Get C pointer to ITEM
        cp = c_loc(item)
        ! Convert C pointer to Fortran pointer
        call c_f_pointer(cp, pitem, [item_size])
        
    end subroutine
    
    
    ! USTORAGE_STORE
    !   Store any object in USTORAGE_T (object is copied)
    !
    !   Parameters:
    !       SELF        (INOUT) USTORAGE_T object to store an object
    !       ITEM        (IN)    Object to store.
    subroutine ustorage_store(self, item)
    
        class(ustorage_t(*)), intent(inout) :: self
        type(*), intent(in) :: item
        integer(c_int8_t), dimension(:), pointer :: pitem
        
        call get_raw_pointer(item, self%len, pitem)
        self%data(:) = pitem(:)
        
    end subroutine
    
    
    ! USTORAGE_RETRIEVE
    !   Retrieve an object from USTORAGE_T (object is copied)
    !
    !   Parameters:
    !       SELF        (IN)    USTORAGE_T instance that has an object stored
    !       ITEM        (INOUT) Fortran object to copy a stored object
    subroutine ustorage_retrieve(self, item)
    
        class(ustorage_t(*)), intent(in) :: self
        type(*), intent(inout) :: item
        integer(c_int8_t), dimension(:), pointer :: pitem  
        
        call get_raw_pointer(item, self%len, pitem)
        pitem(:) = self%data(:)        
        
    end subroutine
    
end module