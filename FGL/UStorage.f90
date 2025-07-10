MODULE ustorage_m
!
! (c) 2024 Alexey V. Cherkaev
!
! Universal storage: low-level structure to help generic programming
!    
USE iso_c_binding, ONLY: c_int8_t, c_loc, c_f_pointer, c_ptr

IMPLICIT NONE

PRIVATE

PUBLIC :: get_raw_pointer, ustorage_t

TYPE ustorage_t(len)
! USTORAGE_T
!   Type incopsulating ability to store any Fortran object
!
!   Interface
!
!       USTORAGE_T(SIZE_ITEM) :: X
!
!       CALL X%STORE(OBJ)
!       CALL X%RETREIVE(OBJ)
  INTEGER, len          :: len
  INTEGER(c_int8_t)     :: data(1:len)
  CONTAINS
  PROCEDURE, PASS(self) :: store => ustorage_store
  PROCEDURE, PASS(self) :: retrieve => ustorage_retrieve
END TYPE


CONTAINS


SUBROUTINE get_raw_pointer(item, item_size, pitem)
! GET_RAW_POINTER
!   Gets Fortran pointer (to INTEGER(C_INT8_T) array) from any Fortran TYPE
!
!   Parameters:
!       ITEM        (IN)    Any Fortran object
!       ITEM_SIZE   (IN)    Byte-size of ITEM 
!       PITEM       (INOUT) Resulting pointer
  TYPE(*), INTENT(IN), TARGET               :: item
  INTEGER, INTENT(IN)                       :: item_size
  INTEGER(c_int8_t), POINTER, INTENT(INOUT) :: pitem(:)
  
  TYPE(c_ptr)                               :: cp
  
  cp = c_loc(item)
  CALL c_f_pointer(cp, pitem, [item_size])
  
END SUBROUTINE


SUBROUTINE ustorage_store(self, item)
! USTORAGE_STORE
!   Store any object IN USTORAGE_T (object is copied)
!
!   Parameters:
!       SELF        (INOUT) USTORAGE_T object to store an object
!       ITEM        (IN)    Object to store.

  CLASS(ustorage_t(*)), INTENT(INOUT) :: self
  TYPE(*), INTENT(IN)                 :: item
  INTEGER(c_int8_t), POINTER          :: pitem(:)
  
  CALL get_raw_pointer(item, self%len, pitem)
  self%data(:) = pitem(:)
    
END SUBROUTINE


SUBROUTINE ustorage_retrieve(self, item)
! USTORAGE_RETRIEVE
!   Retrieve an object from USTORAGE_T (object is copied)
!
!   Parameters:
!       SELF        (IN)    USTORAGE_T instance that has an object stored
!       ITEM        (INOUT) Fortran object to copy a stored object

  CLASS(ustorage_t(*)), INTENT(IN) :: self
  TYPE(*), INTENT(INOUT)           :: item
  INTEGER(c_int8_t), pointer       :: pitem(:)
  
  CALL get_raw_pointer(item, self%len, pitem)
  pitem(:) = self%data(:)        

END SUBROUTINE

END MODULE