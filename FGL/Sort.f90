MODULE sort_m

USE, INTRINSIC :: iso_c_binding,   ONLY : c_loc, c_f_pointer, c_ptr
USE, INTRINSIC :: iso_fortran_env, ONLY : int8
USE            :: sort_int_m, ONLY : isort_int => isort, qsort_int => qsort
USE            :: sort_real_sp_m, ONLY : isort_real_sp => isort, qsort_real_sp => qsort
USE            :: sort_real_dp_m, ONLY : isort_real_dp => isort, qsort_real_dp => qsort
IMPLICIT NONE
PRIVATE
PUBLIC :: compare_fn, isort, qsort

ABSTRACT INTERFACE

  FUNCTION compare_fn(a, b) RESULT(y)
    IMPORT c_ptr
    TYPE(c_ptr), INTENT(IN) :: a, b
    INTEGER :: y
  END FUNCTION

END INTERFACE


INTERFACE isort
  !IMPORT isort_int, isort_real_sp, isort_real_dp
  MODULE PROCEDURE :: isort_gen, isort_int, &
                      isort_real_sp, isort_real_dp
END INTERFACE

INTERFACE qsort
  !IMPORT qsort_int, qsort_real_sp, qsort_real_dp
  MODULE PROCEDURE :: qsort_int, qsort_real_sp, qsort_real_dp
END INTERFACE

CONTAINS


SUBROUTINE isort_gen(items, item_size, cmp)
  TYPE(*), TARGET        :: items(:)
  INTEGER, INTENT(IN)    :: item_size
  PROCEDURE(compare_fn)  :: cmp
  
  INTEGER                :: i, j, nitems, iloc, jloc, ntot
  TYPE(c_ptr)            :: pitem, p1, p2
  INTEGER(int8), POINTER :: item_bits(:)
  INTEGER(int8), TARGET  :: tmp(item_size)
  
  ! Get access to the storage of ITEM through BITS array
  nitems = size(items)
  ntot   = nitems * item_size
  pitem  = c_loc(items)
  p2     = c_loc(tmp)
  CALL c_f_pointer(pitem, item_bits, [ntot])
  
  ! At each iteration, items(1:(i-1)) is sorted
  outer: DO i = 2, nitems
  
    iloc             = (i-1)*item_size+1
    tmp(1:item_size) = item_bits(iloc:iloc - 1 + item_size)
    j                = i
    
    inner: DO WHILE (j > 1)
    
      jloc = (j-1) * item_size + 1
      ! candidate is >= to the last element of the sorted array -- leave it
      p1 = c_loc(items(j-1))
      IF (cmp(p1, p2) <= 0) EXIT inner
      ! Shift ITEMS(J) one position towards the end
      item_bits(jloc: jloc - 1 + item_size) = item_bits(jloc - item_size:jloc-1)
      ! Try next J
      j = j - 1
    END DO inner
    
    jloc = (j-1) * item_size + 1
    ! need to place TMP into items J
    item_bits(jloc:jloc-1+item_size) = tmp(1:item_size)
    
  END DO outer

END SUBROUTINE isort_gen


END MODULE