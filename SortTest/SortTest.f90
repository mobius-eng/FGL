PROGRAM sort_demo
    
USE, INTRINSIC :: iso_c_binding, ONLY : c_ptr, c_f_pointer
USE sort_m

IMPLICIT NONE

INTEGER :: a(10) = [3, 4, 1, 9, 7, 10, 5, 2, 8, 6]
INTEGER :: b(10)
REAL :: c(10)

b(:) = a(:)
c(:) = a(:)

PRINT '(/,T30,A)', 'FGL: Sort testing'
PRINT '(A,/)', REPEAT('=', 80)

PRINT '(T2, A)', 'Sorting using generic interface'
PRINT '(T4, A)', 'Before sorting'
PRINT '(T4, 10I4)', a
CALL sort_insert(a, storage_size(a(1)) / 8, int_cmp)
PRINT '(T4, A)', 'After sorting'
PRINT '(T4,10I4,/)', a

PRINT '(T2, A)', 'Sorting integers'
PRINT '(T4, A)', 'Before sorting'
PRINT '(T4, 10I4)', b
CALL sort_insert(b)
PRINT '(T4, A)', 'After sorting'
PRINT '(T4,10I4,/)', b

PRINT '(T2, A)', 'Sorting reals'
PRINT '(T4, A)', 'Before sorting'
PRINT '(T4, 10F6.1)', c
CALL qsort(c)
PRINT '(T4, A)', 'After sorting'
PRINT '(T4,10F6.1)', c


CONTAINS


INTEGER FUNCTION int_cmp(x, y)
    
  TYPE(c_ptr), INTENT(IN) :: x, y        
  
  INTEGER, POINTER :: px, py
  
  int_cmp = 0

  CALL c_f_pointer(x, px)
  CALL c_f_pointer(y, py)
  
  int_cmp = px - py
    
END FUNCTION


END PROGRAM