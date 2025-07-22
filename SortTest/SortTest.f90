PROGRAM sort_demo
    
USE, INTRINSIC :: iso_c_binding, ONLY : c_ptr, c_f_pointer
USE sort_m

IMPLICIT NONE

INTEGER :: a(10) = [3, 4, 1, 9, 7, 10, 5, 2, 8, 6]
INTEGER :: b(10)
REAL :: c(10)
REAL, ALLOCATABLE :: d1(:), d2(:)
INTEGER :: nd, nt, it, tstart, trate, tend, j, in
REAL :: tq, ti

b(:) = a(:)
c(:) = a(:)

PRINT '(/,T30,A)', 'FGL: Sort testing'
PRINT '(A,/)', REPEAT('=', 80)

PRINT '(T2, A)', 'Sorting using generic interface'
PRINT '(T4, A)', 'Before sorting'
PRINT '(T4, 10I4)', a
CALL isort(a, storage_size(a(1)) / 8, int_cmp)
PRINT '(T4, A)', 'After sorting'
PRINT '(T4,10I4,/)', a

PRINT '(T2, A)', 'Sorting integers'
PRINT '(T4, A)', 'Before sorting'
PRINT '(T4, 10I4)', b
CALL isort(b)
PRINT '(T4, A)', 'After sorting'
PRINT '(T4,10I4,/)', b

PRINT '(T2, A)', 'Sorting reals'
PRINT '(T4, A)', 'Before sorting'
PRINT '(T4, 10F6.1)', c
CALL qsort(c)
PRINT '(T4, A)', 'After sorting'
PRINT '(T4,10F6.1)', c

WRITE(*, '(/,T2,A)') 'Testing sorting on varying size data'

nd = 1024

ALLOCATE(d1(nd), d2(nd))

nt = 50

WRITE (*, '(T12, A, T24, A, T36, A)') 'N', 'INSERT', 'QSORT'

inn: DO in = 1, 7

  WRITE (*, '(T4, I8)', advance='NO') nd
  ti = 0
  tq = 0

  itt: DO it = 1, nt
    CALL RANDOM_NUMBER(d1)
    d2(1:nd) = d1(1:nd)

    CALL SYSTEM_CLOCK(count=tstart, count_rate=trate)
    CALL isort(d1(1:nd))
    CALL SYSTEM_CLOCK(count=tend)
    ti = ti + (tend - tstart) * 1.0 / trate

    DO j = 2, nd
      IF (d1(j) < d1(j-1)) THEN
        WRITE (*, '(T4, A)') 'ERROR: Incorrectly sorted using INSERT'
        STOP
      END IF
    END DO

    CALL SYSTEM_CLOCK(count=tstart, count_rate=trate)
    CALL qsort(d2(1:nd))
    CALL SYSTEM_CLOCK(count=tend)
    tq = tq + (tend - tstart) * 1.0 / trate

    DO j = 2, nd
      IF (d2(j) < d2(j-1)) THEN
        WRITE (*, '(T4, A)') 'ERROR: Incorrectly sorted using QSORT'
        STOP
      END IF
    END DO
  END DO itt

  ti = ti / nt
  tq = tq / nt
  WRITE (*, '(T12, ES9.2, T24, ES9.2)') ti, tq
  nd = nd/2

END DO inn


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