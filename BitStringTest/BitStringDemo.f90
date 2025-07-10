PROGRAM bitstring_demo
    
  USE bitstring_m
  
  IMPLICIT NONE
  
  TYPE(bitstring_t) :: bs, bs2
  LOGICAL, ALLOCATABLE :: larray(:)
  CHARACTER(64), PARAMETER :: fmt_markers =   &
    '(T4,A1,T8,A1,T12,A2,T17,A2,T22,A2,T27,A2)'
  
  WRITE (*, '(T20,A)') 'FGL: BitString testing'
  WRITE (*, '(A)') REPEAT('=', 72)
  WRITE (*, '(T4,A,/)') 'BitString: efficient implementation of 0-1 arrays'
  
  WRITE (*, '(T2,A)') 'Creating zero bit-string of length 25'
  bs = make_zero_bitstring(25)
  WRITE (*, fmt=fmt_markers) '1', '5', '10', '15', '20', '25'
  WRITE (*, fmt=fmt_markers) '|', '|', ' |', ' |', ' |', ' |'
  WRITE (*,'(T4,DT)') bs
  WRITE(*, '(T2,A)') 'Setting bits at positions 1, 4, 24'
  CALL bs%set(1, 1)
  CALL bs%set(4, 1)
  CALL bs%set(24, 1)
  WRITE (*, fmt=fmt_markers) '1', '5', '10', '15', '20', '25'
  WRITE (*, fmt=fmt_markers) '|', '|', ' |', ' |', ' |', ' |'
  WRITE (*,'(T4,DT)') bs
  WRITE (*,'(T4,25L1)') bs%convert_to_logical_array()
  larray = bs%convert_to_logical_array()
  larray(10) = .TRUE.
  WRITE (*, '(T2,A)') 'Convert LOGICAL LEN=16 with set bits: 1, 4, 10'
  bs2 = make_bitstring_from_logical_array(larray(1:16))
  WRITE (*, fmt=fmt_markers) '1', '5', '10', '15', '20', '25'
  WRITE (*, fmt=fmt_markers) '|', '|', ' |', ' |', ' |', ' |'
  WRITE (*,'(T4,DT)') bs2
    
END PROGRAM