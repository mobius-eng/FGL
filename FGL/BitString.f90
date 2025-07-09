MODULE bitstring_m

USE iso_c_binding, ONLY: c_int8_t

IMPLICIT NONE

PRIVATE
PUBLIC bitstring_t, make_zero_bitstring, make_bitstring_from_logical_array

TYPE bitstring_t
  INTEGER :: slen
  INTEGER(c_int8_t), ALLOCATABLE :: buffer(:)
CONTAINS
  PROCEDURE, PASS(self) :: set => bitstring_set
  PROCEDURE, PASS(self) :: get => bitstring_get
  PROCEDURE, PASS(self) :: convert_to_logical_array => &
    bitstring_convert_to_logical_array
  PROCEDURE, PASS(self) :: custom_write => bitstring_write
  GENERIC :: write(formatted) => custom_write
END TYPE


CONTAINS


FUNCTION make_zero_bitstring(slen) RESULT(bs)
  INTEGER, INTENT(IN) :: slen
  TYPE(bitstring_t) :: bs
  ALLOCATE (bs%buffer((slen+7)/8))
  bs%buffer(:) = 0
  bs%slen = slen
END FUNCTION


FUNCTION make_bitstring_from_logical_array(larray) RESULT(bs)
  
  LOGICAL, INTENT(IN) :: larray(:)
  TYPE(bitstring_t) :: bs
  INTEGER :: i
  ALLOCATE (bs%buffer((size(larray)+7)/8))
  bs%slen = size(larray)
  DO i = 1, bs%slen
    CALL bs%set(i, ltoi(larray(i)))
  END DO

  CONTAINS

    PURE FUNCTION ltoi(v)
      LOGICAL, INTENT(IN) :: v
      INTEGER :: ltoi
      IF (v) THEN
        ltoi = 1
      ELSE
        ltoi = 0
      END IF
    END FUNCTION

END FUNCTION make_bitstring_from_logical_array


SUBROUTINE bitstring_set(self, index, val)
  CLASS(bitstring_t), INTENT(INOUT) :: self
  INTEGER, INTENT(IN) :: index, val
  ! array & bit indices; new value for all 8 bits
  INTEGER :: iarray, ibit !, newval
  ! INTEGER(c_int8_t) :: dummy_array(size(self%buffer))
  iarray = (index - 1) / 8 + 1
  ibit = mod(index - 1, 8)
  IF (val == 0) THEN
    self%buffer(iarray) = ibclr(self%buffer(iarray), ibit)
  ELSE
    self%buffer(iarray) = ibset(self%buffer(iarray), ibit)
  END IF
END SUBROUTINE


PURE INTEGER FUNCTION bitstring_get(self, index)
  CLASS(bitstring_t), INTENT(IN) :: self
  INTEGER, INTENT(IN) :: index
  INTEGER :: iarray, ibit
  iarray = (index - 1) / 8 + 1
  ibit = mod(index - 1, 8)
  bitstring_get = ibits(self%buffer(iarray), ibit, 1)
END FUNCTION


FUNCTION bitstring_convert_to_logical_array(self) result (larray)
    CLASS(bitstring_t), INTENT(IN) :: self
    LOGICAL, ALLOCATABLE :: larray(:)
    INTEGER :: i
    ALLOCATE(larray(self%slen))
    DO i = 1, self%slen
        larray(i) = self%get(i) /= 0
    END DO
END FUNCTION

SUBROUTINE bitstring_write(self, unit, iotype, v_list, iostat, iomsg)
  CLASS(bitstring_t), INTENT(IN) :: self
  INTEGER, INTENT(IN) :: unit         ! Internal unit to write to.
  CHARACTER(*), INTENT(IN) :: iotype  ! LISTDIRECTED or DTxxx
  INTEGER, INTENT(IN) :: v_list(:)    ! parameters from fmt spec.
  INTEGER, INTENT(OUT) :: iostat      ! non zero on error, etc.
  CHARACTER(*), INTENT(INOUT) :: iomsg  ! define IF iostat non zero.
  INTEGER :: nfull, noverbits, i, n
  nfull = self%slen / 8
  noverbits = mod(self%slen, 8)
  DO i = 1, nfull
      CALL write_binary(self%buffer(i))
  END DO
  IF (noverbits > 0) THEN
      n = self%buffer(nfull+1)
      !n = shiftl(n, 8 - noverbits)
      !n = ishftc(n, 1)    use iso_c_binding, only: c_int8_t

      DO i = 1, noverbits
          write (unit, '(I1)', iostat = iostat, iomsg = iomsg) iand(n, 1)
          n = ishft(n, -1)
      END DO
  END IF

  CONTAINS

    SUBROUTINE write_binary(nn)
      INTEGER(c_int8_t), INTENT(IN) :: nn
      INTEGER(c_int8_t) :: m, ii
      m = nn
      DO ii = 1, 8
        write (unit, '(I1)', iostat=iostat, iomsg=iomsg) iand(m, 1_c_int8_t)
        m = ishft(m, -1)
      END DO
    END SUBROUTINE
    
END SUBROUTINE
    
END MODULE