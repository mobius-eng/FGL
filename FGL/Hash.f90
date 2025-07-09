MODULE hash_m
    
USE iso_fortran_env, ONLY: int32

IMPLICIT NONE

PRIVATE

INTEGER, PARAMETER, PUBLIC :: ihash = int32

PUBLIC hash

INTERFACE hash
    MODULE PROCEDURE murmur_hash
END INTERFACE


CONTAINS


INTEGER(ihash) FUNCTION get_chunk(nstr, str, i) RESULT(n)
  INTEGER(ihash), INTENT(IN)      :: nstr
  INTEGER(ihash), INTENT(INOUT)   :: i
  CHARACTER(len=nstr), INTENT(IN) :: str
  
  n = 0
  n = ichar(str(i:i), ihash)
  n = ishft(n, 8)
  
  IF (i + 3 <= nstr) THEN
    n = n + ichar(str(i+1:i+1), ihash)
    n = ishft(n, 8)
    n = n + ichar(str(i+2:i+2), ihash)
    n = ishft(n, 8)
    n = n + ichar(str(i+3:i+2), ihash)
    i = i + 4
  ELSEIF (i + 2 <= nstr) THEN
    n = n + ichar(str(i+1:i+1), ihash)
    n = ishft(n, 8)
    n = n + ichar(str(i+2:i+2), ihash)
    n = ishft(n, 8)
    i = nstr + 1
  ELSEIF (i + 1 <= nstr) THEN
    n = n + ichar(str(i+1:i+1), ihash)
    n = ishft(n, 16)
    i = nstr + 1
  ELSE
    n = ishft(n, 16)
    i = nstr + 1
  END IF

END FUNCTION


INTEGER(ihash) FUNCTION murmur_hash(str, seed) RESULT(hsh)
  CHARACTER(len=*), INTENT(IN) :: str
  INTEGER(ihash), INTENT(IN)   :: seed
  INTEGER(ihash)               :: nstr
  INTEGER(ihash)               :: c1, c2, r1, r2, m, n, istr, chunk, k, tmp
  
  ! May need some improvements
  c1 = int(z'cc9e2d51', ihash)
  c2 = int(z'1b873593', ihash)
  r1 = 15
  r2 = 13
  m = 5
  n = int(z'e6546b64', ihash)
  hsh = seed
  nstr = len_trim(str, ihash)
  istr = 1
  
  DO WHILE (istr <= nstr)
    chunk = get_chunk(nstr, str, istr)
    k = chunk
    k = k * c1
    k = ishftc(k, r1)
    k = k * c2
    hsh = ieor(hsh, k)
    hsh = ishftc(hsh, r2)
    hsh = hsh * m + n
  END DO
  
  hsh = ieor(hsh, nstr)
  hsh = ieor(hsh, ishft(hsh, -16))
  tmp = int(z'85ebca6b', ihash)
  hsh = hsh * tmp
  hsh = ieor(hsh, ishft(hsh, -13))
  tmp = int(z'c2b2ae35', ihash)
  hsh = hsh * tmp
  hsh = ieor(hsh, ishft(hsh, -16))
  ! Fortran doesn't have unsigned numbers
  ! Ensure we get positive 32-bit number
  ! Effectively, hash is 31-bit.
  tmp = int(z'7FFFFFFF', ihash)
  hsh = iand(hsh, tmp)
END FUNCTION

END MODULE