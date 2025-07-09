program fjsontest

use fjson_m
use iso_c_binding, only: c_double
use error_m

implicit none

type(fjson_t) :: json, ab, ac, ad, tmp

character(len=16), dimension(2) :: abpath = ["A", "B"], &
    & acpath = ["A", "C"], adpath = ["A", "D"]

real(c_double) :: abx
integer :: acx, istat
character(len=256) :: adx


call json%init_from_file("test.json", istat)
if (istat /= no_error) then
    write (*, '(A)') 'Error opening file'
    call exit(-1)
end if

call json%sub(ab, abpath, istat)
if (istat /= no_error) then
    write (*, '(A)') 'Cannot access A B path'
end if

call json%sub(ac, acpath, istat)
if (istat /= no_error) then
    write (*, '(A)') 'Cannot access A C path'
end if

call json%sub(ad, adpath, istat)
if (istat /= no_error) then
    write (*, '(A)') 'Cannot access A D path'
end if


call ab%get_num(abx, istat)
if (istat /= no_error) then
    write (*, '(A)') 'Cannot read real number from A B path'
end if

call ac%at(tmp, 1, istat)
if (istat /= no_error) then
    write (*, '(A)') 'Cannot access array from A C path'
end if

call tmp%get_int(acx, istat)
if (istat /= no_error) then
    write (*, '(A)') 'Cannot read int from A C 1 path'
end if

call ad%get_str(adx, istat)
if (istat /= no_error) then
    write (*, '(A)') 'Cannot read string from A D path'
end if

write (*,'(A)') "Testing FJSON"
write (*,'(A)') "=================================="
write (*, '(A, F6.4)') "J['A']['B']    = ", abx
write (*, '(A, I6)')   "J['A']['C'][1] = ", acx
write (*, '(A, A, A)') "J['A']['D']    = '", adx, "'"
write (*,'(A)') "=================================="

end program