program fjsontest

use fjson_m

implicit none

type(fjson_t) :: json, ab, ac, ad, tmp

character(len=16), dimension(2) :: abpath = ["A", "B"], &
    & acpath = ["A", "C"], adpath = ["A", "D"]

real(c_double) :: abx
integer :: acx
character(len=256) :: adx


call json%init_from_file("test.json")
ab = json%sub(abpath)
ac = json%sub(acpath)
ad = json%sub(adpath)

abx = ab%get_num()
tmp = ac%at(1)
acx = tmp%get_int()
call ad%get_str(adx)

write (*,'(A)') "Testing FJSON"
write (*,'(A)') "=================================="
write (*, '(A, F6.4)') "J['A']['B']    = ", abx
write (*, '(A, I6)')   "J['A']['C'][1] = ", acx
write (*, '(A, A, A)') "J['A']['D']    = '", adx, "'"
write (*,'(A)') "=================================="

end program