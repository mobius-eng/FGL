program HashDemo
    
use hashtable_m

implicit none

type point_t
    real :: x, y
end type

integer, parameter :: point_size = sizeof(point_t(0.0, 0.0))
integer, parameter :: keylen = 64

character(len=keylen) :: str
type(point_t) :: p, q
type(hashtable_t) :: htbl
integer :: htbl_err

WRITE (*,*) 'Hashtable demo'
call htbl%init(keylen, point_size, init_size = 10)

p%x = 1.0
p%y = 2.0
str = 'Point_1'
call htbl%set(str, p)
p%x = 10.0
p%y = 20.0
str = 'Point_2'
call htbl%set(str, p)

call htbl%get('Point_1', q, htbl_err)
if (htbl_err /= 0) then
  write (*,*) 'Hashtable GET error Point_1'
else
  write (*,*) 'Point_1', q
end if

call htbl%get('Point_2', q, htbl_err)
if (htbl_err /= 0) then
  write (*,*) 'Hashtable GET error Point_2'
else
  write (*,*) 'Point_2', q
end if

call htbl%get('Point_3', q, htbl_err)
if (htbl_err /= 0) then
  write (*,*) 'Cannot get "Point_3" as expected'
else
  write (*,*) 'Unexepectedly getting "Point_3": ', q
end if

end program