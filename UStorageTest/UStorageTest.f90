program ustorage_test


use ustorage_m

implicit none

type point_t
    real :: x, y
end type

integer, parameter :: point_size = sizeof(point_t(0.0, 0.0))
type(ustorage_t(point_size)) :: us
type(point_t) :: p1, p2

p1 = point_t(1.0, 2.0)

call us%store(p1)
call us%retrieve(p2)

write (*, *) p2%x, p2%y


end program