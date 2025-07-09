MODULE list_m
    
USE ustorage_m, ONLY : ustorage_t

IMPLICIT NONE
PRIVATE

TYPE, PUBLIC :: list_t(len)
  INTEGER, len                       :: len
  INTEGER                            :: ilast
  TYPE(ustorage_t(len)), ALLOCATABLE :: items(:)
  CONTAINS
  PROCEDURE, PASS(self) :: adjust_size => list_adjust_size
  PROCEDURE, PASS(self) :: adjust_size_to => list_adjust_size_to
  PROCEDURE, PASS(self) :: push => list_push
  PROCEDURE, PASS(self) :: set => list_set
  PROCEDURE, PASS(self) :: get => list_get
  PROCEDURE, PASS(self) :: pop => list_pop
  PROCEDURE, PASS(self) :: capacity => list_capacity
  PROCEDURE, PASS(self) :: size => list_size
  PROCEDURE, PASS(self) :: init => list_init
END TYPE
    

CONTAINS
    

SUBROUTINE list_init(self, init_capacity)
  CLASS(list_t(*)), INTENT(INOUT) :: self
  INTEGER, INTENT(IN)             :: init_capacity
  TYPE(ustorage_t(self%len))      :: dummy_mold
  ALLOCATE(self%items(init_capacity), source = dummy_mold)
  self%ilast = 0
END SUBROUTINE


SUBROUTINE list_adjust_size(self)
  CLASS(list_t(*)), INTENT(INOUT)         :: self
  TYPE(ustorage_t(self%len)), ALLOCATABLE :: temp(:)
  INTEGER                                 :: nitems
  
  nitems = size(self%items)
  
  IF (self%ilast == nitems) THEN
    CALL move_alloc(self%items, temp)
    ALLOCATE(self%items(nitems * 2))
    self%items(1:self%ilast) = temp
    DEALLOCATE(temp)
  END IF
  
END SUBROUTINE


SUBROUTINE list_adjust_size_to(self, n)
  CLASS(list_t(*)), INTENT(INOUT)         :: self
  INTEGER, INTENT(IN)                     :: n
  TYPE(ustorage_t(self%len)), ALLOCATABLE :: temp(:)
  INTEGER                                 :: nitems
  
  nitems = size(self%items)
  
  IF (n > nitems) THEN
    CALL move_alloc(self%items, temp)
    ALLOCATE(self%items(n))
    self%items(1:self%ilast) = temp
    DEALLOCATE(temp)
  END IF
    
END SUBROUTINE


SUBROUTINE list_push(self, item)
  CLASS(list_t(*)), INTENT(INOUT) :: self
  TYPE(*), INTENT(IN), TARGET     :: item
  CALL self%adjust_size()
  self%ilast = self%ilast + 1
  CALL self%items(self%ilast)%store(item)
END SUBROUTINE


SUBROUTINE list_set(self, i, item)
  CLASS(list_t(*)), INTENT(INOUT) :: self
  INTEGER, INTENT(IN)             :: i
  TYPE(*), INTENT(IN), TARGET     :: item
  CALL self%adjust_size_to(i)
  ! note: items between (old) ilast and i become undefined
  IF (i > self%ilast) self%ilast = i
  CALL self%items(self%ilast)%store(item)
END SUBROUTINE


SUBROUTINE list_get(self, i, item)
  CLASS(list_t(*)), INTENT(IN) :: self
  INTEGER, INTENT(IN)          :: i
  TYPE(*), TARGET              :: item
  CALL self%items(i)%retrieve(item)
END SUBROUTINE


SUBROUTINE list_pop(self, item)
  CLASS(list_t(*)), INTENT(INOUT) :: self
  TYPE(*), INTENT(INOUT), TARGET  :: item
  CALL self%get(self%ilast, item)
  self%ilast = self%ilast - 1
END SUBROUTINE


PURE INTEGER FUNCTION list_capacity(self) RESULT(n)
  CLASS(list_t(*)), INTENT(IN) :: self
  n = size(self%items)
END FUNCTION


PURE INTEGER FUNCTION list_size(self) RESULT(n)
  CLASS(list_t(*)), INTENT(IN) :: self
  n = self%ilast
END FUNCTION


END module
        