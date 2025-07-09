MODULE queue_m

USE list_m
IMPLICIT NONE
PRIVATE

TYPE, PUBLIC :: queue_t(len)
  INTEGER, len :: len
  TYPE(list_t(len)) :: buffer
  INTEGER :: head, tail, count
  CONTAINS
  PROCEDURE, PASS(self) :: push_back => queue_push_back
  PROCEDURE, PASS(self) :: pop_front => queue_pop_front
  PROCEDURE, PASS(self) :: init      => queue_init
  PROCEDURE, PASS(self) :: peek_head  => queue_peek_head
END TYPE


CONTAINS


SUBROUTINE queue_init(self, capacity)
  CLASS(queue_t(*)), INTENT(INOUT) :: self
  INTEGER :: capacity
  CALL self%buffer%init(capacity)
  self%head = 0
  self%tail = 0
  self%count = 0
END SUBROUTINE


SUBROUTINE queue_push_back(self, item)
  CLASS(queue_t(*)), INTENT(INOUT) :: self
  TYPE(*), INTENT(in) :: item
  self%tail = self%tail + 1
  IF (self%tail > self%buffer%capacity()) self%tail = 1
  CALL self%buffer%set(self%tail, item)
  self%count = self%count + 1
  IF (self%count == 1) self%head = self%tail
END SUBROUTINE


SUBROUTINE queue_pop_front(self, item)
  CLASS(queue_t(*)), INTENT(INOUT) :: self
  TYPE(*), INTENT(INOUT) :: item
  IF (self%count > 0) THEN
    CALL self%buffer%get(self%head, item)
    self%count = self%count - 1
    self%head = self%head + 1
    IF (self%head > self%buffer%capacity()) self%head = 1
  END IF
END SUBROUTINE


SUBROUTINE queue_peek_head(self, item)
  CLASS(queue_t(*)), INTENT(INOUT) :: self
  TYPE(*), INTENT(INOUT) :: item
  IF (self%count > 0) CALL self%buffer%get(self%head, item)
END SUBROUTINE


END MODULE