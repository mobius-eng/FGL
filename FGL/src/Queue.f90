module queue_m
    
    use list_m
    implicit none
    
    type queue_t(len)
        integer, len :: len
        type(list_t(len)) :: buffer
        integer :: head, tail, count
    contains
        procedure, pass(self) :: push_back => queue_push_back
        procedure, pass(self) :: pop_front => queue_pop_front
        procedure, pass(self) :: init      => queue_init
        procedure, pass(self) :: peek_head  => queue_peek_head
    end type

contains
    
    subroutine queue_init(self, capacity)
        class(queue_t(*)), intent(inout) :: self
        integer :: capacity
        call self%buffer%init(capacity)
        self%head = 0
        self%tail = 0
        self%count = 0
    end subroutine
    
    subroutine queue_push_back(self, item)
        class(queue_t(*)), intent(inout) :: self
        type(*), intent(in) :: item
        self%tail = self%tail + 1
        if (self%tail > self%buffer%capacity()) self%tail = 1
        call self%buffer%set(self%tail, item)
        self%count = self%count + 1
        if (self%count == 1) self%head = self%tail
    end subroutine
    
    subroutine queue_pop_front(self, item)
        class(queue_t(*)), intent(inout) :: self
        type(*), intent(inout) :: item
        if (self%count > 0) then
            call self%buffer%get(self%head, item)
            self%count = self%count - 1
            self%head = self%head + 1
            if (self%head > self%buffer%capacity()) self%head = 1
        end if
    end subroutine
    
    subroutine queue_peek_head(self, item)
        class(queue_t(*)), intent(inout) :: self
        type(*), intent(inout) :: item
        if (self%count > 0) call self%buffer%get(self%head, item)
    end subroutine

end module