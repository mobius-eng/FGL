module hashtable_m
    
    use hash_m
    
    implicit none
    
    type, abstract :: hashtable_entry_t
    contains
        procedure(hashtable_entry_copy), pass(self), deferred:: copy
        generic :: assignment(=) => copy
    end type
    
    type, abstract :: hashtable_key_t
        integer(ihash) :: hash
    contains
        procedure(hashtable_key_get), deferred, pass(self) :: get
        procedure(hashtable_key_store), deferred, pass(self) :: store
        procedure, pass(self) :: set => hashtable_key_set
    end type
    
    
    abstract interface
        
        subroutine hashtable_entry_copy(self, other)
            import :: hashtable_entry_t
            class(hashtable_entry_t), intent(inout) :: self
            class(hashtable_entry_t), intent(in) :: other
        end subroutine
    
        function hashtable_key_get(self)
            import :: hashtable_key_t
            class(hashtable_key_t), intent(in) :: self
            character(len=:), allocatable :: hashtable_key_get
        end function
        
        subroutine hashtable_key_store(self, key)
            import :: hashtable_key_t
            class(hashtable_key_t), intent(inout) :: self
            character(len=*), intent(in) :: key
        end subroutine
        
    end interface
    
    
    integer, parameter ::   hashtable_default_size = 1024,  &
        &                   hashtable_default_seed = 1
    real, parameter ::      hashtable_default_growth = 1.5
    
    
    type hashtable_t
        integer(ihash) :: seed
        real :: growth_factor
        integer :: size
        logical :: is_initialized
        class(hashtable_entry_t), dimension(:), allocatable :: entries
        class(hashtable_key_t), dimension(:), allocatable :: keys
        logical, dimension(:), allocatable :: occupied
    contains
        procedure, pass(self) :: init => hashtable_init
        procedure, pass(self) :: set => hashtable_set
        procedure, pass(self) :: get => hashtable_get
        procedure, pass(self) :: grow => hashtable_grow
    end type
    
    
contains
    

    subroutine hashtable_key_set(self, key, seed)
        class(hashtable_key_t), intent(inout) :: self
        character(len=*), intent(in) :: key
        integer(ihash) :: seed
        self%hash = hash(key, seed)
        call self%store(key)
    end subroutine
    
    
    subroutine hashtable_init(self, entry, key, init_size, seed, growth_factor)
        class(hashtable_t), intent(inout) :: self
        class(hashtable_entry_t), intent(in) :: entry
        class(hashtable_key_t), intent(in) :: key
        integer, intent(in), optional :: init_size
        integer(ihash), intent(in), optional :: seed
        real, intent(in), optional :: growth_factor
        integer :: n, i, allocate_status
        integer(ihash) :: s
        real :: f
        n = hashtable_default_size
        s = hashtable_default_seed
        f = hashtable_default_growth
        if (present(init_size)) n = init_size
        if (present(seed)) s = seed
        if (present(growth_factor)) f = growth_factor
        self%seed = s
        self%growth_factor = f
        self%size = 0
        allocating: block
            self%is_initialized = .true.
            allocate(self%entries(n), mold = entry, stat = allocate_status)
            if (allocate_status /= 0) exit allocating
            allocate(self%keys(n), mold = key, stat = allocate_status)
            if (allocate_status /= 0) exit allocating
            allocate(self%occupied(n), source = .false., stat = allocate_status)
            if (allocate_status /= 0) exit allocating
            return
        end block allocating
        ! in case of allocation error:
        self%is_initialized = .false.
        if (allocated(self%entries)) deallocate(self%entries)
        if (allocated(self%keys)) deallocate(self%keys)
        if (allocated(self%occupied)) deallocate(self%occupied)
    end subroutine
    
    
    function hashtable_create(entry, key, init_size, seed, growth_factor) result (h)
        class(hashtable_entry_t), intent(in) :: entry
        class(hashtable_key_t), intent(in) :: key
        integer, intent(in), optional :: init_size
        integer(ihash), intent(in), optional :: seed
        real, intent(in), optional :: growth_factor
        type(hashtable_t) :: h
        call h%init(entry, key, init_size, seed, growth_factor)
    end function
    
    
    subroutine hashtable_grow(self)
        class(hashtable_t), intent(inout) :: self
        integer :: capacity, new_capacity, i, j
        class(hashtable_entry_t), dimension(:), allocatable :: temp_entries
        class(hashtable_key_t), dimension(:), allocatable :: temp_keys
        logical, dimension(:), allocatable :: temp_occupied
        capacity = size(self%entries)
        new_capacity = int(capacity * self%growth_factor)
        call move_alloc(self%entries, temp_entries)
        call move_alloc(self%keys, temp_keys)
        call self%init(entry = temp_entries(1), key = temp_keys(1), &
        &   init_size = new_capacity, seed = self%seed,             &
        &   growth_factor = self%growth_factor)
        ! Find new places for each item
        do i = 1, capacity
            j = hashtable_find_position(self, temp_keys(i)%hash, temp_keys(i)%get())
            self%entries(j) = temp_entries(i)
            self%keys(j) = temp_keys(i)
            self%occupied(j) = .true.
        end do
    end subroutine
    
    
    recursive integer function hashtable_find_position(self, hash_key, key) result(i)
        class(hashtable_t), intent(in) :: self
        integer, intent(in) :: hash_key
        character(len=*), intent(in) :: key
        integer :: capacity, istart, j
        capacity = size(self%entries)
        ! Keep track of 0-based indices -- easier this way
        istart = mod(hash_key, capacity)
        ! We know that we won't be searching for more than capacity times
        searching: block
            do j = 0, capacity-1
                i = modulo(istart+j, capacity)
                if (.not. self%occupied(i+1)) exit searching
                if (self%keys(i+1)%hash == hash_key) then
                    if (self%keys(i+1)%get() == key) exit searching
                end if
            end do
            ! Only get here if couldn't find the index
            i = -1
            return
        end block searching
        ! Get here if found the index
        ! Make it 1-based index
        i = i + 1
    end function
    
    
    subroutine hashtable_set(self, key, value)
        class(hashtable_t), intent(inout) :: self
        character(len=*), intent(in) :: key
        class(hashtable_entry_t), intent(in) :: value
        ! integer, intent(out) :: err
        integer(ihash) :: key_hash
        integer :: ipos
        ! err = 0
        key_hash = hash(key, self%seed)
        ipos = hashtable_find_position(self, key_hash, key)
        if (ipos < 1) then
            ! hashtable is full
            call self%grow()
            ipos = hashtable_find_position(self, key_hash, key)
        end if
        if (.not. self%occupied(ipos)) self%size = self%size + 1
        self%occupied(ipos) = .true.
        self%keys(ipos)%hash = key_hash
        call self%keys(ipos)%set(key, self%seed)
        self%entries(ipos) = value
    end subroutine
    
    
    function hashtable_get(self, key) result(value)
        class(hashtable_t), intent(in), target :: self
        character(len=*), intent(in) :: key
        class(hashtable_entry_t), pointer :: value
        integer(ihash) :: key_hash
        integer :: ipos
        value => null()
        key_hash = hash(key, self%seed)
        ipos = hashtable_find_position(self, key_hash, key)
        if (ipos > 1) then
            if (self%occupied(ipos)) value => self%entries(ipos)
        end if
    end function
    
    
end module
    