MODULE hashtable_m

USE ustorage_m
USE hash_m

IMPLICIT NONE
PRIVATE
PUBLIC :: hashtable_t, hashtable_entry_t

TYPE hashtable_entry_t(slen, dlen)
  INTEGER, len :: slen, dlen
  LOGICAL :: is_occupied
  CHARACTER(len=slen) :: key
  INTEGER(ihash) :: hash
  TYPE(ustorage_t(dlen)) :: value
  CONTAINS
  PROCEDURE, PASS(self) :: clear => hashtable_entry_clear
END TYPE

INTEGER, parameter ::   hashtable_default_size = 1024,  &
    &                   hashtable_default_seed = 1
REAL, parameter ::      hashtable_default_growth = 1.5

TYPE hashtable_t(slen, dlen)
  INTEGER, len :: slen, dlen
  INTEGER(ihash) :: seed
  REAL :: growth_factor
  INTEGER :: size
  TYPE(hashtable_entry_t(slen, dlen)), ALLOCATABLE :: entries(:)
  CONTAINS
  PROCEDURE, PASS(self) :: init => hashtable_init
  PROCEDURE, PASS(self) :: set => hashtable_set
  PROCEDURE, PASS(self) :: get => hashtable_get
  PROCEDURE, PASS(self) :: grow => hashtable_grow
END TYPE


CONTAINS


SUBROUTINE hashtable_entry_clear(self)
  CLASS(hashtable_entry_t(*,*)), INTENT(INOUT) :: self
  self%is_occupied = .false.
  self%key         = ''
  self%hash        = 0
END SUBROUTINE
    
SUBROUTINE hashtable_init(self, init_size, seed, growth_factor)
  CLASS(hashtable_t(*,*)), INTENT(INOUT)        :: self
  INTEGER, INTENT(IN), OPTIONAL                 :: init_size
  INTEGER(ihash), INTENT(IN), OPTIONAL          :: seed
  REAL, INTENT(IN), OPTIONAL                    :: growth_factor

  !TYPE(hashtable_entry_t(self%slen, self%dlen)) :: dummy_entry
  INTEGER                                       :: n, i
  INTEGER(ihash)                                :: s
  REAL                                          :: f

  n = hashtable_default_size
  s = hashtable_default_seed
  f = hashtable_default_growth
  IF (present(init_size))     n = init_size
  IF (present(seed))          s = seed
  IF (present(growth_factor)) f = growth_factor
  self%seed          = s
  self%growth_factor = f
  self%size          = 0
  ! ALLOCATE(self%entries(n), source = dummy_entry)
  ALLOCATE(self%entries(n))
  
  DO i = 1, n
    CALL self%entries(i)%clear()
  END DO

END SUBROUTINE


SUBROUTINE hashtable_grow(self)
  CLASS(hashtable_t(*,*)), INTENT(INOUT)      :: self
  INTEGER                                     :: capacity, new_capacity, i, j
  TYPE(hashtable_entry_t(self%slen, self%dlen)), ALLOCATABLE :: temp(:)
  
  capacity     = size(self%entries)
  new_capacity = int(capacity * self%growth_factor)
  
  CALL move_alloc(self%entries, temp)
  CALL self%init(init_size=new_capacity, seed=self%seed, growth_factor=self%growth_factor)
  
  ! Find new places for each item
  DO i = 1, capacity
    j = hashtable_find_position(self, temp(i)%hash, temp(i)%key)
    self%entries(j) = temp(i)
  END DO

END SUBROUTINE


RECURSIVE INTEGER FUNCTION hashtable_find_position(self, hash_key, key) RESULT(i)
  CLASS(hashtable_t(*,*)), INTENT(IN) :: self
  INTEGER, INTENT(IN)                 :: hash_key
  CHARACTER(len=*), INTENT(IN)        :: key
  
  INTEGER                             :: capacity, istart, j
  
  capacity = size(self%entries)
  ! Keep track of 0-based indices -- easier this way
  istart   = mod(hash_key, capacity)
  
  ! We know that we won't besearching for more than capacity times
  searching: BLOCK
    DO j = 0, capacity-1
      i = modulo(istart+j, capacity)
      IF (.not. self%entries(i+1)%is_occupied) EXIT searching
      IF (self%entries(i+1)%hash == hash_key) THEN
        IF (self%entries(i+1)%key == key) EXIT searching
      END IF
    END DO

    ! Only get here IF couldn't find the index
    i = -1
    RETURN

  END BLOCK searching

  ! Get here IF found the index
  ! Make it 1-based index
  i = i + 1

END FUNCTION


SUBROUTINE hashtable_set(self, key, value)
  CLASS(hashtable_t(*,*)), INTENT(INOUT), TARGET :: self
  CHARACTER(len=*), INTENT(IN)           :: key
  TYPE(*), INTENT(IN)                    :: value
  
  INTEGER(ihash)                         :: key_hash
  INTEGER                                :: ipos, nkey
  TYPE(hashtable_entry_t(self%slen,self%dlen)), POINTER :: pentry

  key_hash = hash(key, self%seed)
  ipos = hashtable_find_position(self, key_hash, key)
  
  IF (ipos < 1) THEN
    ! hashtable is full
    CALL self%grow()
    ipos = hashtable_find_position(self, key_hash, key)
  END IF

  IF (.not. self%entries(ipos)%is_occupied) self%size = self%size + 1
  

  nkey = len_trim(key)
  nkey = min(nkey, self%slen)

  pentry => self%entries(ipos)
  pentry%is_occupied = .true.
  pentry%hash        = key_hash
  pentry%key(1:nkey) = key(1:nkey)

  !self%entries(ipos)%is_occupied = .true.
  !self%entries(ipos)%hash        = key_hash
  !self%entries(ipos)%key(1:nkey) = key(1:nkey)
  CALL self%entries(ipos)%value%store(value)

END SUBROUTINE
    

SUBROUTINE hashtable_get(self, key, value, err)
  CLASS(hashtable_t(*,*)), INTENT(IN) :: self
  CHARACTER(len=*), INTENT(IN)        :: key
  TYPE(*), INTENT(INOUT)              :: value
  INTEGER, INTENT(OUT)                :: err

  INTEGER(ihash)                      :: key_hash
  INTEGER                             :: ipos
  
  err = 0
  key_hash = hash(key, self%seed)
  ipos = hashtable_find_position(self, key_hash, key)
  IF (ipos > 1) THEN
    
    IF (self%entries(ipos)%is_occupied) THEN
      CALL self%entries(ipos)%value%retrieve(value)
    ELSE
      err = -1
    END IF

  ELSE

    err = -1

  END IF
  
END SUBROUTINE


END MODULE
    