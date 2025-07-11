#IF SORT_TYPE == SORT_INTEGER

#define SORT_SUFFIX int
#undef SORT_KIND

#elif SORT_TYPE == SORT_REAL

#IF SORT_KIND < 10
#define SORT_SUFFIX real_sp
#ELSE
#define SORT_SUFFIX real_dp
#endif

#endif

#define SORT_NAME_1(SUFFIX) sort_insert_ ## SUFFIX
#define SORT_NAME(S) SORT_NAME_1(S)

#define QSORT_NAME_1(SUFFIX) qsort_ ## SUFFIX
#define QSORT_NAME(S) QSORT_NAME_1(S)



SUBROUTINE SORT_NAME(SORT_SUFFIX)(items)

#ifdef SORT_KIND
  INTEGER, PARAMETER :: wp = SELECTED_REAL_KIND(SORT_KIND)
  REAL(wp), INTENT(INOUT) :: items(:)
#ELSE
  INTEGER, INTENT(INOUT)  :: items(:)
#endif
  
  INTEGER :: i, j
#ifdef SORT_KIND
  REAL(wp) :: tmp
#ELSE
  INTEGER  :: tmp
#endif
  
  ! At each iteration, items(1:(i-1)) are sorted
  iloop: DO i = 2, size(items)
    ! need to keep it as items(i) may be overwritten
    tmp = items(i)
    j = i - 1
    ! Find the place for items(i) among 1:I
    ! initially "vacant" spot is position I (its current)
    ! On each cycle: J+1 is vacant
    jloop: DO WHILE (j >= 1)
      ! ITEMS(I) is IN right order relative to
      ! ITEMS(1:J) - exit
      IF (tmp >= items(j)) EXIT jloop
      ! Shift ITEMS(J) one position towards the END
      ! J+1 position is currently vacant
      items(j+1) = items(j)
      ! Now J is vacant
      ! TMP  < ITEMS(J+1), try next J
      j = j - 1
    END DO jloop
    ! On exit: J+1 is vacant position for TMP
    items(j+1) = tmp
    ! ITEMS(1:I) here are sorted
  END DO iloop
     
END SUBROUTINE

RECURSIVE SUBROUTINE QSORT_NAME(SORT_SUFFIX)(data)

#if SORT_TYPE == SORT_INTEGER
  INTEGER, INTENT(INOUT)  :: data(:)
  INTEGER                 :: pivot
#else
  INTEGER, PARAMETER      :: wp = SELECTED_REAL_KIND(SORT_KIND)
  REAL(wp), INTENT(INOUT) :: data(:)
  REAL(wp)                :: pivot
#endif
  INTEGER                 :: ipivot, n
  INTEGER                 :: i1, i2
  INTEGER, PARAMETER      :: qsort_min_size = 100
  
  n = size(data)  
  
  IF (n < qsort_min_size) THEN
      CALL sort_insert(data)
      RETURN
  END IF
  
  ipivot = (n + 1) / 2
  pivot = data(ipivot)
  CALL three_way_partition(i1, i2)
  IF (i1 > 1) CALL QSORT_NAME(SORT_SUFFIX)(data(1:i1))
  IF (i2 < n) CALL QSORT_NAME(SORT_SUFFIX)(data(i2:n))

  CONTAINS

    SUBROUTINE three_way_partition(i, k)
      INTEGER, INTENT(OUT)    :: i, k
      INTEGER :: j
      i = 1
      k = size(data)
      j = 1
      DO WHILE (j <= k)
        IF (data(j) < pivot) THEN
          CALL swap(data(i), data(j))
          i = i + 1
          j = j + 1
        ELSEIF (data(j) > pivot) THEN
          CALL swap(data(j), data(k))
          k = k - 1
        ELSE
          j = j + 1
        END IF
      END DO
      ! I points to beginning of pivot.
      ! Make I to point to END of < pivot
      i = i - 1
      ! The same for K IN opposite direction
      k = k + 1

    END SUBROUTINE

    SUBROUTINE swap(x, y)
#if SORT_TYPE == SORT_INTEGER
      INTEGER, INTENT(INOUT)  :: x, y
      INTEGER                 :: tmp
#else
      REAL(wp), INTENT(INOUT) :: x, y
      REAL(wp)                :: tmp
#endif
      tmp = x
      x = y
      y = tmp
    END SUBROUTINE

END SUBROUTINE
