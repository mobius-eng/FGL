RECURSIVE SUBROUTINE qsort(data)

  DATA_TYPE, INTENT(INOUT) :: data(:)
  DATA_TYPE :: pivot

  INTEGER                 :: ipivot, n
  INTEGER                 :: i1, i2
  INTEGER, PARAMETER      :: qsort_min_size = 100
  
  n = size(data)  
  
  IF (n < qsort_min_size) THEN
      CALL isort(data)
      RETURN
  END IF
  
  ipivot = (n + 1) / 2
  pivot = data(ipivot)
  CALL three_way_partition(i1, i2)
  IF (i1 > 1) CALL qsort(data(1:i1))
  IF (i2 < n) CALL qsort(data(i2:n))

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
      DATA_TYPE, INTENT(INOUT) :: x, y
      DATA_TYPE :: tmp
      tmp = x
      x = y
      y = tmp
    END SUBROUTINE

END SUBROUTINE
