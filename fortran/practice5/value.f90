!!Parameter gourping
! Program grouping_data
!     Implicit None

!     ! Parameter type of data grouping
!     Integer, Parameter :: SUN = 0, MON = 1, TUE = 2, WED = 3, THU = 4, FRI = 5, SAT = 6

!     ! Seperate the normal input and wrong input
!     Integer :: WKDAY, ERR

!     print *, 'SUN = 0, MON = 1, TUE = 2, WED = 3, THU = 4, FRI = 5, SAT = 6'
!     ! Using Do loop to manage the input selection
!     Do
!         write(*,*) 'Please input the integer number:'
!         read(*,*) WKDAY
!         ERR = 0

!         select case(WKDAY)
!         case (MON:FRI)
!             print *, WKDAY, 'is WEEKDAY!'
!             exit
!         case (SUN,SAT)
!             print *, WKDAY, 'is WEEKEND!'
!             exit
!         case default
!             print *, 'Are you serious?'
!             ERR = 1
!             exit
!         end select

!         if (ERR /= 0) exit

!     End Do

!     Print *, 'The day selection program ended!'
!     stop


! End Program grouping_data




!!Dimension gourping
!!The array is presenting as 'DIMENSION'
! Program dimension_data
!     Implicit None

!     integer :: i
!     integer, Dimension(5) :: arr
!     data arr/1,2,3,4,5/

!     write(*,'(A,1X,A)') 'index', 'value'
!     Do i = 1,5
!         write(*,'(2I5)') i, arr(i)
!     End Do

!     print *
!     write(*,'(A,1X,A)') 'MAX_LOC', 'MAX_VAL'
!     write(*,'(2I5)') maxloc(arr), maxval(arr)

!     print *, 'Program Terminate.'
!     stop
! End program dimension_data











!!The struct-type of data collection
! Program structure
!     implicit None

!     type :: STUDENT
!     character (len=8) :: ID
!     character (len=100) :: NAME
!     character (len=20) :: CONDI1, CONDI2, CONDI3
!     integer :: CHINESE, MATH, LANGUAGE
!     end type

!     type (STUDENT) :: ONE
!     ONE%ID = '01811420'
!     ONE%NAME = 'zijun fang'
!     ONE%CHINESE = 80
!     ONE%CONDI1 = 'PASS - B(-)'
!     ONE%MATH = 97
!     ONE%CONDI2 = 'PASS - A(+)'
!     ONE%LANGUAGE = 85
!     ONE%CONDI3 = 'PASS - B()'

!     write(*,*) '#######Student Information:'
!     write(*,*) '              NAME:', ONE%NAME
!     write(*,*) '        STUDENT ID:', ONE%ID
!     write(*,*) '    CHINESE RESULT:', ONE%CHINESE
!     write(*,*) ' CHINESE CONDITION:', ONE%CONDI1
!     write(*,*) '       MATH RESULT:', ONE%MATH
!     write(*,*) '    MATH CONDITION:', ONE%CONDI2
!     write(*,*) '   LANGUAGE RESULT:', ONE%LANGUAGE
!     write(*,*) 'LANGUAGE CONDITION:', ONE%CONDI3
    
! End Program structure







!!Application of 'struct-type' as Module input
! Module Student_System
!     !Form the type-struct data input system
!     type :: STUDENT
!     character (len=100) :: NAME
!     character (len=8) :: ID
!     character (len=20) :: CONDI1, CONDI2, CONDI3
!     integer :: CHINESE, MATH, LANGUAGE
!     contains
!     procedure :: Print_Student_Information => Print_Student_Information
!     end type STUDENT

!     !Form the printing system
! contains
!     subroutine Print_Student_Information (ONE)
!     class (STUDENT) :: ONE
!     write(*,*) '#######Student Information:'
!     write(*,*) '              NAME:', ONE%NAME
!     write(*,*) '        STUDENT ID:', ONE%ID
!     write(*,*) '    CHINESE RESULT:', ONE%CHINESE
!     write(*,*) ' CHINESE CONDITION:', ONE%CONDI1
!     write(*,*) '       MATH RESULT:', ONE%MATH
!     write(*,*) '    MATH CONDITION:', ONE%CONDI2
!     write(*,*) '   LANGUAGE RESULT:', ONE%LANGUAGE
!     write(*,*) 'LANGUAGE CONDITION:', ONE%CONDI3
!     end subroutine Print_Student_Information
! End Module Student_System

! !this is the main program
! Program struct_apply
!     Use Student_System
!     implicit None

!     type (STUDENT) :: zijunfang

!     zijunfang = STUDENT('Zijun Fang', '01811420', 'Pass -B(-)', 'Pass -A(+)', 'Pass -B()', 80, 97, 85)
!     call zijunfang%Print_Student_Information()

!     print *, 'Program run successful! Terminate.'
!     stop
! End program struct_apply






!!Application of static and dynamic array with storage using
!!Using the sub-routine to solve problems with dynamic array 'ALLOCATABLE'
! subroutine swap(autoarr1, autoarr2, N)
!     implicit None
!     integer :: N
!     real, Dimension(N) :: autoarr1, autoarr2
!     real, ALLOCATABLE, Dimension(:) :: dynarr    !dynamic array as intermedia array for swaping

!     allocate (dynarr(N))
!     dynarr (:N) = autoarr1 (:N)
!     autoarr1 (:N) = autoarr2 (:N)
!     autoarr2 (:N) = dynarr (:N)
!     deallocate (dynarr)

! end subroutine swap


! !this is the main program
! program swap_test
!     implicit none
!     !variables confirmation and definition
!     integer, parameter :: narr = 5
!     integer :: i
!     real, parameter :: pi = 3.1415926
!     real, dimension (narr) :: fixarr1, fixarr2

!     !the data element location of fixed array defined
!     fixarr1 (:narr) = (/(sin(pi*(i - 1.)/(narr - 1)), i = 1, narr)/)
!     fixarr2 (:narr) = (/(cos(pi*(i - 1.)/(narr - 1)), i = 1, narr)/)

!     !original data term in the array
!     !before swapping
!     write(*,*) 'the original array:'
!     write(*, '(7F12.6)') fixarr1 (:narr)
!     write(*, '(7F12.6)') fixarr2 (:narr)

!     call swap(fixarr1, fixarr2, narr)
!     !after swapping
!     write(*,*) 'after swapping method of calculation:'
!     write(*,*) 'after swapping:'
!     write(*, '(7F12.6)') fixarr1 (:narr)
!     write(*, '(7F12.6)') fixarr2 (:narr)
! end program swap_test







