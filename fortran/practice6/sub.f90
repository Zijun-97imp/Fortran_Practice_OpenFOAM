!!Function as the routine including calculation methods
! Program book_info
!     Implicit None

!     integer :: i, publish_year = 2018
!     integer :: PrintCopyRight
!     i = PrintCopyRight('www.google.com/scholar/', publish_year)
!     i = PrintCopyRight('www.github.com/usr/home/', publish_year)
! End Program book_info


! Integer Function PrintCopyRight (cDomain, publish_year)
! Implicit None
! Character (len=*), Intent( IN ) :: cDomain           !Only at here with dummy arguments for defining certain data type
! Integer, Intent( IN ) :: publish_year
! write (*,*) 'Power by ', cDomain, publish_year
! PrintCopyRight = publish_year
! return
! End Function PrintCopyRight




!！Write the matrix, by using the system of subroutine
!!Assumed Shape of matrix dimension and limitation define
! Program matrix
!     implicit None
!     integer :: x (3,3) = reshape( &
!     [1,2,3, &
!      4,5,6, &
!      7,8,9], [3,3])

!     call WriteMatrix( x )

! contains
!     Subroutine Writematrix (iMat)
!         integer, Intent (IN) :: iMat(:,:)
!         integer :: i
!         Do i = 1, size (iMat, dim = 2)
!             write (*,*) iMat (:,i)
!         End Do
!     End Subroutine
! End Program matrix





!!Arugments with the same type of definition
!!Type define should keep the same including the 'main program' and 'subroutine'
! Module typedef
!     implicit None
!     !small loop of type definition
!     type ST
!         real :: a, b
!     end type
! End Module typedef

! Program main
!     use typedef
!     implicit None

!     type (ST) :: st1
!     call sub (st1)

! End Program main

! !subroutine for 'call' section working
! subroutine sub (stda)
!     use typedef
!     implicit None

!     type (ST) :: stda
!     stda%a = 1.0

! end subroutine sub




!!Example of clock time transfer
!!Transferring the clock hour-time to the format: XXhr XXmin XX.XXXsec
Module time_trans
    implicit None
    integer, parameter :: DP = selected_real_kind (p = 9)
    Type Hour_degree
        integer (kind = 2) :: d, m
        real :: s
    End Type Hour_degree

contains
    Type (Hour_degree) Function RecordDegree_to_TypeDegree (rDeg) result (stDeg)
        real (kind = DP), Intent (IN) :: rDeg
        real (kind = DP) :: record
        
        !Using the mode calculation to gain the integer part
        stDeg%d = int(rDeg)
        record = rDeg - stDeg%d
        stDeg%m = (record) * 60.0_DP
        record = record - stDeg%m/60.0_DP
        stDeg%s = (record) * 60.0_DP**2
    End Function RecordDegree_to_TypeDegree
End Module time_trans


program main
    use time_trans
    implicit None
    type (Hour_degree) :: standard
    real (kind = DP) :: recordtype
    do
        read (*,*) recordtype
        standard = RecordDegree_to_TypeDegree(recordtype)
        write (*,*) standard
    end do

    stop
end program

