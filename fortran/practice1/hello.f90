program num
    implicit none
    integer, parameter :: DP = selected_real_kind(r = 50, p = 14)
    real(Kind = DP) :: r1 = 1.0_DP , r2 , r3
    ! real*8 r1 = 1.6e0
    ! real(8) r1 = 1.6D0
    ! double precission r1
    write(*,*) kind(r1) , r1
    r1 = r1 +1.6_DP

    write(*,*) r1
    r1 = r1 + 1.6

    write(*,*) r1
end program num