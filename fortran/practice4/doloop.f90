Program loop
    implicit None
    integer :: sum, i
     
    !! this is the normal number increase
    ! sum = 0
    ! Do i = 1, 100
    !     sum = sum + i
    ! End Do

    ! write(*,*) 'the sum result= ', sum

    !! this is the number increase with gap
    ! sum = 0
    ! Do i = 1, 100, 2
    !     sum = sum + i
    ! End Do
    ! write(*,*) 'the sum result= ', sum

    ! ! this is the number inverse increase
    ! sum = 0
    ! Do i = 100, 90, -1
    !     sum = sum + i
    ! End Do
    ! write(*,*) 'the sum result= ', sum

    sum = 0
    i = 0
    Do while (i < 100)
        i = i + 1   
        sum = sum + i
    End Do
    print *, 'the sum result= ', sum
end program


