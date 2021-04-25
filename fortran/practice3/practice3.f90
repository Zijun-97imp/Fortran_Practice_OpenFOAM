Program main
    Implicit None
    Integer, DIMENSION (2,2) :: a , b

    a = reshape((/1,2,3,4/),(shape(a)))
    b = reshape((/1,3,4,2/),(shape(b)))

    if (all(a==b)) then
        write(*,*) 'a and b are identical.'
    else if (any(a==b)) then
        write(*,*) 'a and b are partly identical.'
    else if (all(a/=b)) then
        write(*,*) 'a and b are fully not identical.'
    end if
    
End program main