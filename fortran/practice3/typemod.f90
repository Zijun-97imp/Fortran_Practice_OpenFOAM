Module types
    type :: var_base
    character (len = 10) :: name
    end type

    type, extends (var_base) :: real_var
    real :: r
    end type

    type, extends (var_base) :: int_var
    integer :: i
    end type

contains
    subroutine pnt_var(var)
        implicit None
        class (var_base), intent (in) :: var

        select type (var)
        class is (real_var)
        write(*,*) 'Real Value' // trim(var%name) // ' = ', var%r
        class is (int_var)
        write(*,*) 'Integer Value' // trim(var%name) // ' = ', var%i
        class default
        error stop 'Unkown Type.'
    end select
    end subroutine
end module



! the main program section
Program main
    use types
    implicit None
    Type (real_var) :: vr = real_var('a', 3.14)
    Type (int_var) :: vi = int_var('b', 3)

    call pnt_var(vr)
    call pnt_var(vi)

End Program
