Module func
    Implicit None
contains
    subroutine readcmd (keyword, pars)
        ! this is the subroutine reads a line of input from command line, and
        ! it returns the first phrase as keyword using variable 'keyword', and the 
        ! returns the following numerical parameters using the allocable real array 'pars'
    
    Character (len=:), allocatable, intent(inout) :: keyword
    real, allocatable, intent(inout) :: pars(:)
    character (len=80) :: cmdline, kwt


        ! read the cmdline of input and output
    read(*, '(a80)') cmdline
    read(cmdline, *) kwt
    if (allocated (keyword)) deallocate(keyword)
    if (allocated (pars)) deallocate(pars)
    allocate (character(len = len_trim(adjustl(kwt))) :: keyword)
    allocate (pars(ct_pars(cmdline)))
    read (cmdline, *) keyword, pars
    end subroutine

    function ct_pars(cmdline) result (npars)
        ! This FUNCTION counts the number of space separated parameters in the 
        ! command line string 'cmdline'.
        ! Abbreviations:
        ! * par = Parameter
        ! * cmd = Command
        ! * tmp = Temporary
        ! * n = Number
    character (len=*), intent(in) :: cmdline
    character (len=:), allocatable :: ctmp
    integer :: npars, i

    npars = 0
    i = 1
    allocate (character(len=len_trim(adjustl(cmdline))) :: ctmp)
    ctmp = trim(adjustl(cmdline))
    do
        if(ctmp(i:i) /= '') then
            i = i + 1
        else
            npars = npars + 1
            do
                i = i + 1
                if (ctmp(i:i) /= '') exit
            end do
        end if
            if (i == len(ctmp)) exit
    end do

    end function
end module




! this is the main program of area select calculation
program ifpractice
    use func
    implicit none
    real, parameter :: pi = acos(-1.)
    character (len=:), allocatable :: keywd
    real, allocatable :: pars(:)


    Do
        call readcmd(keywd , pars)
        
        ! this section for shape selection of name input
        ! first read of keyword to mean 'end' of program
        if (keywd == 'end') then
            exit
        ! second routine of reaching circle area calculation
        else if (keywd == 'cir' .or. keywd == 'sqr') then
            ! the wrong input with judgement 
            if (size(pars) /= 1) then
                write(*,*) 'Incorrect number of parameter.'
                cycle
            end if
        ! third routine of reaching retangular area calculation
        else if (keywd == 'rect') then
            if (size(pars) /= 2) then
                write(*,*) 'Incorrect number of parameter.'
                cycle
            end if
        ! different with other input of shape name
        else
            write(*,*) 'Keyword unknown ...'
            cycle
        end if


        ! this is the section of area calculation
        if (keywd == 'cir') then
            write(*,*) 'Circle Area = ', pi * pars(1)**2
        else if (keywd == 'sqr') then
            write(*,*) 'Square Area = ', pars(1)**2
        else if (keywd == 'rect') then
            write(*,*) 'Retangular Area = ', pars(1)*pars(2)
        else
            write(*,*) 'wrong input. No such directory input. BAD RESULT!'
        end if

    end do



    
end program ifpractice








