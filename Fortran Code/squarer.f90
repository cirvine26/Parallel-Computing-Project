subroutine squarer(x, x_square)
    
    implicit none 
    integer, intent(in) :: x            !input
    integer, intent(out) :: x_square    !output 

    x_square = x**2

end subroutine squarer


!Now try calling the subroutine within a function 
program test_square
    implicit none 
    integer :: x, x_sqr 

    !Read in the wanted number to be squared 
    read*, x 

    !Call the subroutine 
    call squarer(x, x_sqr)

    !Print out the results 
    print*, 'x =', x 
    print*, 'x^2 =', x_sqr 

end program test_square