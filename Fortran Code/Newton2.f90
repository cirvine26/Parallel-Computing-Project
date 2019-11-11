!Rewrite the Newton file but using a subroutine that can find the root
function f(x)
    implicit none 

	real :: f, x
    f = x**3 + x - 3.0
    
end function f 


function df(x)
    !first derivative of f(x)
    implicit none 

	real :: df, x 
    df = 3 * x**2 + 1
    
end function df 


subroutine Newton(f, df, x) !Takes function, derivative and initial guess 
    implicit none 
    integer :: its = 0              !The number of current iterations
    real :: x                       !The initial guess
    integer :: max_it = 20          !The max number of iterations 
    real :: eps = 1.0e-7            !The maximum allowed error on f(x)
    logical :: converged = .FALSE.  !State of convergence

    !Initialse do loop to iterate over the function
    do while(its <= max_it .and. converged .eqv. .FALSE.)
        x = x - (f(x)/df(x))
        its  = its + 1 
        if (abs(f(x)) < = eps) converged = .TRUE.
    end do 

end subroutine Newton

program test
    !Run a test to see that the  subroutine works 
    implicit none 
    real :: x = 2
    real :: f, df

    call Newton(f, df, x)

    print*, 'The root of the function was found to be' x 

end program test