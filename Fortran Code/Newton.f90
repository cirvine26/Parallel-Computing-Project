program Newton 
	!write a program to solve equations via Newton root approximation

	implicit none 

	integer :: its = 0
	integer :: maxits = 50
	logical :: converged = .FALSE. 
	real :: eps = 1.0e-6			!Maximum error that is allowed 
	real :: x = 10						!intial guess 


	!do loop with a conditional included 
	!Could this be replaced by a subroutine?
	do while (converged .eqv. .FALSE. .and. its < maxits)
		x = x - (f(x)/df(x))
		print*, x, f(x)
		its = its + 1 
		if (abs(f(x)) <= eps) converged = .TRUE.
	end do 
	if (converged .eqv. .TRUE.) then
		print*, 'Newton converged after', its, 'steps.' 
	else 
		print*, 'Newton did not converge.'
	end if 

	contains 
	!Define the needed functions for the loop
	function f(x)
		real :: f, x
		f = x**3 + x - 3.0
	end function f 

	function df(x)
		!first derivative of f(x)
		real :: df, x 
		df = 3 * x**2 + 1
	end function df 

end program Newton
