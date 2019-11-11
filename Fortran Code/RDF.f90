!Begin with wanting to find the RDF of the 1s, 2s and 2p shells 
!The RDF is 4pir^2 times the square of the WF

program orbs
    implicit none 
    integer :: dim = 50
    real, dimension(50) :: R, ONE_S_RDF
    real:: PI
    integer :: i 

    !Define the value of Pi 
    PI = 4*atan(1.0)

    !Loop over the elements of the radius array 
    do i=1, dim
        R(i) = i
    end do

    !Store the probability values in an array and then scale them
    ONE_S_RDF = ONE_S(R)
    call scale(ONE_S_RDF, dim)
    print*, ONE_S_RDF

    
    !Define the needed functions and subroutines
    contains
    !The 1s orbital 
    function ONE_S(r)
        implicit none 
        !Define the variables 
        real, dimension(dim) :: ONE_S, WF, WFSQR, RDF, r
        
        !Define the wavefunction
        WF = exp(-r/2.0)
        WFSQR = WF**2
        RDF = 4 * PI * r**2 * WFSQR

        !Assign the value to the function
        ONE_S = RDF
    end function ONE_S

    !Orbital Scaler function
    subroutine scale(RDF, dim)
        implicit none 
        !Initialise the needed variables 
        integer :: dim
        real, dimension(dim) :: RDF
        real :: biggest = 0.0 
        
        !Find the largest value and store it 
        do i = 1, dim
            if (biggest < RDF(i)) then
                biggest = RDF(i)
            end if 
        end do 

        !Divide all values by the largest value so the largest value
        !is 1 
        do i = 1, dim
            RDF(i) = RDF(i)/biggest
        end do 
    end subroutine scale

end program orbs