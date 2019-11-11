!In this code we will attempt to make resource intensive code to 
!see the effects of timing the code as it is executed 
!We will do this by looking at matrix multiplications and scaling up their dimensions


program matrix_mul
    !Import the OMP library 
    !$ use omp_lib
    
    implicit none 
    integer,parameter :: dim = 2000
    real, dimension(:,:), allocatable :: a, b, c
    integer :: i, j, k, num, cores, repeats
    real :: tick, tock, sum
    
    !Allocate the dimensions of the matrices - stops the compiler from freaking
    allocate(a(dim,dim), b(dim,dim), c(dim,dim))
        
    !Loop over matrix a to fill the elements 
    !Define a parallel do to reduce the workload 
    !
    !$OMP PARALLEL DO collapse(2)
    do i = 1, dim
        do j = 1, dim
            a(j,i) = (real(i) * real(j))**0.5
        end do 
    end do 
    !$OMP END PARALLEL DO 

    
    
    !Loop over matrix b to fill the elements 
    !$OMP PARALLEL DO collapse(2)
    do i = 1, dim
        do j = 1, dim
            b(j,i) = (4*real(i) - real(j))**0.3
        end do 
    end do 
    !$OMP END PARALLEL DO 
    

    !Specify parallel region
    !Iterate over all cores within a do loop to get the speed-up
    do cores=1,64    
        !Now perform the multiplication and store in c 
        !$ tick = OMP_get_wtime()
        !
        !Here we will implement a first principles method of matrix multiplication
        !This is so it may be easily parallelised and its effects measured
        !
        !$OMP PARALLEL NUM_THREADS(cores)

        !Loop ten times to get an average time
        do repeats=1,10
        
            !$OMP DO collapse(2) private(sum)
            do i=1, dim
                do j=1,dim
                    sum=0.0
                    do k=1,dim
                        sum = sum + a(k,j) * b(i,k)
                    end do 
                    c(i,j) = sum
                end do 
            end do 
            !$OMP END DO 
        
        end do
        !$ tock = OMP_get_wtime()


        !Include a section to refer to how many threads are being used 
        !$ num=OMP_GET_NUM_THREADS()
        
        !End parallel region
        !$OMP END PARALLEL


        !$ print*, 'The time to multiply the two matrices was', (tock - tick)/10
        !$ print*, 'The number of cores used was', num 
    end do

end program matrix_mul