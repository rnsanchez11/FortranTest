
program intTrapezoidal

	use mpi
	implicit none
 
	integer :: i
	integer :: ierror ! To control errors in MPI calls
	integer :: rank   ! Unique number received by each process
	integer :: num_proc ! Total number of processes
	real*8   :: wtime
 
	!integer :: lo, ln, tamIntervalos, inicio, final, numrank, dest, tag, n
	!real*8 :: sumaParcial, sumaTotal, tamIntervalos, inicio, final
	real*8, external :: f
    real*8 :: a, b, r, lo, ln, tamIntervalos, inicio, final, intTotal
    integer :: n, numrank
 
	! Initialize MPI. This must be the first MPI call
	call MPI_Init(ierror)
	! Get the number of processes
	call MPI_Comm_size(MPI_COMM_WORLD, num_proc, ierror)
	! Get the individual process rank
	call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)
 

	lo = 1
	ln = 100
	tamIntervalos = (ln-lo+1)/(num_proc)
	inicio =  (rank)*tamIntervalos
	if (rank == 0) then
	inicio =  lo + (rank)*tamIntervalos
	endif
	final = (rank+1)*tamIntervalos
	write(*,*) 'rank', rank, 'nini', inicio, 'nfin', final

    !hacer integral numerica
    a = inicio ; b = final ; n = 100 ; r = 0.0
    call integral(f,a,b,n,r)
    print*, 'rank', rank, 'the result is ' ,r
	call MPI_SEND(r, 1, MPI_DOUBLE, 0, 1, MPI_COMM_WORLD, ierror)

	intTotal = 0
	if (rank == 0) then
	   do numrank = 0, num_proc-1
		  call MPI_RECV(r, 1, MPI_DOUBLE, numrank, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
		  intTotal = intTotal + r
	   enddo
	   write(*,*) 'rank', rank,'integral Total', intTotal
	 end if


    ! No more MPI calls after Finalize
		call MPI_Finalize(ierror)
		stop
	
end program

!--------------------------------------------------------------------------------------------------
function f(x)
    implicit none
    real*8 :: f, x
    
    f = x**3

    return
end function f

!--------------------------------------------------------------------------------------------------
subroutine integral(f,a,b,n,r)

    real*8, external :: f
    real*8 :: a, b, r, h, x
    integer :: i, n

    r = 0.0
    h = (b-a)/n
    do i = 1, n 
    x = a + h*(i-1)
    r = r + h*((f(x)) + (f(x+h)))/2.0   !h*(f(x) + f(x+h))/2.0
    enddo

end subroutine integral
