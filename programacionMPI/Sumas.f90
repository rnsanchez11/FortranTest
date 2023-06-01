program main

   use mpi_f08
   implicit none

   integer :: i
   integer :: ierror ! To control errors in MPI calls
   integer :: rank   ! Unique number received by each process
   integer :: num_proc ! Total number of processes
   real*8   :: wtime

   integer :: lo, ln, sumaParcial, sumaTotal, tamIntervalos, inicio, final, numrank, dest, tag
   !real*8 :: sumaParcial, sumaTotal, tamIntervalos, inicio, final
    

   ! Initialize MPI. This must be the first MPI call
   call MPI_Init(ierror)
   ! Get the number of processes
   call MPI_Comm_size(MPI_COMM_WORLD, num_proc, ierror)
   ! Get the individual process rank
   call MPI_Comm_rank(MPI_COMM_WORLD, rank, ierror)


   !Discretizacion del dominio
   lo = 1
   ln = 10
   tamIntervalos = (ln-lo+1)/(num_proc)
   inicio = ln - (rank+1)*tamIntervalos +1
   final = ln - (rank)*tamIntervalos

   write(*,*) 'rank', rank, 'nini', inicio, 'nfin', final
   
   !suma parcial de cada procesador
   sumaParcial = 0
   do i = inicio, final
      sumaParcial = sumaParcial + i
   end do
   call MPI_SEND(sumaParcial, 1, MPI_INT, 0, 1, MPI_COMM_WORLD, ierror)
   write(*,*) 'rank', rank,'suma parcial', sumaParcial

   !suma total hecha por el procesador 0
   sumaTotal = 0
      if (rank == 0) then
         wtime = MPI_Wtime()
         do numrank = 0, num_proc-1
            call MPI_RECV(sumaParcial, 1, MPI_INT, numrank, 1, MPI_COMM_WORLD, MPI_STATUS_IGNORE, ierror)
            sumaTotal = sumaTotal + sumaParcial
         enddo
         write(*,*) 'rank', rank,'suma Total', sumaTotal

       end if

           ! Ranks are intrinsic to each process and this conditional is legal
      if (rank == 0) then
      write (*, '(a)') ''
      write (*, '(a,i2,2x,a)') 'RANK:', rank, ' Master process reporting:'
      write (*, '(a,i2,2x,a)') 'RANK:', rank, ' Normal end of execution for all'
      end if

    ! No more MPI calls after Finalize
    call MPI_Finalize(ierror)

    stop
 
end program