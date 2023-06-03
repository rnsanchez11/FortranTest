          program hello
           include 'mpif.h'
           integer rank, size, ierror, tag, status(MPI_STATUS_SIZE)
   
           call MPI_INIT(ierror)
           call MPI_COMM_SIZE(MPI_COMM_WORLD, size, ierror)
           call MPI_COMM_RANK(MPI_COMM_WORLD, rank, ierror)
           !print*, 'node', rank, ': Hello world!'

           
           n=100
           n_proc = n/size
           n_ini = 1 + rank*n_proc
           n_fin = (rank + 1)*n_proc
           print*, rank,'n_ini',n_ini,'n_fin',n_fin
           call MPI_FINALIZE(ierror)
          end

