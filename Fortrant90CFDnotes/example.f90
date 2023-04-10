program Example_4dos_CFD
    implicit none
    
    real*8 :: k,L,Area,Ta,Tb,q,dx
    double precision :: aw,ae,ap
    integer, parameter :: n=5.0 !discretizing whit n elements
    integer :: i
    real*8, dimension (n,n) :: A
    real*8, dimension (n) :: b,x
    
    Area=1.0; Ta=100.0; Tb=200.0; q=1000000.0; L=0.02; k=0.5 
    
    !grid
    dx=L/n
    print*, dx
    
    !compute the coefficient of the matrix A
    aw=k*Area/dx
    ae=k*Area/dx
    ap=ae+aw
    print*, aw,ae,ap
    
    A=0.0; b=0; x=0
    do i=2,n-1 !number of equations (internal points)
    A(i,i)=ap !Ap
    A(i, i-1)=-aw !Aw
    A(i, i+1)=-ae !Ae
    b(i)=q*Area*dx
    enddo
    
    i=1 !BC in A, T=Ta 
    A(i,i)=3*k*Area/dx !Ap 1,1
    A(i,i+1)=-k*Area/dx !Ae 1,2
    b(i)=q*Area*dx+2*k*Area/dx*Ta !b1
    
    i=n !BC in B, T=Tb
    A(i,i)=3*k*Area/dx !Ap
    A(i,i-1)=-k*Area/dx !Aw
    b(i)=q*Area*dx+2*k*Area/dx*Tb 
    
    !write (*,*)'Matriz A and vector b before of the solution'
    call print_matrix(A,n,n) !A
    call print_matrix(b,n,1) !b
    
    call gauss(n,a,b,x)
    print*, 'Solution'
    do i=1,n
    write(*,*) i,x(i)
    end do
    
    end program
    
    
    
    
    !------------------------------------------------------------------------------------------------
    subroutine print_matrix(M,fil,col)
    implicit none
    integer fil,col
    double precision M(fil,col)
    integer i
    write (*,*)'-------------------------------------------------------------------------------------'
    do i=1,fil
    write(*,*) M(i,:)
    end do
    write (*,*)'-------------------------------------------------------------------------------------'
    end subroutine
    !------------------------------------------------------------------------------------------------
    
    
    !------------------------------------------------------------------------------------------------
    subroutine gauss(n,a,b,x)
    implicit none
    integer :: n,k,i,j
    real*8 :: a(n,n),b(n),x(n),em
    
    do k=1,n-1
        do j=k+1,n
            do i=k+1,n
        !--- Modifica todos los elementos arriba de la diagonal
                em=a(k,j)/a(k,k)
                a(i,j)=a(i,j)-a(i,k)*(em)
            enddo
        enddo
    enddo
    
    do k=1,n-1
        do i=k+1,n
        !------ Modificia b, el ultimo (n) es el deseado para x_n
            b(i)=b(i)-(a(i,k)/a(k,k))*b(k)
        enddo
    enddo
    
    x(n)=b(n)/a(n,n)
    do i=n-1,1,-1
        do j=n,i+1,-1
            x(i)=x(i)-a(i,j)*x(j)
        enddo
        x(i)=x(i)+b(i)
        x(i)=x(i)/a(i,i)
    enddo
    
    
    return
    end subroutine
    !------------------------------------------------------------------------------------------------
    