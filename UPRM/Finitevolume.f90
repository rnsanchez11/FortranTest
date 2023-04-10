program Finitevolume
    implicit none
    !Ejemplo Finite volume
    !Datos
    !Barra
    
    !Ecuacion diferencial
    !d/dx(k dT/dx) = 0

    !variables

    real :: L,TA,TB,k,A,dx,dx2,Q11,Q12,Qnn,Qn
    integer :: i
    integer, parameter :: nodos = 5.0 
    real, dimension (nodos,nodos) :: A1
    real, dimension (nodos) :: b,x1
    real, dimension (nodos+1) :: x
    real, dimension (nodos+2) :: xx, yy


    !double precision :: L,TA,TB,k,A,dx,dx2,Q11,Q12,Qnn,Qn
    !integer :: i
    !integer, parameter :: nodos = 5.0 
    !double precision, dimension (nodos,nodos) :: A1
    !double precision, dimension (nodos) :: b,x1
    !double precision, dimension (nodos+1) :: x
    !double precision, dimension (nodos+2) :: xx, yy

    !Datos
    L = 0.5
    TA = 100
    TB = 500
    k = 1000
    A = 0.01
    dx = L/nodos
    dx2 = dx/2


    !Discretizacion
    x(1) = 0
        do i = 2, nodos+1
            x(i) = x(i-1) + dx
        enddo
    do i = 1, nodos+1
        x(i) = x(i) + dx2
    enddo
    x(nodos+1) = x(nodos+1) - dx2


    !Para N1
    Q11 = -3*k*A/dx
    Q12 = k*A/dx
    !Para Nn
    Qnn = -3*k*A/dx
    Qn = k*A/dx

    do i = 2, nodos-1
        A1(i,i-1) = Qn
        A1(i,i) = -2*Qn
        A1(i,i+1) = Qn
    enddo

    A1(1,1) = Q11
    A1(1,2) = Q12
    A1(nodos,nodos) = Qnn
    A1(nodos,nodos-1) = Qn
    b(1) = -2*k*A*TA/dx
    b(nodos) = -2*k*A*TB/dx

    do i = 1, nodos
    print*, A1(i,:)
    enddo

    print*, '---------------------------------------------------------------------------------'
    print*, b



end program Finitevolume

    