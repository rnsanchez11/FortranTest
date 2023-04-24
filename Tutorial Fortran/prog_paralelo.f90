
program integralnum

    real*8, external :: f
    real*8 :: a, b, r
    integer :: n, i

    !hacer inntegral numerica
    a = -100.0 ; b = 115.0 ; n = 10000 ; r = 0.0

    !$omp parallel do
    do i = 1, 100000
        call integral(f,a,b,n,r)
        r = 10*i + r**2 / i**2
    enddo
    !$omp end parallel do

    !$omp parallel do
    do i = 1, 100000
        call integral(f,a,b,n,r)
        r = 100*i + r**3 / i**9
    enddo

    do i = 1, 100000
        call integral(f,a,b,n,r)
        r = 1000*i + r**2*i / (i+2)**2
    enddo

    print*, 'the result is ' ,r
    return

end program integralnum
 

!--------------------------------------------------------------------------------------------------
function f(x)
    implicit none
    real*8 :: f, x
    
    f = x**3 - x**2 + x - 25

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

