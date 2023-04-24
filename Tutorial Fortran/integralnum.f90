
program integralnum

    real*8, external :: f
    real*8 :: a, b, r
    integer :: n

    !hacer inntegral numerica
    a = 0.0 ; b = 1.0 ; n = 10 ; r = 0.0
    call integral(f,a,b,n,r)
    print*, 'the result is ' ,r

return
end program integralnum
 

!--------------------------------------------------------------------------------------------------
function f(x)
    implicit none
    real*8 :: f, x
    
    f = x

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

