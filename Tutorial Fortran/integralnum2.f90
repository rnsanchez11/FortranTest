
program integralnum2

    real*8, external :: f
    real*8 :: a, b, r, params(3)
    integer :: n

    params(1) = 1.0
    params(2) = 0.0
    params(3) = 1.0

    !hacer inntegral numerica
    a = 0.0 ; b = 1.0 ; n = 50 ; r = 0.0 ;
    call integral2(f,params,a,b,n,r)
    print*, 'the result is ' ,r

return
end program integralnum2
 

!--------------------------------------------------------------------------------------------------
function f(x,p)
    implicit none
    real*8 :: f, x, p(*)
    
    f = p(1)*x + p(2)/p(3)

    return
end function f

!--------------------------------------------------------------------------------------------------
subroutine integral2(f,p,a,b,n,r)

    real*8, external :: f
    real*8 :: a, b, r, h, x, p(*)
    integer :: i, n

    r = 0.0
    h = (b-a)/n
    do i = 1, n 
        x = a + h*(i-1)
        r = r + h*((f(x,p)) + (f(x+h,p)))/2.0   !h*(f(x) + f(x+h))/2.0
    enddo

end subroutine integral2
