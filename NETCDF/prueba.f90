program read
implicit none

integer, parameter :: nx = 192, ny = 128, nz = 160  ! Dimensions
real*8, dimension(ny,1) :: vprofile, ydistance, y1, y3
integer :: fu2, y

open (action='read', file='ygrid.plo', unit=fu2, status='old')
   do y = 1, ny
   read(fu2,*) y1(y,1), ydistance(y,1), y3(y,1)
   enddo
   print*, ydistance
close (fu2)

end program
