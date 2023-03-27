program read_value
    implicit none
    integer :: age

    !lee valores que le pido al usuario
    print *, 'please enter your age'
    read(*,*) age 

    print *, 'your age is: ', age
end program read_value

