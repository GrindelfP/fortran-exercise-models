program MCPI
    implicit none

    integer :: i, total_points, inside_points
    real :: x, y, pi

    call random_seed()

    print *, 'Enter number of points to simulate!'
    read *, total_points

    inside_points = 0

    do i = 1, total_points
        call random_number(x)
        call random_number(y)

        if (x**2 + y**2 <= 1.0) then
            inside_points = inside_points + 1
        end if
    end do

    pi = 4.0 * real(inside_points) / real(total_points)

    print *, "Points inside: ", inside_points
    print *, "Approximate PI: ", pi
    print *, "Error: ", abs(pi - 3.141592653589793238462643383279)
    
end program MCPI
