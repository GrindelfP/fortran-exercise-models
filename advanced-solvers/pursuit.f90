program PURSUIT_CURVE_MODERN
    use ode_solver_m
    use models_m, only : k_pursuit, pursuit_f 
    implicit none
    
    integer, parameter :: N = 10000, M = 10
    integer :: i, j
    real(8) :: h = -0.001d0, k_start = 0.6d0, k_step = 0.1d0
    real(8) :: X(0:N)
    real(8) :: YZ(2), YZ_next(2)
    real(8) :: Y_results(0:N, 1:M)

    X(0) = 1.0d0
    do i = 1, N
        X(i) = X(i-1) + h
    end do

    do j = 1, M
        k_pursuit = k_start + (j-1) * k_step                    
        ! IT IS GLOBAL

        YZ = [0.0d0, 0.0d0] 
        Y_results(0, j) = YZ(1)

        do i = 1, N
            if (X(i-1) <= 1.0d-6) then
                Y_results(i:N, j) = Y_results(i-1, j)
                exit
            end if

            call rk4_step(pursuit_f, X(i-1), YZ, h, YZ_next)
            
            YZ = YZ_next
            Y_results(i, j) = YZ(1)
        end do
    end do

    open(unit=10, file='pursuit.dat', status='replace')
    do i = 0, N
        write(10, '(F10.5, 10F12.6)') X(i), Y_results(i, :)
        if (X(i) <= 0.0d0) exit
    end do
    close(10)

    print *, "Calculations done! Results -> pursuit.dat"

end program PURSUIT_CURVE_MODERN