program PURSUIT_CURVE
    implicit none
    
    integer, parameter :: N = 10000, M = 10
    integer :: i, j
    real(8) :: h = -0.001d0, k_start = 0.6d0, k_step = 0.1d0, k
    real(8) :: X(0:N)
    real(8) :: YZ(0:2), YZ_next(0:2)
    real(8) :: Y_results(0:N, 0:M)

    ! GRID
    X(0) = 1.0d0
    do i = 1, N
        X(i) = X(i-1) + h
    end do


    ! k iteration
    do j = 1, M

        k = k_start + (j-1) * k_step

        ! ICS
        YZ(0) = 0.0d0
        YZ(1) = 0.0d0
        Y_results(0, j) = YZ(1)

        ! SIMULATION
        do i = 1, N
            ! small x escape
            if (X(i-1) <= 1.0d-6) then
                Y_results(i:N, j) = Y_results(i-1, j)
                exit
            end if
            call rk4(h, k, X(i-1), YZ, YZ_next)
            Y_results(i, j) = YZ_next(0)
            YZ = YZ_next
        end do
    end do

    open(unit=10, file='pursuit.dat', status='replace')
        do i = 0, N
            write(10, '(F10.5, 10F12.6)') X(i), Y_results(i, :)
            if (X(i) <= 0.0d0) exit
        end do
    close(10)

contains

    function f(x, Y, k) result(res)
        real(8), intent(in) :: x, Y(2), k
        real(8) :: res(2)
    
        res(1) = Y(2)                                 ! y' = z
        res(2) = k * sqrt(1.0d0 + Y(2)**2.0d0) / x    ! z' = k/x(1 + z^2)^0.5
        
    end function f

    subroutine rk4(h, k, tn, Yn, Ym)
        real(8), intent(in) :: h, k, tn
        real(8), intent(in) :: Yn(2)
        real(8), intent(out) ::  Ym(2)
        
        real(8) :: k1(2), k2(2), k3(2), k4(2)

        k1 = f(tn, Yn, k)
        k2 = f(tn + h * 0.5, Yn + (h * k1) * 0.5, k)
        k3 = f(tn + h * 0.5, Yn + (h * k2) * 0.5, k)
        k4 = f(tn + h, Yn + h * k3, k)

        Ym = Yn + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
        
    end subroutine rk4

end program PURSUIT_CURVE