program TOM_N_JERRY
    implicit none

    integer, parameter :: N = 100000
    integer :: i
    real(8) :: h = 0.001d0, R = 1.0d0
    real(8) :: T(0:N), X(0:N), Y(0:N)
    real(8) :: XY(0:2), XY_next(0:2)

    ! TIME GRID
    T(0) = 0.0d0
    do i = 1, N
        T(i) = T(i-1) + h
    end do

    ! ICS
    X(0) = 0
    Y(0) = 0
    XY(0) = X(0)
    XY(1) = Y(0) 

    ! SIMULATION
    do i = 1, N
        call rk4(h, R, T(i-1), XY, XY_next)
        X(i) = XY_next(0)
        Y(i) = XY_next(1)
        XY = XY_next
    end do

    open(unit=10, file='tomjerry.dat', status='replace')
    do i = 0, N
        write(10, *) X(i), Y(i)
    end do
    close(10)

contains

    function f(t, Y, R) result(res)
        real(8), intent(in) :: t, Y(2), R
        real(8) :: res(2)

        res(1) = R * cos(t) - Y(1)
        res(2) = R * sin(t) - Y(2)
        
    end function f

    subroutine rk4(h, R, tn, Yn, Ym)
        real(8), intent(in) :: h, R, tn
        real(8), intent(in) :: Yn(2)
        real(8), intent(out) ::  Ym(2)
        
        real(8) :: k1(2), k2(2), k3(2), k4(2)

        k1 = f(tn, Yn, R)
        k2 = f(tn + h * 0.5, Yn + (h * k1) * 0.5, R)
        k3 = f(tn + h * 0.5, Yn + (h * k2) * 0.5, R)
        k4 = f(tn + h, Yn + h * k3, R)

        Ym = Yn + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
        
    end subroutine rk4

end program TOM_N_JERRY