program FOUR_BEATLES
    implicit none
    
    integer, parameter :: N = 100000
    integer :: i
    real(8) :: X(0:N), Y(0:N), XY(0:2), XY_next(0:2)
    real(8) :: h = 0.001d0, t_curr = 0.0d0

    ! ICS
    X(0) = 1.0d0
    Y(0) = 0.0d0
    XY(0) = X(0)
    XY(1) = Y(0)

    ! SIMULATION CYCLE
    do i = 1, N
        call rk4(h, t_curr, XY, XY_next)
        t_curr = t_curr + h
        X(i) = XY_next(0)
        Y(i) = XY_next(1)
        XY = XY_next
    end do

    open(unit=10, file='beatles.dat', status='replace')
    do i = 0, N
        write(10, *) X(i), Y(i)
    end do
    close(10)

contains 

    function f(t, Y) result(res)
        real(8), intent(in) :: t
        real(8), intent(in) :: Y(2)
        real(8) :: res(2)
        
        res(1) = -Y(2) - Y(1) 
        res(2) = Y(1) - Y(2)
        
    end function f

    subroutine rk4(h, tn, Yn, Ym)
        real(8), intent(in) :: h, tn
        real(8), intent(in) :: Yn(2)
        real(8), intent(out) ::  Ym(2)
        
        real(8) :: k1(2), k2(2), k3(2), k4(2)

        k1 = f(tn, Yn)
        k2 = f(tn + h * 0.5, Yn + (h * k1) * 0.5)
        k3 = f(tn + h * 0.5, Yn + (h * k2) * 0.5)
        k4 = f(tn + h, Yn + h * k3)

        Ym = Yn + h / 6 * (k1 + 2 * k2 + 2 * k3 + k4)
        
    end subroutine rk4

end program FOUR_BEATLES