module models_m
    implicit none
    
    public :: k_pursuit, k_beatles, pursuit_f, tom_jerry_f, beatles_f, orbit_f
    
    real(8) :: k_pursuit = 0.6d0, k_beatles = 1.0d0
    real(8) :: tom_R = 1.0d0
    real(8) :: I_joseph, beta_joseph, A_joseph, omega_joseph

contains

    ! PURSUIT CURVE
    function pursuit_f(x, Y) result(res)
        real(8), intent(in) :: x, Y(:)
        real(8)             :: res(size(Y))
        
        res(1) = Y(2)                                 
        res(2) = k_pursuit * sqrt(1.0d0 + Y(2)**2) / x
    end function

    ! TOM AND JERRY
    function tom_jerry_f(t, Y) result(res)
        real(8), intent(in) :: t, Y(:)
        real(8)             :: res(size(Y))

        res(1) = tom_R * cos(t) - Y(1)
        res(2) = tom_R * sin(t) - Y(2)
    end function

    ! FOUR BEATLES
    function beatles_f(t, Y) result(res)
        real(8), intent(in) :: t, Y(:)
        real(8) :: res(size(Y))
        
        res(1) = k_beatles * (-Y(2) - Y(1))
        res(2) = k_beatles * (Y(1) - Y(2))
    end function beatles_f

    ! ORBITAL MOTION (Two-body problem)
    function orbit_f(t, Y) result(res)
        real(8), intent(in) :: t, Y(:)
        real(8)             :: res(size(Y))
        real(8)             :: r
        
        ! Y(1)=x, Y(2)=y, Y(3)=vx, Y(4)=vy
        r = sqrt(Y(1)**2 + Y(2)**2)
        
        res(1) = Y(3)                         ! dx/dt = vx
        res(2) = Y(4)                         ! dy/dt = vy
        res(3) = -Y(1) / r**3                 ! dvx/dt 
        res(4) = -Y(2) / r**3                 ! dvy/dt
    end function orbit_f

    ! JOSEPHSON JUNCTION
    function joseph_f(t, Y) result(res)
        real(8), intent(in) :: t, Y(:)
        real(8) :: res(size(Y))
    
        res(1) = Y(2)
        res(2) = I_joseph - sin(Y(1)) - beta_joseph * Y(2) + A_joseph * sin(omega_joseph * t)
    end function joseph_f

end module models_m
