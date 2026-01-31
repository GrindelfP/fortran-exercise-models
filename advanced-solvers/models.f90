module models_m
    implicit none
    
    public :: k_pursuit, k_beatles, pursuit_f, tom_jerry_f
    
    real(8) :: k_pursuit = 0.6d0, k_beatles = 1.0d0
    real(8) :: tom_R = 1.0d0

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

    ! FOUR PEATLES
    function f(t, Y) result(res)
        real(8), intent(in) :: t, Y(:)
        real(8) :: res(size(Y))
        
        res(1) = k_beatles * (-Y(2) - Y(1))
        res(2) = k_beatles * (Y(1) - Y(2))
    end function f

end module models_m
