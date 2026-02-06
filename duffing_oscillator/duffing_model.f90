!------------------------------------------------------------------------------
! MODULE: DUFFING_MODEL
! DESCRIPTION: System of differential equations for Duffing Oscillator.
!------------------------------------------------------------------------------
module duffing_model_m
    implicit none
    
    ! Model parametres
    type :: duffing_params
        real(8) :: alpha
        real(8) :: beta        
        real(8) :: gamma
        real(8) :: delta
        real(8) :: omega
    end type duffing_params

    type(duffing_params), save :: system_params

contains

    function duffing_system(t, Y) result(dY)
        real(8), intent(in) :: t, Y(:)
        real(8) :: dY(size(Y))
        
        real(8) :: v, x
        x = Y(1)
        v = Y(2)

        ! u = x'
        ! u' = -δu - αx - βx^3 + γcos(ωt)
        dY(1) = v
        dY(2) = - system_params%delta * v &
                - system_params%alpha * x &
                - system_params%beta * x**3 &
                + system_params%gamma * cos(system_params%omega * t)
        
    end function duffing_system

end module duffing_model_m
