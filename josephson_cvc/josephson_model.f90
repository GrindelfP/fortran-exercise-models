!------------------------------------------------------------------------------
! MODULE: JOSEPHSON_MODEL
! DESCRIPTION: Defines the system of differential equations for the JJ.
!------------------------------------------------------------------------------
module josephson_model_m
    use ode_solver_m
    implicit none
    
    ! Parameters structure
    type :: josephson_params
        real(8) :: I      ! Normalized Current
        real(8) :: A      ! Radiation Amplitude
        real(8) :: beta   ! Dissipation parameter
        real(8) :: omega  ! Radiation frequency
        real(8) :: Ic2    ! Second harmonic amplitude
    end type josephson_params

    type(josephson_params), save :: sys_params

contains

    !--------------------------------------------------------------------------
    ! FUNCTION: josephson_derivs
    ! Matches the 'ode_func' interface required by ode_solver_m
    ! Y(1) = Voltage (V)
    ! Y(2) = Phase (phi)
    ! Y(3) = Radiation Phase (u)
    !--------------------------------------------------------------------------
    function josephson_derivs(t, Y) result(dY)
        real(8), intent(in) :: t
        real(8), intent(in) :: Y(:)
        real(8) :: dY(size(Y))
        
        real(8) :: V, phi, u
        
        V   = Y(1)
        phi = Y(2)
        u   = Y(3)
        
        ! Equation: dV/dt = I + A*sin(u) - beta*V - sin(phi) - Ic2*sin(2*phi)
        dY(1) = sys_params%I + &
                sys_params%A * sin(u) - &
                sys_params%beta * V - &
                sin(phi) - &
                sys_params%Ic2 * sin(2.0d0 * phi)
        
        ! Equation: dphi/dt = V
        dY(2) = V
        
        ! Equation: du/dt = omega
        dY(3) = sys_params%omega
        
    end function josephson_derivs

end module josephson_model_m
