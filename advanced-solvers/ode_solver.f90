!------------------------------------------------------------------------------
! Grindelf Productions
!------------------------------------------------------------------------------
!
! MODULE:  ODE_SOLVER
!
!> @author
!> GrindelfP
!
! DESCRIPTION: 
!>  Solvers for systems of differential equations of first order.
!
! REVISION HISTORY:
! 2026 Jan 31 - Initial Version
!------------------------------------------------------------------------------
module ode_solver_m
    implicit none

    abstract interface
        function ode_func(t, Y) result(res)
            real(8), intent(in) :: t
            real(8), intent(in) :: Y(:)
            real(8) :: res(size(Y))
        end function ode_func
    end interface
    
contains

    subroutine rk4_step(f, t, Y, h, Y_next)
        procedure(ode_func) :: f                    ! GENERIC DE RHS
        real(8), intent(in) :: t, h                 ! independent variable, step
        real(8), intent(in) :: Y(:)                 ! current functions values vector
        real(8), intent(out) ::  Y_next(size(Y))    ! next functions values vector

        real(8) :: k1(size(Y)), k2(size(Y)), k3(size(Y)), k4(size(Y))
        
        k1 = f(t, Y)
        k2 = f(t + 0.5d0*h, Y + 0.5d0*h*k1)
        k3 = f(t + 0.5d0*h, Y + 0.5d0*h*k2)
        k4 = f(t + h, Y + h*k3)

        Y_next = Y + (h/6.0d0) * (k1 + 2.0d0*k2 + 2.0d0*k3 + k4)
        
    end subroutine rk4_step


end module ode_solver_m
