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


    ! DORMAND-PRINCE CONSTANTS
    real(8), parameter :: c(6) = [0.2d0, 0.3d0, 0.8d0, 8.d0/9.d0, 1.0d0, 1.0d0]
    
    real(8), parameter :: a21 = 1.d0/5.d0
    real(8), parameter :: a31 = 3.d0/40.d0,       a32 = 9.d0/40.d0
    real(8), parameter :: a41 = 44.d0/45.d0,      a42 = -56.d0/15.d0,      a43 = 32.d0/9.d0
    real(8), parameter :: a51 = 19372.d0/6561.d0, a52 = -25360.d0/2187.d0, a53 = 64448.d0/6561.d0, a54 = -212.d0/729.d0
    real(8), parameter :: a61 = 9017.d0/3168.d0,  a62 = -355.d0/33.d0,     a63 = 46732.d0/5247.d0, a64 = 49.d0/176.d0, a65 = -5103.d0/18656.d0

    real(8), parameter :: b(7) = [35.d0/384.d0, 0.d0, 500.d0/1113.d0, 125.d0/192.d0, -2187.d0/6784.d0, 11.d0/84.0d0, 0.d0]
    real(8), parameter :: e(7) = [35.d0/384.d0 - 5179.d0/57600.d0, 0.d0, &
                                  500.d0/1113.d0 - 7571.d0/16695.d0, &
                                  125.d0/192.d0 - 393.d0/640.d0, &
                                  -2187.d0/6784.d0 - (-92097.d0/339200.d0), &
                                  11.d0/84.0d0 - 187.d0/2100.d0, &
                                  -1.d0/40.d0]
    
contains

    subroutine rk4(f, t, Y, h)
        procedure(ode_func) :: f                    ! GENERIC DE RHS
        real(8), intent(in) ::  h                   ! independent variable, step
        real(8), intent(inout) :: t, Y(:)           ! functions values vector

        real(8) :: k1(size(Y)), k2(size(Y)), k3(size(Y)), k4(size(Y))
        
        k1 = f(t, Y)
        k2 = f(t + 0.5d0*h, Y + 0.5d0*h*k1)
        k3 = f(t + 0.5d0*h, Y + 0.5d0*h*k2)
        k4 = f(t + h, Y + h*k3)

        Y = Y + (h/6.0d0) * (k1 + 2.0d0*k2 + 2.0d0*k3 + k4)
        t = t + h
        
    end subroutine rk4

    subroutine rkdp45(f, t, Y, h, tol, lowlim)
        procedure(ode_func) :: f
        real(8), intent(inout) :: t, h, Y(:)
        real(8), intent(in)    :: tol, lowlim
        
        real(8) :: k(size(Y), 7)
        real(8) :: Y_next(size(Y)), Y_err(size(Y))
        real(8) :: error, factor

        ! K1
        k(:,1) = f(t, Y)

        do
            ! K2-K6
            k(:,2) = f(t + c(1)*h, Y + h*(a21*k(:,1)))
            k(:,3) = f(t + c(2)*h, Y + h*(a31*k(:,1) + a32*k(:,2)))
            k(:,4) = f(t + c(3)*h, Y + h*(a41*k(:,1) + a42*k(:,2) + a43*k(:,3)))
            k(:,5) = f(t + c(4)*h, Y + h*(a51*k(:,1) + a52*k(:,2) + a53*k(:,3) + a54*k(:,4)))
            k(:,6) = f(t + c(5)*h, Y + h*(a61*k(:,1) + a62*k(:,2) + a63*k(:,3) + a64*k(:,4) + a65*k(:,5)))
            
            ! Y NEW (5th ORDER)
            Y_next = Y + h * matmul(k(:,1:6), b(1:6))
            
            ! K7
            k(:,7) = f(t + h, Y_next)
            
            ! ERROR EVAL
            Y_err = h * matmul(k, e)
            error = maxval(abs(Y_err)) / max(maxval(abs(Y)), 1.0d-8)

            if (error <= tol) then             ! IF ERROR TOLERABLE -> STEP ACCEPTED
                t = t + h
                Y = Y_next
                if (error > 0) then
                    factor = 0.9d0 * (tol/error)**0.2d0
                else
                    factor = 5.0d0 
                end if
                h = h * min(5.0d0, max(0.2d0, factor))
                exit
            else                               ! IF ERROR INTOLERABLE -> STEP UPDATED
                factor = 0.9d0 * (tol/error)**0.25d0
                h = h * min(1.0d0, max(0.1d0, factor))
                
                if (abs(h) < lowlim) then
                    print *, "ERROR: Step size below limit at t =", t
                    stop
                end if
            end if
        end do
    end subroutine rkdp45

end module ode_solver_m
