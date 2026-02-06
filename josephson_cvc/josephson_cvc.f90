!------------------------------------------------------------------------------
! PROGRAM: JOSEPHSON_CVC
! DESCRIPTION: Calculates the Current-Voltage Characteristic (CVC)
!              using the RCSJ model.
!------------------------------------------------------------------------------
program josephson_cvc
    use ode_solver_m
    use josephson_model_m
    implicit none

    ! Simulation Settings (matching Python notebook)
    real(8) :: I_max = 2.0d0
    real(8) :: dI    = 0.005d0
    real(8) :: T_max = 500.0d0
    real(8) :: T_min = 100.0d0
    real(8) :: dt    = 0.05d0
    
    ! State variables: Y = [V, phi, u]
    real(8) :: Y(3)
    
    ! Control loop variables
    real(8) :: current_I, V_avg
    integer :: direction ! 1 for up, -1 for down
    integer, parameter :: out_unit = 10
    
    ! --- PARAMETER SETUP ---
    sys_params%beta  = 0.2d0
    sys_params%A     = 1.0d0
    sys_params%omega = 2.0d0
    sys_params%Ic2   = 1.0d0
    
    ! Open output file
    open(unit=out_unit, file='cvc_data.dat', status='replace')
    
    ! Initial conditions (V=0, phi=0, u=0)
    Y = 0.0d0
    
    ! Start Current Sweep (0 -> I_max -> 0)
    current_I = 0.0d0
    direction = 1
    
    print *, "Calculating CVC with A =", sys_params%A, " beta =", sys_params%beta
    
    do
        ! Update parameter I for the ODE function
        sys_params%I = current_I
        
        ! Evolve system and calculate average voltage
        ! Note: Y is updated in place, providing the adiabatic initial 
        ! condition for the next step (hysteresis logic)
        call calculate_avg_voltage(Y, T_max, T_min, dt, V_avg)
        
        ! Write result: I  <V>
        write(out_unit, *) current_I, V_avg
        
        ! Update Current
        current_I = current_I + (direction * dI)
        
        ! Logic to reverse direction at I_max and stop at 0
        if (direction == 1 .and. current_I >= I_max) then
            direction = -1
        elseif (direction == -1 .and. current_I <= 0.0d0) then
            exit
        end if
    end do
    
    close(out_unit)
    print *, "Calculation complete. Data stored in 'cvc_data.dat'."

contains

    !--------------------------------------------------------------------------
    ! SUBROUTINE: calculate_avg_voltage
    ! Solves ODE from t=0 to t_end. Calculates average V from t_avg_start.
    !--------------------------------------------------------------------------
    subroutine calculate_avg_voltage(Y_state, t_end, t_avg_start, h, v_mean)
        real(8), intent(inout) :: Y_state(:)
        real(8), intent(in)    :: t_end, t_avg_start, h
        real(8), intent(out)   :: v_mean
        
        real(8) :: t_curr, v_accum
        integer :: n_steps, avg_count, i
        
        t_curr = 0.0d0
        v_accum = 0.0d0
        avg_count = 0
        n_steps = int(t_end / h)
        
        do i = 1, n_steps
            ! Perform one RK4 step
            ! Note: The generic rk4 in ode_solver updates Y_state and t_curr
            call rk4(josephson_derivs, t_curr, Y_state, h, 1.d-8, 1.d-10)
            
            ! Accumulate voltage for averaging if we passed transient time
            if (t_curr >= t_avg_start) then
                v_accum = v_accum + Y_state(1) ! Y(1) is Voltage
                avg_count = avg_count + 1
            end if
        end do
        
        ! Calculate mean
        if (avg_count > 0) then
            v_mean = v_accum / real(avg_count, 8)
        else
            v_mean = Y_state(1)
        end if
        
    end subroutine calculate_avg_voltage

end program josephson_cvc