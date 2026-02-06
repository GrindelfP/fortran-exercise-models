program name
    use ode_solver_m
    use duffing_model_m
    implicit none
    
    real(8) :: t, h, tol, lowlim, t_end
    real(8) :: Y(2)
    real(8) :: next_frame, frame_dt

    ! Parameters
    t      = 0.0d0
    h      = 0.01d0      
    tol    = 1.0d-8
    lowlim = 1.0d-12 
    t_end  = 1500.0d0
    
    ! Animation setup
    frame_dt = 0.2d0   
    next_frame = 0.0d0

    ! System params
    system_params%alpha = -1.0d0
    system_params%beta  = 1.0d0
    system_params%gamma = 0.3d0
    system_params%delta = 0.1d0
    system_params%omega = 1.5d0

    ! Initial condditions
    Y = [1.0d0, 0.0d0]

    ! Simulation + animation 
    open(unit=10, file='duffing_anim.dat', status='replace')    
    do while (t < t_end)
        call rkdp45(duffing_system, t, Y, h, tol, lowlim)
        if (t >= next_frame) then
            write(10, '(3F14.8)') t, Y(1), Y(2)
            next_frame = t + frame_dt
        end if
    end do
    close(10)
end program name