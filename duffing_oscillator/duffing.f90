program name
    use ode_solver_m
    use duffing_model_m
    implicit none
    
    real(8) :: t, h, tol, lowlim, t_end
    real(8) :: Y(2)

    ! Simulation params
    t      = 0.0d0
    h      = 0.01d0      
    tol    = 1.0d-8
    lowlim = 1.0d-12 
    t_end  = 1500.0d0

    ! Duffing system params
    system_params%alpha = -1.0d0
    system_params%beta  = 1.0d0
    system_params%gamma = 0.3d0
    system_params%delta = 0.1d0
    system_params%omega = 1.5d0

    Y = [1.0d0, 0.0d0]

    open(unit=10, file='duffing.dat', status='replace')

    ! Initial condition
    write(10, '(3F14.8)') t, Y(1), Y(2)

    ! Simulation loop
    do while (t < t_end)
        call rkdp45(duffing_system, t, Y, h, tol, lowlim)
        write(10, '(3F14.8)') t, Y(1), Y(2)
    end do

    close(10)

end program name