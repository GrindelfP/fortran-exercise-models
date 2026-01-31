program JOSEPHSON
    use ode_solver_m
    use models_m, only : joseph_f, I_joseph, beta_joseph, A_joseph, omega_joseph

    implicit none

    real(8) :: t, h, tol, lowlim, t_end
    real(8) :: Y(2)

    tol = 1.0d-6       
    lowlim = 1.0d-12   
    t_end = 300.0d0     

    open(unit=10, file='josephson.dat', status='replace')
    
    t = 0.0d0
    h = 0.001d0
    Y = [0.0d0, 0.0d0]
    
    I_joseph = 0.1
    beta_joseph = 0.2
    A_joseph = 0.5
    omega_joseph = 2

    write(10, '(F12.6, F12.6, F14.10)') t, Y(1), Y(2)

    do while (t < t_end)
        if (abs(t - t_end) < abs(h)) h = t_end - t        
        call rkdp45(joseph_f, t, Y, h, tol, lowlim)
        write(10, '(F12.6, F12.6, F14.10)') t, Y(1), Y(2)
    end do
    close(10)
    print *, "Calculation finished. Data in josephson.dat"

end program JOSEPHSON