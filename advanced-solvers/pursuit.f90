program PURSUIT_ADAPTIVE_FINAL
    use ode_solver_m
    use models_m, only : k_pursuit, pursuit_f 
    implicit none
    
    integer :: j
    real(8) :: t, h, tol, lowlim, t_end
    real(8) :: Y(2)

    tol = 1.0d-6       
    lowlim = 1.0d-12   
    t_end = 0.0001d0     

    open(unit=10, file='pursuit.dat', status='replace')

    do j = 1, 10
        k_pursuit = 0.6d0 + (j-1) * 0.1d0                    
        t = 1.0d0
        h = -0.001d0
        Y = [0.0d0, 0.0d0]

        write(10, '(F12.6, F12.6, F14.10)') t, Y(1), h

        do while (t > t_end)
            if (abs(t - t_end) < abs(h)) h = t_end - t
            
            call rkdp45(pursuit_f, t, Y, h, tol, lowlim)
            
            write(10, '(F12.6, F12.6, F14.10)') t, Y(1), h
        end do

        write(10, *) 
        write(10, *) 
        print *, "Completed k =", k_pursuit
    end do

    close(10)
    print *, "Calculation finished. Data in pursuit.dat"
end program PURSUIT_ADAPTIVE_FINAL