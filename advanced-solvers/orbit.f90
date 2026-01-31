program ORBIT_TEST
    use ode_solver_m
    use models_m, only : orbit_f
    implicit none
    
    real(8) :: t, h, tol, t_end
    real(8) :: Y(4)

    t = 0.0d0
    h = 0.01d0      
    tol = 1.0d-8    
    t_end = 50.0d0
    
    Y = [1.0d0, 0.0d0, 0.4d0, 0.4d0]

    open(unit=10, file='orbit.dat', status='replace')

    do while (t < t_end)
        if (t + h > t_end) h = t_end - t
        call rkdp45(orbit_f, t, Y, h, tol, 1.0d-12)
        write(10, '(3F14.8)') Y(1), Y(2)
    end do

    write(10, *)
    write(10, *)

    t = 0.0d0
    h = 0.01d0 

    do while (t < t_end)
        if (t + h > t_end) h = t_end - t
        call rk4(orbit_f, t, Y, h)
        write(10, '(3F14.8)') Y(1), Y(2)
    end do

    close(10)
end program ORBIT_TEST