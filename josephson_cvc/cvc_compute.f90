program CVC_COMPUTE_M
    use JOSEPHSON_MODEL_M
    implicit none

    real(8) :: t, h, tol, lowlim, t_end
    real(8) :: Y(3)  

    open(unit=10, file='josephson.dat', status='replace')
    
    t = 0.0d0
    h = 0.001d0
    Y = [0.0d0, 3.0d0, 0.0d0]
    
    I = 0.5
    beta = 0.2
    A = 0.
    omega = 2

    write(10, '(F12.6, F12.6, F14.10)') t, Y(1), Y(2)

    do while (t < t_end)
        if (abs(t - t_end) < abs(h)) h = t_end - t        
        call rkdp45(joseph_f, t, Y, h, tol, lowlim)
        write(10, '(F12.6, F12.6, F14.10)') t, Y(1), Y(2)
    end do
    close(10)
    print *, "Calculation finished. Data in josephson.dat"
    
    

end program CVC_COMPUTE_M