program lagrangeInterp

        real (kind=8), dimension(1524) :: xpt, ypt
        real (kind=8) :: yLIP
        integer :: cntr, nNum

        CALL getData(xpt,ypt,cntr)
                
        nNum = NINT(xpt(cntr-1)-xpt(1))        

        open(110, file = 'LIPData.txt')
               
        do i = 1, 110, 1

                CALL LIP(xpt, ypt, cntr, real(i,8), yLIP)
                write(110, *) i, yLIP
        enddo

end program lagrangeInterp


subroutine getData(xpt, ypt, cntr)

        implicit none
        
        real (kind=8), dimension(1524) :: xpt, ypt
        real (kind=8) :: x, y
        integer :: errval, cntr
        
        open(unit=15, file="data.txt", status="old")

        errval = 0
        cntr = 0

        do while (errval /= -1)

                read(15, *, iostat=errval) x, y
                
                if(errval == 0) then
                        cntr = cntr + 1
                        xpt(cntr) = x
                        ypt(cntr) = y
                else
                        close(15)
                endif
        enddo

end subroutine getData

subroutine LIP(xpt, ypt, cntr, xNew, yNew)

        implicit none
        
        real (kind=8), dimension(1524) :: xpt, ypt
        real (kind=8) :: xNew, yNew, term
        integer :: i, j, cntr
        
        yNew = 0.D0
        

        do i=1, cntr, 1

                term = 1

                do j=1, cntr, 1

                        if (j /= i) then           
                                term = term*(xNew - xpt(j))/(xpt(i) - xpt(j))
                        endif
                enddo   
                yNew = yNew + term*ypt(i)
        enddo              

end subroutine LIP
