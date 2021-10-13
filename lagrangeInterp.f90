program lagrangeInterp

        real (kind=8), dimension(1524) :: xpt, ypt
        real (kind=8) :: yLIP
        integer :: cntr, nNum

        CALL getData(xpt,ypt,cntr)
                
        nNum = NINT(xpt(cntr-1)-xpt(1))        

        open(nNum, file = 'LIPData.txt')
        
        do i=NINT(xpt(1)) , NINT(xpt(cntr-1)), 1
                CALL LIP(xpt, ypt, cntr, real(i,8), yLIP)
                write(nNum, *) i, yLIP
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

subroutine LIP(xpt, ypt, cntr, NewX, newY)

        implicit none
        
        real (kind=8), dimension(1524) :: xpt, ypt
        real (kind=8) :: newX, newY, term
        integer :: i, j, cntr
        
        do i=0, cntr, 1
                term = ypt(i)
                do j=0, cntr, 1
                        if (j /= i) then           
                                term = term*(newX - xpt(j))/(xpt(i) - xpt(j))
                        endif
                enddo   
                newY = newY + term
        enddo              

end subroutine LIP
