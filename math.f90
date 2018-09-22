module math

    implicit none
    public
    contains
        function trapz(x,y,uniGrid,lowInd,upInd) result(integral)
            complex :: integral
            complex, dimension(:) :: x,y
            complex :: delX
            integer, optional :: lowInd, upInd
            integer :: lI, uI
            logical, optional :: uniGrid
            logical :: uG
            integer :: i

            if (size(x)/=size(y)) then
                write(*,*) "x and y arrays not equal size."
                error stop 1
            endif

            if (size(x)<3) then
                write(*,*) "x array must have length 3 or greater."
                error stop 1
            endif

            if (.not.(present(lowInd))) then
                lI=1
            else
                lI=lowInd
            endif

            if (.not.(present(upInd))) then
                uI=size(x)
            else
                uI=upInd
            endif

            if (.not.(present(uniGrid))) then
                uG=.false.
            else
                uG=uniGrid
            endif

            if (uG) then
                delX=x(2)-x(1)
                integral=y(1)+y(size(y))
                do i=2, size(y)-1, 1
                    integral=integral+2*y(i)
                enddo
                integral=integral*(delX/2)
            else
                integral=0
                do i=2, size(x), 1
                    integral=integral+(y(i)+y(i-1))*(x(i)-x(i-1))/2
                enddo
            endif

        end function trapz

        function createArray(arMin, arMax, arN) result(ar)

            real, intent(in) :: arMin, arMax
            integer, intent(in) :: arN

            real :: dar
            real :: ar(arN)
            integer :: i

            dar=(arMax-arMin)/(arN-1)
            do i=1, arN, 1
                ar(i)=arMin+(i-1)*dar
            enddo

        end function createArray

end module math
