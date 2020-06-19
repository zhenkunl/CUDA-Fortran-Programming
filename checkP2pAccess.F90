program checkP2pAccess

    use cudafor

    implicit none

    integer , allocatable :: p2pOK(:,:)
    integer :: nDevices , i, j, istat
    type (cudaDeviceProp) :: prop

    istat = cudaGetDeviceCount(nDevices)
    write(*,"('Number of CUDA -capable devices: ', i0 ,/)") nDevices

    do i = 0, nDevices -1
        istat = cudaGetDeviceProperties(prop , i)
        write(*,"('Device ', i0, ': ', a)") i, trim(prop%name)
    enddo
    write(*,*)

    allocate(p2pOK(0:nDevices -1, 0:nDevices -1))
    p2pOK = 0

    do j = 0, nDevices -1
        do i = j+1, nDevices -1
            istat = cudaDeviceCanAccessPeer(p2pOK(i,j), i, j)
            p2pOK(j,i) = p2pOK(i,j)
        end do
    end do

    do i = 0, nDevices -1
        write(*,"(3x,i3)", advance='no') i
    enddo
    write(*,*)

    do j = 0, nDevices -1
        write(*,"(i3)", advance='no') j
        do i = 0, nDevices -1
            if (i == j) then
                write(*,"(2x,'-',3x)", advance='no')
            else if (p2pOK(i,j) == 1) then
                write(*,"(2x, 'Y',3x)",advance='no')
            else
                write(*,"(6x)",advance='no')
            end if
        end do
        write(*,*)
    end do

end program checkP2pAccess