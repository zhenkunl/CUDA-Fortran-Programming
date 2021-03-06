program mpiDevices

    use cudafor
    use mpi

    implicit none

    ! global array size
    integer , parameter :: n = 1024*1024
    ! MPI variables
    integer :: myrank , nprocs , ierr
    ! device
    type(cudaDeviceProp) :: prop
    integer(int_ptr_kind ()) :: freeB , totalB , freeA , totalA
    real , device , allocatable :: d(:)
    integer :: i, j, istat

    ! MPI initialization
    call MPI_init(ierr)
    call MPI_comm_rank(MPI_COMM_WORLD , myrank , ierr)
    print*, 'myrank = ', myrank
    call MPI_comm_size(MPI_COMM_WORLD , nProcs , ierr)
    print*, 'nProcs = ', nProcs

    ! print compute mode for device
    istat = cudaGetDevice(j)
    istat = cudaGetDeviceProperties(prop , j)
    do i = 0, nprocs -1
    call MPI_BARRIER(MPI_COMM_WORLD , ierr)
    if (myrank == i) write(*,"('[',i0,'] using device: ', &
    i0, ' in compute mode: ', i0)") &
    myrank , j, prop%computeMode
    enddo

    ! get memory use before large allocations ,
    call MPI_BARRIER(MPI_COMM_WORLD , ierr)
    istat = cudaMemGetInfo(freeB , totalB)

    ! now allocate arrays , one rank at a time
    do j = 0, nProcs -1

    ! allocate on device associated with rank j
    call MPI_BARRIER(MPI_COMM_WORLD , ierr)
    if (myrank == j) allocate(d(n))

    ! Get free memory after allocation
    call MPI_BARRIER(MPI_COMM_WORLD , ierr)
    istat = cudaMemGetInfo(freeA , totalA)

    write(*,"(' [',i0,'] after allocation on rank: ', i0, &
    ', device arrays allocated: ', i0)") &
    myrank , j, (freeB -freeA)/n/4

    end do

    deallocate(d)
    call MPI_Finalize(ierr)

end program mpiDevices