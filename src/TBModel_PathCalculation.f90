submodule (TBModel) PathCalculation
  contains

  module procedure PathCalc
    use OMP_LIB
    implicit none
    integer                ::  i, j, k, TotKp, fp
    character(len=:), allocatable :: BandFileName
    real(dp)                 ::  dk(3)
    real(dp), allocatable    ::  kpts(:,:)
    real(dp), allocatable    ::  Bands(:,:)
    real(dp), allocatable    ::  kLenght(:)
    complex(dp), allocatable ::  HH(:,:)

    ! Allocate Memory for BandCalculation
    TotKp = sum(nPath) - nPath(size(nPath)) + 1

    call TitleBox("BEGINNING kPATH CALCULATION")
    call inLine("Total kPoints", TotKp)


    allocate(Bands(TotKp, MSize))
    allocate(HH(MSize, MSize))
    allocate(kpts(TotKp, 3))
    allocate(kLenght(TotKp))

    k = 1
    kLenght(1) = 0.0d0
    do i = 1, size(nPath) - 1
      dk = (FullPath(i + 1, :) - FullPath(i, :)) / (nPath(i))
      do j = 1, nPath(i)
        kpts(k, :) = FullPath(i, :) + (j - 1)*dk
        kLenght(k + 1) = kLenght(k) + NORM2(dk)
        k = k + 1
      enddo
    enddo
    kpts(k, :) =  BandCalc(FullPath(size(nPath), :))

    !call inLine("kPoints Calculation Progress: ", k, TotKp)
    !$omp parallel do private(i)
     do i = 1, TotKp
       Bands(i, :) = BandCalc(kpts(i, :))
       !call inLine("kPoints Calculation Progress: ", i, TotKp, OMP_GET_THREAD_NUM())
     enddo
    !$omp end parallel do

    !Escreve Bandas
    BandFileName = trim(SystemName) // "_band.dat"
    open(newunit = fp, file = BandFileName, action = 'write' )
    do j = 1, MSize
      do i = 1, TotKp
        write(fp, *) kLenght(i), Bands(i, j)
    enddo
      write(fp, *) ''
    enddo
    close(fp)

    call LineBox()
  end procedure PathCalc

end submodule PathCalculation
