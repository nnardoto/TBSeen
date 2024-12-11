module TBModel
    use fdf
    use mfi_lapack
    use TermTools
    use iso_fortran_env, only : dp=>real64

    implicit none

    ! System information to load system
    public :: LoadSystem
    public :: LoadHamiltonian
    public :: BandCalc
    public :: PathCalc

    private
        real(dp), parameter  :: pi = 3.1415926535897932384626433832795028841971693
        character(len = 200) :: SystemName, HFile, SFile
        integer :: MSize, nFock
        logical :: isOrthogonal
        integer, allocatable :: Degen(:), iRn(:,:)
        complex(dp), allocatable :: H(:,:,:), S(:,:,:)

        ! PathCalculation
        real(dp), allocatable :: FullPath(:,:)
        integer , allocatable :: nPath(:)

        ! Parallel Variables
        integer nThreads

    interface
        ! load and build the tight binding model for system
        module subroutine LoadSystem()
        end subroutine

        module subroutine SysConfig()
        end subroutine

        module subroutine LoadHamiltonian()
        end subroutine

        module subroutine LoadOverlap()
        end subroutine

        ! To BandCalculation
        module function BandCalc(Kp) result(EigVal)
          real(dp),  dimension(3)    :: kp
          real(dp), allocatable      :: EigVal(:)
        end function

        ! To PathBand
        module subroutine PathCalc()
        end subroutine
    end interface

end module
