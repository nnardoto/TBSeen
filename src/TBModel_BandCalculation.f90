submodule (TBModel) BandCalculation
    contains
        module procedure Ortho_BandCalc
            implicit none
            complex(dp), parameter  :: JJ = (0.0d0, 2.0d0)
            complex(dp) :: Phi
            complex(dp), allocatable :: HH(:,:)
            integer  :: i

            ! Allocation
            allocate(HH(MSize, MSize))
            allocate(EigVal(MSize))

            ! Initialize with zeros
            HH = (0.0d0, 0.0d0)
            do i = 1, nFock
                Phi = JJ*pi*dot_product(iRn(i,:), Kp)
                HH = HH + H(i, :,:) * exp(Phi)
            enddo
            ! todo lowdin Diagonalization

            ! Eigenvalue Calculation
            call mfi_heevd(HH, EigVal)
        end procedure Ortho_BandCalc



        module procedure nonOrtho_BandCalc
            implicit none
            complex(dp), parameter  :: JJ = (0.0d0, 2.0d0)
            complex(dp) :: Phi
            complex(dp), allocatable :: HH(:,:)
            integer  :: i

            ! Allocation
            allocate(HH(MSize, MSize))
            allocate(EigVal(MSize))


            call TextBox("nonOrthogonal mode not implemented")
            call exit(0)

            ! Initialize with zeros
            HH = (0.0d0, 0.0d0)
            do i = 1, nFock
                Phi = JJ*pi*dot_product(iRn(i,:), Kp)
                HH = HH + H(i, :,:) * exp(Phi)
            enddo
            ! todo lowdin Diagonalization

            ! Eigenvalue Calculation
            call mfi_heevd(HH, EigVal)
        end procedure nonOrtho_BandCalc

end submodule BandCalculation
