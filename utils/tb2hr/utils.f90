module utils
    use iso_fortran_env, only : dp=>real64

    public :: LoadSystem
    public :: Export2Hr

    private
        character(len=:), allocatable  :: FileName
        integer                        :: nFock, mSize
        integer, allocatable           :: Degen(:)
        real(dp)                        :: Rn(3, 3)
        integer, allocatable           :: iRn(:,:)
        complex(dp), allocatable        :: H(:,:,:)
        complex(dp), allocatable        :: Rhop(:,:,:,:) ! motif

        !todo
        !w90 hamiltoniana
        !.model hamiltoniana
    contains
        subroutine LoadArguments()
            implicit none
            integer N

            call get_command_argument(1, length = N)
            allocate(character(N) :: FileName)
            call get_command_argument(1, FileName)

            print*, FileName
        end subroutine LoadArguments

        subroutine LoadSystem
            integer        ::  fp, ii, jj, i, j
            real(dp)       ::  R, Im
            real(dp)       ::  a1, a1j, a2, a2j, a3, a3j

            !read filename by terminal arguments
            call LoadArguments()

            ! open as newunit
            open(action = 'read', file=FileName, newunit = fp)
                read(fp, *)
                read(fp, *) Rn(1, :)
                read(fp, *) Rn(2, :)
                read(fp, *) Rn(3, :)
                read(fp, *) mSize
                read(fp, *) nFock

                ! time to allocate
                allocate(H(nFock, mSize, mSize))
                allocate(Rhop(3, nFock, mSize, mSize))
                allocate(degen(nFock))
                allocate(iRn(nFock, 3))

                ! degen read, 15 elements by line
                if ((nFock / 15) .gt. 1) then
                    do i = 1, (nFock / 15)
                        read(fp, *) Degen((i - 1)*15 + 1:(i - 1)*15 + 15)
                    enddo
                end if

                ! Last line of degenerecences
                read(fp, *) Degen((i - 1)*15 + 1:(i - 1)*15 + MOD(nFock, 15))
                read(fp, *)

                ! begin hamiltonian read
                do i = 1, nFock
                    read(fp, *) iRn(i, :)
                    do j = 1, mSize*mSize
                        read(fp, *) ii, jj, R, Im
                        H(i, ii, jj) = complex(R, Im)
                    enddo
                    if (i < nFock) read(fp, *)
                enddo
                read(fp,*) ! line skip

                ! begin reading orbital localization -- for motif
                do i = 1, nFock
                    read(fp, *) ! skip iRn
                    do j = 1, mSize*mSize
                        read(fp, *) ii, jj, a1, a1j, a2, a2j, a3, a3j
                        Rhop(1, i, ii, jj) = complex(a1, a1j)
                        Rhop(2, i, ii, jj) = complex(a2, a2j)
                        Rhop(3, i, ii, jj) = complex(a3, a3j)
                    enddo
                    if (i < nFock) read(fp, *)
                enddo
                close(fp)

        end subroutine LoadSystem

subroutine Export2Hr
            implicit none
            character(len=len(FileName)) :: outfile ! +2 because (.model=.dat+2)
            integer :: iunit, stat, i, j, k
            integer :: filePos, GlobalIndex
            real(8) :: a1, a2, a3

            ! Find the position of '.dat' in the filename
            filePos = index(FileName, '_tb.dat')
            if (filePos > 0) then
                ! Replace '_tb.dat' with '_hr.dat'
                outfile = FileName(1:filePos-1) // '_hr.dat'
            else
                ! If no '.dat' found, just append '.model'
                outfile = FileName // '_hr.dat'
            end if

            ! Open the file for writing (create it if it doesn't exist, overwrite if it does)
            open(NEWUNIT=iunit, file=trim(outfile), action='write', status='replace', iostat=stat)
            
            write(iunit, '(A)') '! Converted from _tb.dat to _hr.dat using FreeTBX Code'
            write(iunit, *) MSize
            write(iunit, *) nFock

            ! Write Degenerecency (multiplicity) block
            do i = 1, nFock/15 + 1
                do j = 1, 15
                    GlobalIndex = (i - 1)*15 + j
                    if (GlobalIndex <= nFock) then
                        write(iunit, '(I5)', advance = 'no') Degen(GlobalIndex)
                    endif
                enddo
                write(iunit, *) ' '
            enddo

            do i=1, nFock
               do j=1, mSize
                   do k=1,mSize
                       write(iunit, '(5I4, E18.8, E18.8)') iRn(i,:), j, k, real(H(i, j, k)), imag(H(i, j, k))
                   end do
               end do
            end do
            ! ------------------------------------------------------------------------------------ !
            close(iunit)
        end subroutine
end module
