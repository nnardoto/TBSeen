submodule (TBModel) Configuration           
  contains                                       
                                                 
    ! main routine for load and build the system
    module procedure LoadSystem
      use OMP_LIB
      character(len=:), allocatable  :: FileName          
      integer                        :: N                 

      !------------------------------------------------ 
      ! Catch Number of Threads
      !------------------------------------------------ 
      nThreads = OMP_GET_MAX_THREADS() 
      
      call TitleBox("Parallelization Settings")
      call LTextBox("::> Using OpenMP")
      call inLine("...> Threads Number: ", nThreads)

      !------------------------------------------------ 
      ! load input file from command-line argument      
      !------------------------------------------------ 
      call get_command_argument(1, length = N)            

      if (N < 1) then
        call TitleBox("THERE IS NO INPUT FILE")
        call exit()  
      endif

      allocate(character(N) :: FileName)
      call get_command_argument(1, FileName)
        
      
      ! -----------------------------------------------
      ! Load using Flexive Data Format Library
      ! -----------------------------------------------
      call fdf_init(FileName, "log")      


      ! -----------------------------------------------
      ! grep all parameters from input file
      ! -----------------------------------------------
      SystemName = fdf_get("SystemName", "null")
      HFile      = fdf_get("HFile"     , "null")
      SFile      = fdf_get("SFile"     , "null")

      ! -----------------------------------------------
      ! grep to do calculations
      ! -----------------------------------------------
      ! todo organizaton of calculations to do
      call MakePath()

      ! Allocation of Matrices of System
      call SysConfig()
    end procedure LoadSystem                               

    module procedure SysConfig
      implicit none

      integer :: hr_MSize, olp_MSize, hr_num, olp_num
      integer :: hrFile, olpFile

      ! =============================================================================
      !                          Minimal Consistence check
      ! =============================================================================
      open(action = 'read', file = HFile, newunit =  hrFile)
        read( hrFile, *)
        read( hrFile, *)  hr_MSize
        read( hrFile, *)  hr_num
        isOrthogonal = .true.
      close(hrFile)

      if(SFile /= "null") then 
        open(action = 'read', file = SFile, newunit = olpFile)
          read(olpFile, *)
          read(olpFile, *) olp_MSize
          read(olpFile, *) olp_num
          isOrthogonal = .false.
          call TitleBox("NON ORTHOGONAL SYSTEM")
        close(olpFile)
      else
          call TitleBox("ORTHOGONAL SYSTEM")
      endif
      
      if(isOrthogonal .eqv. .false.) then
        if(hr_MSize /= olp_MSize) then
          call TitleBox("The Size of Overlap Matrices are not equal to Fock Matrices")
          call exit()
        endif

        if(hr_num /= olp_num) then                                          
          call TitleBox("The Number of Overlap Matrices are not equal to Fock Matrices") 
          call exit()                                     
        endif
      endif
      ! ============================================================================= 
      !                             Allocation of Matrices                            
      ! ============================================================================= 
      call LTextBox("Beginning Allocation Process")
      call LineBox()
      MSize = hr_MSize
      nFock = hr_num

      call inLine("Number of Matrices", nFock)
      call inLine("Matrices Size (N,N)", MSize) 
      
      allocate(Degen(nFock))           
      allocate(H(nFock, MSize, MSize)) 
      allocate(S(nFock, MSize, MSize)) 
      allocate(iRn(nFock, 3))         
      
      call TextBox("")
      call LTextBox("::> Allocation Successful")
      call LoadHamiltonian()
      
      call LTextBox("...> Fock Matrices Loaded")
      
      
      if(isOrthogonal .eqv. .false.) then
        call LoadOverlap()
        call LTextBox("...> Overlap Matrices Loaded")
      endif

    call LineBox()
    end procedure SysConfig

    module procedure LoadHamiltonian
      implicit none
      integer :: fp
      integer :: i, ii, jj, l, m, n
      integer :: N, nM, idx
      real(kind = 4) :: R, Im
      
      open(action = 'read', file = HFile, newunit = fp)
        read(fp, *)
        read(fp, *) 
        read(fp, *) 

        ! 15 elements by line
        do i = 1, (nFock / 15)
          read(fp, *) Degen((i - 1)*15 + 1:(i - 1)*15 + 15)
        enddo
       
        ! Last line of degenerecences
        read(fp, *) Degen((i - 1)*15 + 1:(i - 1)*15 + MOD(nFock, 15))

        ! Begin Hamiltonian Read
        nM = MSize * MSize        ! Number of Elements in each matrix
        N = nFock*nM              ! Total Number of Elements

        do i = 1, N 
          read(fp, *) l, m, n, ii, jj, R, Im
          idx = i / nM + 1 - (nM - mod(i, nM))/nM
          iRn(idx, 1) = l
          iRn(idx, 2) = m
          iRn(idx, 3) = n
          H(idx, jj, ii) = complex(R, Im)
        enddo
      close(fp)
    end procedure LoadHamiltonian

    module procedure LoadOverlap                                  
      implicit none                                                   
      integer :: fp                                                   
      integer :: i, ii, jj, l, m, n                                   
      integer :: N, nM, idx                                           
      real(kind = 4) :: R, Im                                         
                                                                      
      open(action = 'read', file = HFile, newunit = fp)               
        read(fp, *)                                                   
        read(fp, *)                                              
        read(fp, *)                                              
                                                                      
        ! 15 elements by line                                         
        do i = 1, (nFock / 15)                                        
          read(fp, *) Degen((i - 1)*15 + 1:(i - 1)*15 + 15)           
        enddo                                                         
                                                                      
        ! Last line of degenerecences                                 
        read(fp, *) Degen((i - 1)*15 + 1:(i - 1)*15 + MOD(nFock, 15)) 
                                                                      
                                                                      
        ! Begin Hamiltonian Read                                      
        nM = MSize * MSize        ! Number of Elements in each matrix 
        N = nFock*nM              ! Total Number of Elements          

        do i = 1, N                                                   
          read(fp, *) l, m, n, ii, jj, R, Im                          
          idx = i / nM + 1 - (nM - mod(i, nM))/nM 
          iRn(idx, 1) = l                                             
          iRn(idx, 2) = m                                             
          iRn(idx, 3) = n                                             
          S(idx, jj, ii) = complex(R, Im)                             
        enddo                                                         
      close(fp)                                                       
    end procedure LoadOverlap                                                    


    subroutine MakePath()
      implicit none
      type(parsed_line), pointer :: pline
      type(block_fdf)            :: KPath
      integer                    :: i, j, N

      ! Get Number of KPOINTS
      N = 0
      if(fdf_block("KPath", KPath)) THEN 
        do while(fdf_bline(KPath, pline))       
          N = N + 1                    
        enddo                            
      endif       
      call fdf_bclose(KPath)           

      allocate(FullPath(N, 3))
      allocate(nPath(N))

    ! Save FullPath
    if(fdf_block('KPath', KPath)) then
      j = 1
      do while(fdf_bline(KPath, pline))         
          do i = 1, 3                       
            FullPath(j, i) = fdf_breals(pline, i)
            nPath(j)       = fdf_bintegers(pline, 1) 
          enddo
    
          j = j + 1
      enddo
    endif                                 
                                          
    call fdf_bclose(KPath)                

    end subroutine

end submodule Configuration                                           
