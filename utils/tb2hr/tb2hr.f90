program tb2hr
    use utils
    implicit none

    ! Load FileName from Arguments List
    call LoadSystem()
    call Export2hr()
end program tb2hr
