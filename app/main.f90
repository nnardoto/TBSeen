program FreeTBX
  use TBModel
  use TermTools

  implicit none
  real BeginTime, StepTime, TotalTime

  call MainTitle("TBSeen: A Tight Binding Code for Beyond DFT Exploration", "Nathanael N. Batista", "nnardoto@gmail.com", "v0.0.1")
  ! Catch arguments from terminal and load system
  call LoadSystem()
  call CPU_TIME(BeginTime)
  call PathCalc()
  call CPU_TIME(TotalTime)
  call TitleBox("End Calculations")
  call inLine("Execution Time (s): ", TotalTime - BeginTime)
  call LineBox()
end program
