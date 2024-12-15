/******************************************************************************
  openmx2xatu.c:
  =============

    openmx2xatu.x extract the Kohn-Sham Hamiltonian from filename.scfout, and 
    write it in xatu input format

    N. N. Batista
    nnardoto@gmail.com
    17/Jun./2024
    
    Adaptation of analysis_example.c from openmx@3.9 code
    https://www.openmx-square.org/	

  
*******************************************************************************/

#include "indexing.h"
#include "read_scfout.h"


void main(int argc, char *argv[]) 
{
    // read scfout from openmx@3.9
    read_scfout(argv);
        
    MakeMap();
    Export2TBX("SystemName.model");
    Clear();
}

