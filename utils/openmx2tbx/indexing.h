//#define ToEV 27.2113961
//#define ToAng 0.5291773

// Maping of Matrices, only for 2D cases
int **Map;
int N, nMat, MSize;
int Index(int l, int m, int n);

static const double ToEV  = 27.2114079527f;
static const double ToAng =  0.529177249f;

void MakeMap();
void Clear();
void ExtractHamiltonian(double ***NewHH, double ****RH);
void ExtractOverlap(double ***NewOverlap, double ****Overlap);
void Export2TBX(char* SystemName);
