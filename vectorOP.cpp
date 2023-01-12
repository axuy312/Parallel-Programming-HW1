#include "PPintrin.h"

// implementation of absSerial(), but it is vectorized using PP intrinsics
void absVector(float *values, float *output, int N)
{
  __pp_vec_float x;
  __pp_vec_float result;
  __pp_vec_float zero = _pp_vset_float(0.f);
  __pp_mask maskAll, maskIsNegative, maskIsNotNegative;

  //  Note: Take a careful look at this loop indexing.  This example
  //  code is not guaranteed to work when (N % VECTOR_WIDTH) != 0.
  //  Why is that the case?
  for (int i = 0; i < N; i += VECTOR_WIDTH)
  {

    // All ones
    maskAll = _pp_init_ones();

    // All zeros
    maskIsNegative = _pp_init_ones(0);

    // Load vector of values from contiguous memory addresses
    _pp_vload_float(x, values + i, maskAll); // x = values[i];

    // Set mask according to predicate
    _pp_vlt_float(maskIsNegative, x, zero, maskAll); // if (x < 0) {

    // Execute instruction using mask ("if" clause)
    _pp_vsub_float(result, zero, x, maskIsNegative); //   output[i] = -x;

    // Inverse maskIsNegative to generate "else" mask
    maskIsNotNegative = _pp_mask_not(maskIsNegative); // } else {

    // Execute instruction ("else" clause)
    _pp_vload_float(result, values + i, maskIsNotNegative); //   output[i] = x; }

    // Write results back to memory
    _pp_vstore_float(output + i, result, maskAll);
  }
}

void clampedExpVector(float *values, int *exponents, float *output, int N)
{
  //
  // PP STUDENTS TODO: Implement your vectorized version of
  // clampedExpSerial() here.
  //
  // Your solution should work for any value of
  // N and VECTOR_WIDTH, not just when VECTOR_WIDTH divides N
  //
  __pp_vec_float x;
  __pp_vec_int y;
  __pp_vec_float result;
  __pp_vec_int zeroI = _pp_vset_int(0);
  __pp_vec_int ones = _pp_vset_int(1);
  __pp_vec_float maxv = _pp_vset_float(9.999999f);
  __pp_mask maskAll, maskResult, maskIsEqual, maskIsNotNegative, maskIsPositive, maskIsGTMax;
  for (int i = 0; i < N; i += VECTOR_WIDTH)
  {

    // All ones
    maskAll = _pp_init_ones();

    // All zeros
    maskIsEqual = _pp_init_ones(0);
	//Out of bount check
	int maskLen = (i+VECTOR_WIDTH > N)? N%VECTOR_WIDTH : VECTOR_WIDTH;
	maskResult = _pp_init_ones(maskLen);

    // Load vector of values from contiguous memory addresses
    _pp_vload_float(result, values + i, maskAll); // r = values[i];
    _pp_vload_float(x, values + i, maskAll); //      x = values[i];
    _pp_vload_int(y, exponents + i, maskAll); //     y = exponents[i];

    // Set mask according to predicate
    _pp_veq_int(maskIsEqual, y, zeroI, maskResult); // if (x == 0) {

    // Execute instruction using mask ("if" clause)
    _pp_vset_float(result, 1.f, maskIsEqual); //         output[i] = 1;

    // Inverse maskIsEqual to generate "else" mask
    maskIsNotNegative = _pp_mask_not(maskIsEqual); // } else {
	
	_pp_vgt_int(maskIsPositive, y, zeroI, maskAll);//   if (y > 0)
    _pp_vsub_int(y, y, ones, maskIsPositive); //              y -= 1;
	
	while (true)
	{
		maskIsPositive;
		_pp_vgt_int(maskIsPositive, y, zeroI, maskAll);//if (y > 0){
		_pp_vmult_float(result, result, x, maskIsPositive);// r *= x;
		_pp_vsub_int(y, y, ones, maskIsPositive);//     	  y -= 1;
		int cntBits = _pp_cntbits(maskIsPositive);
		if (cntBits == 0)
		{
			break;
		}
	}
	
	_pp_vgt_float(maskIsGTMax, result, maxv, maskAll);//    if (r > 9.999999f)
	_pp_vset_float(result, 9.999999f, maskIsGTMax);//            r = 9.999999f;

	// Write results back to memory
	_pp_vstore_float(output + i, result, maskResult);
  }
}

// returns the sum of all elements in values
// You can assume N is a multiple of VECTOR_WIDTH
// You can assume VECTOR_WIDTH is a power of 2
float arraySumVector(float *values, int N)
{

  //
  // PP STUDENTS TODO: Implement your vectorized version of arraySumSerial here
  //
	//N / VECTOR_WIDTH + VECTOR_WIDTH
  float *y = new float[N]();
  __pp_vec_float x, tx;
  __pp_mask maskResult, maskNotResult, oneMask, maskAll;
  
  
  maskAll = _pp_init_ones();
  oneMask = _pp_init_ones(1);
  
  for (int i = 0; i < N; i += VECTOR_WIDTH)
  {
	int maskLen = (i+VECTOR_WIDTH > N)? N%VECTOR_WIDTH : VECTOR_WIDTH;
	maskResult = _pp_init_ones(maskLen);

    _pp_vload_float(x, values + i, maskResult);
    _pp_vstore_float(y + i, x, maskResult);
  }
  
  for (int widN = N; widN > 1; widN = (widN+VECTOR_WIDTH-1)/VECTOR_WIDTH)
  {
	int index = 0;
	int tmpN = (widN < N)? widN : N;
	for (int i = 0; i < tmpN; i += VECTOR_WIDTH)
	{
	  // All ones
	  int maskLen = (i+VECTOR_WIDTH > tmpN)? tmpN%VECTOR_WIDTH : VECTOR_WIDTH;
	  maskResult = _pp_init_ones(maskLen);
	  maskNotResult = _pp_mask_not(maskResult);
	  
	  // Load vector of values from contiguous memory addresses
	  _pp_vload_float(x, y + i, maskAll); // x = values[i];
	  _pp_vset_float(x, 0.f, maskNotResult);

	  int widV = maskLen;
	  while (widV > 1)
	  {
		  _pp_hadd_float(tx, x);
		  _pp_interleave_float(x, tx);
		  
		  widV += 1;
		  widV /= 2;
		  
		  maskResult = _pp_init_ones(widV);
		  maskNotResult = _pp_mask_not(maskResult);
		  _pp_vset_float(x, 0.f, maskNotResult);
	  }
	  _pp_vstore_float(y + index, x, maskAll);
	  index += 1;
	}
  }
  _pp_vstore_float(y, x, maskAll);

  return y[0];
}