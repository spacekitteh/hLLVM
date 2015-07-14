#include<stdio.h>
#include<stdlib.h>
#include<stdint.h>

void check_int2 (char * hd, uint64_t s1, uint64_t s2) {
  if (s1 != s2) 
    {
      printf ("%s %ld != %ld\n", hd, s1, s2);
      printf ("llvm_sizeof = %ld\n", s1);
      printf ("Llvm.Query.HirType.sizeof = %ld\n", s2);
      printf ("sizeof check failed\n");
      abort();
    }
  /*
  else
    {
      printf ("llvm_sizeof = %d passed\n", s1);
      printf ("Llvm.Query.HirType.sizeof = %d passed\n", s2);
      return;
    }
  */
}
