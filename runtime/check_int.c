#include<stdio.h>
#include<stdlib.h>

void check_int2 (char * hd, int s1, int s2) {
  if (s1 != s2) 
    {
      printf ("%s %d != %d\n", hd, s1, s2);
      printf ("llvm_sizeof = %d passed\n", s1);
      printf ("Llvm.Query.HirType.sizeof = %d passed\n", s2);
      abort();
    }
  else
    {
      printf ("llvm_sizeof = %d passed\n", s1);
      printf ("Llvm.Query.HirType.sizeof = %d passed\n", s2);
      return;
    }
}
