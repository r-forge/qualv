/* lcs.c to calculate longest common sequence */
#include <R.h>
#include <Rdefines.h>
#define max(A,B) ((A)>(B) ? (A) : (B))

SEXP  lcs(SEXP a, SEXP b, SEXP r_n_char) {
    SEXP M, LCS, LLCS, va, vb, QSI, list, list_names; 
    int *PM, *Pva, *Pvb;
    double *PQSI;
    int  i, j, na,nb,l, pos, n_char, PLLCS; 
    char *names[7] = {"a","b", "LLCS", "LCS", "QSI", "va", "vb"};

    /* Converting data from R */
    PROTECT(a = AS_CHARACTER(a));
    PROTECT(b = AS_CHARACTER(b));
    PROTECT(r_n_char = AS_INTEGER(r_n_char));

    n_char = INTEGER_POINTER(r_n_char)[0];

    na = length(a)+1; //vorher m
    nb = length(b)+1; //vorher n
    l = max(na, nb) - 1;

    char *Pa[na-1], *Pb[nb-1]; //pointers to strings
    
    //Obtain strings from R
    for(i = 0; i < na-1; i++) {
        Pa[i]= R_alloc(strlen(CHAR(STRING_ELT(a, i))), sizeof(char));
        strcpy(Pa[i], CHAR(STRING_ELT(a,i)));
    }
    for(j = 0; j < nb-1; j++){
        Pb[j]= R_alloc(strlen(CHAR(STRING_ELT(b, j))), sizeof(char));
        strcpy(Pb[j], CHAR(STRING_ELT(b,j)));
    }

    //build matrix to store calculation results for LCS
    PROTECT(M = allocMatrix(INTSXP, (na),(nb)));
    PM = INTEGER(M);

    //Initialize 
    for(i = 0; i < na; i++) {
        for(j = 0; j < nb; j++){
             PM[i+na*j] = 0;
        }
    }

    //Compare each character or string. 
    //if there is a match, the "match counter" stored
    //in PM is incremented by one.
    for(i = 1; i < na; i++) {
        for(j = 1; j < nb; j++){
            if (strcmp(Pa[i - 1],Pb[j - 1])==0) {
                PM[i + na * j] = PM[(i - 1) + na*(j - 1)] + 1;
            }
            else {
                PM[i + na* j] = max(PM[i + na*(j - 1)], PM[(i - 1) + na* j]);
            }
         }
     }
    //Read LCS-number from bottom right corner in PM
    PLLCS = PM[(na-1) + (na)*(nb-1)];
    PROTECT(LLCS = NEW_INTEGER(1));
    INTEGER(LLCS)[0]=PLLCS;


    //Variables to store additional results
    PROTECT(va = NEW_INTEGER(PLLCS));
    PROTECT(vb = NEW_INTEGER(PLLCS));
    PROTECT(QSI = NEW_NUMERIC(1));
    Pva = INTEGER(va);
    Pvb = INTEGER(vb);
    char *PLCS[PLLCS];
    for(i=0;i<PLLCS;i++){
        PLCS[i]= R_alloc(n_char, sizeof(char));
    }
    PQSI = NUMERIC_POINTER(QSI);
   
     //build LCS-sequence by traversing PM from bottom right towards
     //top left
     i = na-1; 
     j = nb-1;
     pos = PLLCS;

    while (i > 0 && j > 0) {
       if (PM[i + na* j] == (PM[(i - 1) + na*( j - 1)] + 1) 
               && strcmp(Pa[i - 1], Pb[j - 1])==0) {
                strcpy(PLCS[pos-1] , Pa[i-1]);
                Pva[pos-1] = i; 
                Pvb[pos-1] = j;
                i--;
                j--;
                pos--;
       } else {
            if (PM[(i - 1) + na* j] > PM[i + (j - 1)*na]) {
                i--; 
            } else {
                j--; 
            }
       }
    }
    
    //Calculate fraction
    PQSI[0] = (double)PLLCS/(double)l;

    //Prepare LCS strings for retruning to R
    PROTECT(LCS = allocVector(STRSXP, PLLCS));
    for(i=0;i<PLLCS;i++){
        SET_STRING_ELT(LCS, i,  mkChar(PLCS[i]));
    }


    //Generate List
    // a character string vector of the "names" attribute of the objects in our list
   PROTECT(list_names = allocVector(STRSXP, 7));
   for(i = 0; i < 7; i++)
      SET_STRING_ELT(list_names, i,  mkChar(names[i]));
 
   PROTECT(list = allocVector(VECSXP, 7)); // Creating a list with 7 vector elements
   SET_VECTOR_ELT(list, 0, a);         // attaching myint vector to list
   SET_VECTOR_ELT(list, 1, b);      // attaching mydouble vector to list
   SET_VECTOR_ELT(list, 2, LLCS);  
   SET_VECTOR_ELT(list, 3, LCS);  
   SET_VECTOR_ELT(list, 4, QSI);  
   SET_VECTOR_ELT(list, 5, va);  
   SET_VECTOR_ELT(list, 6, vb);  

   setAttrib(list, R_NamesSymbol, list_names); //and attaching the vector names

    UNPROTECT(11);

   return(list);

}
