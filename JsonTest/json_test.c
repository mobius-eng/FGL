#include <stdlib.h>
#include <stdio.h>
#include "cjson.h"

int main()
{
    CJSON* j;
    cjson_int32_t istat;
    cjson_new_from_file(&j, "test.json", &istat);
    if (istat != CJSON_SUCCESS)
    {
        fprintf(stderr, "Error in initializing CJSON: %ld\n", istat);
        return -1;
    }
    char* ab_path[3] = {"A", "B", NULL};
    char* ac_path[3] = {"A", "C", NULL};
    char* ad_path[3] = {"A", "D", NULL};
    
    CJSON* ab;
    cjson_sub(&ab, j, ab_path, &istat);
    if (istat != CJSON_SUCCESS)
    {
        fprintf(stderr, "Error in reading path 'A' 'B'\n");
        return -1;
    }

    CJSON* ac;
    cjson_sub(&ac, j, ac_path, &istat);
    if (istat != CJSON_SUCCESS)
    {
        fprintf(stderr, "Error in reading path 'A' 'C'\n");
        return -1;
    }
    
    CJSON* ad;
    cjson_sub(&ad, j, ad_path, &istat);
    if (istat != CJSON_SUCCESS)
    {
        fprintf(stderr, "Error in reading path 'A' 'D'\n");
    }


    char *wrong_path[3] = {"A", "X", NULL};
    CJSON *emptyj;
    cjson_sub(&emptyj, j, wrong_path, &istat);
    if (istat != CJSON_SUCCESS)
    {
        printf("Wrong path detected as expected\n");
    }
    else
    {
        fprintf(stderr, "Wrong path was not detected\n");
        return -1;
    }

    double abx;
    cjson_get_num(&abx, ab, &istat);
    if (istat != CJSON_SUCCESS)
    {
        fprintf(stderr, "Cannot read double\n");
        return -1;
    }

    cjson_int32_t acx;
    CJSON *jtmp;
    cjson_at(&jtmp, ac, 1, &istat);
    if (istat != CJSON_SUCCESS)
    {
        fprintf(stderr, "Cannot access array\n");
        return -1;
    }
    cjson_get_int(&acx, jtmp, &istat);
    if (istat != CJSON_SUCCESS)
    {
        fprintf(stderr, "Cannot read int from array\n");
    }

    char adx[256];
    cjson_int32_t len;
    cjson_get_str(adx, ad, 256, &istat, &len);
    if (istat != CJSON_SUCCESS)
    {
        fprintf(stderr, "Cannot read string\n");
        return -1;
    }

    cjson_delete(j, &istat);
    if (istat != CJSON_SUCCESS)
    {
        fprintf(stderr, "Cannot free memory\n");
    }

    printf("Testing JSON C interace\n");
    printf("=========================================================\n");
    printf("J[\"A\"][\"B\"]    = %.4f\n", abx);
    printf("J[\"A\"][\"C\"][1] = %ld\n", acx);
    printf("J[\"A\"][\"D\"]    = \"%s\"\n", adx);
    printf("=========================================================\n");
    return 0;
}