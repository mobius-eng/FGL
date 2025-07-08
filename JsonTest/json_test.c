#include <stdlib.h>
#include <stdio.h>
#include "cjson.h"

int main()
{
    CJSON* j = cjson_new_from_file("test.json");
    char* ab_path[3] = {"A", "B", NULL};
    char* ac_path[3] = {"A", "C", NULL};
    char* ad_path[3] = {"A", "D", NULL};
    CJSON* ab = cjson_sub(j, ab_path);
    CJSON* ac = cjson_sub(j, ac_path);
    CJSON* ad = cjson_sub(j, ad_path);
    double abx = cjson_get_num(ab);
    int acx = cjson_get_int(cjson_at(ac, 1));
    char adx[256];
    cjson_get_str(adx, ad, 256);
    cjson_delete(j);
    printf("Testing JSON C interace\n");
    printf("=========================================================\n");
    printf("J[\"A\"][\"B\"]    = %.4f\n", abx);
    printf("J[\"A\"][\"C\"][1] = %d\n", acx);
    printf("J[\"A\"][\"D\"]    = \"%s\"\n", adx);
    printf("=========================================================\n");
    return 0;
}