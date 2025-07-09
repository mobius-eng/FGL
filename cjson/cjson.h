#pragma once

#ifdef __cplusplus
#include <cstdint>
typedef std::int32_t cjson_int32_t;
extern "C" {
#else

#include <stdint.h>
typedef int32_t cjson_int32_t;

#endif

#ifndef __cplusplus
#include <stdint.h>
#endif


#ifndef CJSON_TYPE
typedef void CJSON;
#endif

#define CJSON_SUCCESS 0
#define CJSON_INVALID_INPUT_ERROR -1
#define CJSON_FILE_ERROR -2
#define CJSON_PARSE_ERROR -3
#define CJSON_ACCESS_ERROR -4


void   cjson_new_from_file(CJSON **j, const char* file_name, cjson_int32_t *iostat);
CJSON* cjson_new_empty();
void   cjson_delete(CJSON* json, cjson_int32_t *istat);
void   cjson_sub(CJSON **newj, CJSON *j, char **path, cjson_int32_t *istat);
void   cjson_at(CJSON **newj, CJSON *j, cjson_int32_t index, cjson_int32_t *istat);
void   cjson_get_num(double *x, CJSON *j, cjson_int32_t *istat);
void   cjson_get_int(cjson_int32_t *x, CJSON *j, cjson_int32_t *istat);
void   cjson_get_str(char *dest, CJSON *j, cjson_int32_t max_char, cjson_int32_t *istat, cjson_int32_t *len);


/*
void cjson_write_to_file(CJSON* json, const char* file_name);
void cjson_put_number(CJSON* json, const char** path, int path_depth, double number);
void cjson_put_string(CJSON* json, const char** path, int path_depth, const char* string);
void cjson_put_num_vector(CJSON* json, const char** path, int path_depth, double* vector, int length);
void cjson_make_vector(CJSON* json, const char** path, int path_depth);
void cjson_set_vector_item(CJSON* json, const char** path, int path_depth, int index, CJSON* item);

double cjson_read_number_g(CJSON* json, const char** spath, const int* ipath, const int* index_selector);
void cjson_put_number_g(CJSON* json, const char** spath, const int* ipath, const int* index_selector, double number);
void cjson_read_string_g(CJSON* json, const char** spath, const int* ipath, const int* index_selector, char* dest, int max_count);
void cjson_put_string_g(CJSON* json, const char** spath, const int* ipath, const int* index_selector, const char* string);

*/

#ifdef __cplusplus
}
#endif