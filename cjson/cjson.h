#pragma once

#ifdef __cplusplus
extern "C" {
#endif

#ifndef CJSON_TYPE
    typedef void CJSON;
#endif

    CJSON* cjson_read_from_file(const char* file_name);
    CJSON* cjson_make_empty();
    void cjson_write_to_file(CJSON* json, const char* file_name);
    void cjson_put_number(CJSON* json, const char** path, int path_depth, double number);
    void cjson_put_string(CJSON* json, const char** path, int path_depth, const char* string);
    void cjson_put_num_vector(CJSON* json, const char** path, int path_depth, double* vector, int length);
    void cjson_make_vector(CJSON* json, const char** path, int path_depth);
    void cjson_set_vector_item(CJSON* json, const char** path, int path_depth, int index, CJSON* item);
    void cjson_delete(CJSON* json);
    double cjson_read_number_g(CJSON* json, const char** spath, const int* ipath, const int* index_selector);
    void cjson_put_number_g(CJSON* json, const char** spath, const int* ipath, const int* index_selector, double number);
    void cjson_read_string_g(CJSON* json, const char** spath, const int* ipath, const int* index_selector, char* dest, int max_count);
    void cjson_put_string_g(CJSON* json, const char** spath, const int* ipath, const int* index_selector, const char* string);


#ifdef __cplusplus
}
#endif