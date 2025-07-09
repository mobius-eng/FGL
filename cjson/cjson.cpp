#include <fstream>
#include <cstring>
#include <cmath>
#include <string>

#include "json.hpp"
typedef nlohmann::json CJSON;
#define CJSON_TYPE
#include "cjson.h"


/*
static json& get_from_path(CJSON& j, const char** path, int path_depth)
{
    if (path_depth == 0)
        return j;
    else
        return get_from_path(j[path[0]], path + 1, path_depth - 1);
}

static json& get_from_path(CJSON& j, const char** spath, const int* ipath, const int* index_selector)
{
    if (*index_selector == 0)
        return get_from_path(j[*spath], spath + 1, ipath, index_selector + 1);
    else if (*index_selector == 1)
        return get_from_path(j[*ipath], spath, ipath + 1, index_selector + 1);
    else
        return j;
}

*/


extern "C" {

void cjson_new_from_file(CJSON **j, const char* file_name, cjson_int32_t *iostat)
{
    try
    {
        std::ifstream file(file_name);
        *j = new nlohmann::json(nlohmann::json::parse(file));
        *iostat = CJSON_SUCCESS;
    }
    catch(const std::ifstream::failure &e)
    {
        *iostat = CJSON_FILE_ERROR;
    }
    catch(...)
    {
        *iostat = CJSON_PARSE_ERROR;
    }
}

CJSON* cjson_new_empty()
{
    return new nlohmann::json();
}

void cjson_delete(CJSON *json, cjson_int32_t* istat)
{
    try
    {
        delete json;
        *istat = CJSON_SUCCESS;
    }
    catch(...)
    {
        *istat = CJSON_ACCESS_ERROR;
    }
}

void cjson_sub(CJSON **newj, CJSON *j, char **path, cjson_int32_t *istat)
{   
    if (path == NULL || j == NULL || j->is_null())
    {
        // invalid input
        *istat = CJSON_INVALID_INPUT_ERROR;
        return;
    }

    int i = 0;
    while (path[i] != NULL)
    {
        j = &((*j)[path[i]]);
        if (j->is_null())
        {
            *istat = CJSON_ACCESS_ERROR;
            return;
        }
        i++;
    }
    *newj = j;
    *istat = CJSON_SUCCESS;
}

void cjson_at(CJSON **newj, CJSON *j, cjson_int32_t index, cjson_int32_t *istat)
{
    if (j == NULL || index < 0 || (!j->is_array()))
    {
        *istat = CJSON_INVALID_INPUT_ERROR;
        return;
    }

    *newj = &((*j)[index]);
    
    if ((*newj)->is_null())
    {
        *istat = CJSON_ACCESS_ERROR;
        return;
    }

    *istat = CJSON_SUCCESS;
}

void cjson_get_num(double *x, CJSON *j, cjson_int32_t *istat)
{
    if (j == NULL || (!j->is_number()))
    {
        *istat = CJSON_INVALID_INPUT_ERROR;
        return;
    }
    *x = j->get<double>();
    *istat = CJSON_SUCCESS;
}

void cjson_get_int(cjson_int32_t *x, CJSON *j, cjson_int32_t *istat)
{
    if (j == NULL || (!j->is_number_integer()))
    {
        *istat = CJSON_INVALID_INPUT_ERROR;
        return;
    }
    int n = j->get<int>();
    *x = n;
    *istat = CJSON_SUCCESS;
}

void cjson_get_str(char *dest, CJSON *j, cjson_int32_t max_char, cjson_int32_t *istat, cjson_int32_t *len)
{
    if (dest == NULL || j == NULL || (!j->is_string()) || max_char <= 0)
    {
        *istat = CJSON_INVALID_INPUT_ERROR;
        return;
    }
    try
    {
        std::string s = j->get<std::string>();
        int n = s.length();
        *len = n < max_char? n : max_char-1;
        std::strncpy(dest, s.c_str(), *len);
        dest[*len] = '\0';
        *istat = CJSON_SUCCESS;
    }
    catch(...)
    {
        *istat = CJSON_ACCESS_ERROR;
    }
}

/*
void cjson_write_to_file(CJSON* json, const char* file_name)
{
    std::ofstream file(file_name);
    file << *json;
    file.close();
}

void cjson_put_number(CJSON* json, const char** path, int path_depth, double number)
{
    get_from_path(*json, path, path_depth) = number;
}

void cjson_put_string(CJSON* json, const char** path, int path_depth, const char* string)
{
    get_from_path(*json, path, path_depth) = string;
}

void cjson_put_num_vector(CJSON* json, const char** path, int path_depth, double* vector, int length)
{
    std::vector<double> v(length);
    for (int i = 0; i < length; i++) {
        v[i] = vector[i];
    }
    get_from_path(*json, path, path_depth) = v;
}

void cjson_make_vector(CJSON* json, const char** path, int path_depth)
{
    get_from_path(*json, path, path_depth) = json::array();
}

void cjson_set_vector_item(CJSON* json, const char** path, int path_depth, int index, CJSON* item)
{
    get_from_path(*json, path, path_depth)[index] = *item;
}

void cjson_delete(CJSON* json)
{
    delete json;
}

double cjson_read_number_g(CJSON* json, const char** spath, const int* ipath, const int* index_selector)
{
    return static_cast<double>(get_from_path(*json, spath, ipath, index_selector));
}

void cjson_put_number_g(CJSON* json, const char** spath, const int* ipath, const int* index_selector, double number)
{
    get_from_path(*json, spath, ipath, index_selector) = number;
}

void cjson_read_string_g(CJSON* json, const char** spath, const int* ipath, const int* index_selector, char* dest, int max_count)
{
    std::string s = get_from_path(*json, spath, ipath, index_selector);
    std::strncpy(dest, s.c_str(), max_count);
}

void cjson_put_string_g(CJSON* json, const char** spath, const int* ipath, const int* index_selector, const char* string)
{
    get_from_path(*json, spath, ipath, index_selector) = string;
}
*/
}