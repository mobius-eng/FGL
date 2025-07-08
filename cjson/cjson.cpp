#include <fstream>
#include <cstring>
#include <cmath>

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

CJSON* cjson_new_from_file(const char* file_name)
{
    
    std::ifstream file(file_name);
    CJSON* j = new nlohmann::json(nlohmann::json::parse(file));
    //file >> *j;
    //file.close();
    return j;
}

CJSON* cjson_new_empty()
{
    return new nlohmann::json();
}

void cjson_delete(CJSON *json)
{
    delete json;
}

CJSON *cjson_sub(CJSON *j, const char **path)
{
    if (*path == NULL)
    {
        return j;
    }
    return cjson_sub(&(*j)[path[0]], path+1);
}

CJSON *cjson_at(CJSON *j, int_fast32_t index)
{
    return &((*j)[index]);
}

double cjson_get_num(CJSON *j)
{
    if (j->is_number())
        return j->get<double>();
    else
        return NAN;
}

int_fast32_t cjson_get_int(CJSON *j)
{
    if (j->is_number_integer())
        return j->get<int>();
    else
        return NAN;
}

void cjson_get_str(char *dest, CJSON *j, int_fast32_t max_char)
{
    if(j->is_string())
    {
        std::string s = j->get<std::string>();
        std::strncpy(dest, s.c_str(), max_char);
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