#include "hc.h"
#include <assert.h>
#include <stdlib.h>
#include <string.h>
#include <stdio.h>


typedef struct {float x, y, z;} Point;
#define HC_POINT Point
typedef struct {float x, y, z;} Vector;
#define HC_VECTOR Vector
typedef struct {float a, b, c, d;} Plane;
#define HC_PLANE Plane
typedef struct {float red, green, blue;} RGB;
#define HC_RGB RGB
typedef struct {float red, green, blue, alpha;} RGBA;
#define HC_RGBA RGBA


HC_KEY hx_associate (HC_KEY key, HC_POINTER_SIZED_INT tag, int insert);

#define DEFINE(key, tag) hx_associate (key, tag, 1)
#define LOOKUP(tag) hx_associate (0, tag, 0)
#define K HC_Create_Segment



HC_KEY CGS_Read_Metafile (char const * basename);
void CGS_MSet_Vertex_Normals (HC_KEY geomKey, int offset, int count, char const * file);







