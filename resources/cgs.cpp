#include "cgs.h"




typedef struct key_node {
	struct key_node *left, *right;
	HC_KEY					key;
	HC_POINTER_SIZED_INT	tag;
} Key_Node;

Key_Node *key_table = (Key_Node *)0;

HC_KEY hx_associate (HC_KEY key, HC_POINTER_SIZED_INT tag, int insert) {
	Key_Node **linker = &key_table;
	while (1) {
		if (*linker == (Key_Node *)0) {
			if (!insert) return (HC_KEY)-1;
			*linker = (Key_Node *)malloc (sizeof (Key_Node));
			(*linker)->key = key;
			(*linker)->tag = tag;
			(*linker)->left = (*linker)->right = (Key_Node *)0;
			return key;
		}
		else {
			Key_Node *node = *linker;
			if (node->tag == tag) {
				if (insert) node->key = key;
				return node->key;
			} else
				linker = (node->tag > tag) ? &node->right : &node->left;
		}
	}
}



HC_KEY CGS_Read_Metafile (char const * basename) {
	char file[512];
	sprintf(file, "C:/techsoft3d/issues/20000/cgs/extract/%s", basename);

	HC_KEY geomKey;

	HC_Open_Segment("/cgs-shelf");{
		HC_Read_Metafile(file, ".", "");
		HC_Begin_Contents_Search(".", "geometry");{
			if (!HC_Find_Contents(0, &geomKey)) {
				assert(false);
				geomKey = HC_ERROR_KEY;
			}
		}HC_End_Contents_Search();
	}HC_Close_Segment();

	HC_Move_By_Key(geomKey, ".");

	return geomKey;
}


void CGS_MSet_Vertex_Normals (HC_KEY geomKey, int offset, int count, char const * file) {
	return;
}








