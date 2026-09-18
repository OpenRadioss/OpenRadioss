#include <stdlib.h>
#include <stdio.h>
#define CHECK_GB_CALL(infogrb, call) infogrb = (call); if (infogrb != GrB_SUCCESS){ printf("Warning: GrB failure at line %d, %s\n", __LINE__, __FILE_NAME__); system("sync");}