//Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2023 Altair Engineering Inc.
//Copyright>
//Copyright>    This program is free software: you can redistribute it and/or modify
//Copyright>    it under the terms of the GNU Affero General Public License as published by
//Copyright>    the Free Software Foundation, either version 3 of the License, or
//Copyright>    (at your option) any later version.
//Copyright>
//Copyright>    This program is distributed in the hope that it will be useful,
//Copyright>    but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>    GNU Affero General Public License for more details.
//Copyright>
//Copyright>    You should have received a copy of the GNU Affero General Public License
//Copyright>    along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>
//Copyright>
//Copyright>    Commercial Alternative: Altair Radioss Software
//Copyright>
//Copyright>    As an alternative to this open-source version, Altair also offers Altair Radioss
//Copyright>    software under a commercial license.  Contact Altair to discuss further if the
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.
#include <hardware.inc>

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <libxml/xmlmemory.h>
#include <libxml/parser.h>
#include <libxml/encoding.h>
#include <libxml/xmlwriter.h>

void browseTreeCount(xmlNodePtr noeud , int *cpt);
void browseTreeRead(xmlNodePtr noeud, int cpt, int isInput, int isValue, int isDataType, int isSubmodel, int *varDataType, char* varName, int* ivarValue, double* dvarValue, int *IPSET_PARAM);
int parseInputHstpFileCount(char *filename ATTRIBUTE_UNUSED, int *nbInput);
int parseInputHstpFileRead(char *filename ATTRIBUTE_UNUSED, int *nbInput, int *ITYP_PARAM, char* NAME_PARAM,int *IVAL_PARAM,double *DVAL_PARAM, int *IPSET_PARAM);
