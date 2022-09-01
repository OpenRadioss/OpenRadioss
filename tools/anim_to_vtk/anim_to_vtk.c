//Copyright>        OpenRadioss
//Copyright>        Copyright (C) 1986-2022 Altair Engineering Inc.
//Copyright>    
//Copyright>        This program is free software: you can redistribute it and/or modify
//Copyright>        it under the terms of the GNU Affero General Public License as published by
//Copyright>        the Free Software Foundation, either version 3 of the License, or
//Copyright>        (at your option) any later version.
//Copyright>    
///Copyright>        This program is distributed in the hope that it will be useful,
//Copyright>        but WITHOUT ANY WARRANTY; without even the implied warranty of
//Copyright>        MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
//Copyright>        GNU Affero General Public License for more details.
//Copyright>    
//Copyright>        You should have received a copy of the GNU Affero General Public License
//Copyright>        along with this program.  If not, see <https://www.gnu.org/licenses/>.
//Copyright>    
//Copyright>    
//Copyright>        Commercial Alternative: Altair Radioss Software 
//Copyright>    
//Copyright>        As an alternative to this open-source version, Altair also offers Altair Radioss 
//Copyright>        software under a commercial license.  Contact Altair to discuss further if the 
//Copyright>        commercial version may interest you: https://www.altair.com/radioss/.    

// For linux compilation :
// g++ -DLINUX -o anim_to_vtk.linux64.exe anim_to_vtk.c -lz

// To launch conversion :
// anim_to_vtk.linux64.exe  animationFile > vtkFile

#include <iostream>
using std::cout;
using std::endl;

#include <stdlib.h>
#include <string.h>
#include <zlib.h>
#include <math.h>

// X
#define FASTMAGI4 0x5426
#define FASTMAGI5 0x5427
#define FASTMAGI6 0x5428
#define FASTMAGI7 0x5429
#define FASTMAGI8 0x542a
#define FASTMAGI9 0x542b
#define FASTMAGI10 0x542c

#define MAX_CAR 81

// Macros to swap bytes
#define SWAP_BYTE(a, b) \
    {                   \
        char tmp;       \
        tmp = (a);      \
        (a) = (b);      \
        (b) = tmp;      \
    }

#define SWAP_2BYTES(shrtPtr)         \
    {                                \
        char *src = (char *)shrtPtr; \
        SWAP_BYTE(src[0], src[1]);   \
    }

#define SWAP_4BYTES(intPtr)         \
    {                               \
        char *src = (char *)intPtr; \
        SWAP_BYTE(src[0], src[3]);  \
        SWAP_BYTE(src[1], src[2]);  \
    }

#define SWAP_8BYTES(dblPtr)         \
    {                               \
        char *src = (char *)dblPtr; \
        SWAP_BYTE(src[0], src[7]);  \
        SWAP_BYTE(src[1], src[6]);  \
        SWAP_BYTE(src[2], src[5]);  \
        SWAP_BYTE(src[3], src[4]);  \
    }

#define SWAP_MANY2BYTES(intPtr, number)                     \
    {                                                       \
        int ptrIndex;                                       \
        for (ptrIndex = 0; ptrIndex < (number); ptrIndex++) \
            SWAP_2BYTES(&(intPtr[ptrIndex]));               \
    }

#define SWAP_MANY4BYTES(intPtr, number)                     \
    {                                                       \
        int ptrIndex;                                       \
        for (ptrIndex = 0; ptrIndex < (number); ptrIndex++) \
            SWAP_4BYTES(&(intPtr[ptrIndex]));               \
    }

#define SWAP_MANY8BYTES(intPtr, number)                     \
    {                                                       \
        int ptrIndex;                                       \
        for (ptrIndex = 0; ptrIndex < (number); ptrIndex++) \
            SWAP_8BYTES(&(intPtr[ptrIndex]));               \
    }

#define SWAP_BYTESINDATA(itemList, itemCount, sizeOfItem) \
    {                                                     \
        short *shortCopy;                                 \
        int *intCopy;                                     \
        double *doubleCopy;                               \
        switch ((sizeOfItem))                             \
        {                                                 \
        case 1:                                           \
            break;                                        \
        case 2:                                           \
            shortCopy = (short *)(itemList);              \
            SWAP_MANY2BYTES(shortCopy, (itemCount));      \
            break;                                        \
        case 4:                                           \
            intCopy = (int *)(itemList);                  \
            SWAP_MANY4BYTES(intCopy, (itemCount));        \
            break;                                        \
        case 8:                                           \
            doubleCopy = (double *)(itemList);            \
            SWAP_MANY8BYTES(doubleCopy, (itemCount));     \
            break;                                        \
        }                                                 \
    }

// ****************************************
// read in file
// ****************************************
int Ufread(void *pchar, size_t sizeOfItem,
           size_t numItems,
           gzFile gzFile,
           bool text = false)
{
    int ret = gzread(gzFile, pchar, sizeOfItem * numItems);
    if (ret < 0)
    {
        cout << "Error in reading file" << endl;
    }
    if (ret == 0)
    {
        cout << "Error in reading file: reached EOF" << endl;
    }
#if (__NUTC__ || LINUX || WIN32) // Swap bytes
    SWAP_BYTESINDATA(pchar, numItems, sizeOfItem);
#endif
    if (text)
        ((char *)pchar)[sizeOfItem * numItems] = '\0';
    return ret;
}

// ****************************************
// open file
// ****************************************
gzFile openPostFile(char *fileName)
{

    gzFile aFile = gzopen(fileName, "rb");
    return aFile;
}

#define FASTMAGI10 0x542c
// used to convert a short to a float
#define SHORT2FLOAT 3000.

typedef unsigned char Boolean;

int magic;     // object format
int flagA[10]; // array of flags
float a_time;  // state time

// 3D GEOMETRY
int nbElts3D;       // number of 8nodes elements
int nbParts3D;      // number of parts
int nbEFunc3D;      // number of element scalar values
int nbTens3D;       // number of tensors
int *connect3DA;    // connectivity array of 8  nbElts3D
Boolean *delElt3DA; // are the elements deleted or not, array of nbElts3D
int *defPart3DA;    // part definition: array of nbParts3D
char **pText3DA;    // part name: array of nbParts3D
char **fText3DA;    // array of scalar name: nbEFunc3D
float *eFunc3DA;    // scalar value per element array of nbEFunc3D*nbElts3D
char **tText3DA;    // tensor name array of nbTens3D
float *tensVal3DA;  // tens value array of 6*nbTens3D*nbElt3D
float *eMass3DA;    // nbElt3D, elt mass
int *elNum3DA;      // nbElt3D, intern elt numbering

// 2D GEOMETRY
int nbFacets;                      // number of 4nodes elements
int nbNodes;                       // total number of nodes
int nbParts;                       // number of parts
int nbFunc;                        // number of nodal scalar values
int nbEFunc;                       // number of element scalar values
int nbVect;                        // number of vectors
int nbTens;                        // number of tensors
int nbSkew;                        // number of skews
short *skewShortValA;              // array of the skew values defined in short * 3000 ;
float *skewValA;                   // array of the skew values for each elt
float *coorA;                      // coordinates array of 3*nbNodes
int *connectA;                     // connectivity array of 4 * nbFacets
char *delEltA;                     // are the elements deleted or not, array of nbFacets
int *defPartA;                     // part definition: array of nbParts
char **pTextA;                     // part name: array of nbParts
short *normShortA;                 // facet normal in short : array of 3*nbNodes
float *normFloatA;                 // facet normal in float : array of 3*nbNodes
float normVal, *normA;             // temporary
char **fTextA;                     // array of scalar name: nbFunc+nbEFunc
float *funcA;                      // scalar value per node array of nbFunc*nbNodes
float *eFuncA;                     // scalar value per element array of nbEFunc*nbFacets
char **vTextA;                     // vect name array of nbVect
float *vectValA;                   // vect val array of 3*nbVect*nbNodes
float *vectNormA;                  // vect norm array of nbVect*nbNodes
float *vectNormTmpA, *vectValTmpA; // temporary
char **tTextA;                     // tensor name array of nbTens
float *tensValA;                   // tens value array of 3*nbTens*nbElt
float *nMassA, *eMassA;            // nbNodes, nbElt, nodal, elt mass
int *nodNumA, *elNumA;             // nbNodes, nbElt, intern node/elt nb

// 1D geometry
int nbElts1D;       // number of 2nodes elements
int nbParts1D;      // number of parts
int nbEFunc1D;      // number of lin. elt scalar values
int nbTors1D;       // number of torseur values
int isSkew1D;       // number of skews
int *connect1DA;    // element connectivity array
Boolean *delElt1DA; // delEltA indicates which elts are deleted or not
int *defPart1DA;    // parts definition: array
char **pText1DA;    // part names array
char **fText1DA;    // array of scalar function names
float *eFunc1DA;    // array of element scalar values
char **tText1DA;    // array of tensor names
float *torsVal1DA;  // read the array of 3 forces, 3 moments torsor values for each elt
int *elt2Skew1DA;   // array of the skew number for each elt
float *eMass1DA;    // mass :elementar mass
int *elNum1DA;      // element numbering

// hierarchy
int nbSubsets;          // number of subsets
char *subsetText;       // subset name
int numParent;          // parent number
int nbSubsetSon;        // nb subset sons
int *subsetSonA;        // subset son list
int nbSubPart2D;        // nb 2D subparts
int *subPart2DA;        // 2D subpart list
int nbSubPart3D;        // nb 3D subparts
int *subPart3DA;        // 3D subpart list
int nbSubPart1D;        // nb 1D subparts
int *subPart1DA;        // 1D subpart list
int nbMaterial;         // material number
int nbProperties;       // number of Properties
char **materialTextA;   // material names
char **propertiesTextA; // property names
int *materialTypeA;     // material types
int *propertiesTypeA;   // property types
int *part2subset2DA;    // array of subset for each part
int *partMaterial2DA;   // array of material for each part
int *partProperties2DA; // array of properties for each part
int *part2subset3DA;    // array of subset for each part
int *partMaterial3DA;   // array of material for each part
int *partProperties3DA; // array of properties for each part
int *part2subset1DA;    // array of subset for each part
int *partMaterial1DA;   // array of material for each part
int *partProperties1DA; // array of properties for each part

// NODES/ELTS FOR Time History
int nbNodesTH;       // number of Time History nodes
int nbElts2DTH;      // number of Time History 2D elements
int nbElts3DTH;      // number of Time History 3D elements
int nbElts1DTH;      // number of Time History 1D elements
int *nodes2THA;      // node list
char **n2thTextA;    // node names
int *elt2DTHA;       // elt 2D list
char **elt2DthTextA; // elt 2D name
int *elt3DTHA;       // elt 3D list
char **elt3DthTextA; // elt 3D name
int *elt1DTHA;       // elt 1D list
char **elt1DthTextA; // elt 1D name

// SPH ELTS
int nbEltsSPH;
int nbPartsSPH;
int nbEFuncSPH;
int nbTensSPH;
int *connecSPH;
Boolean *delEltSPH; // are the elements deleted or not
int *defPartSPH;
float *eFuncSPH;
char **scalTextSPH;
char **tensTextSPH;
float *tensValSPH;
char **pTextSPH;
float *eMassSPH;
int *nodNumSPH;
int nbSubsetsSPH;
int *numParentSPH;
int *matPartSPH;
int *propPartSPH;
#define SKIPP_INT 0
#define SKIPP_SHORT 1
#define SKIPP_FLOAT 2
#define SKIPP_PCHAR 3
#define SKIPP_BOOL 4

// ****************************************
// replace ' ' with '_'
// ****************************************
void replaceUnderscore(char *ptr)
{
    char *ptr2 = ptr;
    do
    {
        *ptr2 = *ptr++;
        if (*ptr2 == ' ')
            *ptr2 = '_';
        ptr2++;
    } while (*ptr);
}

// ****************************************
// convert an A-File to ascii vtk format
// ****************************************
void readRadiossAnim(char *fileName)
{
    gzFile inf;
    char tmpText[128];
    int i, j;

    inf = openPostFile(fileName);
    if (!inf)
    {
        cout << "Can't open input file " << fileName << endl;
        exit(1);
    }
    Ufread(&magic, sizeof(int), 1, inf);

    switch (magic)
    {
    case FASTMAGI10:
    {
        Ufread(&a_time, sizeof(float), 1, inf); // time of the file
        Ufread(tmpText, sizeof(char), 81, inf); // Time text
        Ufread(tmpText, sizeof(char), 81, inf); // ModAnim text
        Ufread(tmpText, sizeof(char), 81, inf); // RadiossRun text
                                                // array of 10 flags
                                                //        flagA[0] defines if theflagA mass is saved or not
                                                //        flagA[1] defines if the node-element numbering arrays are saved or not
                                                //        flagA[2] defines format :if there is 3D geometry
                                                //        flagA[3] defines format :if there is 1D geometry
                                                //        flagA[4] defines hierarchy
                                                //        flagA[5] defines node/elt list for TH
                                                //        flagA[6] defines if there is a new skew for tensor 2D
                                                //        flagA[7] define if there is SPH format
                                                //        flagA[8] to flagsA[9] are not yet used

        Ufread(flagA, sizeof(int), 10, inf);
        // ********************
        // 2D GEOMETRY
        // ********************
        Ufread(&nbNodes, sizeof(int), 1, inf);  // number of nodes
        Ufread(&nbFacets, sizeof(int), 1, inf); // number of 4nodes elements
        Ufread(&nbParts, sizeof(int), 1, inf);  // number of parts
        Ufread(&nbFunc, sizeof(int), 1, inf);   // number of nodal scalar values
        Ufread(&nbEFunc, sizeof(int), 1, inf);  // number of elemt scalar values
        Ufread(&nbVect, sizeof(int), 1, inf);   // number of vector values
        Ufread(&nbTens, sizeof(int), 1, inf);   // number of tensor values
        Ufread(&nbSkew, sizeof(int), 1, inf);   // number of skews array of the skew values defined in short * 3000

        if (nbSkew)
        {
            skewShortValA = (short *)malloc(nbSkew * 6 * sizeof(short));
            skewValA = (float *)malloc(nbSkew * 6 * sizeof(float));
            Ufread(skewShortValA, sizeof(short), nbSkew * 6,
                   inf);
            for (i = 0; i < 6 * nbSkew; i++)
            {
                skewValA[i] = ((float)skewShortValA[i]) /
                              SHORT2FLOAT;
            }
        }

        // coordinates array: containing the x,y,z coordinates of each node
        coorA = (float *)malloc(3 * nbNodes * sizeof(float));
        Ufread(coorA, sizeof(float), 3 * nbNodes, inf);

        // element connectivity array with local node numbering [0 to (nbNodes-1)]
        if (nbFacets)
        {
            connectA = (int *)malloc(nbFacets * 4 * sizeof(int));
            Ufread(connectA, sizeof(int), nbFacets * 4, inf);

            // deleted elements : the deleted elements stay in their original parts,
            // the delEltA indicates which elements are deleted or not
            delEltA = (char *)malloc(nbFacets * sizeof(char));
            Ufread(delEltA, sizeof(char), nbFacets, inf);
            int nbDel2D = 0;
            for (int idel = 0; idel < nbFacets; idel++)
            {
                int int2D = (int)(delEltA[idel]);
                if (int2D != 0)
                    nbDel2D++;
            }
        }
        // parts definition: array containing an index on thelast facet which defines each part.
        if (nbParts != 0)
        {
            defPartA = (int *)malloc(nbParts * sizeof(int));
            Ufread(defPartA, sizeof(int), nbParts, inf);

            // part texts which defines the name of each part Each name does not exceed 50 characters.
            pTextA = (char **)malloc(nbParts * sizeof(char *));
            for (i = 0; i < nbParts; i++)
            {
                Ufread(tmpText, sizeof(char), 50, inf);
                pTextA[i] = strdup(tmpText);
            }
        }
        // array of the norm values for each nodes the norm are defined in short * 3000

        normShortA = (short *)malloc(3 * nbNodes * sizeof(short));
        normFloatA = (float *)malloc(3 * nbNodes * sizeof(float));
        Ufread(normShortA, sizeof(short), 3 * nbNodes, inf);
        for (i = 0; i < 3 * nbNodes; i++)
        {
            normFloatA[i] = ((float)normShortA[i]) / SHORT2FLOAT;
        }

        for (normA = normFloatA; normA < normFloatA + 3 * nbNodes;)
        {
            normVal = (*normA * *normA++);
            normVal += (*normA * *normA++);
            normVal += (*normA * *normA++);
            if ((normVal < .8) || (normVal > 1.2))
            {
                normA -= 3;
                *normA++ = 0.57735;
                *normA++ = 0.57735;
                *normA++ = 0.57735;
            }
        }
        free(normShortA); // array of the norm values for each nodes in FLOAT
        // scalar values
        if (nbFunc + nbEFunc)
        {
            // array of total scalar functions names (nodal +  element)
            fTextA = (char **)malloc((nbFunc + nbEFunc) * sizeof(char *));
            for (i = 0; i < (nbFunc + nbEFunc); i++)
            {
                Ufread(tmpText, sizeof(char), 81, inf);
                fTextA[i] = strdup(tmpText);
            }
            funcA = (float *)malloc(nbFunc * nbNodes * sizeof(float));
            eFuncA = (float *)malloc(nbEFunc * nbFacets * sizeof(float));
            if (nbFunc)
            {
                Ufread(funcA, sizeof(float), nbNodes * nbFunc, inf);
            }

            if (nbEFunc)
            {
                Ufread(eFuncA, sizeof(float), nbFacets * nbEFunc, inf);
            }
        }

        // vectors values
        if (nbVect)
        {
            // array of vector names
            vTextA = (char **)malloc(nbVect * sizeof(char *));
            for (int iv = 0; iv < nbVect; iv++)
            {
                Ufread(tmpText, sizeof(char), 81, inf);
                vTextA[iv] = strdup(tmpText);
            }
        }
        // read the array of x,y,z vector values for each node and compute the norm array
        vectValA = (float *)malloc(3 * nbNodes * nbVect * sizeof(float));
        Ufread(vectValA, sizeof(float), 3 * nbNodes * nbVect, inf);

        // tensors values
        if (nbTens)
        {
            // array of tensor names
            tTextA = (char **)malloc(nbTens * sizeof(char *));
            for (i = 0; i < nbTens; i++)
            {
                Ufread(tmpText, sizeof(char), 81, inf);
                tTextA[i] = strdup(tmpText);
            }
            // read the array of x,y,xy tensor values for each
            //    element
            tensValA = (float *)malloc(nbFacets * 3 * nbTens *
                                       sizeof(float));

            Ufread(tensValA, sizeof(float), nbTens * nbFacets * 3, inf);
        }

        // mass : elementar and nodal masses
        if (flagA[0] == 1)
        {
            eMassA = (float *)malloc(nbFacets * sizeof(float));
            Ufread(eMassA, sizeof(float), nbFacets, inf);

            nMassA = (float *)malloc(nbNodes * sizeof(float));
            Ufread(nMassA, sizeof(float), nbNodes, inf);
        }

        // node and element numbering
        if (flagA[1])
        {
            nodNumA = (int *)malloc(nbNodes * sizeof(int));
            Ufread(nodNumA, sizeof(int), nbNodes, inf);

            elNumA = (int *)malloc(nbFacets * sizeof(int));
            Ufread(elNumA, sizeof(int), nbFacets, inf);

            // contains the internal node-element numbers
        }
        if (flagA[4])
        { // hierarchy
            // array of subset for each part
            part2subset2DA = (int *)malloc(nbParts * sizeof(int));
            Ufread(part2subset2DA, sizeof(int), nbParts, inf);
            // array of material for each part
            partMaterial2DA = (int *)malloc(nbParts * sizeof(int));
            Ufread(partMaterial2DA, sizeof(int), nbParts, inf);
            // array of properties for each part
            partProperties2DA = (int *)malloc(nbParts * sizeof(int));
            Ufread(partProperties2DA, sizeof(int), nbParts,
                   inf);
        }
        // ********************
        // 3D GEOMETRY
        // ********************
        if (flagA[2])
        {
            Ufread(&nbElts3D, sizeof(int), 1, inf);  // number of  8nodes elements
            Ufread(&nbParts3D, sizeof(int), 1, inf); // number of parts
            Ufread(&nbEFunc3D, sizeof(int), 1, inf); // number of vol. elt scalar values
            Ufread(&nbTens3D, sizeof(int), 1, inf);  // number of tensor values

            // element connectivity array with local node
            //    numbering [0 to (nbNodes-1)]
            //        first element 1st node, first element 2nd node,
            //        first element 3rd node, first element 4th node,
            //        first element 5th node, first element 6th node,
            //        first element 7th node, first element 8th node,

            //        second element 1st node, second element 2nd node,
            //                                ...

            connect3DA = (int *)malloc(nbElts3D * 8 * sizeof(int));
            Ufread(connect3DA, sizeof(int), nbElts3D * 8, inf);

            // As the deleted elements stay in their original
            //    parts,
            //        the delEltA indicates which elements are deleted
            //        or not
            delElt3DA = (Boolean *)malloc(nbElts3D * sizeof(Boolean));
            Ufread(delElt3DA, sizeof(Boolean), nbElts3D, inf);
            int nbEltDel3D = 0;
            for (int idel = 0; idel < nbElts3D; idel++)
            {
                if ((int)(delElt3DA[idel]) != 0)
                    nbEltDel3D++;
            }

            // parts definition: array containing an index on the
            // last facet
            //         which defines each part.
            //         So the 1st part begins with the facet 0 and ends
            //         with the facet
            //         defPartA[0].
            //         The part number i begins with the facet
            //         "defPartA[i-1]" and ends
            //         with the facet "defPartA[i]".

            defPart3DA = (int *)malloc(nbParts3D * sizeof(int));
            Ufread(defPart3DA, sizeof(int), nbParts3D, inf);

            // part texts which defines the name of each part
            //    Each name does not exceed 50 characters.
            pText3DA = (char **)malloc(nbParts3D * sizeof(char
                                                              *));
            for (i = 0; i < nbParts3D; i++)
            {
                Ufread(tmpText, sizeof(char), 50, inf);
                pText3DA[i] = strdup(tmpText);
            }
            // scalar values
            if (nbEFunc3D)
            {
                // array of scalar functions names
                fText3DA = (char **)malloc(nbEFunc3D * sizeof(char *));
                for (i = 0; i < nbEFunc3D; i++)
                {
                    Ufread(tmpText, sizeof(char), 81, inf);
                    fText3DA[i] = strdup(tmpText);
                }
                // array of nodal,element scalar values
                eFunc3DA = (float *)malloc(nbEFunc3D * nbElts3D *
                                           sizeof(float));
                Ufread(eFunc3DA, sizeof(float), nbEFunc3D * nbElts3D, inf);
            }

            // tensors values
            if (nbTens3D)
            {
                // array of tensor names
                tText3DA = (char **)malloc(nbTens3D * sizeof(char *));
                for (i = 0; i < nbTens3D; i++)
                {
                    Ufread(tmpText, sizeof(char), 81, inf);
                    tText3DA[i] = strdup(tmpText);
                }

                // read the array of x,y,z,xy,yz,zx tensor values for each element
                tensVal3DA = (float *)malloc(nbElts3D * 6 *
                                             nbTens3D * sizeof(float));
                Ufread(tensVal3DA, sizeof(float), nbElts3D * 6 * nbTens3D, inf);
            }
            // mass : nodal, elementar mass
            if (flagA[0] == 1)
            {
                eMass3DA = (float *)malloc(nbElts3D * sizeof(float));
                Ufread(eMass3DA, sizeof(float), nbElts3D, inf);
            }
            // node and element numbering
            if (flagA[1] == 1)
            {
                elNum3DA = (int *)malloc(nbElts3D * sizeof(int));
                Ufread(elNum3DA, sizeof(int), nbElts3D, inf);

                // contains the internal node-element numbers
            }
            if (flagA[4])
            {
                // hierarchy
                // array of subset for each part
                part2subset3DA = (int *)malloc(nbParts3D * sizeof(int));
                Ufread(part2subset3DA, sizeof(int), nbParts3D,
                       inf);
                // array of material for each part
                partMaterial3DA = (int *)malloc(nbParts3D *
                                                sizeof(int));
                Ufread(partMaterial3DA, sizeof(int), nbParts3D,
                       inf);
                // array of properties for each part
                partProperties3DA = (int *)malloc(nbParts3D *
                                                  sizeof(int));
                Ufread(partProperties3DA, sizeof(int), nbParts3D,
                       inf);
            }
        }
        // ********************
        // 1D GEOMETRY
        // ********************
        if (flagA[3])
        {
            Ufread(&nbElts1D, sizeof(int), 1, inf);  // number of
                                                     //    2nodes elements
            Ufread(&nbParts1D, sizeof(int), 1, inf); // number
                                                     //    of parts
            Ufread(&nbEFunc1D, sizeof(int), 1, inf); // number
                                                     //    of line. elt scalar values
            Ufread(&nbTors1D, sizeof(int), 1, inf);  // number of
                                                     //        torseur values
            Ufread(&isSkew1D, sizeof(int), 1, inf);  // is there
                                                     //        any skews
                                                     // element connectivity array with local node
                                                     //            numbering [0 to (nbNodes-1)]
                                                     //    first element 1st node, first element 2nd node,
                                                     //    second element 1st node, second element 2nd node,
                                                     //    ...

            connect1DA = (int *)malloc(nbElts1D * 2 * sizeof(int));
            Ufread(connect1DA, sizeof(int), nbElts1D * 2, inf);

            // As the deleted elements stay in their original parts,
            //    the delEltA indicates which elements are deleted or not
            delElt1DA = (Boolean *)malloc(nbElts1D * sizeof(Boolean));
            Ufread(delElt1DA, sizeof(Boolean), nbElts1D, inf);

            // parts definition: array containing an index on the
            //    last facet
            //    which defines each part.
            //    So the 1st part begins with the facet 0 and ends
            //    with the facet
            //    defPartA[0].
            //    The part number i begins with the facet
            //    "defPartA[i-1]" and ends
            //    with the facet "defPartA[i]".

            defPart1DA = (int *)malloc(nbParts1D * sizeof(int));
            Ufread(defPart1DA, sizeof(int), nbParts1D, inf);

            // part texts which defines the name of each part
            //    Each name does not exceed 50 characters.
            pText1DA = (char **)malloc(nbParts1D * sizeof(char
                                                              *));
            for (i = 0; i < nbParts1D; i++)
            {
                Ufread(tmpText, sizeof(char), 50, inf);
                pText1DA[i] = strdup(tmpText);
            }

            // scalar values
            if (nbEFunc1D)
            {
                // array of scalar functions names
                fText1DA = (char **)malloc(nbEFunc1D * sizeof(char *));
                for (i = 0; i < nbEFunc1D; i++)
                {
                    Ufread(tmpText, sizeof(char), 81, inf);
                    fText1DA[i] = strdup(tmpText);
                }
                // array of nodal,element scalar values
                eFunc1DA = (float *)malloc(nbEFunc1D * nbElts1D *
                                           sizeof(float));
                Ufread(eFunc1DA, sizeof(float), nbElts1D * nbEFunc1D, inf);
            }

            // tensors values
            if (nbTors1D)
            {
                // array of tensor names
                tText1DA = (char **)malloc(nbTors1D * sizeof(char *));
                for (i = 0; i < nbTors1D; i++)
                {
                    Ufread(tmpText, sizeof(char), 81, inf);
                    tText1DA[i] = strdup(tmpText);
                }

                // read the array of x,y,z,xy,yz,zx tensor values for each element
                torsVal1DA = (float *)malloc(nbElts1D * 9 *
                                             nbTors1D * sizeof(float));
                Ufread(torsVal1DA, sizeof(float), nbElts1D * 9 * nbTors1D, inf);
            }

            if (isSkew1D)
            {
                // array of the skew number for each elt
                elt2Skew1DA = (int *)malloc(nbElts1D * sizeof(int));
                Ufread(elt2Skew1DA, sizeof(int), nbElts1D, inf);
            }
            // mass : nodal, elementar mass
            if (flagA[0] == 1)
            {
                eMass1DA = (float *)malloc(nbElts1D * sizeof(float));
                Ufread(eMass1DA, sizeof(float), nbElts1D, inf);
            }
            // node and element numbering
            if (flagA[1] == 1)
            {
                elNum1DA = (int *)malloc(nbElts1D * sizeof(int));
                Ufread(elNum1DA, sizeof(int), nbElts1D, inf);

                // contains the internal node-element numbers
            }
            if (flagA[4])
            { // hierarchy
                // array of subset for each part
                part2subset1DA = (int *)malloc(nbParts1D * sizeof(int));
                Ufread(part2subset1DA, sizeof(int), nbParts1D,
                       inf);
                // array of material for each part
                partMaterial1DA = (int *)malloc(nbParts1D *
                                                sizeof(int));
                Ufread(partMaterial1DA, sizeof(int), nbParts1D,
                       inf);
                // array of properties for each part
                partProperties1DA = (int *)malloc(nbParts1D *
                                                  sizeof(int));
                Ufread(partProperties1DA, sizeof(int), nbParts1D,
                       inf);
            }
        }
        // hierarchy
        if (flagA[4])
        {
            Ufread(&nbSubsets, sizeof(int), 1, inf); // number of subsets
            for (i = 0; i < nbSubsets; i++)
            {
                // subset name
                subsetText = (char *)malloc(50 * sizeof(char));
                Ufread(subsetText, sizeof(char), 50, inf);
                // parent number
                Ufread(&numParent, sizeof(int), 1, inf);
                // number of subsets sons
                nbSubsetSon = 0;
                Ufread(&nbSubsetSon, sizeof(int), 1, inf);
                // list of son subset
                if (nbSubsetSon)
                {
                    subsetSonA = (int *)malloc(nbSubsetSon * sizeof(int));
                    Ufread(subsetSonA, sizeof(int), nbSubsetSon, inf);
                }
                // number of 2D SubParts
                Ufread(&nbSubPart2D, sizeof(int), 1, inf);
                if (nbSubPart2D)
                {
                    // list of 2D SubParts
                    subPart2DA = (int *)malloc(nbSubPart2D * sizeof(int));
                    Ufread(subPart2DA, sizeof(int), nbSubPart2D, inf);
                }
                // number of 3D SubParts
                Ufread(&nbSubPart3D, sizeof(int), 1, inf);
                if (nbSubPart3D)
                {
                    // list of 3D SubParts
                    subPart3DA = (int *)malloc(nbSubPart3D * sizeof(int));
                    Ufread(subPart3DA, sizeof(int), nbSubPart3D, inf);
                }
                // number of 1D SubParts
                Ufread(&nbSubPart1D, sizeof(int), 1, inf);
                if (nbSubPart1D)
                {
                    // list of 1D SubParts
                    subPart1DA = (int *)malloc(nbSubPart1D * sizeof(int));
                    Ufread(subPart1DA, sizeof(int), nbSubPart1D, inf);
                }
            }

            // number of Material
            Ufread(&nbMaterial, sizeof(int), 1, inf);
            // number of Properties
            Ufread(&nbProperties, sizeof(int), 1, inf);
            // material names
            materialTextA = (char **)malloc(nbMaterial * sizeof(char *));
            for (i = 0; i < nbMaterial; i++)
            {
                Ufread(tmpText, sizeof(char), 50, inf);
                materialTextA[i] = strdup(tmpText);
            }
            // material types
            materialTypeA = (int *)malloc(nbMaterial * sizeof(int));
            Ufread(materialTypeA, sizeof(int), nbMaterial, inf);
            // properties names
            propertiesTextA = (char **)malloc(nbProperties *
                                              sizeof(char *));
            for (i = 0; i < nbProperties; i++)
            {
                Ufread(tmpText, sizeof(char), 50, inf);
                propertiesTextA[i] = strdup(tmpText);
            }
            // properties types
            propertiesTypeA = (int *)malloc(nbProperties *
                                            sizeof(int));
            Ufread(propertiesTypeA, sizeof(int), nbProperties,
                   inf);
        }
        // ********************
        // NODES/ELTS FOR Time History ( nodes & elems that are also selected for Time History output)
        // ********************
        if (flagA[5])
        {
            Ufread(&nbNodesTH, sizeof(int), 1, inf);  // number of Time History nodes
            Ufread(&nbElts2DTH, sizeof(int), 1, inf); // number of Time History 2D elements
            Ufread(&nbElts3DTH, sizeof(int), 1, inf); // number of Time History 3D elements
            Ufread(&nbElts1DTH, sizeof(int), 1, inf); // number of Time History 1D elements
            // node list
            nodes2THA = (int *)malloc(nbNodesTH * sizeof(int));
            Ufread(nodes2THA, sizeof(int), nbNodesTH, inf);
            // node names
            n2thTextA = (char **)malloc(nbNodesTH * sizeof(char *));
            for (i = 0; i < nbNodesTH; i++)
            {
                Ufread(tmpText, sizeof(char), 50, inf);
                n2thTextA[i] = strdup(tmpText);
            }
            // elt 2D list
            elt2DTHA = (int *)malloc(nbElts2DTH * sizeof(int));
            Ufread(elt2DTHA, sizeof(int), nbElts2DTH, inf);
            // elt 2D name
            elt2DthTextA = (char **)malloc(nbElts2DTH * sizeof(char *));
            for (i = 0; i < nbElts2DTH; i++)
            {
                Ufread(tmpText, sizeof(char), 50, inf);
                elt2DthTextA[i] = strdup(tmpText);
            }
            // elt 3D list
            elt3DTHA = (int *)malloc(nbElts3DTH * sizeof(int));
            Ufread(elt3DTHA, sizeof(int), nbElts3DTH, inf);
            // elt 3D name
            elt3DthTextA = (char **)malloc(nbElts3DTH * sizeof(char *));
            for (i = 0; i < nbElts3DTH; i++)
            {
                Ufread(tmpText, sizeof(char), 50, inf);
                elt3DthTextA[i] = strdup(tmpText);
            }
            // elt 1D list
            elt1DTHA = (int *)malloc(nbElts1DTH * sizeof(int));
            Ufread(elt1DTHA, sizeof(int), nbElts1DTH, inf);
            // elt 1D name
            elt1DthTextA = (char **)malloc(nbElts1DTH * sizeof(char *));
            for (i = 0; i < nbElts1DTH; i++)
            {
                Ufread(tmpText, sizeof(char), 50, inf);
                elt1DthTextA[i] = strdup(tmpText);
            }
        }
        // ********************
        // READ SPH PART */
        // ********************
        if (flagA[7])
        {
            int isph;

            Ufread(&nbEltsSPH, sizeof(int), 1, inf);
            Ufread(&nbPartsSPH, sizeof(int), 1, inf);
            Ufread(&nbEFuncSPH, sizeof(int), 1, inf);
            Ufread(&nbTensSPH, sizeof(int), 1, inf);

            if (nbEltsSPH != 0)
            {
                connecSPH = (int *)malloc(nbEltsSPH * sizeof(int));
                Ufread(connecSPH, sizeof(int), nbEltsSPH, inf);

                delEltSPH = (Boolean *)malloc(nbEltsSPH * sizeof(Boolean));
                Ufread(delEltSPH, sizeof(Boolean), nbEltsSPH, inf);
            }
            if (nbPartsSPH != 0)
            {
                defPartSPH = (int *)malloc(nbPartsSPH * sizeof(int));
                Ufread(defPartSPH, sizeof(int), nbPartsSPH, inf);

                // part texts which defines the name of each part
                //  Each name does not exceed 50 characters.
                pTextSPH = (char **)malloc(nbPartsSPH * sizeof(char *));
                for (i = 0; i < nbPartsSPH; i++)
                {
                    Ufread(tmpText, sizeof(char), 50, inf);
                    pTextSPH[i] = strdup(tmpText);
                }
            }

            if (nbEFuncSPH != 0)
            {
                scalTextSPH = (char **)malloc(nbEFuncSPH * sizeof(char *));
                for (int isph = 0; isph < nbEFuncSPH; isph++)
                {
                    Ufread(tmpText, sizeof(char), 81, inf);
                    scalTextSPH[isph] = strdup(tmpText);
                }

                eFuncSPH = (float *)malloc(nbEFuncSPH * nbEltsSPH * sizeof(float));
                Ufread(eFuncSPH, sizeof(float), nbEFuncSPH * nbEltsSPH, inf);
            }
            if (nbTensSPH != 0)
            { // SPH tensors are just like 3D tensors
                tensTextSPH = (char **)malloc(nbTensSPH * sizeof(char *));
                for (int isph = 0; isph < nbTensSPH; isph++)
                {
                    //   Ufread(tmpText, sizeof(char), 81, inf);
                    Ufread(tmpText, sizeof(char), 81, inf, true);
                    tensTextSPH[isph] = strdup(tmpText);
                }

                tensValSPH = (float *)malloc(nbEltsSPH * nbTensSPH * 6 * sizeof(float));
                Ufread(tensValSPH, sizeof(float), nbEltsSPH * nbTensSPH * 6, inf);
            }
            // sph mass
            if (flagA[0] == 1)
            {
                eMassSPH = (float *)malloc(nbEltsSPH * sizeof(float));
                Ufread(eMassSPH, sizeof(float), nbEltsSPH, inf);
            }

            // sph numbering
            if (flagA[1] == 1)
            {
                nodNumSPH = (int *)malloc(nbEltsSPH * sizeof(int));
                Ufread(nodNumSPH, sizeof(int), nbEltsSPH, inf);
            }
            // SPH HIERARCHY
            if (flagA[4])
            {
                // parent number
                numParentSPH = (int *)malloc(nbPartsSPH * sizeof(int));
                Ufread(numParentSPH, sizeof(int), nbPartsSPH, inf);
                matPartSPH = (int *)malloc(nbPartsSPH * sizeof(int));
                Ufread(matPartSPH, sizeof(int), nbPartsSPH, inf);
                propPartSPH = (int *)malloc(nbPartsSPH * sizeof(int));
                Ufread(propPartSPH, sizeof(int), nbPartsSPH, inf);
            }
        } // end if flag7



        // ********************
        // vtk ascii output 
        // ********************

        cout << "# vtk DataFile Version 3.0" << endl;
        cout << "vtk output" << endl;
        cout << "ASCII" << endl;
        cout << "DATASET UNSTRUCTURED_GRID" << endl;

        cout << "FIELD FieldData 2" << endl;
        cout << "TIME 1 1 double" << endl;
        cout << a_time << endl;
        cout << "CYCLE 1 1 int" << endl;
        cout << 0 << endl;

        // nodes
        cout << "POINTS " << nbNodes << " float" << endl;
        for (int inod = 0; inod < nbNodes; inod++)
        {
            cout << coorA[(3 * inod)] << " "
                 << coorA[(3 * inod) + 1] << " "
                 << coorA[(3 * inod) + 2] << endl;
        }
        cout << endl;

        // elements connectivity
        if (nbElts1D + nbFacets + nbElts3D + nbEltsSPH)
        {
            cout << "CELLS " << nbElts1D + nbFacets + nbElts3D + nbEltsSPH << " " << nbElts1D * 3 + nbFacets * 5 + nbElts3D * 9 + nbEltsSPH * 2 << endl;
            for (int icon = 0; icon < nbElts1D; icon++)
            {
                cout << 2 << " "
                     << connect1DA[(icon * 2)] << " "
                     << connect1DA[(icon * 2) + 1] << endl;
            }
            for (int icon = 0; icon < nbFacets; icon++)
            {
                cout << 4 << " "
                     << connectA[(icon * 4)] << " "
                     << connectA[(icon * 4) + 1] << " "
                     << connectA[(icon * 4) + 2] << " "
                     << connectA[(icon * 4) + 3] << endl;
            }
            for (int icon = 0; icon < nbElts3D; icon++)
            {
                cout << 8 << " "
                     << connect3DA[(icon * 8)] << "  "
                     << connect3DA[(icon * 8) + 1] << "  "
                     << connect3DA[(icon * 8) + 2] << "  "
                     << connect3DA[(icon * 8) + 3] << "  "
                     << connect3DA[(icon * 8) + 4] << "  "
                     << connect3DA[(icon * 8) + 5] << "  "
                     << connect3DA[(icon * 8) + 6] << "  "
                     << connect3DA[(icon * 8) + 7] << endl;
            }
            for (int icon = 0; icon < nbEltsSPH; icon++)
            {
                cout << 1 << " "
                     << connecSPH[icon] << endl;
            }
        }
        cout << endl;

        // elements type
        if (nbElts1D + nbFacets + nbElts3D + nbEltsSPH)
        {
            cout << "CELL_TYPES " << nbElts1D + nbFacets + nbElts3D + nbEltsSPH << endl;
            for (int icon = 0; icon < nbElts1D; icon++)
            {
                cout << 3 << endl;
            }
            for (int icon = 0; icon < nbFacets; icon++)
            {
                cout << 9 << endl;
            }
            for (int icon = 0; icon < nbElts3D; icon++)
            {
                cout << 12 << endl;
            }
            for (int icon = 0; icon < nbEltsSPH; icon++)
            {
                cout << 1 << endl;
            }
        }
        cout << endl;

        // node id
        cout << "SCALARS " << "ELEMENT_ID int 1" << endl;
        cout << "LOOKUP_TABLE default" << endl;
        for (int inod = 0; inod < nbNodes; inod++)
        {
            cout << nodNumA[inod] << endl;
        }
        cout << endl;

        // nodal scalars & vectors
        cout << "POINT_DATA " << nbNodes << endl;
        for (int ifun = 0; ifun < nbFunc; ifun++)
        {
            replaceUnderscore(fTextA[ifun + nbEFunc]);
            cout << "SCALARS " << fTextA[ifun + nbEFunc] << " float 1" << endl;
            cout << "LOOKUP_TABLE default" << endl;
            for (int inod = 0; inod < nbNodes; inod++)
            {
                cout << funcA[(ifun * nbNodes) + inod] << endl;
            }
            cout << endl;
        }

        for (int ivect = 0; ivect < nbVect; ivect++)
        {
            replaceUnderscore(vTextA[ivect]);
            cout << "VECTORS " << vTextA[ivect] << " float" << endl;

            for (int inod = 0; inod < nbNodes; inod++)
            {
                cout << vectValA[(3 * inod) + (ivect * 3 * nbNodes)] << " " << vectValA[(3 * inod) + 1 + (ivect * 3 * nbNodes)] << " " << vectValA[(3 * inod) + 2 + (ivect * 3 * nbNodes)] << endl;
            }
            cout << endl;
        }

        cout << "CELL_DATA " << nbElts1D + nbFacets + nbElts3D + nbEltsSPH << endl;

        
        // element id
        cout << "SCALARS " << "ELEMENT_ID int 1" << endl;
        cout << "LOOKUP_TABLE default" << endl;
        for (int iel = 0; iel < nbElts1D; iel++)
        {
            cout << elNum1DA[iel] << endl;
        }
        for (int iel = 0; iel < nbFacets; iel++)
        {
            cout << elNumA[iel] << endl;
        }
        for (int iel = 0; iel < nbElts3D; iel++)
        {
            cout << elNum3DA[iel] << endl;
        }
        for (int iel = 0; iel < nbEltsSPH; iel++)
        {
            cout << nodNumSPH[iel] << endl;
        }
        cout << endl;

        // element erosion status ( 0:off, 1:on )
        cout << "SCALARS " << "EROSION_STATUS int 1" << endl;
        cout << "LOOKUP_TABLE default" << endl;
        for (int iel = 0; iel < nbElts1D; iel++)
        {
            cout << (int)(delElt1DA[iel]==true) << endl;
        }
        for (int iel = 0; iel < nbFacets; iel++)
        {
            cout << (int)(delEltA[iel]==true) << endl;
        }
        for (int iel = 0; iel < nbElts3D; iel++)
        {
            cout << (int)(delElt3DA[iel]==true) << endl;
        }
        for (int iel = 0; iel < nbEltsSPH; iel++)
        {
            cout << (int)(delEltSPH[iel]==true) << endl;
        }
        cout << endl;


        // elemental scalars & tensors 

        for (int iefun = 0; iefun < nbEFunc1D; iefun++)
        {
            replaceUnderscore(fText1DA[iefun]);
            cout << "SCALARS 1DELEM_" << fText1DA[iefun] << " float 1" << endl;
            cout << "LOOKUP_TABLE default" << endl;
            for (int iel = 0; iel < nbElts1D; iel++)
            {
                cout << eFunc1DA[(iefun * nbElts1D) + iel] << endl;
            }
            for (int iel = 0; iel < nbFacets; iel++)
            {
                cout << 0 << endl;
            }
            for (int iel = 0; iel < nbElts3D; iel++)
            {
                cout << 0 << endl;
            }
            for (int iel = 0; iel < nbEltsSPH; iel++)
            {
                cout << 0 << endl;
            }
            cout << endl;
        }

        
        for (int iefun = 0; iefun < nbEFunc; iefun++)
        {
            replaceUnderscore(fTextA[iefun]);
            cout << "SCALARS 2DELEM_" << fTextA[iefun] << " float 1" << endl;
            cout << "LOOKUP_TABLE default" << endl;
            for (int iel = 0; iel < nbElts1D; iel++)
            {
                cout << 0 << endl;
            }
            for (int iel = 0; iel < nbFacets; iel++)
            {
                cout << eFuncA[(iefun * nbFacets) + iel] << endl;
            }
            for (int iel = 0; iel < nbElts3D; iel++)
            {
                cout << 0 << endl;
            }
            for (int iel = 0; iel < nbEltsSPH; iel++)
            {
                cout << 0 << endl;
            }
            cout << endl;
        }
        for (int ietens = 0; ietens < nbTens; ietens++)
        {
            replaceUnderscore(tTextA[ietens]);
            cout << "TENSORS 2DELEM_" << tTextA[ietens] << " float" << endl;
            for (int iel = 0; iel < nbElts1D; iel++)
            {
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
            }
            for (int iel = 0; iel < nbFacets; iel++)
            {
                cout << tensValA[(iel * 3) + (ietens * 3 * nbFacets)] << " " << tensValA[(iel * 3) + 2 + (ietens * 3 * nbFacets)] << " "
                     << "0 " << endl;
                cout << tensValA[(iel * 3) + 2 + (ietens * 3 * nbFacets)] << " " << tensValA[(iel * 3) + 1 + (ietens * 3 * nbFacets)] << " "
                     << "0 " << endl;
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
            }
            for (int iel = 0; iel < nbElts3D; iel++)
            {
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
            }
            for (int iel = 0; iel < nbEltsSPH; iel++)
            {
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
            }
            cout << endl;
        }

        for (int iefun = 0; iefun < nbEFunc3D; iefun++)
        {
            replaceUnderscore(fText3DA[iefun]);
            cout << "SCALARS 3DELEM_" << fText3DA[iefun] << " float 1" << endl;
            cout << "LOOKUP_TABLE default" << endl;
            for (int iel = 0; iel < nbElts1D; iel++)
            {
                cout << 0 << endl;
            }
            for (int iel = 0; iel < nbFacets; iel++)
            {
                cout << 0 << endl;
            }
            for (int iel = 0; iel < nbElts3D; iel++)
            {
                cout << eFunc3DA[(iefun * nbElts3D) + iel] << endl;
            }
            for (int iel = 0; iel < nbEltsSPH; iel++)
            {
                cout << 0 << endl;
            }
            cout << endl;
        }

        for (int ietens = 0; ietens < nbTens3D; ietens++)
        {
            replaceUnderscore(tText3DA[ietens]);
            cout << "TENSORS 3DELEM_" << tText3DA[ietens] << " float" << endl;
            for (int iel = 0; iel < nbElts1D; iel++)
            {
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
            }
            for (int iel = 0; iel < nbFacets; iel++)
            {
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
            }
            for (int iel = 0; iel < nbElts3D; iel++)
            {
                cout << tensVal3DA[(iel * 6) + (ietens * 6 * nbElts3D)] << " " << tensVal3DA[(iel * 6) + 3 + (ietens * 6 * nbElts3D)] << " " << tensVal3DA[(iel * 6) + 4 + (ietens * 6 * nbElts3D)] << endl;
                cout << tensVal3DA[(iel * 6) + 3 + (ietens * 6 * nbElts3D)] << " " << tensVal3DA[(iel * 6) + 1 + (ietens * 6 * nbElts3D)] << " " << tensVal3DA[(iel * 6) + 5 + (ietens * 6 * nbElts3D)] << endl;
                cout << tensVal3DA[(iel * 6) + 4 + (ietens * 6 * nbElts3D)] << " " << tensVal3DA[(iel * 6) + 5 + (ietens * 6 * nbElts3D)] << " " << tensVal3DA[(iel * 6) + 2 + (ietens * 6 * nbElts3D)] << endl;
            }
            for (int iel = 0; iel < nbEltsSPH; iel++)
            {   
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
            }
            cout << endl;
        }
        
        for (int iefun = 0; iefun < nbEFuncSPH; iefun++)
        {
            replaceUnderscore(scalTextSPH[iefun]);
            cout << "SCALARS SPHELEM_" << scalTextSPH[iefun] << " float 1" << endl;
            cout << "LOOKUP_TABLE default" << endl;
            for (int iel = 0; iel < nbElts1D; iel++)
            {
                cout << 0 << endl;
            }
            for (int iel = 0; iel < nbFacets; iel++)
            {
                cout << 0 << endl;
            }
            for (int iel = 0; iel < nbElts3D; iel++)
            {
                cout << 0 << endl;
            }
            for (int iel = 0; iel < nbEltsSPH; iel++)
            {
                cout << eFuncSPH[(iefun * nbEltsSPH) + iel] << endl;
            }
            cout << endl;
        }

        for (int ietens = 0; ietens < nbTensSPH; ietens++)
        {
            replaceUnderscore(tensTextSPH[ietens]);
            cout << "TENSORS SPHELEM_" << tensTextSPH[ietens] << " float" << endl;
            for (int iel = 0; iel < nbElts1D; iel++)
            {
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
            }
            for (int iel = 0; iel < nbFacets; iel++)
            {
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
            }
            for (int iel = 0; iel < nbElts3D; iel++)
            {
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
                cout << "0 "
                     << "0 "
                     << "0 " << endl;
            }
            for (int iel = 0; iel < nbEltsSPH; iel++)
            {
                cout << tensValSPH[(iel * 6) + (ietens * 6 * nbEltsSPH)] << " " << tensValSPH[(iel * 6) + 3 + (ietens * 6 * nbEltsSPH)] << " " << tensValSPH[(iel * 6) + 4 + (ietens * 6 * nbEltsSPH)] << endl;
                cout << tensValSPH[(iel * 6) + 3 + (ietens * 6 * nbEltsSPH)] << " " << tensValSPH[(iel * 6) + 1 + (ietens * 6 * nbEltsSPH)] << " " << tensValSPH[(iel * 6) + 5 + (ietens * 6 * nbEltsSPH)] << endl;
                cout << tensValSPH[(iel * 6) + 4 + (ietens * 6 * nbEltsSPH)] << " " << tensValSPH[(iel * 6) + 5 + (ietens * 6 * nbEltsSPH)] << " " << tensValSPH[(iel * 6) + 2 + (ietens * 6* nbEltsSPH)] << endl;
            }
            cout << endl;
        }

        gzclose(inf);
        break;
    } // end case block

    default:
        cout << "Error in Anim Files version " << endl;
        gzclose(inf);
        exit(1);
    } // end switch
}

// *******************
int main(int argc, char **argv)
{
    int i;

    if (argc < 2)
    {
        cout << "Call a filename" << endl;
        exit(1);
    }
    readRadiossAnim(argv[1]);

    exit(0);
}
