//Copyright>
//Copyright> Copyright (C) 1986-2024 Altair Engineering Inc.
//Copyright>
//Copyright> Permission is hereby granted, free of charge, to any person obtaining 
//Copyright> a copy of this software and associated documentation files (the "Software"), 
//Copyright> to deal in the Software without restriction, including without limitation 
//Copyright> the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or 
//Copyright> sell copies of the Software, and to permit persons to whom the Software is 
//Copyright> furnished to do so, subject to the following conditions:
//Copyright> 
//Copyright> The above copyright notice and this permission notice shall be included in all 
//Copyright> copies or substantial portions of the Software.
//Copyright> 
//Copyright> THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
//Copyright> IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
//Copyright> FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
//Copyright> AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, 
//Copyright> WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR 
//Copyright> IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
//Copyright>

// For linux compilation :
// g++ -DLINUX -o anim_to_vtk.linux64.exe anim_to_vtk.cpp

// To launch conversion :
// anim_to_vtk.linux64.exe  animationFile > vtkFile

#include <iostream>
using std::cout;

#include <stdlib.h>
#include <string.h>
#include <math.h>

#ifdef _WIN64

#include <winsock2.h>
#define htobe64(x) htonll(x)

#else

#include <arpa/inet.h>

#endif


// X
#define FASTMAGI4 0x5426
#define FASTMAGI5 0x5427
#define FASTMAGI6 0x5428
#define FASTMAGI7 0x5429
#define FASTMAGI8 0x542a
#define FASTMAGI9 0x542b
#define FASTMAGI10 0x542c

#define MAX_CAR 81

inline void SWAP_MANY2BYTES(uint16_t *intPtr, size_t number)
{
    for (size_t ptrIndex = 0; ptrIndex < (number); ptrIndex++)
    {
        intPtr[ptrIndex] = htons((intPtr[ptrIndex]));
    }
}

inline void SWAP_MANY4BYTES(int *intPtr, size_t number)
{
    for (size_t ptrIndex = 0; ptrIndex < (number); ptrIndex++)
    {
        intPtr[ptrIndex] = ntohl((intPtr[ptrIndex]));
    }
}

inline void SWAP_MANY8BYTES(double *intPtr, size_t number)
{
    for (size_t ptrIndex = 0; ptrIndex < (number); ptrIndex++)
    {
        intPtr[ptrIndex] = htobe64(intPtr[ptrIndex]);
    }
}

inline void SWAP_BYTESINDATA(void *itemList, size_t itemCount, size_t sizeOfItem)
{
    uint16_t *uint16_tCopy;
    int *intCopy;
    double *doubleCopy;
    switch ((sizeOfItem))
    {
    case 1:
        break;
    case 2:
        uint16_tCopy = (uint16_t *)(itemList);
        SWAP_MANY2BYTES(uint16_tCopy, (itemCount));
        break;
    case 4:
        intCopy = (int *)(itemList);
        SWAP_MANY4BYTES(intCopy, (itemCount));
        break;
    case 8:
        doubleCopy = (double *)(itemList);
        SWAP_MANY8BYTES(doubleCopy, (itemCount));
        break;
    }
}

template <class T>
inline void freeStarStar(T **c, const int &n)
{
    for (int i = 0; i < n; ++i)
    {
        free(c[i]);
    }
    free(c);
}

// ****************************************
// read in file
// ****************************************
int Ufread(void *pchar, size_t sizeOfItem,
           size_t numItems,
           FILE *OpenedFile,
           bool text = false)
{
    int ret = fread(pchar, sizeOfItem, numItems, OpenedFile);

    if (ret < 0)
    {
        cout << "Error in reading file"
             << "\n";
    }
#if (__NUTC__ || LINUX || WIN32) // Swap bytes
    SWAP_BYTESINDATA(pchar, numItems, sizeOfItem);
#endif
    if (text)
        ((char *)pchar)[sizeOfItem * numItems] = '\0';
    return ret;
}

#define FASTMAGI10 0x542c
// used to convert a uint16_t to a float

#define SHORT2FLOAT 3000.
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
    typedef unsigned char Boolean;

    int magic;     // object format
    int flagA[10]; // array of flags
    float a_time;  // state time

    // 3D GEOMETRY
    int nbElts3D = 0;             // number of 8nodes elements
    int nbParts3D = 0;            // number of parts
    int nbEFunc3D = 0;            // number of element scalar values
    int nbTens3D = 0;             // number of tensors
    int *connect3DA = nullptr;    // connectivity array of 8  nbElts3D
    Boolean *delElt3DA = nullptr; // are the elements deleted or not, array of nbElts3D
    int *defPart3DA = nullptr;    // part definition: array of nbParts3D
    char **pText3DA = nullptr;    // part name: array of nbParts3D
    char **fText3DA = nullptr;    // array of scalar name: nbEFunc3D
    float *eFunc3DA = nullptr;    // scalar value per element array of nbEFunc3D*nbElts3D
    char **tText3DA = nullptr;    // tensor name array of nbTens3D
    float *tensVal3DA = nullptr;  // tens value array of 6*nbTens3D*nbElt3D
    float *eMass3DA = nullptr;    // nbElt3D, elt mass
    int *elNum3DA = nullptr;      // nbElt3D, intern elt numbering

    // 2D GEOMETRY
    int nbFacets = 0;                           // number of 4nodes elements
    int nbNodes = 0;                            // total number of nodes
    int nbParts = 0;                            // number of parts
    int nbFunc = 0;                             // number of nodal scalar values
    int nbEFunc = 0;                            // number of element scalar values
    int nbVect = 0;                             // number of vectors
    int nbTens = 0;                             // number of tensors
    int nbSkew = 0;                             // number of skews
    uint16_t *skewShortValA = nullptr;          // array of the skew values defined in uint16_t * 3000 ;
    float *skewValA = nullptr;                  // array of the skew values for each elt
    float *coorA = nullptr;                     // coordinates array of 3*nbNodes
    int *connectA = nullptr;                    // connectivity array of 4 * nbFacets
    char *delEltA = nullptr;                    // are the elements deleted or not, array of nbFacets
    int *defPartA = nullptr;                    // part definition: array of nbParts
    char **pTextA = nullptr;                    // part name: array of nbParts
    uint16_t *normShortA = nullptr;             // facet normal in uint16_t : array of 3*nbNodes
    float *normFloatA = nullptr;                // facet normal in float : array of 3*nbNodes
    char **fTextA = nullptr;                    // array of scalar name: nbFunc+nbEFunc
    float *funcA = nullptr;                     // scalar value per node array of nbFunc*nbNodes
    float *eFuncA = nullptr;                    // scalar value per element array of nbEFunc*nbFacets
    char **vTextA = nullptr;                    // vect name array of nbVect
    float *vectValA = nullptr;                  // vect val array of 3*nbVect*nbNodes
    char **tTextA = nullptr;                    // tensor name array of nbTens
    float *tensValA = nullptr;                  // tens value array of 3*nbTens*nbElt
    float *nMassA = nullptr, *eMassA = nullptr; // nbNodes, nbElt, nodal, elt mass
    int *nodNumA = nullptr, *elNumA = nullptr;  // nbNodes, nbElt, intern node/elt nb

    // 1D geometry
    int nbElts1D = 0;             // number of 2nodes elements
    int nbParts1D = 0;            // number of parts
    int nbEFunc1D = 0;            // number of lin. elt scalar values
    int nbTors1D = 0;             // number of torseur values
    int isSkew1D = 0;             // number of skews
    int *connect1DA = nullptr;    // element connectivity array
    Boolean *delElt1DA = nullptr; // delEltA indicates which elts are deleted or not
    int *defPart1DA = nullptr;    // parts definition: array
    char **pText1DA = nullptr;    // part names array
    char **fText1DA = nullptr;    // array of scalar function names
    float *eFunc1DA = nullptr;    // array of element scalar values
    char **tText1DA = nullptr;    // array of tensor names
    float *torsVal1DA = nullptr;  // read the array of 3 forces, 3 moments torsor values for each elt
    int *elt2Skew1DA = nullptr;   // array of the skew number for each elt
    float *eMass1DA = nullptr;    // mass :elementar mass
    int *elNum1DA = nullptr;      // element numbering

    // hierarchy
    int nbSubsets = 0;                // number of subsets
    char *subsetText = nullptr;       // subset name
    int numParent = 0;                // parent number
    int nbSubsetSon = 0;              // nb subset sons
    int *subsetSonA = nullptr;        // subset son list
    int nbSubPart2D = 0;              // nb 2D subparts
    int *subPart2DA = nullptr;        // 2D subpart list
    int nbSubPart3D = 0;              // nb 3D subparts
    int *subPart3DA = nullptr;        // 3D subpart list
    int nbSubPart1D = 0;              // nb 1D subparts
    int *subPart1DA = nullptr;        // 1D subpart list
    int nbMaterials = 0;              // material number
    int nbProperties = 0;             // number of Properties
    char **materialTextA = nullptr;   // material names
    char **propertiesTextA = nullptr; // property names
    int *materialTypeA = nullptr;     // material types
    int *propertiesTypeA = nullptr;   // property types
    int *part2subset2DA = nullptr;    // array of subset for each part
    int *partMaterial2DA = nullptr;   // array of material for each part
    int *partProperties2DA = nullptr; // array of properties for each part
    int *part2subset3DA = nullptr;    // array of subset for each part
    int *partMaterial3DA = nullptr;   // array of material for each part
    int *partProperties3DA = nullptr; // array of properties for each part
    int *part2subset1DA = nullptr;    // array of subset for each part
    int *partMaterial1DA = nullptr;   // array of material for each part
    int *partProperties1DA = nullptr; // array of properties for each part

    // NODES/ELTS FOR Time History
    int nbNodesTH = 0;             // number of Time History nodes
    int nbElts2DTH = 0;            // number of Time History 2D elements
    int nbElts3DTH = 0;            // number of Time History 3D elements
    int nbElts1DTH = 0;            // number of Time History 1D elements
    int *nodes2THA = nullptr;      // node list
    char **n2thTextA = nullptr;    // node names
    int *elt2DTHA = nullptr;       // elt 2D list
    char **elt2DthTextA = nullptr; // elt 2D name
    int *elt3DTHA = nullptr;       // elt 3D list
    char **elt3DthTextA = nullptr; // elt 3D name
    int *elt1DTHA = nullptr;       // elt 1D list
    char **elt1DthTextA = nullptr; // elt 1D name

    // SPH ELTS
    int nbEltsSPH = 0;
    int nbPartsSPH = 0;
    int nbEFuncSPH = 0;
    int nbTensSPH = 0;
    int *connecSPH = nullptr;
    Boolean *delEltSPH = nullptr; // are the elements deleted or not
    int *defPartSPH = nullptr;
    float *eFuncSPH = nullptr;
    char **scalTextSPH = nullptr;
    char **tensTextSPH = nullptr;
    float *tensValSPH = nullptr;
    char **pTextSPH = nullptr;
    float *eMassSPH = nullptr;
    int *nodNumSPH = nullptr;
    int *numParentSPH = nullptr;
    int *matPartSPH = nullptr;
    int *propPartSPH = nullptr;

    FILE *inf;
    char tmpText[128];
    int i;

    inf = fopen(fileName, "rb");
    if (!inf)
    {
        cout << "Can't open input file " << fileName << "\n";
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
        Ufread(&nbSkew, sizeof(int), 1, inf);   // number of skews array of the skew values defined in uint16_t * 3000

        if (nbSkew)
        {
            skewShortValA = (uint16_t *)malloc(nbSkew * 6 * sizeof(uint16_t));
            skewValA = (float *)malloc(nbSkew * 6 * sizeof(float));
            Ufread(skewShortValA, sizeof(uint16_t), nbSkew * 6,
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
        // array of the norm values for each nodes the norm are defined in uint16_t * 3000

        normShortA = (uint16_t *)malloc(3 * nbNodes * sizeof(uint16_t));
        normFloatA = (float *)malloc(3 * nbNodes * sizeof(float));
        Ufread(normShortA, sizeof(uint16_t), 3 * nbNodes, inf);
        for (i = 0; i < 3 * nbNodes; i++)
        {
            normFloatA[i] = ((float)normShortA[i]) / SHORT2FLOAT;
        }

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
            Ufread(partProperties2DA, sizeof(int), nbParts, inf);
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
            pText3DA = (char **)malloc(nbParts3D * sizeof(char *));
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
                Ufread(part2subset3DA, sizeof(int), nbParts3D, inf);
                // array of material for each part
                partMaterial3DA = (int *)malloc(nbParts3D * sizeof(int));
                Ufread(partMaterial3DA, sizeof(int), nbParts3D, inf);
                // array of properties for each part
                partProperties3DA = (int *)malloc(nbParts3D *
                                                  sizeof(int));
                Ufread(partProperties3DA, sizeof(int), nbParts3D, inf);
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
            pText1DA = (char **)malloc(nbParts1D * sizeof(char *));
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
                free(subsetText);
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
                    free(subsetSonA);
                }
                // number of 2D SubParts
                Ufread(&nbSubPart2D, sizeof(int), 1, inf);
                if (nbSubPart2D)
                {
                    // list of 2D SubParts
                    subPart2DA = (int *)malloc(nbSubPart2D * sizeof(int));
                    Ufread(subPart2DA, sizeof(int), nbSubPart2D, inf);
                    free(subPart2DA);
                }
                // number of 3D SubParts
                Ufread(&nbSubPart3D, sizeof(int), 1, inf);
                if (nbSubPart3D)
                {
                    // list of 3D SubParts
                    subPart3DA = (int *)malloc(nbSubPart3D * sizeof(int));
                    Ufread(subPart3DA, sizeof(int), nbSubPart3D, inf);
                    free(subPart3DA);
                }
                // number of 1D SubParts
                Ufread(&nbSubPart1D, sizeof(int), 1, inf);
                if (nbSubPart1D)
                {
                    // list of 1D SubParts
                    subPart1DA = (int *)malloc(nbSubPart1D * sizeof(int));
                    Ufread(subPart1DA, sizeof(int), nbSubPart1D, inf);
                    free(subPart1DA);
                }
            }

            // number of Material
            Ufread(&nbMaterials, sizeof(int), 1, inf);
            // number of Properties
            Ufread(&nbProperties, sizeof(int), 1, inf);
            // material names
            materialTextA = (char **)malloc(nbMaterials * sizeof(char *));
            for (i = 0; i < nbMaterials; i++)
            {
                Ufread(tmpText, sizeof(char), 50, inf);
                materialTextA[i] = strdup(tmpText);
            }
            // material types
            materialTypeA = (int *)malloc(nbMaterials * sizeof(int));
            Ufread(materialTypeA, sizeof(int), nbMaterials, inf);
            // properties names
            propertiesTextA = (char **)malloc(nbProperties * sizeof(char *));
            for (i = 0; i < nbProperties; i++)
            {
                Ufread(tmpText, sizeof(char), 50, inf);
                propertiesTextA[i] = strdup(tmpText);
            }
            // properties types
            propertiesTypeA = (int *)malloc(nbProperties * sizeof(int));
            Ufread(propertiesTypeA, sizeof(int), nbProperties, inf);
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

        cout << "# vtk DataFile Version 3.0"
             << "\n";
        cout << "vtk output"
             << "\n";
        cout << "ASCII"
             << "\n";
        cout << "DATASET UNSTRUCTURED_GRID"
             << "\n";

        cout << "FIELD FieldData 2"
             << "\n";
        cout << "TIME 1 1 double"
             << "\n";
        cout << a_time << "\n";
        cout << "CYCLE 1 1 int"
             << "\n";
        cout << 0 << "\n";

        // nodes
        cout << "POINTS " << nbNodes << " float"
             << "\n";
        for (int inod = 0; inod < nbNodes; inod++)
        {
            cout << coorA[(3 * inod)] << " "
                 << coorA[(3 * inod) + 1] << " "
                 << coorA[(3 * inod) + 2] << "\n";
        }
        cout << "\n";

        // elements connectivity
        if (nbElts1D + nbFacets + nbElts3D + nbEltsSPH)
        {
            cout << "CELLS " << nbElts1D + nbFacets + nbElts3D + nbEltsSPH << " " << nbElts1D * 3 + nbFacets * 5 + nbElts3D * 9 + nbEltsSPH * 2 << "\n";
            for (int icon = 0; icon < nbElts1D; icon++)
            {
                cout << 2 << " "
                     << connect1DA[(icon * 2)] << " "
                     << connect1DA[(icon * 2) + 1] << "\n";
            }
            for (int icon = 0; icon < nbFacets; icon++)
            {
                cout << 4 << " "
                     << connectA[(icon * 4)] << " "
                     << connectA[(icon * 4) + 1] << " "
                     << connectA[(icon * 4) + 2] << " "
                     << connectA[(icon * 4) + 3] << "\n";
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
                     << connect3DA[(icon * 8) + 7] << "\n";
            }
            for (int icon = 0; icon < nbEltsSPH; icon++)
            {
                cout << 1 << " "
                     << connecSPH[icon] << "\n";
            }
        }
        cout << "\n";

        // elements type
        if (nbElts1D + nbFacets + nbElts3D + nbEltsSPH)
        {
            cout << "CELL_TYPES " << nbElts1D + nbFacets + nbElts3D + nbEltsSPH << "\n";
            for (int icon = 0; icon < nbElts1D; icon++)
            {
                cout << 3 << "\n";
            }
            for (int icon = 0; icon < nbFacets; icon++)
            {
                cout << 9 << "\n";
            }
            for (int icon = 0; icon < nbElts3D; icon++)
            {
                cout << 12 << "\n";
            }
            for (int icon = 0; icon < nbEltsSPH; icon++)
            {
                cout << 1 << "\n";
            }
        }
        cout << "\n";

        // nodal scalars & vectors
        cout << "POINT_DATA " << nbNodes << "\n";

        // node id
        cout << "SCALARS "
             << "NODE_ID int 1"
             << "\n";
        cout << "LOOKUP_TABLE default"
             << "\n";
        for (int inod = 0; inod < nbNodes; inod++)
        {
            cout << nodNumA[inod] << "\n";
        }
        cout << "\n";

        for (int ifun = 0; ifun < nbFunc; ifun++)
        {
            replaceUnderscore(fTextA[ifun]);
            cout << "SCALARS " << fTextA[ifun] << " float 1"
                 << "\n";
            cout << "LOOKUP_TABLE default"
                 << "\n";
            for (int inod = 0; inod < nbNodes; inod++)
            {
                cout << funcA[(ifun * nbNodes) + inod] << "\n";
            }
            cout << "\n";
        }

        for (int ivect = 0; ivect < nbVect; ivect++)
        {
            replaceUnderscore(vTextA[ivect]);
            cout << "VECTORS " << vTextA[ivect] << " float"
                 << "\n";

            for (int inod = 0; inod < nbNodes; inod++)
            {
                cout << vectValA[(3 * inod) + (ivect * 3 * nbNodes)] << " " << vectValA[(3 * inod) + 1 + (ivect * 3 * nbNodes)] << " " << vectValA[(3 * inod) + 2 + (ivect * 3 * nbNodes)] << "\n";
            }
            cout << "\n";
        }

        cout << "CELL_DATA " << nbElts1D + nbFacets + nbElts3D + nbEltsSPH << "\n";

        // element id
        cout << "SCALARS "
             << "ELEMENT_ID int 1"
             << "\n";
        cout << "LOOKUP_TABLE default"
             << "\n";
        for (int iel = 0; iel < nbElts1D; iel++)
        {
            cout << elNum1DA[iel] << "\n";
        }
        for (int iel = 0; iel < nbFacets; iel++)
        {
            cout << elNumA[iel] << "\n";
        }
        for (int iel = 0; iel < nbElts3D; iel++)
        {
            cout << elNum3DA[iel] << "\n";
        }
        for (int iel = 0; iel < nbEltsSPH; iel++)
        {
            cout << nodNumSPH[iel] << "\n";
        }
        cout << "\n";

        // part id
        cout << "SCALARS "
             << "PART_ID int 1"
             << "\n";
        cout << "LOOKUP_TABLE default"
             << "\n";

        int part1dIndex = 0;
        int part2dIndex = 0;
        int part3dIndex = 0;
        int part0dIndex = 0;

        for (int iel = 0; iel < nbElts1D; iel++)
        {

            if (part1dIndex < nbParts1D)
            {
                if (iel == defPart1DA[part1dIndex])
                    part1dIndex++;
            }

            if (part1dIndex < nbParts1D)
            {

                cout << atoi(pText1DA[part1dIndex]) << "\n";
            }
            else
            {
                cout << 0 << "\n";
            }
        }
        for (int iel = 0; iel < nbFacets; iel++)
        {
            if (iel == defPartA[part2dIndex])
                part2dIndex++;
            cout << atoi(pTextA[part2dIndex]) << "\n";
        }
        for (int iel = 0; iel < nbElts3D; iel++)
        {
            if (iel == defPart3DA[part3dIndex])
                part3dIndex++;
            cout << atoi(pText3DA[part3dIndex]) << "\n";
        }
        for (int iel = 0; iel < nbEltsSPH; iel++)
        {
            if (iel == defPartSPH[part0dIndex])
                part0dIndex++;
            cout << atoi(pTextSPH[part0dIndex]) << "\n";
        }
        cout << "\n";

        // element erosion status ( 0:off, 1:on )
        cout << "SCALARS "
             << "EROSION_STATUS int 1"
             << "\n";
        cout << "LOOKUP_TABLE default"
             << "\n";
        for (int iel = 0; iel < nbElts1D; iel++)
        {
            cout << (int)(delElt1DA[iel] == true) << "\n";
        }
        for (int iel = 0; iel < nbFacets; iel++)
        {
            cout << (int)(delEltA[iel] == true) << "\n";
        }
        for (int iel = 0; iel < nbElts3D; iel++)
        {
            cout << (int)(delElt3DA[iel] == true) << "\n";
        }
        for (int iel = 0; iel < nbEltsSPH; iel++)
        {
            cout << (int)(delEltSPH[iel] == true) << "\n";
        }
        cout << "\n";

        // elemental scalars & tensors

        for (int iefun = 0; iefun < nbEFunc1D; iefun++)
        {
            replaceUnderscore(fText1DA[iefun]);
            cout << "SCALARS 1DELEM_" << fText1DA[iefun] << " float 1"
                 << "\n";
            cout << "LOOKUP_TABLE default"
                 << "\n";
            for (int iel = 0; iel < nbElts1D; iel++)
            {
                cout << eFunc1DA[(iefun * nbElts1D) + iel] << "\n";
            }
            for (int iel = 0; iel < nbFacets; iel++)
            {
                cout << 0 << "\n";
            }
            for (int iel = 0; iel < nbElts3D; iel++)
            {
                cout << 0 << "\n";
            }
            for (int iel = 0; iel < nbEltsSPH; iel++)
            {
                cout << 0 << "\n";
            }
            cout << "\n";
        }

        for (int iefun = 0; iefun < nbTors1D; iefun++)
        {
            for (int j = 0; j < 9; j++)
            {
                replaceUnderscore(tText1DA[iefun]);
                if (j == 0) cout << "SCALARS 1DELEM_" << tText1DA[iefun] << "F1" << " float 1" << "\n";
                if (j == 1) cout << "SCALARS 1DELEM_" << tText1DA[iefun] << "F2" << " float 1" << "\n";
                if (j == 2) cout << "SCALARS 1DELEM_" << tText1DA[iefun] << "F3" << " float 1" << "\n";
                if (j == 3) cout << "SCALARS 1DELEM_" << tText1DA[iefun] << "M1" << " float 1" << "\n";
                if (j == 4) cout << "SCALARS 1DELEM_" << tText1DA[iefun] << "M2" << " float 1" << "\n";
                if (j == 5) cout << "SCALARS 1DELEM_" << tText1DA[iefun] << "M3" << " float 1" << "\n";
                if (j == 6) cout << "SCALARS 1DELEM_" << tText1DA[iefun] << "M4" << " float 1" << "\n";
                if (j == 7) cout << "SCALARS 1DELEM_" << tText1DA[iefun] << "M5" << " float 1" << "\n";
                if (j == 8) cout << "SCALARS 1DELEM_" << tText1DA[iefun] << "M6" << " float 1" << "\n";
                cout << "LOOKUP_TABLE default"
                     << "\n";
                for (int iel = 0; iel < nbElts1D; iel++)
                {
                    cout << torsVal1DA[(9 * iefun * nbElts1D) + iel * 9 + j] << "\n";
                }
                for (int iel = 0; iel < nbFacets; iel++)
                {
                    cout << 0 << "\n";
                }
                for (int iel = 0; iel < nbElts3D; iel++)
                {
                    cout << 0 << "\n";
                }
                for (int iel = 0; iel < nbEltsSPH; iel++)
                {
                    cout << 0 << "\n";
                }
                cout << "\n";
            }
        }

        for (int iefun = 0; iefun < nbEFunc; iefun++)
        {
            replaceUnderscore(fTextA[iefun + nbFunc]);
            cout << "SCALARS 2DELEM_" << fTextA[iefun + nbFunc] << " float 1"
                 << "\n";
            cout << "LOOKUP_TABLE default"
                 << "\n";
            for (int iel = 0; iel < nbElts1D; iel++)
            {
                cout << 0 << "\n";
            }
            for (int iel = 0; iel < nbFacets; iel++)
            {
                cout << eFuncA[(iefun * nbFacets) + iel] << "\n";
            }
            for (int iel = 0; iel < nbElts3D; iel++)
            {
                cout << 0 << "\n";
            }
            for (int iel = 0; iel < nbEltsSPH; iel++)
            {
                cout << 0 << "\n";
            }
            cout << "\n";
        }
        for (int ietens = 0; ietens < nbTens; ietens++)
        {
            replaceUnderscore(tTextA[ietens]);
            cout << "TENSORS 2DELEM_" << tTextA[ietens] << " float"
                 << "\n";
            for (int iel = 0; iel < nbElts1D; iel++)
            {
                cout << "0 "
                     << "0 "
                     << "0 "
                     << "\n";
                cout << "0 "
                     << "0 "
                     << "0 "
                     << "\n";
                cout << "0 "
                     << "0 "
                     << "0 "
                     << "\n";
            }
            for (int iel = 0; iel < nbFacets; iel++)
            {
                cout << tensValA[(iel * 3) + (ietens * 3 * nbFacets)] << " " << tensValA[(iel * 3) + 2 + (ietens * 3 * nbFacets)] << " "
                     << "0 "
                     << "\n";
                cout << tensValA[(iel * 3) + 2 + (ietens * 3 * nbFacets)] << " " << tensValA[(iel * 3) + 1 + (ietens * 3 * nbFacets)] << " "
                     << "0 "
                     << "\n";
                cout << "0 "
                     << "0 "
                     << "0 "
                     << "\n";
            }
            for (int iel = 0; iel < nbElts3D; iel++)
            {
                cout << "0 "
                     << "0 "
                     << "0 "
                     << "\n";
                cout << "0 "
                     << "0 "
                     << "0 "
                     << "\n";
                cout << "0 "
                     << "0 "
                     << "0 "
                     << "\n";
            }
            for (int iel = 0; iel < nbEltsSPH; iel++)
            {
                cout << "0 "
                     << "0 "
                     << "0 "
                     << "\n";
                cout << "0 "
                     << "0 "
                     << "0 "
                     << "\n";
                cout << "0 "
                     << "0 "
                     << "0 "
                     << "\n";
            }
            cout << "\n";
        }

        for (int iefun = 0; iefun < nbEFunc3D; iefun++)
        {
            replaceUnderscore(fText3DA[iefun]);
            cout << "SCALARS 3DELEM_" << fText3DA[iefun] << " float 1"
                 << "\n";
            cout << "LOOKUP_TABLE default"
                 << "\n";
            for (int iel = 0; iel < nbElts1D; iel++)
            {
                cout << 0 << "\n";
            }
            for (int iel = 0; iel < nbFacets; iel++)
            {
                cout << 0 << "\n";
            }
            for (int iel = 0; iel < nbElts3D; iel++)
            {
                cout << eFunc3DA[(iefun * nbElts3D) + iel] << "\n";
            }
            for (int iel = 0; iel < nbEltsSPH; iel++)
            {
                cout << 0 << "\n";
            }
            cout << "\n";
        }

        for (int ietens = 0; ietens < nbTens3D; ietens++)
        {
            replaceUnderscore(tText3DA[ietens]);
            cout << "TENSORS 3DELEM_" << tText3DA[ietens] << " float"
                 << "\n";
            for (int iel = 0; iel < nbElts1D; iel++)
            {
                cout << "0 "
                     << "0 "
                     << "0 "
                     << "\n";
                cout << "0 "
                     << "0 "
                     << "0 "
                     << "\n";
                cout << "0 "
                     << "0 "
                     << "0 "
                     << "\n";
            }
            for (int iel = 0; iel < nbFacets; iel++)
            {
                cout << "0 "
                     << "0 "
                     << "0 "
                     << "\n";
                cout << "0 "
                     << "0 "
                     << "0 "
                     << "\n";
                cout << "0 "
                     << "0 "
                     << "0 "
                     << "\n";
            }
            for (int iel = 0; iel < nbElts3D; iel++)
            {
                cout << tensVal3DA[(iel * 6) + (ietens * 6 * nbElts3D)] << " " << tensVal3DA[(iel * 6) + 3 + (ietens * 6 * nbElts3D)] << " " << tensVal3DA[(iel * 6) + 4 + (ietens * 6 * nbElts3D)] << "\n";
                cout << tensVal3DA[(iel * 6) + 3 + (ietens * 6 * nbElts3D)] << " " << tensVal3DA[(iel * 6) + 1 + (ietens * 6 * nbElts3D)] << " " << tensVal3DA[(iel * 6) + 5 + (ietens * 6 * nbElts3D)] << "\n";
                cout << tensVal3DA[(iel * 6) + 4 + (ietens * 6 * nbElts3D)] << " " << tensVal3DA[(iel * 6) + 5 + (ietens * 6 * nbElts3D)] << " " << tensVal3DA[(iel * 6) + 2 + (ietens * 6 * nbElts3D)] << "\n";
            }
            for (int iel = 0; iel < nbEltsSPH; iel++)
            {
                cout << "0 "
                     << "0 "
                     << "0 "
                     << "\n";
                cout << "0 "
                     << "0 "
                     << "0 "
                     << "\n";
                cout << "0 "
                     << "0 "
                     << "0 "
                     << "\n";
            }
            cout << "\n";
        }

        if (flagA[7])
        {
            for (int iefun = 0; iefun < nbEFuncSPH; iefun++)
            {
                replaceUnderscore(scalTextSPH[iefun]);
                cout << "SCALARS SPHELEM_" << scalTextSPH[iefun] << " float 1"
                     << "\n";
                cout << "LOOKUP_TABLE default"
                     << "\n";
                for (int iel = 0; iel < nbElts1D; iel++)
                {
                    cout << 0 << "\n";
                }
                for (int iel = 0; iel < nbFacets; iel++)
                {
                    cout << 0 << "\n";
                }
                for (int iel = 0; iel < nbElts3D; iel++)
                {
                    cout << 0 << "\n";
                }
                for (int iel = 0; iel < nbEltsSPH; iel++)
                {
                    cout << eFuncSPH[(iefun * nbEltsSPH) + iel] << "\n";
                }
                cout << "\n";
            }

            for (int ietens = 0; ietens < nbTensSPH; ietens++)
            {
                replaceUnderscore(tensTextSPH[ietens]);
                cout << "TENSORS SPHELEM_" << tensTextSPH[ietens] << " float"
                     << "\n";
                for (int iel = 0; iel < nbElts1D; iel++)
                {
                    cout << "0 "
                         << "0 "
                         << "0 "
                         << "\n";
                    cout << "0 "
                         << "0 "
                         << "0 "
                         << "\n";
                    cout << "0 "
                         << "0 "
                         << "0 "
                         << "\n";
                }
                for (int iel = 0; iel < nbFacets; iel++)
                {
                    cout << "0 "
                         << "0 "
                         << "0 "
                         << "\n";
                    cout << "0 "
                         << "0 "
                         << "0 "
                         << "\n";
                    cout << "0 "
                         << "0 "
                         << "0 "
                         << "\n";
                }
                for (int iel = 0; iel < nbElts3D; iel++)
                {
                    cout << "0 "
                         << "0 "
                         << "0 "
                         << "\n";
                    cout << "0 "
                         << "0 "
                         << "0 "
                         << "\n";
                    cout << "0 "
                         << "0 "
                         << "0 "
                         << "\n";
                }
                for (int iel = 0; iel < nbEltsSPH; iel++)
                {
                    cout << tensValSPH[(iel * 6) + (ietens * 6 * nbEltsSPH)] << " " << tensValSPH[(iel * 6) + 3 + (ietens * 6 * nbEltsSPH)] << " " << tensValSPH[(iel * 6) + 4 + (ietens * 6 * nbEltsSPH)] << "\n";
                    cout << tensValSPH[(iel * 6) + 3 + (ietens * 6 * nbEltsSPH)] << " " << tensValSPH[(iel * 6) + 1 + (ietens * 6 * nbEltsSPH)] << " " << tensValSPH[(iel * 6) + 5 + (ietens * 6 * nbEltsSPH)] << "\n";
                    cout << tensValSPH[(iel * 6) + 4 + (ietens * 6 * nbEltsSPH)] << " " << tensValSPH[(iel * 6) + 5 + (ietens * 6 * nbEltsSPH)] << " " << tensValSPH[(iel * 6) + 2 + (ietens * 6 * nbEltsSPH)] << "\n";
                }
                cout << "\n";
            }
        }

        fclose(inf);
        break;
    } // end case block

    default:
        cout << "Error in Anim Files version "
             << "\n";
        fclose(inf);
        exit(1);
    } // end switch

    if (flagA[2])
    {
        if (connect3DA != nullptr)
            free(connect3DA);
        if (delElt3DA != nullptr)
            free(delElt3DA);
        if (defPart3DA != nullptr)
            free(defPart3DA);
        if (pText3DA != nullptr)
            freeStarStar(pText3DA, nbParts3D);
        if (fText3DA != nullptr)
            freeStarStar(fText3DA, nbEFunc3D);
        if (eFunc3DA != nullptr)
            free(eFunc3DA);
        if (tText3DA != nullptr)
            freeStarStar(tText3DA, nbTens3D);
        if (tensVal3DA != nullptr)
            free(tensVal3DA);
        if (eMass3DA != nullptr)
            free(eMass3DA);
        if (elNum3DA != nullptr)
            free(elNum3DA);
    }

    if (skewShortValA != nullptr)
        free(skewShortValA);
    if (skewValA != nullptr)
        free(skewValA);
    if (coorA != nullptr)
        free(coorA);
    if (connectA != nullptr)
        free(connectA);
    if (delEltA != nullptr)
        free(delEltA);
    if (defPartA != nullptr)
        free(defPartA);
    if (pTextA != nullptr)
        freeStarStar(pTextA, nbParts);
    if (normShortA != nullptr)
        free(normShortA);
    if (normFloatA != nullptr)
        free(normFloatA);
    if (fTextA != nullptr)
        freeStarStar(fTextA, nbFunc + nbEFunc);
    if (funcA != nullptr)
        free(funcA);
    if (eFuncA != nullptr)
        free(eFuncA);
    if (vTextA != nullptr)
        freeStarStar(vTextA, nbVect);
    if (vectValA != nullptr)
        free(vectValA);
    if (tTextA != nullptr)
        freeStarStar(tTextA, nbTens);
    if (tensValA != nullptr)
        free(tensValA);

    if (flagA[0] == 1)
    {
        if (nMassA != nullptr)
            free(nMassA);
        if (eMassA != nullptr)
            free(eMassA);
    }

    if (flagA[1])
    {
        if (nodNumA != nullptr)
            free(nodNumA);
        if (elNumA != nullptr)
            free(elNumA);
    }

    if (flagA[3])
    {
        if (connect1DA != nullptr)
            free(connect1DA);
        if (delElt1DA != nullptr)
            free(delElt1DA);
        if (defPart1DA != nullptr)
            free(defPart1DA);
        if (pText1DA != nullptr)
            freeStarStar(pText1DA, nbParts1D);
        if (fText1DA != nullptr)
            freeStarStar(fText1DA, nbEFunc1D);
        if (eFunc1DA != nullptr)
            free(eFunc1DA);
        if (tText1DA != nullptr)
            freeStarStar(tText1DA, nbTors1D);
        if (torsVal1DA != nullptr)
            free(torsVal1DA);
        if (elt2Skew1DA != nullptr)
            free(elt2Skew1DA);
        if (eMass1DA != nullptr)
            free(eMass1DA);
        if (elNum1DA != nullptr)
            free(elNum1DA);
    }

    if (flagA[4])
    {
        if (materialTextA != nullptr)
            freeStarStar(materialTextA, nbMaterials);
        if (propertiesTextA != nullptr)
            freeStarStar(propertiesTextA, nbProperties);
        if (materialTypeA != nullptr)
            free(materialTypeA);
        if (propertiesTypeA != nullptr)
            free(propertiesTypeA);
        if (part2subset2DA != nullptr)
            free(part2subset2DA);
        if (partMaterial2DA != nullptr)
            free(partMaterial2DA);
        if (partProperties2DA != nullptr)
            free(partProperties2DA);
        if (part2subset3DA != nullptr)
            free(part2subset3DA);
        if (partMaterial3DA != nullptr)
            free(partMaterial3DA);
        if (partProperties3DA != nullptr)
            free(partProperties3DA);
        if (part2subset1DA != nullptr)
            free(part2subset1DA);
        if (partMaterial1DA != nullptr)
            free(partMaterial1DA);
        if (partProperties1DA != nullptr)
            free(partProperties1DA);
    }

    if (flagA[5])
    {
        if (nodes2THA != nullptr)
            free(nodes2THA);
        if (n2thTextA != nullptr)
            freeStarStar(n2thTextA, nbNodesTH);
        if (elt2DTHA != nullptr)
            free(elt2DTHA);
        if (elt2DthTextA != nullptr)
            freeStarStar(elt2DthTextA, nbElts2DTH);
        if (elt3DTHA != nullptr)
            free(elt3DTHA);
        if (elt3DthTextA != nullptr)
            freeStarStar(elt3DthTextA, nbElts3DTH);
        if (elt1DTHA != nullptr)
            free(elt1DTHA);
        if (elt1DthTextA != nullptr)
            freeStarStar(elt1DthTextA, nbElts1DTH);
    }

    if (flagA[7])
    {
        if (connecSPH != nullptr)
            free(connecSPH);
        if (delEltSPH != nullptr)
            free(delEltSPH);
        if (defPartSPH != nullptr)
            free(defPartSPH);
        if (eFuncSPH != nullptr)
            free(eFuncSPH);
        if (scalTextSPH != nullptr)
            freeStarStar(scalTextSPH, nbEFuncSPH);
        if (tensTextSPH != nullptr)
            freeStarStar(tensTextSPH, nbTensSPH);
        if (tensValSPH != nullptr)
            free(tensValSPH);
        if (pTextSPH != nullptr)
            freeStarStar(pTextSPH, nbPartsSPH);
        if (eMassSPH != nullptr)
            free(eMassSPH);
        if (nodNumSPH != nullptr)
            free(nodNumSPH);
        if (numParentSPH != nullptr)
            free(numParentSPH);
        if (matPartSPH != nullptr)
            free(matPartSPH);
        if (propPartSPH != nullptr)
            free(propPartSPH);
    }
}

// *******************
int main(int argc, char **argv)
{
    if (argc < 2)
    {
        cout << "Call a filename"
             << "\n";
        exit(1);
    }
    readRadiossAnim(argv[1]);

    exit(0);
}
