/*Copyright>    OpenRadioss
//Copyright>    Copyright (C) 1986-2025 Altair Engineering Inc.
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
//Copyright>    commercial version may interest you: https://www.altair.com/radioss/.*/

#include <UTILS/win32_utils.h>  



#include <UTILS/error.h>

#include <locale>

#include "mv_type.h"
#include "mv_full_type.h"


/* --------- Constructors & Destructor --------- */

MvFullType_t::MvFullType_t(object_type_e type,const MvSubtype_t *subtype_p) : 
  myType(type),
  mySubtypePtr(subtype_p),
  myIdPoolStr("")
{}


MvFullType_t::MvFullType_t(const CFGKernel & cfgkernel, object_type_e type,const string &subtype, const string &idpoolstr) :
  myType(type),
  mySubtypePtr(cfgkernel.get_subtype(type,subtype)),
  myIdPoolStr(idpoolstr)
{
    if (mySubtypePtr == NULL)
    {
        myIdPoolStr = subtype;
    }
}


MvFullType_t::MvFullType_t(const CFGKernel& cfgkernel, const string &type,const string &subtype, const string &idpoolstr) :
  myType(MV_get_type(type)),
  mySubtypePtr(cfgkernel.get_subtype(myType,subtype)),
  myIdPoolStr(idpoolstr)
{
    if (mySubtypePtr == NULL)
    {
        myIdPoolStr = subtype;
    }
}

//static connection_type_e loc_get_connection_type(const string &con_keywords);
typedef map<string, connection_type_e> LocMapStringSubType_t; 
//static connection_type_e loc_get_connection_type(const string &op) {
//  static map<string,connection_type_e> a_op_level_map;
//  static bool            a_first=true;
//  connection_type_e econtype = CONNECTION_UNKNOWN;
//  //
//  if(a_first) {
//    a_first=false;
//    //
//    a_op_level_map["UNKNOWN"]     =CONNECTION_UNKNOWN;
//    a_op_level_map["SPOTWELD"]    =CONNECTION_SPOTWELD;
//    a_op_level_map["BOLT"]        =CONNECTION_BOLT;
//    a_op_level_map["WELDING_LINE"]=CONNECTION_WELDING_LINE;
//    a_op_level_map["GLUE"]        =CONNECTION_GLUE;
//    a_op_level_map["HEMMING"]     =CONNECTION_HEMMING;
//  }
//  //
//  LocMapStringSubType_t::iterator a_it=a_op_level_map.find(op); 
//  if(a_it==a_op_level_map.end()) return econtype;
//  return (*a_it).second;
//}

MvFullType_t::MvFullType_t(const CFGKernel &cfgkernel,const string &fulltype) :
  myType(HCDI_OBJ_TYPE_NULL),
  mySubtypePtr(NULL)
{
  if(fulltype[0]!='/') {
    myType=MV_get_type(fulltype);
  } else {
    string a_str="";
    int i=1,n=(int)(fulltype.size());
    while(i<n && fulltype[i]!='/') a_str+=fulltype[i++];
    myType=MV_get_type(a_str);
    a_str="";
    if(i<n) {
        ++i;
        a_str=fulltype.substr(i,n-i);
/*special treatment used for connection for kow to handle  subtype of subtype*/ 
        /* need_to_be_checked */
        /*
        if(myType==CONNECTION)
        {
            int j = 0;
            string a_sub_str="";
            if(fulltype[i-1]=='/')
            {
                j = i;
                while(j<n && fulltype[j]!='/') j++;
                if(j<n) {
                    ++j;
                    a_sub_str=fulltype.substr(j,n-j);
                    a_str=fulltype.substr(i,j-i-1);
                    connection_type_e econtype = loc_get_connection_type(a_str);
                    mySubtypePtr=MV_get_subtype(myType,a_sub_str,econtype);
                }
                else
                {
                    mySubtypePtr=MV_get_subtype(myType,a_str);
                }
            }
            else
            {
                mySubtypePtr=MV_get_subtype(myType,a_str);
            }
        }
        else
        */
        {

            mySubtypePtr = cfgkernel.get_subtype(myType, a_str);
            if (mySubtypePtr == NULL)
            {
                myIdPoolStr = a_str;
            }

        }
    }
  }
}


/* --------- Operators --------- */

bool MvFullType_t::operator==(const MvFullType_t &fulltype) const {
  return (myType==fulltype.myType && mySubtypePtr==fulltype.mySubtypePtr);
}

bool MvFullType_t::operator!=(const MvFullType_t &fulltype) const {
  return (myType!=fulltype.myType || mySubtypePtr!=fulltype.mySubtypePtr);
}

bool MvFullType_t::operator<(const MvFullType_t &fulltype) const {
    //
    if (myType == fulltype.myType) {
        return (mySubtypePtr != fulltype.mySubtypePtr &&
            fulltype.mySubtypePtr == NULL);
    }
    //
    return false;
}

bool MvFullType_t::operator<=(const MvFullType_t &fulltype) const {
    if (myType == fulltype.myType && mySubtypePtr == fulltype.mySubtypePtr) return true;
    return (*this < fulltype);
}

bool MvFullType_t::operator>(const MvFullType_t &fulltype) const {
    //
    if (myType == fulltype.myType) {
        return (mySubtypePtr != fulltype.mySubtypePtr &&
            mySubtypePtr == NULL);
    }
    //
    return false;
}

bool MvFullType_t::operator>=(const MvFullType_t &fulltype) const {
  if(myType==fulltype.myType && mySubtypePtr==fulltype.mySubtypePtr) return true;
  return (*this>fulltype);
}


/* --------- Getting strings --------- */

const string &MvFullType_t::getTypeStr() const {
  return MV_get_type(myType);
}

const string &MvFullType_t::getSubtypeStr() const {
  static const string a_unknown="UNKNOWN";
  if(mySubtypePtr==NULL) return a_unknown; else return mySubtypePtr->getKeyword();
}

MvFullType_t::operator string() const {
  string a_str="/";
  a_str+=getTypeStr();
  if(mySubtypePtr!=NULL) {
    a_str+="/";
    a_str+=getSubtypeStr();
  }
  return a_str;
}


/* --------- Comparison class --------- */

bool MvFullTypeLess_t::operator()(const MvFullType_t &ft0,const MvFullType_t &ft1) const {
  return (ft0.getType()==ft1.getType()) ? 
    ((ft0.getSubtypePtr()==ft1.getSubtypePtr()) ? (ft0.getIdPoolStr()<ft1.getIdPoolStr()): (ft0.getSubtypePtr()<ft1.getSubtypePtr()) ):
    (ft0.getType()<ft1.getType()) ;
}


/* --------- Set of full types --------- */


/* Did some changes here.Diff it and check it. need_to_be_checked */
bool MvFullTypeSet_t::operator<=(const MvFullTypeSet_t &fts1) const {
  const MvFullTypeSet_t &fts0   = (*this);
  //
  const MvFullTypeSet_t& a_fts1 = fts1;

  bool a_result=true;
  MvFullTypeSet_t::const_iterator a_it     = fts0.begin();
  MvFullTypeSet_t::const_iterator a_it_end = fts0.end();
  //
  while(a_result && a_it!=a_it_end) {
    const MvFullType_t &full_type=(*a_it);
    a_result=(full_type<=a_fts1);
    ++a_it;
  }
  //
  return a_result;
}



bool MvFullTypeSet_t::operator<(const MvFullTypeSet_t &fts1) const {
  const MvFullTypeSet_t &fts0=(*this);
  return (fts0<=fts1 && !(fts1<=fts0));
}



bool MvFullTypeSet_t::operator==(const MvFullTypeSet_t &fts1) const {
  const MvFullTypeSet_t &fts0=(*this);
  return (fts0<=fts1 && fts1<=fts0);
}



/* --------- Non member fonctions --------- */

bool operator<=(const MvFullType_t &full_type,const MvFullTypeSet_t &full_type_set) {
  bool a_result=false;
  MvFullTypeSet_t::const_iterator a_it=full_type_set.begin();
  MvFullTypeSet_t::const_iterator a_it_end=full_type_set.end();
  //
  while(!a_result && a_it!=a_it_end) {
    a_result=(full_type<=(*a_it));
    ++a_it;
  }
  //
  return a_result;
}


/* --------- Static functions --------- */


bool MvFullTypeOrderLess_t::operator()(const MvFullType_t &ft0,const MvFullType_t &ft1) const {
  if(ft0.getType()==ft1.getType())
  {
     const MvSubtype_t *s1 = ft0.getSubtypePtr();
     const MvSubtype_t *s2 = ft1.getSubtypePtr();
     if(s1 != NULL && s2 != NULL)
     {
         /*for inserting in alphabetic order*/
        const string &myKeyword1 = s1->getKeyword();
        const string &myKeyword2 = s2->getKeyword();

        int size1 = (int)myKeyword1.size();
        int size2 = (int)myKeyword2.size();
        std::locale loc;
        int is_digit1 = 0, is_digit2 = 0, i=0, j=0;
        for(i=0; i<size1; i++)
        {
            is_digit1 = isdigit(myKeyword1[i], loc);
            if(is_digit1)
                break;
        }

        for(j=0; j<size2; j++)
        {
            is_digit2 = isdigit(myKeyword2[j], loc);
            if(is_digit2)
                break;
        }

        // both are alpha numeric
        if(is_digit1 && is_digit2)
        {
            string test_str1 = "", test_str2 = "";
            for(;i<size1; i++)
            {
                is_digit1 = isdigit(myKeyword1[i], loc);
                if(is_digit1)
                    test_str1+=myKeyword1[i];
            }

            for(;j<size2; j++)
            {
                is_digit2 = isdigit(myKeyword2[j], loc);
                if(is_digit2)
                    test_str2+=myKeyword2[j];
            }

            char *chk = NULL;
            long int chk_val1 = strtol(test_str1.c_str(), &chk, 10);
            long int chk_val2 = strtol(test_str2.c_str(), &chk, 10);
            return (chk_val1 < chk_val2);
        }
        else
            return (myKeyword1 < myKeyword2);
     }
     else
     {
         return (s1<s2);
     }
  }
  else
  {
      return (ft0.getType()<ft1.getType());
  }
  return false;
}
