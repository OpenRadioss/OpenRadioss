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

#include "mv_descriptor.h"
#include "mv_attribute_expression.h"
#include "mv_data_if_feature.h"


typedef MvExpression_t *MvExpressionPtr_t;

static MvDataFeatureList_t::iterator find(MvDataFeatureList_t &dfl,const MvDataFeature_t *df_p);


/* --------- Constructors & destructor --------- */

typedef const MvDataFeature_t *MvDataFeaturePtr_t;

MvDataIfFeature_t::MvDataIfFeature_t(int nb_tests) :
  MvDataFeature_t           (DFT_IF,""),
  myNbTests                 (nb_tests>0  ? nb_tests                             : 0),
  myExpressionPtrArray      (myNbTests>0 ? new MvExpressionPtr_t[myNbTests]     : NULL),
  myFeaturePtrListArray     (myNbTests>0 ? new MvDataFeatureList_t[myNbTests+1] : NULL),
  myFeaturePtrReducedListArray(myNbTests > 0 ? new MvDataFeatureList_t[myNbTests + 1] : NULL),
  myWrongFeaturePtrListArray(myNbTests>0 ? new MvDataFeatureList_t[myNbTests+1] : NULL),
  myWrongFeaturePtrReducedListArray(myNbTests > 0 ? new MvDataFeatureList_t[myNbTests + 1] : NULL),
  myTestFeatures(),
  myPrevFeature(NULL)
{}

MvDataIfFeature_t::~MvDataIfFeature_t() {
    if(myNbTests) 
    {
       for(int i=0;i<myNbTests;++i) 
       {
          myExpressionPtrArray[i]->setDelete(true);
          delete myExpressionPtrArray[i];
       }
       delete [] myExpressionPtrArray;
       delete [] myFeaturePtrListArray;
       delete [] myFeaturePtrReducedListArray;
       delete [] myWrongFeaturePtrListArray;
       delete [] myWrongFeaturePtrReducedListArray;
    }
}


/* --------- Creation --------- */


void MvDataIfFeature_t::setTest(const MvDescriptor_t *,int i,
                const MvDataFeatureSet_t &tests,
                MvExpression_t *expr_p,const MvDataFeatureList_t &ftl, const MvDataFeatureList_t& ftl_reduced)
{
  MvDataFeatureSet_t::const_iterator a_it_begin = tests.begin();
  MvDataFeatureSet_t::const_iterator a_it_end   = tests.end();
  MvDataFeatureSet_t::const_iterator a_it;
  for(a_it=a_it_begin;a_it!=a_it_end;++a_it) myTestFeatures.insert(*a_it);
  myExpressionPtrArray[i]  = expr_p;
  myFeaturePtrListArray[i] = ftl;
  myFeaturePtrReducedListArray[i] = ftl_reduced;
}


void MvDataIfFeature_t::setDefault(const MvDataFeatureList_t &ftl) {
  myFeaturePtrListArray[myNbTests]=ftl;
}

void MvDataIfFeature_t::setDefaultReduced(const MvDataFeatureList_t& ftl) {
  myFeaturePtrReducedListArray[myNbTests] = ftl;
}

void MvDataIfFeature_t::updateWrongFeaturePtrLists() {
  for(int i=0;i<=myNbTests;++i) {
    MvDataFeatureList_t &a_right_fl=myFeaturePtrListArray[i];
    MvDataFeatureList_t &a_wrong_fl=myWrongFeaturePtrListArray[i];
    a_wrong_fl.clear();
    //
    for(int j=0;j<=myNbTests;++j) if(j!=i) {
      const MvDataFeatureList_t &a_fl=getFeaturePtrList(j);
      MvDataFeatureList_t::const_iterator a_it_begin = a_fl.begin();
      MvDataFeatureList_t::const_iterator a_it_end   = a_fl.end();
      MvDataFeatureList_t::const_iterator a_it;
      for(a_it=a_it_begin;a_it!=a_it_end;++a_it) {
	MvDataFeatureList_t::iterator a_w_it=find(a_wrong_fl,*a_it);
	if(a_w_it==a_wrong_fl.end()) a_wrong_fl.push_back(*a_it);
      }
    }
    //
    MvDataFeatureList_t::const_iterator a_r_it_begin = a_right_fl.begin();
    MvDataFeatureList_t::const_iterator a_r_it_end   = a_right_fl.end();
    MvDataFeatureList_t::const_iterator a_r_it;
    for(a_r_it=a_r_it_begin;a_r_it!=a_r_it_end;++a_r_it) a_wrong_fl.remove(*a_r_it);
  }
  // update reduced
  for (int i = 0; i <= myNbTests; ++i) {
      MvDataFeatureList_t& a_right_fl = myFeaturePtrReducedListArray[i];
      MvDataFeatureList_t& a_wrong_fl = myWrongFeaturePtrReducedListArray[i];
      a_wrong_fl.clear();
      //
      for (int j = 0; j <= myNbTests; ++j) if (j != i) {
          const MvDataFeatureList_t& a_fl = getFeaturePtrReducedList(j);
          MvDataFeatureList_t::const_iterator a_it_begin = a_fl.begin();
          MvDataFeatureList_t::const_iterator a_it_end = a_fl.end();
          MvDataFeatureList_t::const_iterator a_it;
          for (a_it = a_it_begin; a_it != a_it_end; ++a_it) {
              MvDataFeatureList_t::iterator a_w_it = find(a_wrong_fl, *a_it);
              if (a_w_it == a_wrong_fl.end()) a_wrong_fl.push_back(*a_it);
          }
      }
      //
      MvDataFeatureList_t::const_iterator a_r_it_begin = a_right_fl.begin();
      MvDataFeatureList_t::const_iterator a_r_it_end = a_right_fl.end();
      MvDataFeatureList_t::const_iterator a_r_it;
      for (a_r_it = a_r_it_begin; a_r_it != a_r_it_end; ++a_r_it) a_wrong_fl.remove(*a_r_it);
  }
  
  /*
  for(i=0;i<=myNbTests;++i) {
    MvDataFeatureList_t::const_iterator a_it_begin,a_it_end,a_it;
    //
    cout << "TRUE[" << i << "]:" << endl;
    const MvDataFeatureList_t &a_right_fl=getFeaturePtrList(i);
    a_it_begin = a_right_fl.begin();
    a_it_end   = a_right_fl.end();
    for(a_it=a_it_begin;a_it!=a_it_end;++a_it) cout << "  \"" << (*a_it)->getTitle() << "\"" << endl;
    //
    cout << "FALSE[" << i << "]:" << endl;
    const MvDataFeatureList_t &a_wrong_fl=getWrongFeaturePtrList(i);
    a_it_begin = a_wrong_fl.begin();
    a_it_end   = a_wrong_fl.end();
    for(a_it=a_it_begin;a_it!=a_it_end;++a_it) cout << "  \"" << (*a_it)->getTitle() << "\"" << endl;
  }
  */
  
}



/* --------- Output in an output stream --------- */

ostream &MvDataIfFeature_t::display(ostream &os,const MvDescriptor_t &descr,int level) const {
  int i,j;
  for(j=0;j<level;j++) os << "  ";
  //
  for(i=0;i<getNbTests();++i) {
    if(i!=0) os << "else ";
    os << "if(";
    myExpressionPtrArray[i]->display(os,descr) << ") {" << endl;
    //
    bool a_is_optional=false;
    MvDataFeatureList_t::const_iterator a_it_begin = getFeaturePtrList(i).begin();
    MvDataFeatureList_t::const_iterator a_it_end   = getFeaturePtrList(i).end();
    MvDataFeatureList_t::const_iterator a_it;
    ++level;
    for(a_it=a_it_begin;a_it!=a_it_end;++a_it) {
      if(a_it==a_it_begin || (*a_it)->isOptional()!=a_is_optional) {
	a_is_optional=(*a_it)->isOptional();
	--level; for(j=0;j<level;j++) os << "  "; ++level;
	if(a_is_optional) os << "optional:"; else os << "mandatory:";
	os << endl;
      }
      (*a_it)->display(os,descr,level);
      os << endl;
    }
    --level;
    for(j=0;j<level;j++) os << "  ";
    os << "} ";
  }
  //
  if(getDefaultFeaturePtrList().size()>0) {
    os << "else {" << endl;

/*    bool a_is_optional;*/
    bool a_is_optional = false;

    MvDataFeatureList_t::const_iterator a_it_begin = getDefaultFeaturePtrList().begin();
    MvDataFeatureList_t::const_iterator a_it_end   = getDefaultFeaturePtrList().end();
    MvDataFeatureList_t::const_iterator a_it;
    ++level;
    for(a_it=a_it_begin;a_it!=a_it_end;++a_it) {
      if(a_it==a_it_begin || (*a_it)->isOptional()!=a_is_optional) {
	a_is_optional=(*a_it)->isOptional();
	--level; for(j=0;j<level;j++) os << "  "; ++level;
	if(a_is_optional) os << "optional:"; else os << "mandatory:";
	os << endl;
      }
      (*a_it)->display(os,descr,level);
      os << endl;
    }
    --level;
    for(j=0;j<level;j++) os << "  ";
    os << "}";
  }
  //
  return os;
}

/* --------- Static functions --------- */

static MvDataFeatureList_t::iterator find(MvDataFeatureList_t &dfl,const MvDataFeature_t *df_p) {
  MvDataFeatureList_t::iterator a_it     = dfl.begin();
  MvDataFeatureList_t::iterator a_it_end = dfl.end();
  while(a_it!=a_it_end && (*a_it)!=df_p) ++a_it;
  return a_it;
}




