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
#ifndef MV_DESCRIPTOR_PARSER_H
#define MV_DESCRIPTOR_PARSER_H


#include <UTILS/mv_stl_various.h>
#include <PARSER/mv_parser_base.h> 


#include "mv_utils.h"
#include "mv_full_type.h"
#include "mv_dimension.h"
#include "mv_orientation.h"
#include "mv_data_size_feature.h"
#include "mv_function_types.h"
#include "mv_descriptor.h"
#include "mv_data_point_feature.h"
#include "mv_filter.h"

#define PseudoCardList_t void

typedef map<int,MvDataFeature_t *> MvIKeywordFeatureMap_t;


class MvDescriptorParser_t : public MvParserBase_t {

public:    // Constructor and destructor
  MvDescriptorParser_t(void* cfgkernel, const string &fullname,object_type_e obj_type,bool isUser=false);

public:    // Parsing the file and getting the descriptor
  MvDescriptor_t *getDescriptorPtr();

private:   // Attributes
  void   readAttributes(MvDescriptor_t *descr_p);
  int    readAttribute(MvDomain_e domain,MvDescriptor_t *descr_p,const string &skeyword);
  string readDependence(MvDomain_e domain,MvDescriptor_t *descr_p,const string &skeyword);
  void   readAttributeValue(MvDomain_e domain,MvDescriptor_t *descr_p,const string &skeyword);
  void   readAttributeSize(MvDomain_e domain,MvDescriptor_t *descr_p,const string &skeyword);
  void   readAttributeArray(MvDomain_e domain,MvDescriptor_t *descr_p,const string &skeyword);

private:   // Defaults
  void readDefaults(MvDescriptor_t *descr_p);
  void readSKeywordsIdentifier(MvDescriptor_t *descr_p);
private:   // Check
  void readTests(MvDescriptor_t *descr_p);

private:   // GUI (data features)
  void readFeatures(MvDescriptor_t *descr_p);
  void readFeaturesAttributes(bool do_step, string& feature_str, map<string, string>& value_map);
  void setFeaturesAttributes(MvDataFeature_t *feature_p, map<string, string>& value_map);
  MvDataFeature_t *readScalarFeature(const MvDescriptor_t *descr_p,bool do_check_ikw=true);
  MvDataFeature_t *readSizeFeature(const MvDescriptor_t *descr_p,bool do_check_ikw=true);
  MvDataFeature_t *readSizeRadioFeature(const MvDescriptor_t *descr_p, MvDomain_e domain, bool do_check_ikw=true);
  MvDataFeature_t *readFileFeature(const MvDescriptor_t *descr_p,bool do_check_ikw=true);
  MvDataFeature_t *readDirFeature(const MvDescriptor_t *descr_p,bool do_check_ikw=true);
  MvDataFeature_t *readTripleFeature(const MvDescriptor_t *descr_p,bool is_point=false);
  
  MvDataFeature_t *readRadioFeature(const MvDescriptor_t *descr_p,MvDomain_e domain,bool do_check_ikw=true);
  
  MvDataFeature_t *readRadioArrayFeature(const MvDescriptor_t *descr_p,bool do_check_ikw=true);
  MvDataFeature_t *readFlagFeature(const MvDescriptor_t *descr_p,bool do_check_ikw=true);
  MvDataFeature_t *readFlagListFeature(const MvDescriptor_t *descr_p,bool do_check_ikw=true);
  
  MvDataFeature_t *readArrayFeature(const MvDescriptor_t *descr_p,MvDomain_e domain);
  MvDataFeature_t *readArrayToSingleFeature(const MvDescriptor_t *descr_p,MvDomain_e domain);                     
  
  
  MvDataFeature_t *readMatrixFeature(const MvDescriptor_t *descr_p);
  
  MvDataFeature_t *readDataFeature(const MvDescriptor_t *descr_p, MvDataFeatureType_e dft_type, bool do_check_ikw=true, bool inside_if=false);
  MvDataFeature_t *readToolFeature(const MvDescriptor_t *descr_p,bool do_check_ikw=true);
  
  MvDataFeature_t *readFunctionFeature(const MvDescriptor_t *descr_p,MvFunctionType_e func_type,bool do_check_ikw=true);
  
  MvDataFeature_t *readSupportFeature(const MvDescriptor_t *descr_p);
  MvDataFeature_t *readAppendFeature(const MvDescriptor_t *descr_p);
  
  MvDataFeature_t *readIfFeature(const MvDescriptor_t *descr_p,MvDomain_e domain);
  
  MvDataFeature_t *readSeparatorFeature();
  MvDataFeature_t *readUnitFeature();
  MvDataFeature_t *readAssignFeature(const MvDescriptor_t *descr_p, MvDataFeatureType_e dft_type, bool do_check_ikw=true);
  void fillFilterDetails(string& feature_keyword, StringVect_t& attrib_vect, StringVect_t& value_vect, StringVect_t& criteria_vect, StringVect_t& unit_vect,
      StringVect_t& message_vect, int* nb_filters);
private:   // Format (I/O)
  void       readFormat(MvDescriptor_t *descr_p);
  string     readNextCard(const MvDescriptor_t *descr_p,const string &card_type,PseudoCardList_t *card_list_p); 
  ff_cell_t *readCell(const MvDescriptor_t *descr_p,const string &format);                                      
  void       readCard(const MvDescriptor_t *descr_p,PseudoCardList_t *card_list_p,bool is_free, const string &card_type); 
  void       readBlankCard(const MvDescriptor_t *descr_p,PseudoCardList_t *card_list_p);
  void       readCommentCard(const MvDescriptor_t *descr_p,PseudoCardList_t *card_list_p);                      
  void       readList(const MvDescriptor_t *descr_p,PseudoCardList_t *card_list_p);                             
  void       readCellList(const MvDescriptor_t *descr_p,PseudoCardList_t *card_list_p,bool is_free);            
  void       readObjectList(const MvDescriptor_t *descr_p,PseudoCardList_t *card_list_p,bool is_free);          
  void       readCardList(const MvDescriptor_t *descr_p,PseudoCardList_t *card_list_p, bool is_free);
  void       readSubobjectsCard(const MvDescriptor_t *descr_p,PseudoCardList_t *card_list_p);                   
  string     readIfCard(const MvDescriptor_t *descr_p,PseudoCardList_t *card_list_p);                           
  ff_cell_t *readIfCell(const MvDescriptor_t *descr_p, const string &format);
  void loc_multi_array_not_acceptable(const MvDescriptor_t *descr_p,int a_cell_ikw);
  void loc_multi_array_not_acceptable(const MvDescriptor_t *descr_p,const ff_cell_t *a_cell_p);
  void       readCardAssign(const MvDescriptor_t *descr_p, PseudoCardList_t *card_list_p) ;
  void       readCardAssignData(const MvDescriptor_t* descr_p, ff_card_t** card_p, int ikeyword, string& formula, value_type_e& assigned_valtype, string& str_io);
private:   // Drawables
  void readDrawables(MvDescriptor_t *descr_p);
  void readScalarDrawable(MvDomain_e domain,const string &name,MvDrawableAccess_e access,
              MvDescriptor_t *descr_p);
  void readSubDrawableDrawable(MvDomain_e domain,const string &name,MvDrawableAccess_e access,
                   MvDescriptor_t *descr_p) ;
  void readWhileZeroDrawable(MvDomain_e domain,const string &name,MvDrawableAccess_e access,
                   MvDescriptor_t *descr_p) ;
  void readShellThicknessDrawable(MvDomain_e domain,const string &name,MvDrawableAccess_e access,
                   MvDescriptor_t *descr_p) ;
  void readEvalDrawable(MvDomain_e domain,const string &name,MvDrawableAccess_e access,
            MvDescriptor_t *descr_p);
  void readTimeStepDrawable(MvDomain_e domain,const string &name,MvDrawableAccess_e access,
                MvDescriptor_t *descr_p);
  void readOptDrawable(MvDrawableType_e drawable_type,
               MvDomain_e domain,const string &name,MvDrawableAccess_e access,
               MvDescriptor_t *descr_p);
  
  void readVolumeDrawable(MvDomain_e domain,const string &name,MvDrawableAccess_e access,
              MvDescriptor_t *descr_p);
  
  
  void readAreaDrawable(MvDomain_e domain,const string &name,MvDrawableAccess_e access,
            MvDescriptor_t *descr_p);
  

  
 private:   // Parameters
  void readParameters(MvDescriptor_t *descr_p);
  
  string readConditionalParameters(MvDomain_e        domain,
                   MvIParamAccess_e  iaccess,
                   MvOParamAccess_e  oaccess,
                   MvDescriptor_t   *descr_p);
  void readParameter(MvDomain_e         domain,
             const string      &param_name,
             MvIParamAccess_e   access,
             MvOParamAccess_e   oaccess,
             MvDescriptor_t    *descr_p,
             MvIParamDescr_t  **iparam_descr_pp=NULL,
             MvOParamDescr_t  **oparam_descr_pp=NULL);
  MvIParamDescr_t *readScalarIParam(MvDomain_e domain,const string &name,MvIParamAccess_e access,MvDescriptor_t *descr_p);
  MvIParamDescr_t *readSubparamIParam(MvDomain_e domain,const string &name,MvIParamAccess_e access,MvDescriptor_t *descr_p);
  
  MvIParamDescr_t *readTransformationIParam(MvIParamType_e ipt,MvDomain_e domain,const string &name,MvIParamAccess_e access,MvDescriptor_t *descr_p);
  
  
  

  
 private: // Definitions
  void readDefinitions(MvDescriptor_t *descr_p);
    

private:   // Parsing
  int                getNextIKeyword(const MvDescriptor_t *descr_p,string *skeyword_p=NULL);
  MvDimension_e      getNextDimension(vector<string> *argVect=NULL);
  MvDataTripleFeatureType_e getNextDifferentiatorType();
  MvOrientation_e    getNextOrientation(int *nbr_p,int *nbc_p);
  object_type_e      getNextObjectType();
  
  MvObjectTypeSet_t *getNextObjectTypes(MvObjectTypeSet_t *otypes_p=NULL);
  
  MvFullType_t       getNextObjectFullType();
  MvFullTypeSet_t   *getNextObjectFullTypes(MvFullTypeSet_t *fts_p=NULL); 
  MvDomain_e         getNextDomain();
  MvExpression_t    *getNextExpressionPtr(const MvDescriptor_t *descr_p,bool do_delete=true,MvDataFeatureSet_t *features_p=NULL);
  MvExpression_t    *getNextSingleExpressionPtr(const MvDescriptor_t *descr_p,bool do_delete=true,const MvDataFeature_t **ft_pp=NULL); 

private:   // String keyword -> integer keyword
  int getIKeyword(const string &skeyword);

private:   // Integer keyword -> feature
  bool                   isUsed(int ikeyword) const;
  void                   addFeature(int ikeyword,MvDataFeature_t *dft_p);
  const MvDataFeature_t *getFeaturePtr(int ikeyword) const;
  void                   clearFeatureMap();
  void getSubtypesListForMultiObject(vector<string> &vect_subtype);
  void readPossibleSubtypeList(MvDescriptor_t *descr_p, int a_ikeyword);
private:  // Size map
  MvDataSizeFeature_t *getSizeFeaturePtr(int ikeyword) const;
  void                 addSizeFeature(int ikeyword,MvDataSizeFeature_t *dsf_p);

private:   // Optional data features (state flag)
  inline bool isOptional() const { return myIsOptional; }
  inline void setOptional(bool is_optional) { myIsOptional=is_optional; }

private:   // Post-treatements
  void postRead(const MvDescriptor_t *descr_p) const; 

  
protected: // Messages
  inline const char *getMsg(int i) const { return MV_get_msg_array(MSGT_KERNEL)[i]; }
  

private:   // Types
  typedef map<MvDomain_e,MvIKeywordSet_t> MvLockedIKeywordMap_t;

private:   // Datas
  bool                   myIsUser;
  int                    myCurrentIKeyword;
  object_type_e          myObjectType;
  MvIKeywordFeatureMap_t myIKeywordFeatureMap;
  MvIKeywordFeatureMap_t mySizeMap;
  bool                   myIsOptional;
  MvLockedIKeywordMap_t  myLockedIKeywords;    
  void*                  mycfgkernel = nullptr;
};


#endif //MV_DESCRIPTOR_PARSER_H




