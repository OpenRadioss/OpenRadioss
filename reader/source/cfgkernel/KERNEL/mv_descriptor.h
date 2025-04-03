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
#ifndef MV_DESCRIPTOR_H
#define MV_DESCRIPTOR_H


#include <UTILS/mv_string.h>
#include <UTILS/mv_iostream.h>
#include <UTILS/mv_stl_various.h>
#include <KERNEL_BASE/descriptor_API.h> 


#include "mv_subtype.h"
#include "mv_domain.h"
#include "mv_ikeyword_containers.h"
#include "mv_data_feature.h"
#include "mv_data_support_feature.h"
#include "mv_file_format.h"
#include "mv_test.h"
#include "mv_dependence.h"
#include "mv_drawable.h"
#include "mv_iparam_descr.h"
#include "mv_oparam_descr.h"           
#include "mv_cond_param_descriptors.h" 
#include "mv_data_selection_feature.h"
#include "mv_pre_descriptor.h"

#include "mv_type.h"
#include "HCDI/hcdi_mv_descriptor.h"
// IKeywords
#define MvPseudoIKeywordListMap_t        void
// Defaults
#define MvPseudoDefaults_t               void 

#define MvPseudoIdentifierMap_t          void
// Data features
#define MvPseudoDataFeatureListMap_t     void
// Tests
#define MvPseudoTestListMap_t            void
// Conditional ikeywords (dependences)
#define MvPseudoDependenceListMap_t      void
// Conditional parameter descriptors
#define MvPseudoCondIParamDescrListMap_t void 
#define MvPseudoCondOParamDescrListMap_t void 
// Drawables
#define MvPseudoDrawableMap_t            void
#define MvPseudoDrawableNameMap_t        void
// Input parameters
#define MvPseudoIParamDescrMap_t         void
#define MvPseudoIParamDescrNameMap_t     void
// Output parameters
#define MvPseudoOParamDescrMap_t         void 
#define MvPseudoOParamDescrNameMap_t     void 
// Definitions
#define MvPseudoDefinitionMap_t          void 
//Selection features
#define MvPseudoSelectionListMap_t       void
// Keyword mapping
#define MvPseudoKeywordKeywordMap_t            void


typedef vector<dimension_size_t> MvSizeVector;  

// default attributes enum, these attributes need not defined in ATTRIBUTE block of cfg file
typedef enum cfg_reserve_ikeyword_s
{
    /*_IOMODE_ ikeyword*/
    CFG_IKEYWORD_IO_MODE = 1,
    /**/
    CFG_IKEYWORD_LAST
} cfg_reserve_ikeyword_e;

/** @name MvMultiIndex_t*/
//@{
class MvMultiIndex_t :public vector<int>
{
 public:
   /// Constructor
   //ARRAY[X]
   MvMultiIndex_t(int x) {this->push_back(x);};                        
   //ARRAY[X][Y]
   MvMultiIndex_t(int x,int y) {this->push_back(x);this->push_back(y);};      
   //ARRAY[X][Y][Z]
   MvMultiIndex_t(int x,int y,int z) {this->push_back(x);this->push_back(y);this->push_back(z);};     
   //we use defaut copy constructor and operator=().
   /// Destructor
   ~MvMultiIndex_t(){};
};
//@}


/** @name Descriptors*/
//@{


class MvDescriptor_t : public IDescriptor {
/// Descriptor class
  friend ostream &operator<<(ostream &os,const MvDescriptor_t &descr);

public: /** @name Constructors and destructor*/
  //@{
  /// Constructor
  MvDescriptor_t(int user_id=-1);
  /// Destructor
  ~MvDescriptor_t();
  //@}

  /**@name MCDS description*/
  //@{

public: /**@name Creation*/
  //@{
  /// Adds a value attribute
  void addValue(MvDomain_e domain,value_type_e vtype,int ikw,const string &skw,const string &comment);
  /// Adds an object attribute
  void addObject(MvDomain_e domain,object_type_e otype,int ikw,const string &skw,const string &comment);
  /// Adds a size attribute
  void addSize(MvDomain_e domain,int ikw,const string &skw,const string &comment);
  /// Adds a value array attribute
  void addValueArray(MvDomain_e domain,value_type_e vtype,int ikw,const string &skw,const string &comment,
		     attribute_type_e array_type,
                     const MvSizeVector & sizeArrayVector);
  /// Adds an object array attribute
  void addObjectArray(MvDomain_e domain,object_type_e otype,int ikw,const string &skw,const string &comment,
		      attribute_type_e array_type,
                      const MvSizeVector & sizeArrayVector);
  //@}

public: /**@name Acces to descriptor*/
  //@{
  /// Returns true if the descriptor is USER
  bool isUser() const;
  /// Gets the User ID of the descriptor
  int getUserId() const;
  /// Sets the User ID of the descriptor
  void setUserId(int id);
  /// Gets the MCDS descriptor
  const descriptor_t *getDescriptorPtr() const { return myDescriptorPtr; }
  //@}

public: /* Keywords */
  // Get the first integer keyword
  int    getFirstIKeyword()                  const;
  // Get the next integer keyword (return END_ARGS if last reached)
  int    getNextIKeyword(int ikeyword)       const;
  // Adds an integer keyword into a domain
  void   addDomainIKeyword(MvDomain_e domain,int ikeyword);

public: /**@name Keywords*/
  //@{
  /// Gets the integer keyword from the string keyword
  int    getIKeyword(const string &skeyword) const;
  /// Gets the string keyword from the integer keyword
  string getSKeyword(int ikeyword)           const;
  /// Gets the const char* keyword from the integer keyword
  int  getSKeyword(int ikeyword, const char **skeyword) const;
  /// Gets the string keyword from the solver name (deprecated)
  virtual const string & getSKeywordFromSolverName(const string &solvername) const;
  /// Gets the skeywords with their conditions from the solver name
  virtual const MvCondKeywordList_t& getSKeywordsFromSolverName(const string &solvername) const;
  /// Gets integer keywords from string keywords
  MvIKeywordSet_t *getIKeywords(const MvSKeywordSet_t &skw_set,MvIKeywordSet_t *ikw_set_p=NULL) const;
  /// Gets string keywords from integer keywords
  MvSKeywordSet_t *getSKeywords(const MvIKeywordSet_t &ikw_set,MvSKeywordSet_t *skw_set_p=NULL) const;
  /// Gets support's integer keywords
  MvIKeywordSet_t *getSupportIKeywords(int domain,MvIKeywordSet_t *ikw_set_p=NULL) const;
  /// Gets domain's integer keywords
  const MvIKeywordList_t &getDomainIKeywords(MvDomain_e domain) const;
  /// Gets integer keywords of given domains
  MvIKeywordList_t *getIKeywords(int domains,MvIKeywordList_t *ikw_list_p=NULL) const;
  /// Gets integer keywords of given domains and a given value type
  MvIKeywordList_t *getIKeywords(int domains,value_type_e vtype,MvIKeywordList_t *ikw_list_p=NULL) const;
  /// Gets integer keywords of given domains and a given attribute type
  MvIKeywordList_t *getIKeywords(int domains,attribute_type_e atype,MvIKeywordList_t *ikw_list_p=NULL) const;
  /// Gets integer keywords of given domains and given attribute and value type
  MvIKeywordList_t *getIKeywords(int domains,attribute_type_e atype,value_type_e vtype,
				 MvIKeywordList_t *ikw_list_p=NULL) const;

  int getAllIKeywords(MvDomain_e domain, MvAtypeVtypeList_t  &atype_vtypeikeywordlst) const;
  int getAllIKeywords(MvDomain_e domain, map< int, vector<MvIKeywordList_t> >  &map_type_keywordlst) const;
  MvIKeywordList_t *getAllIKeywordsHavingDefaults(int domains, MvIKeywordList_t* ikw_list_p = NULL) const;
  void getIdentifiers(MvIKeywordList_t *pikeyidentifierlst) const;
  int getMaxIKeyword() const;

  //void getMultiObjectTypes(int ikeyword, MvFullTypeSet_t &set) const;
  //@}

public: /**@name Iterators*/
  //@{
  /// Gets the begin iterator
  inline iterator begin() const { return iterator(this,getFirstIKeyword()); }
  /// Gets the begin iterator
  inline iterator end()   const { return iterator(this,END_ARGS); }
  //@}

public: /**@name Common data*/
  //@{
  /// Gets the attribute type (ATYPE_VALUE, ATYPE_SIZE, ATYPE_STATIC_ARRAY, ATYPE_DYNAMIC_ARRAY)
  attribute_type_e getAttributeType(int ikeyword) const;
  /// Gets the value type (VTYPE_BOOL VTYPE_UINT VTYPE_INT, VTYPE_FLOAT, VTYPE_STRING, VTYPE_OBJECT)
  value_type_e     getValueType(int ikeyword)     const;
  /// Gets the comment (or title)
  string           getComment(int ikeyword)       const;
  /// Gets the solver name
  string           getSolverName(int ikeyword)    const;
  // Gets the attribute length in the format block
  int   getAttributeLength(int ikeyword)       const;
  //@}

public: /**@name Object data*/
  //@{
  /// Gets the type of object (for an object value or an object array)
  object_type_e getObjectType(int ikeyword) const;
  bool isMultiType(int ikeyword) const;
  bool hasObjectAttribSubtype(int ikeyword, int *nb_subtype) const;
  //@}

public: /**@name Array data*/
  //@{
  /// Gets the size of a static array
  int    getSize(int ikeyword)         const;
  /// Gets the integer keyword giving the size of a dynamic array
  int    getSizeIKeyword(int ikeyword) const;
  /// Gets the string keyword giving the size of a dynamic array
  string getSizeSKeyword(int ikeyword) const;
  /// Gets the ikeywords of the dynamic arrays of a "size" attribute
  MvIKeywordSet_t *getSizeConnectedIKeywords(int size_ikw,MvIKeywordSet_t *ikw_set_p=NULL) const;
  
  /// Returns true if the array is a main array
  bool isMainArray(int ikeyword) const;
  
  
  MvIKeywordSet_t* getMainArrayIKeywords(MvIKeywordSet_t* ikw_set_p = NULL) const;
  MvIKeywordSet_t* getSecondaryArrayIKeywords(int ikeyword, 
					  MvIKeywordSet_t* ikw_set_p = NULL) const;
  
  
  bool isSecondaryArray(int ikeyword) const;
  
  
  MvIKeywordSet_t* getAlternateSupportIKeywords(MvIKeywordSet_t* ikw_set_p = NULL) const;
  
  
  void getDimensionSize(int ikeyword,MvSizeVector & sizeArrayVector) const ;
  bool isMultiDimensionalArray(int ikeyword) const;
  /* Update feature attributes of a feature*/
  void UpdateFeatureAttributes(MvDataFeature_t* feature_p, feature_attribute_type_e attrib, int val, set<value_type_e>* vset = NULL) const;
  
  //@}

  //@}

public: /** @name Dependences */
  //@{
  /// Adds a dependence
  void addDependence(MvDomain_e domain,const MvDependence_t *depend_p);
  /// Gets the dependences for given domains
  MvDependenceList_t *getDependences(int domains,MvDependenceList_t *dl_p=NULL) const;
  //@}

public: /** @name Default values*/
  //@{
  /// Returns true if the attribute admits a default value in one of the given domains
  bool hasDefault(int ikeyword,int domains) const;
  /// Returns true if the attribute admits a default value in one of the given domains
  inline bool hasDefault(const string &skeyword,int domains) const {
    return hasDefault(getIKeyword(skeyword),domains); 
  }
  /// Gets the default value of an integer attribute in one of the given domains
  bool getBoolDefaultValue(int ikeyword,int domains,bool *has_default_p=NULL) const;
  int getBoolDefaultValue(const string &skeyword,int domains,bool *has_default_p=NULL) const { 
    return getBoolDefaultValue(getIKeyword(skeyword),domains,has_default_p); 
  }

  int getIntDefaultValue(int ikeyword,int domains,bool *has_default_p=NULL) const;
  unsigned int getUIntDefaultValue(int ikeyword,int domains,bool *has_default_p=NULL) const;
  unsigned int getUIntDefaultValue(const string &skeyword, int domains, bool *has_default_p = NULL) const {
      return getUIntDefaultValue(getIKeyword(skeyword), domains, has_default_p);
  }
  void getIdentifierValue(int domains, MvIntIdentifierMap_t  &identifierlst) const;
  int getIdentifierValue(int domains, int ikeyword) const;
  int getIdentifierValue(int domains, const string &skeyword) const;
  int getIKeywordFromIdentifierValue(int domains, int identifier) const; 
  /// Gets the default value of an integer attribute in one of the given domains
  int getIntDefaultValue(const string &skeyword,int domains,bool *has_default_p=NULL) const { 
    return getIntDefaultValue(getIKeyword(skeyword),domains,has_default_p); 
  }
  /// Gets the default value of a float (double) attribute in one of the given domains
  double getFloatDefaultValue(int ikeyword,int domains,bool *has_default_p=NULL) const;
  /// Gets the default value of a float (double) attribute in one of the given domains
  inline double getFloatDefaultValue(const string &skeyword,int domains,bool *has_default_p=NULL) const { 
    return getFloatDefaultValue(getIKeyword(skeyword),domains,has_default_p); 
  }
  /// Gets the default value of a string attribute in one of the given domains
  string getStringDefaultValue(int ikeyword,int domains,bool *has_default_p=NULL) const;
  /// Gets the default value of a string attribute in one of the given domains
  inline string getStringDefaultValue(const string &skeyword,int domains,bool *has_default_p=NULL) const { 
    return getStringDefaultValue(getIKeyword(skeyword),domains,has_default_p); 
  }
  /// Sets the default value of an integer attribute for the given domain
  void setBoolDefaultValue(int ikeyword,MvDomain_e domain,bool value);
  void setIntDefaultValue(int ikeyword,MvDomain_e domain,int value);
  inline void setIntDefaultValue(const string &skeyword,MvDomain_e domain,int value) { 
    setIntDefaultValue(getIKeyword(skeyword),domain,value); 
  }
  void setUIntDefaultValue(int ikeyword,MvDomain_e domain,unsigned int value);

  /// Sets the default value of an integer attribute for the given domain

  /// Sets the default value of an integer attribute for the given domain
  inline void setUIntDefaultValue(const string &skeyword,MvDomain_e domain,unsigned int value) { 
    setUIntDefaultValue(getIKeyword(skeyword),domain,value); 
  }
  void setIntIdentifierValue(int ikeyword,MvDomain_e domain,int value);

  /// Sets the default value of a float (double) attribute for the given domain
  void setFloatDefaultValue(int ikeyword,MvDomain_e domain,double value);
  /// Sets the default value of a float (double) attribute for the given domain
  inline void setFloatDefaultValue(const string &skeyword,MvDomain_e domain,double value) { 
    setFloatDefaultValue(getIKeyword(skeyword),domain,value); 
  }
  /// Sets the default value of a string attribute for the given domain
  void setStringDefaultValue(int ikeyword,MvDomain_e domain,const string &value);
  /// Sets the default value of a string attribute for the given domain
  inline void setStringDefaultValue(const string &skeyword,MvDomain_e domain,const string &value) { 
    setStringDefaultValue(getIKeyword(skeyword),domain,value); 
  }
  /// Sets the default value of an object attribute for the given domain.
  /// This is only "dummy", only NONE can be defined in cfg file, so no value is passed here.
  virtual void setObjectDefaultValue(int ikeyword,MvDomain_e domain);
  //@}


public: // Domains
  int getNbDomains() const;

public: /**@name Data features */
  //@{
  /// Adds a data feature for a given domain
  void setDataFeature(MvDomain_e domain,const MvDataFeature_t *df_p, bool skip_reduced=false);
  
  /// Gets the domain of a data feature
  MvDomain_e getDomain(const MvDataFeature_t *df_p) const;
  
  /** Gets a data feature list for given domains.<br>
      <br>
      @param domain Logical union (OR) of domains (ex: DOM_COMMON | DOM_FLUID)
      @param dfl_p  Pointer on the list of features (if NULL, is allocated) (input/output) 
      <br>
      @see MvDomain_e
   */
  MvDataFeatureList_t *getDataFeatures(int domains,MvDataFeatureList_t *dfl_p=NULL, bool get_reduced=false) const;
  /// Gets a data feature list of a given type for given domains
  MvDataFeatureList_t *getDataFeatures(int domains,MvDataFeatureType_e a_dft,
				       MvDataFeatureList_t *dfl_p=NULL, bool add_containing_array_feat=true) const;
  /// Gets features implicated in "if" features
  MvDataFeatureSet_t *getConditionalDataFeatures(int domains,MvDataFeatureSet_t *dfs_p=NULL) const;
  /// Gets features which are tests of "if" features
  MvDataFeatureSet_t *getConditionDataFeatures(int domains,MvDataFeatureSet_t *dfs_p=NULL) const;
  
  
  MvDataFeatureSet_t *getConditionalDimensionFeatures(MvDataFeatureType_e feature_type,
						      bool                array_subfeatures,
						      MvDataFeatureSet_t *dfs_p=NULL) const;
  
  /// Gets features with conditional dimensions
  MvDataFeatureSet_t *getConditionalDimensionFeatures(MvDataFeatureSet_t *dfs_p=NULL) const;
  /// Gets features which are tests of features with conditional dimensions
  MvDataFeatureSet_t *getConditionDimensionFeatures(MvDataFeatureSet_t *dfs_p=NULL) const;
  
  MvDataFeatureSet_t *getConditionalDimensionFeatures(const MvDataFeature_t *cond_p,
						      MvDataFeatureType_e    feature_type,
						      bool                   array_subfeatures,
						      MvDataFeatureSet_t    *dfs_p=NULL) const;
  
  /// Gets features with conditional dimensions implicated in the given test feature
  MvDataFeatureSet_t *getConditionalDimensionFeatures(const MvDataFeature_t *cond_p,MvDataFeatureSet_t *dfs_p=NULL) const;
  /// Gets If features in which given feature is associated in the tests
  MvDataFeatureSet_t *getIfFeatures(int domains, const MvDataFeature_t *test_p,MvDataFeatureSet_t *dfs_p=NULL) const;
  
  /// Gets the feature from ikeyword, if possible. Not optimized and using linear search.
  const MvDataFeature_t* getIkeywordDataFeature(int domains, int ikeyword) const;
  /// Gets the MvSelectionFeature for given name such as SECONDARY or MAIN if does not find any, return NULL 
  const MvDataSelectionFeature_t* getSelectionFeature(const char *rootname) const;
  //@}
private:
  /// Adds an MvSelectionFeature corresponding to the given ikeyword
  MvDataSelectionFeature_t* addSelectionFeature(MvDomain_e domain,int ikeyword) const;
  /// Adds (generates) all MvSelectionFeatures
  void addSelectionFeatures(MvDomain_e domain) const;

public: /**@name Tests */
  //@{
  /// Add a data feature for a given domain
  void addTest(MvDomain_e domain,const MvTest_t *test_p);
  /** Gets a test list for given domains.<br>
      <br>
      @param domain Logical union (OR) of domains (ex: DOM_COMMON | DOM_FLUID)
      @param test_p  Pointer on the list of tests (if NULL, is allocated) (input/output) 
      <br>
      @see MvDomain_e
   */
  MvTestList_t *getTests(int domain,MvTestList_t *tl_p=NULL) const;
  //@}

public: /**@name File formats */
  //@{
  void addFileFormat(MvFileFormat_e ff_id,fileformat_t *ff_p);
  /// Gets a MCDS file format
  const fileformat_t *getFileFormatPtr(MvFileFormat_e ff_id) const;
  /// Gets a MCDS lower version file format
  const fileformat_t *getLowerFileFormatPtr(MvFileFormat_e ff_id) const; 
  /// Get Dyna latest available file format (input parameter (MvFileFormat_e) might change)
  const fileformat_t *getDynaFileFormatPtr(MvFileFormat_e *ff_id_p) const;
  /// Gets a LSDYNA MCDS file format
  const fileformat_t *getDynaFileFormatPtr(MvFileFormat_e ff_id) const;
  /// Gets a RADIOSS MCDS file format
  const fileformat_t *getRadiossFileFormatPtr(MvFileFormat_e ff_id) const;
  /// Posttreats the MvFileFormat to init additional attribute information (solver name and length)
  void postTreatFileFormat(MvFileFormat_e ff_id);
  /// Posttreats the ff_card_t to init attribute solver name and length
  void postTreatFileFormat(ff_card_t *card_p, string& comment,
      MvExpressionList_t& expressionList);
  /// Posttreats the ff_cell_t to init attribute solver name and length
  int postTreatFileFormat(ff_cell_t *cell_p, const char *comment, int position,
      MvExpressionList_t& expressionList);
  /// Inits the definitions block attributes solver names
  void initDefinitionsAttributesSolverNames();
  /// Inits the attribute solver name for the ikeyword
  void initAttributeSolverNames(const string &solver_name, int ikeyword, bool set_skeyword=false,
      const MvExpressionList_t& expressionList=MvExpressionList_t());
  /// Inits the attributes solver names which are not in the format block.
  void initOtherAttributesSolverNames();
  /// Inits the attribute length for the ikeyword
  void initAttributeLength(int ikeyword, int length);
  /// Gets a info about all subobject cards
  const MvSubobjectsCardInfoList_t& GetSubobjectsCardInfoList() const { return mySubobjectsCardInfoList; }
  //@}

public: /**@name Drawables */
  //@{
  void addDrawable(MvDomain_e domain,const MvDrawable_t *drawable_p);
  /// Gets a drawable from its name
  const MvDrawable_t *getDrawablePtr(const string &name) const;
  /// Gets the drawables from the given domains
  MvDrawablePtrSet_t *getDrawables(int domain,MvDrawablePtrSet_t* drawable_set_p=NULL) const;
  /// Gets the drawables of given access from the given domains
  MvDrawablePtrSet_t *getDrawables(int domain,MvDrawableAccess_e access,
				   MvDrawablePtrSet_t* drawable_set_p=NULL) const;
  /// Gets the drawable names from the given domains
  MvDrawableNameSet_t *getDrawableNames(int domain,MvDrawableNameSet_t* dname_set_p=NULL) const;
  /// Gets the drawable names of given access from the given domains
  MvDrawableNameSet_t *getDrawableNames(int domain,MvDrawableAccess_e access,
					MvDrawableNameSet_t* dname_set_p=NULL) const;
  //@}

  
public: /**@name M-Xplore's parameters */
  //@{
  // Adds an input parameter
  void addIParamDescr(MvDomain_e domain,const MvIParamDescr_t *iparam_p);
  /// Gets a input parameter from its name
  const MvIParamDescr_t *getIParamDescrPtr(const string &name) const;
  
  /// Gets the input parameters from the given domains
  MvIParamDescrPtrList_t *getIParamDescrs(int domains,MvIParamDescrPtrList_t *iparams_p=NULL) const;
  /// Gets the input parameters of given access from the given domains
  MvIParamDescrPtrList_t *getIParamDescrs(int domains,MvIParamAccess_e access,
					  MvIParamDescrPtrList_t* iparams_p=NULL) const;
  
  
  // Adds an output parameter
  void addOParamDescr(MvDomain_e domain,const MvOParamDescr_t *oparam_p);
  /// Gets a output parameter from its name
  const MvOParamDescr_t *getOParamDescrPtr(const string &name) const;
  /// Gets the output parameters from the given domains
  MvOParamDescrPtrList_t *getOParamDescrs(int domains,MvOParamDescrPtrList_t *oparams_p=NULL) const;
  /// Gets the input parameters of given access from the given domains
  MvOParamDescrPtrList_t *getOParamDescrs(int domains,MvOParamAccess_e access,
					  MvOParamDescrPtrList_t* oparams_p=NULL) const; 
  
  //@}
  

  
public: /** @name Conditional I/O parameter descriptors */
  //@{
  /// Adds conditional i-parameter descriptors
  void addCondIParamDescriptors(MvDomain_e domain,const MvCondIParamDescriptors_t *cipd_p);
  /// Adds conditional o-parameter descriptors
  void addCondOParamDescriptors(MvDomain_e domain,const MvCondOParamDescriptors_t *copd_p);
  /// Gets the conditional i-parameter descriptors for given domains
  MvCondIParamDescrList_t *getCondIParamDescriptors(int domains,MvCondIParamDescrList_t *cipdl_p=NULL) const;
  /// Gets the conditional o-parameter descriptors for given domains
  MvCondOParamDescrList_t *getCondOParamDescriptors(int domains,MvCondOParamDescrList_t *copdl_p=NULL) const;
  //@}
  

  
public: /**@name Definitions */
  //@{
  // Adds a definition
  void addDefinition(MvDomain_e domain,const string &name,const MvIKeywordSet_t &ikeywords);
  /// Gets a definition
  const MvIKeywordSet_t &getDefinition(int domain,const string &name) const;

  bool hasIKeyInDefinition(int domain,const string &name, int ikey) const ;
  //@}
  
  void setConfigType(unsigned int configtype) ;
  void setHmType(unsigned int hmtype) ;
  void setKeyword(string keyword);
  void setCardImage(char* cardImage);
  void setIdPool(short int idpool);
  unsigned int  getConfigType() const;
  unsigned int  getHmType() const;
  const string& getKeyword() const;
  char* getCardImage() const;
  short int getIdPool() const;
  ostream& display(ostream& os) const;
protected: // Output
  // Output in an output stream

  ostream &displayAttribData(ostream &os, int ikw) const;
  ostream &displayAttribData(ostream &os, MvDomain_e domain, MvIKeywordList_t &list, MvIKeywordList_t *local_attrib_list, int max_size_skw) const;
  ostream &displaySpaces(ostream &os, int skeyw_size, int max_skeyw_size) const;
private: // Datas
  // MCDS descriptor
  descriptor_t                     *myDescriptorPtr;
  // IKeywords
  MvPseudoIKeywordListMap_t        *myIKeywordListMapPtr;
  // Defaults
  MvPseudoDefaults_t               *myDefaultsPtr;

  //skeywords int alias 
  MvPseudoIdentifierMap_t          *myIdentifiersPtr;


  // Data features
  MvPseudoDataFeatureListMap_t     *myDataFeatureListMapPtr;
  // Data features
  MvPseudoDataFeatureListMap_t     *myDataFeatureReducedListMapPtr;
  // Check
  MvPseudoTestListMap_t            *myTestListMapPtr;
  // Conditional ikeywords (dependences)
  MvPseudoDependenceListMap_t      *myDependenceListMapPtr;
  // Conditional paramater descriptors
  MvPseudoCondIParamDescrListMap_t *myCondIParamDescrListMapPtr; 
  MvPseudoCondOParamDescrListMap_t *myCondOParamDescrListMapPtr; 
  // Drawable
  MvPseudoDrawableMap_t            *myDrawableListMapPtr;
  MvPseudoDrawableNameMap_t        *myDrawableListNameMapPtr;
  // Input paramaters
  MvPseudoIParamDescrMap_t         *myIParamDescrListMapPtr;     
  MvPseudoIParamDescrNameMap_t     *myIParamDescrListNameMapPtr; 
  // Output paramaters
  MvPseudoOParamDescrMap_t         *myOParamDescrListMapPtr;     
  MvPseudoOParamDescrNameMap_t     *myOParamDescrListNameMapPtr; 
  // Definitions
  MvPseudoDefinitionMap_t          *myDefinitionMapPtr;          
  // Keyword mapping
  MvPseudoKeywordKeywordMap_t      *mySolverNameSKeywordMapPtr;

  map<string, MvCondKeywordList_t>  mySolverNameSKeywordsMap;

  MvSubobjectsCardInfoList_t        mySubobjectsCardInfoList;

  mutable MvPseudoSelectionListMap_t       *mySelectionMapPtr;
  unsigned int myConfigType;
  unsigned int myHmType;
  string       myKeyword;
  char*        myCardImage;
  short int    myIdPool;
};

/// Output of an object in an output stream
ostream &operator<<(ostream &os,const MvDescriptor_t &descr);

/// Getting a descriptor from object's full type
//const MvDescriptor_t *get_descriptor(const MvFullType_t &fulltype);
/// Getting a descriptor from object's type and subtype
//const MvDescriptor_t *get_descriptor(object_type_e type,const MvSubtype_t *subtype_p);
/// Getting a descriptor from object's type and keyword
//const MvDescriptor_t *get_descriptor(object_type_e type,const string &keyword); 
/// Getting a descriptor from string
//const MvDescriptor_t *get_descriptor(const string &fulltype);
/// Getting a user descriptor from id
//const MvDescriptor_t *MV_get_user_descriptor(object_type_e type,int user_id);


/// Getting a user descriptor from keyword
//const MvDescriptor_t *MV_get_user_descriptor(object_type_e type,const string &keyword);
//const MvDescriptor_t *MV_get_user_descriptor_userkeyword(object_type_e type,const string &keyword, string &first_user_keyword);

//void MV_init_descriptors();  
//void MV_close_descriptors(); 
//
//const MvDescriptor_t *MV_get_user_descriptor_using_idpool(object_type_e type, short int idPool);
//@}
#endif //MV_DESCRIPTOR_H




