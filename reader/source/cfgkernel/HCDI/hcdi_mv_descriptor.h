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
#ifndef HCDI_MV_DESCRIPTOR_H
#define HCDI_MV_DESCRIPTOR_H


#include <UTILS/mv_string.h>
#include <UTILS/mv_iostream.h>
#include <UTILS/mv_stl_various.h>
#include <KERNEL_BASE/descriptor_API.h> 


#include <KERNEL/mv_subtype.h>
#include <KERNEL/mv_domain.h>
#include <KERNEL/mv_ikeyword_containers.h>
#include <KERNEL/mv_data_feature.h>
#include <KERNEL/mv_data_support_feature.h>
#include <KERNEL/mv_file_format.h>
#include <KERNEL/mv_test.h>
#include <KERNEL/mv_dependence.h>
#include <KERNEL/mv_drawable.h>
#include <KERNEL/mv_iparam_descr.h>
#include <KERNEL/mv_oparam_descr.h>           
#include <KERNEL/mv_cond_param_descriptors.h> 
#include <KERNEL/mv_data_selection_feature.h>
#include <UTILS/direction_utils.h>
#include <KERNEL/mv_data_uncond_scalar_feature.h>
#include <KERNEL_BASE/GlobalApplication_enum.h>
#include <unordered_map>
#include "hcdi.h"
#include <KERNEL/mv_type.h>
#include <KERNEL/mv_type.h>
#include <KERNEL/mv_domain.h>
// IKeywords
#define MvPseudoIKeywordListMap_t        void
// Defaults
#define MvPseudoDefaults_t               void 
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
typedef vector<MvExpression_t>           MvExpressionList_t;
typedef vector<pair<string, MvExpressionList_t>> MvCondKeywordList_t;
typedef vector<string> child_t;
typedef map<string, child_t> leaf_t;
typedef map<string, leaf_t> leafmap_t;
typedef map<string, leafmap_t> leafmapmap_t;
typedef struct MvSubobjectsCardInfo_s {
    int ikeyword;
    const char *full_type;
    MvExpressionList_t conditions;
    const char *plnkatt;
    const char *clnkatt;
} MvSubobjectsCardInfo_t;
typedef vector<MvSubobjectsCardInfo_t> MvSubobjectsCardInfoList_t;


typedef vector<dimension_size_t> MvSizeVector;  

/// Descriptor Interface class
class IDescriptor
{

public: /**@name Creation*/

  /// Destructor
    virtual ~IDescriptor() {} 

  //@{
  /// Adds a value attribute
  virtual void addValue(MvDomain_e domain,value_type_e vtype,int ikw,const string &skw,const string &comment)=0;
  /// Adds an object attribute
  virtual void addObject(MvDomain_e domain,object_type_e otype,int ikw,const string &skw,const string &comment)=0;
  /// Adds a size attribute
  virtual void addSize(MvDomain_e domain,int ikw,const string &skw,const string &comment)=0;
  /// Adds a value array attribute
  virtual void addValueArray(MvDomain_e domain,value_type_e vtype,int ikw,const string &skw,const string &comment,
		     attribute_type_e array_type,
                     const MvSizeVector & sizeArrayVector)=0;
  /// Adds an object array attribute
  virtual void addObjectArray(MvDomain_e domain,object_type_e otype,int ikw,const string &skw,const string &comment,
		      attribute_type_e array_type,
                      const MvSizeVector & sizeArrayVector)=0;
  //@}

public: /**@name Acces to descriptor*/
  //@{
  /// Returns true if the descriptor is USER
  virtual bool isUser() const = 0;
  /// Gets the User ID of the descriptor
  virtual int getUserId() const = 0;
  /// Sets the User ID of the descriptor
  virtual void setUserId(int id) = 0;
  /// Gets the MCDS descriptor
  virtual const descriptor_t *getDescriptorPtr() const  = 0;
  //@}

public: /* Keywords */
  // Get the first integer keyword
  virtual int    getFirstIKeyword()                  const = 0;
  // Get the next integer keyword (return END_ARGS if last reached)
  virtual int    getNextIKeyword(int ikeyword)       const = 0;
  // Adds an integer keyword into a domain
  virtual void   addDomainIKeyword(MvDomain_e domain,int ikeyword) = 0;

public: /**@name Keywords*/
  //@{
  /// Gets the integer keyword from the string keyword
  virtual int    getIKeyword(const string &skeyword) const = 0;
  /// Gets the string keyword from the integer keyword
  virtual string getSKeyword(int ikeyword)           const = 0;
  /// Gets the const char* keyword from the integer keyword
  virtual int getSKeyword(int ikeyword, const char** skeyword) const = 0;
  /// Gets the string keyword from the solver name
  virtual const string & getSKeywordFromSolverName(const string &solvername) const = 0;
  /// Gets the skeywords with their conditions from the solver name
  virtual const MvCondKeywordList_t& getSKeywordsFromSolverName(const string &solvername) const = 0;
  /// Gets integer keywords from string keywords
  virtual MvIKeywordSet_t *getIKeywords(const MvSKeywordSet_t &skw_set,MvIKeywordSet_t *ikw_set_p=NULL) const = 0;
  /// Gets string keywords from integer keywords
  virtual MvSKeywordSet_t *getSKeywords(const MvIKeywordSet_t &ikw_set,MvSKeywordSet_t *skw_set_p=NULL) const = 0;
  /// Gets support's integer keywords
  virtual MvIKeywordSet_t *getSupportIKeywords(int domain,MvIKeywordSet_t *ikw_set_p=NULL) const = 0;
  /// Gets domain's integer keywords
  virtual const MvIKeywordList_t &getDomainIKeywords(MvDomain_e domain) const = 0;
  /// Gets integer keywords of given domains
  virtual MvIKeywordList_t *getIKeywords(int domains,MvIKeywordList_t *ikw_list_p=NULL) const = 0;
  /// Gets integer keywords of given domains and a given value type
  virtual MvIKeywordList_t *getIKeywords(int domains,value_type_e vtype,MvIKeywordList_t *ikw_list_p=NULL) const = 0;
  /// Gets integer keywords of given domains and a given attribute type
  virtual MvIKeywordList_t *getIKeywords(int domains,attribute_type_e atype,MvIKeywordList_t *ikw_list_p=NULL) const = 0;
  /// Gets integer keywords of given domains and given attribute and value type
  virtual MvIKeywordList_t *getIKeywords(int domains,attribute_type_e atype,value_type_e vtype,
				 MvIKeywordList_t *ikw_list_p=NULL) const = 0;
  virtual int getAllIKeywords(MvDomain_e domain, MvAtypeVtypeList_t  &atype_vtypeikeywordlst) const =0;
  virtual int getAllIKeywords(MvDomain_e domain, map< int, vector<MvIKeywordList_t> >  &map_type_keywordlst) const =0;
  virtual MvIKeywordList_t* getAllIKeywordsHavingDefaults(int domains, MvIKeywordList_t* ikw_list_p = NULL) const=0;
  virtual void getIdentifiers(MvIKeywordList_t *pikeyidentifierlst) const = 0;
  virtual int getMaxIKeyword() const =0;

 // virtual void getMultiObjectTypes(int ikeyword, MvFullTypeSet_t &set) const = 0;
  //@}

public: /**@name Iterators*/
  //@{
  /// Class iterator
  class iterator {
  public:
    /// Constructor
    iterator(const MvDescriptor_t *descr_p=NULL,int ikeyword=END_ARGS);
  public:
    /// Gets the ikeyword
    inline int operator*() const { return myIKeyword; }
    /// Incrementation
    const iterator &operator++();
    /// Equality
    bool operator==(const iterator &it) const;
    /// Unequality
    bool operator!=(const iterator &it) const;
  private:
    const MvDescriptor_t *myDescriptorPtr;
    int                   myIKeyword;
  };
  /// Gets the begin iterator
  virtual inline iterator begin() const  = 0;
  /// Gets the begin iterator
  virtual inline iterator end()   const  = 0;
  //@}

public: /**@name Common data*/ 
  //@{
  /// Gets the attribute type (ATYPE_VALUE, ATYPE_SIZE, ATYPE_STATIC_ARRAY, ATYPE_DYNAMIC_ARRAY)
  virtual attribute_type_e getAttributeType(int ikeyword) const = 0;
  /// Gets the value type (VTYPE_INT, VTYPE_UINT, VTYPE_FLOAT, VTYPE_STRING, VTYPE_OBJECT)
  virtual value_type_e     getValueType(int ikeyword)     const = 0;
  /// Gets the comment (or title)
  virtual string           getComment(int ikeyword)       const = 0;
  /// Gets the solver name
  virtual string           getSolverName(int ikeyword)    const = 0;
  // Gets the attribute length in the format block
  virtual int   getAttributeLength(int ikeyword)          const = 0;
  //@}

public: /**@name Object data*/
  //@{
  /// Gets the type of object (for an object value or an object array)
  virtual object_type_e getObjectType(int ikeyword) const = 0;
  virtual bool isMultiType(int ikeyword) const = 0;
  virtual bool hasObjectAttribSubtype(int ikeyword, int *nb_subtype) const = 0;
  //@}

public: /**@name Array data*/
  //@{
  /// Gets the size of a static array
  virtual int    getSize(int ikeyword)         const = 0;
  /// Gets the integer keyword giving the size of a dynamic array
  virtual int    getSizeIKeyword(int ikeyword) const = 0;
  /// Gets the string keyword giving the size of a dynamic array
  virtual string getSizeSKeyword(int ikeyword) const = 0;
  /// Gets the ikeywords of the dynamic arrays of a "size" attribute
  virtual MvIKeywordSet_t *getSizeConnectedIKeywords(int size_ikw,MvIKeywordSet_t *ikw_set_p=NULL) const = 0;
  
  /// Returns true if the array is a main array
  virtual bool isMainArray(int ikeyword) const = 0;
  
  
  virtual MvIKeywordSet_t* getMainArrayIKeywords(MvIKeywordSet_t* ikw_set_p = NULL) const = 0;
  virtual MvIKeywordSet_t* getSecondaryArrayIKeywords(int ikeyword, 
					  MvIKeywordSet_t* ikw_set_p = NULL) const = 0;
  
  
  virtual bool isSecondaryArray(int ikeyword) const = 0;
  
  
  virtual  MvIKeywordSet_t* getAlternateSupportIKeywords(MvIKeywordSet_t* ikw_set_p = NULL) const = 0;
  
  
  virtual void getDimensionSize(int ikeyword,MvSizeVector & sizeArrayVector) const  = 0;
  virtual bool isMultiDimensionalArray(int ikeyword) const = 0;
  /* Update feature attributes of a feature*/
  virtual void UpdateFeatureAttributes(MvDataFeature_t* feature_p, feature_attribute_type_e attrib, int val, set<value_type_e>* vset = NULL) const = 0;

  
  //@}

  //@}

public: /** @name Dependences */
  //@{
  /// Adds a dependence
  virtual void addDependence(MvDomain_e domain,const MvDependence_t *depend_p) = 0;
  /// Gets the dependences for given domains
  virtual MvDependenceList_t *getDependences(int domains,MvDependenceList_t *dl_p=NULL) const = 0;
  //@}

public: /** @name Default values*/
  //@{
  /// Returns true if the attribute admits a default value in one of the given domains
  virtual bool hasDefault(int ikeyword,int domains) const = 0;
  /// Returns true if the attribute admits a default value in one of the given domains
  virtual inline bool hasDefault(const string &skeyword,int domains) const = 0;

  /// Gets the default value of an integer attribute in one of the given domains
  virtual bool getBoolDefaultValue(int ikeyword,int domains,bool *has_default_p=NULL) const = 0;

  virtual int getBoolDefaultValue(const string &skeyword,int domains,bool *has_default_p=NULL) const = 0;
  /// Gets the default value of an integer attribute in one of the given domains
  virtual int getIntDefaultValue(int ikeyword,int domains,bool *has_default_p=NULL) const = 0;

  virtual unsigned int getUIntDefaultValue(int ikeyword,int domains,bool *has_default_p=NULL) const = 0;
  virtual unsigned int getUIntDefaultValue(const string &skeyword, int domains, bool *has_default_p = NULL) const = 0;
  virtual void getIdentifierValue(int domains, MvIntIdentifierMap_t  &identifierlst) const = 0;
  virtual int getIdentifierValue(int domains, int ikeyword) const = 0;
  virtual int getIdentifierValue(int domains, const string &skeyword) const = 0;
  virtual int getIKeywordFromIdentifierValue(int domains, int identifier) const=0; 
  /// Gets the default value of an integer attribute in one of the given domains
  virtual int getIntDefaultValue(const string &skeyword,int domains,bool *has_default_p=NULL) const  = 0;
  /// Gets the default value of a float (double) attribute in one of the given domains
  virtual double getFloatDefaultValue(int ikeyword,int domains,bool *has_default_p=NULL) const = 0;
  /// Gets the default value of a float (double) attribute in one of the given domains
  virtual inline double getFloatDefaultValue(const string &skeyword,int domains,bool *has_default_p=NULL) const  = 0;
  /// Gets the default value of a string attribute in one of the given domains
  virtual string getStringDefaultValue(int ikeyword,int domains,bool *has_default_p=NULL) const = 0;
  /// Gets the default value of a string attribute in one of the given domains
  virtual inline string getStringDefaultValue(const string &skeyword,int domains,bool *has_default_p=NULL) const  = 0;
  virtual void setBoolDefaultValue(int ikeyword,MvDomain_e domain,bool value) = 0;

  virtual void setIntDefaultValue(int ikeyword,MvDomain_e domain,int value) = 0;
  virtual inline void setIntDefaultValue(const string &skeyword,MvDomain_e domain,int value) = 0;
  /// Sets the default value of an unsigned integer attribute for the given domain
  virtual void setUIntDefaultValue(int ikeyword,MvDomain_e domain,unsigned int value) = 0;
  /// Sets the default value of an unsigned integer attribute for the given domain
  virtual inline void setUIntDefaultValue(const string &skeyword,MvDomain_e domain,unsigned int value) = 0;

  virtual inline void setIntIdentifierValue(int ikeyword,MvDomain_e domain,int value) = 0;
  /// Sets the default value of a float (double) attribute for the given domain
  virtual void setFloatDefaultValue(int ikeyword,MvDomain_e domain,double value) = 0;
  /// Sets the default value of a float (double) attribute for the given domain
  virtual inline void setFloatDefaultValue(const string &skeyword,MvDomain_e domain,double value)  = 0;
  /// Sets the default value of a string attribute for the given domain
  virtual void setStringDefaultValue(int ikeyword,MvDomain_e domain,const string &value) = 0;
  /// Sets the default value of a string attribute for the given domain
  virtual inline void setStringDefaultValue(const string &skeyword,MvDomain_e domain,const string &value) = 0;
  /// Sets the default value of an object attribute for the given domain.
  /// This is only "dummy", only NONE can be defined in cfg file, so no value is passed here.
  virtual void setObjectDefaultValue(int ikeyword,MvDomain_e domain) = 0;

  //@}
  
public: // Domains
  virtual int getNbDomains() const = 0;

public: /**@name Data features */
  //@{
  /// Adds a data feature for a given domain
  virtual void setDataFeature(MvDomain_e domain,const MvDataFeature_t *df_p, bool skip_reduced = false) = 0;
  
  /// Gets the domain of a data feature
  virtual MvDomain_e getDomain(const MvDataFeature_t *df_p) const = 0;
  
  /** Gets a data feature list for given domains.<br>
      <br>
      @param domain Logical union (OR) of domains (ex: DOM_COMMON | DOM_FLUID)
      @param dfl_p  Pointer on the list of features (if NULL, is allocated) (input/output) 
      <br>
      @see MvDomain_e
   */
  virtual MvDataFeatureList_t *getDataFeatures(int domains,MvDataFeatureList_t *dfl_p=NULL, bool get_reduced = false) const = 0;
  /// Gets a data feature list of a given type for given domains
  virtual MvDataFeatureList_t *getDataFeatures(int domains,MvDataFeatureType_e a_dft,
				       MvDataFeatureList_t *dfl_p=NULL, bool add_containing_array_feat=true) const = 0;
  /// Gets features implicated in "if" features
  virtual MvDataFeatureSet_t *getConditionalDataFeatures(int domains,MvDataFeatureSet_t *dfs_p=NULL) const = 0;
  /// Gets features which are tests of "if" features
  virtual MvDataFeatureSet_t *getConditionDataFeatures(int domains,MvDataFeatureSet_t *dfs_p=NULL) const = 0;
  
  
  virtual MvDataFeatureSet_t *getConditionalDimensionFeatures(MvDataFeatureType_e feature_type,
						      bool                array_subfeatures,
						      MvDataFeatureSet_t *dfs_p=NULL) const = 0;
  
  /// Gets features with conditional dimensions
  virtual MvDataFeatureSet_t *getConditionalDimensionFeatures(MvDataFeatureSet_t *dfs_p=NULL) const = 0;
  /// Gets features which are tests of features with conditional dimensions
  virtual MvDataFeatureSet_t *getConditionDimensionFeatures(MvDataFeatureSet_t *dfs_p=NULL) const = 0;
  
  virtual MvDataFeatureSet_t *getConditionalDimensionFeatures(const MvDataFeature_t *cond_p,
						      MvDataFeatureType_e    feature_type,
						      bool                   array_subfeatures,
						      MvDataFeatureSet_t    *dfs_p=NULL) const = 0;
  
  /// Gets features with conditional dimensions implicated in the given test feature
  virtual MvDataFeatureSet_t *getConditionalDimensionFeatures(const MvDataFeature_t *cond_p,MvDataFeatureSet_t *dfs_p=NULL) const = 0;
  /// Gets If features in which given feature is associated in the tests
  virtual MvDataFeatureSet_t *getIfFeatures(int domains, const MvDataFeature_t *test_p,MvDataFeatureSet_t *dfs_p=NULL) const = 0;
  
  /// Gets the feature from ikeyword, if possible.
  virtual const MvDataFeature_t* getIkeywordDataFeature(int domains, int ikeyword) const = 0;
  /// Gets the MvSelectionFeature for given name such as SECONDARY or MAIN if does not find any, return NULL 
  virtual const MvDataSelectionFeature_t* getSelectionFeature(const char *rootname) const = 0;
  //@}

public: /**@name Tests */
  //@{
  /// Add a data feature for a given domain
  virtual void addTest(MvDomain_e domain,const MvTest_t *test_p) = 0;
  /** Gets a test list for given domains.<br>
      <br>
      @param domain Logical union (OR) of domains (ex: DOM_COMMON | DOM_FLUID)
      @param test_p  Pointer on the list of tests (if NULL, is allocated) (input/output) 
      <br>
      @see MvDomain_e
   */
  virtual MvTestList_t *getTests(int domain,MvTestList_t *tl_p=NULL) const = 0;
  //@}

public: /**@name File formats */
  //@{
  virtual void addFileFormat(MvFileFormat_e ff_id,fileformat_t *ff_p) = 0;
  /// Gets a MCDS file format
  virtual const fileformat_t *getFileFormatPtr(MvFileFormat_e ff_id) const = 0;
  /// Gets a MCDS lower version file format
  virtual const fileformat_t *getLowerFileFormatPtr(MvFileFormat_e ff_id) const = 0; 
  /// Get Dyna latest available file format (input parameter (MvFileFormat_e) might change)
  virtual const fileformat_t *getDynaFileFormatPtr(MvFileFormat_e *ff_id_p) const = 0;
  /// Gets a LSDYNA MCDS file format
  virtual const fileformat_t *getDynaFileFormatPtr(MvFileFormat_e ff_id) const = 0;
  /// Gets a RADIOSS MCDS file format
  virtual const fileformat_t *getRadiossFileFormatPtr(MvFileFormat_e ff_id) const = 0;
  /// Posttreats the MvFileFormat to init additional attribute information (solver name and length)
  virtual void postTreatFileFormat(MvFileFormat_e ff_id) = 0;
  /// Posttreats the ff_card_t to init attribute solver name and length
  virtual void postTreatFileFormat(ff_card_t *card_p, string& comment,
      MvExpressionList_t& expressionList) = 0;
  /// Posttreats the ff_cell_t to init attribute solver name and length
  virtual int postTreatFileFormat(ff_cell_t *cell_p, const char *comment, int position,
      MvExpressionList_t& expressionList) = 0;
  /// Inits the definitions block attributes solver names
  virtual void initDefinitionsAttributesSolverNames() = 0;
  /// Inits the attribute solver name for the ikeyword
  virtual void initAttributeSolverNames(const string &solver_name, int ikeyword, bool set_skeyword=false,
      const MvExpressionList_t& expressionList=MvExpressionList_t()) = 0;
  /// Inits the attributes solver names which are not in the format block.
  virtual void initOtherAttributesSolverNames() = 0;
  /// Inits the attribute length for the ikeyword
  virtual void initAttributeLength(int ikeyword, int length) = 0;
  /// Gets a info about all subobject cards
  virtual const MvSubobjectsCardInfoList_t& GetSubobjectsCardInfoList() const = 0;
  //@}

public: /**@name Drawables */
  //@{
  virtual void addDrawable(MvDomain_e domain,const MvDrawable_t *drawable_p) = 0;
  /// Gets a drawable from its name
  virtual const MvDrawable_t *getDrawablePtr(const string &name) const = 0;
  /// Gets the drawables from the given domains
  virtual MvDrawablePtrSet_t *getDrawables(int domain,MvDrawablePtrSet_t* drawable_set_p=NULL) const = 0;
  /// Gets the drawables of given access from the given domains
  virtual MvDrawablePtrSet_t *getDrawables(int domain,MvDrawableAccess_e access,
				   MvDrawablePtrSet_t* drawable_set_p=NULL) const = 0;
  /// Gets the drawable names from the given domains
  virtual MvDrawableNameSet_t *getDrawableNames(int domain,MvDrawableNameSet_t* dname_set_p=NULL) const = 0;
  /// Gets the drawable names of given access from the given domains
  virtual MvDrawableNameSet_t *getDrawableNames(int domain,MvDrawableAccess_e access,
					MvDrawableNameSet_t* dname_set_p=NULL) const = 0;
  //@}

  
public: /**@name M-Xplore's parameters */
  //@{
  // Adds an input parameter
  virtual void addIParamDescr(MvDomain_e domain,const MvIParamDescr_t *iparam_p) = 0;
  /// Gets a input parameter from its name
  virtual const MvIParamDescr_t *getIParamDescrPtr(const string &name) const = 0;
  
  /// Gets the input parameters from the given domains
  virtual MvIParamDescrPtrList_t *getIParamDescrs(int domains,MvIParamDescrPtrList_t *iparams_p=NULL) const = 0;
  /// Gets the input parameters of given access from the given domains
  virtual MvIParamDescrPtrList_t *getIParamDescrs(int domains,MvIParamAccess_e access,
					  MvIParamDescrPtrList_t* iparams_p=NULL) const = 0;
  
  
  // Adds an output parameter
  virtual void addOParamDescr(MvDomain_e domain,const MvOParamDescr_t *oparam_p) = 0;
  /// Gets a output parameter from its name
  virtual const MvOParamDescr_t *getOParamDescrPtr(const string &name) const = 0;
  /// Gets the output parameters from the given domains
  virtual MvOParamDescrPtrList_t *getOParamDescrs(int domains,MvOParamDescrPtrList_t *oparams_p=NULL) const = 0;
  /// Gets the input parameters of given access from the given domains
  virtual MvOParamDescrPtrList_t *getOParamDescrs(int domains,MvOParamAccess_e access,
					  MvOParamDescrPtrList_t* oparams_p=NULL) const = 0; 
  
  //@}
  

  
public: /** @name Conditional I/O parameter descriptors */
  //@{
  /// Adds conditional i-parameter descriptors
  virtual void addCondIParamDescriptors(MvDomain_e domain,const MvCondIParamDescriptors_t *cipd_p) = 0;
  /// Adds conditional o-parameter descriptors
  virtual void addCondOParamDescriptors(MvDomain_e domain,const MvCondOParamDescriptors_t *copd_p) = 0;
  /// Gets the conditional i-parameter descriptors for given domains
  virtual MvCondIParamDescrList_t *getCondIParamDescriptors(int domains,MvCondIParamDescrList_t *cipdl_p=NULL) const = 0;
  /// Gets the conditional o-parameter descriptors for given domains
  virtual MvCondOParamDescrList_t *getCondOParamDescriptors(int domains,MvCondOParamDescrList_t *copdl_p=NULL) const = 0;
  //@}
  

  
public: /**@name Definitions */
  //@{
  // Adds a definition
  virtual void addDefinition(MvDomain_e domain,const string &name,const MvIKeywordSet_t &ikeywords) = 0;
  /// Gets a definition
  virtual const MvIKeywordSet_t &getDefinition(int domain,const string &name) const = 0;
  
  virtual bool hasIKeyInDefinition(int domain,const string &name, int ikey) const = 0;

  virtual void setConfigType(unsigned int configtype) = 0;
  virtual void setHmType(unsigned int hmtype) = 0;
  virtual void setKeyword(string keyword) = 0;
  virtual void setIdPool(short int idpool) = 0;
  virtual unsigned int  getConfigType() const = 0 ;
  virtual unsigned int  getHmType() const = 0 ;
  virtual const string& getKeyword() const = 0 ;
  virtual char* getCardImage() const = 0;
  virtual short int getIdPool() const = 0;
protected: // Output
  // Output in an output stream
  virtual ostream &display(ostream &os) const =0;


};
typedef IDescriptor* IDESCRIPTORHANDLE;


EXTERNC HC_DATA_DLL_API IDESCRIPTORHANDLE HCDI_GetDescriptorHandle(const char *fulltype);

EXTERNC HC_DATA_DLL_API IDESCRIPTORHANDLE HCDI_GetDescriptorHandleFromKeyword(int type,const string &keyword);

EXTERNC HC_DATA_DLL_API IDESCRIPTORHANDLE HCDI_GetDescriptorHandleUserKeywordFromKeyword(int type, int hm_config_type, int hm_type, const string &keyword, string &first_user_keyword);

EXTERNC HC_DATA_DLL_API void HCDI_GetUserKeywordFromConfigHmType(int type, int config_type, int hm_type, string &first_user_keyword);

EXTERNC HC_DATA_DLL_API IDESCRIPTORHANDLE HCDI_GetDescriptorHandleFromUserID(int type,int user_id);

EXTERNC HC_DATA_DLL_API IDESCRIPTORHANDLE HCDI_GetDescriptorHandleFromHMConfigHMType(int type,int hm_config_type, int hm_type);

EXTERNC HC_DATA_DLL_API IDESCRIPTORHANDLE HCDI_GetDescriptorHandleFromType(int type);

EXTERNC HC_DATA_DLL_API int HCDI_get_all_domains();
EXTERNC HC_DATA_DLL_API IDESCRIPTORHANDLE HCDI_GetDescriptorHandleFromFullType(const MvFullType_t &fulltype);

EXTERNC HC_DATA_DLL_API object_type_e HCDI_get_entitytype(const string &keyword);

EXTERNC HC_DATA_DLL_API const string &HCDI_get_entitystringtype(int entity_type);

EXTERNC HC_DATA_DLL_API void HCDI_Close_Kernel();

EXTERNC HC_DATA_DLL_API dir_type_e HCDI_MV_get_direction(const char *dir,int is_extended);
EXTERNC HC_DATA_DLL_API const char *HCDI_MV_get_direction_str(dir_type_e dir,int is_extended);
EXTERNC HC_DATA_DLL_API int  HCDI_MV_get_directions(const char *dir,int *xdir_p,int *ydir_p,int*zdir_p);
EXTERNC HC_DATA_DLL_API const char *HCDI_MV_get_directions_str(int xdir,int ydir,int zdir);
EXTERNC HC_DATA_DLL_API void HCDIgetAllKeywordConfigTypes(int etype, vector< std::pair<unsigned int,   string> > &aListConfig);
EXTERNC HC_DATA_DLL_API void HCDIgetAllConfigTypes(int etype, vector< std::pair<unsigned int, string> >& aListConfig, bool withTitle);
EXTERNC HC_DATA_DLL_API void HCDIGetEntityDescriptors(int etype, vector< std::pair<string,  const IDescriptor *> > &aListConfigIDescriptor);
EXTERNC HC_DATA_DLL_API void HCDIGetAllTypesTitle(vector< std::pair<obj_type_e, string> > &vec_type_title);
EXTERNC HC_DATA_DLL_API void HCDIGetUserDiscreteListFromType(unsigned int etype, map < string , vector< string> > &mapDiscreteList);
EXTERNC HC_DATA_DLL_API void HCDIGetInternalDiscreteListFromType(unsigned int etype, map < string , vector< string> > &mapDiscreteList);
EXTERNC HC_DATA_DLL_API void HCDIGetConfigHMTypeCardImageFromKeyword(unsigned int etype, string &keyword, int *configType, int *hmType, string &cardImage); 

EXTERNC HC_DATA_DLL_API void HCDIfindKeywordParent(int etype, bool isuser, string &keyword, vector< string> &parent);
/*extern "C" HC_DATA_DLL_API const obj_type_e HCDIgetHCTypeFromIdentifer(string &hm_type_str, string &cardimage); */
EXTERNC HC_DATA_DLL_API void HCDIgetIdPool(int etype, int hm_config_type, int hm_type, string &cardimage, int *id_pool);
EXTERNC HC_DATA_DLL_API void HCDIgetChildKeywordListForGivenKeyword(int etype, string &keyword, vector<string> &keyword_list);
/* make sure etype has cardimage */
EXTERNC HC_DATA_DLL_API void HCDIgetChildCardImageListForGivenKeyword(int etype, string &card_image, vector<string> &card_image_list);
EXTERNC HC_DATA_DLL_API void HCDI_StringTokenize(const string& str, vector<string>& pTokens, string delem);
EXTERNC HC_DATA_DLL_API void HCDIgetKeywordTypeMap(map<string, obj_type_e> &keyword_type_map, bool add_first_parent);
EXTERNC HC_DATA_DLL_API bool HCDIgetTypeHasSubtype(int etype);
EXTERNC HC_DATA_DLL_API bool HCDIGetFirstUserNameForGvnEtypeUserID(const string &etype, int userid, string &firstusername);
EXTERNC HC_DATA_DLL_API bool HCDIGetFirstUserNameForGvnEtypeConfigHMType(const string &etype, int config, int hmtype, string &firstusername);
EXTERNC HC_DATA_DLL_API bool HCDIGetFirstUserNameForGvnEtypeKeyword(int etype, string& cardimage, string& firstusername);
EXTERNC HC_DATA_DLL_API MvDataUncondScalarFeature_t* HCDI_GetAllocatedUnCondScalarFeatureHandle(const string &title, int  ikeyword, MvDimension_e dimension, vector<string>& argVect);/*User of this API should deallocate the memory*/
EXTERNC HC_DATA_DLL_API bool HCDIhasIdPool(int etype);
EXTERNC HC_DATA_DLL_API MvKeywordSet_t* HCDIgetKeywords(object_type_e otype, const MvSubtype_t* subtype_p, MvKeywordSet_t* keywords_p);
EXTERNC HC_DATA_DLL_API void* HCDIGetCurrentCFGKernel();
EXTERNC HC_DATA_DLL_API bool HCDIgetInfo(const void* data_cfg_p, int etype, const string& htype, leafmapmap_t& list);
EXTERNC HC_DATA_DLL_API bool HCDILoadDataHiearchyAtUserLocation(obj_type_e type, const string &htype, bool is_user, leafmapmap_t& list);
EXTERNC HC_DATA_DLL_API bool HCDIgetInfoBasedOnHtypeTitle(const bool &isuser, int etype, string& htype, string& title_htype, string& sub_htype, string& title_sub_htype, string& sub_htype_child, child_t& result_vector);

EXTERNC HC_DATA_DLL_API int HCDIGetObjTypeFlags(obj_type_e type, string& username);
EXTERNC HC_DATA_DLL_API bool HCDIHasObjTypeFlags(obj_type_e type, int flag);
EXTERNC HC_DATA_DLL_API bool HCDIHasObjTypeWithUserNameFlags(obj_type_e type, const string &username, int flag);
EXTERNC HC_DATA_DLL_API int HCDIGetObjTypeChildFlags(obj_type_e type, unsigned int config);
EXTERNC HC_DATA_DLL_API int HCDIGetMaxProfileBit();
EXTERNC HC_DATA_DLL_API void HCDIQueryHierarchy(int etype, const bool& isuser, vector< vector <pair< string, string>> >& input_vect, vector<pair< string, vector<string>>>& output_name_value_vect);
EXTERNC HC_DATA_DLL_API void HCDIFillFlagsMap(map<string, int>& type_username_str_bit_map, bool add_child);
EXTERNC HC_DATA_DLL_API void HCDIFillHmUnsupportedObjectType(obj_type_e type, set<obj_type_e>& type_set);
EXTERNC HC_DATA_DLL_API void HCDIFillHmUnsupportedStrSetForObjectType(obj_type_e type, set<string>& type_username_set);

EXTERNC HC_DATA_DLL_API void HCDIGetAllObjectTypesFlagged(const string& flag, set<obj_type_e>& type_set);
EXTERNC HC_DATA_DLL_API void HCDIGetAllObjectUsernamesFlagged(obj_type_e type, const string& flag, set<string>& type_username_set);

EXTERNC HC_DATA_DLL_API void HCDIGetAllLoadedCFGKernels(vector<string>& allLoadedCfgKernels);
EXTERNC HC_DATA_DLL_API const MvSubtype_t* HCDIGetSubtypePtrFromFullType(const string& fulltype);

EXTERNC HC_DATA_DLL_API int HCDI_TestTool(const string& path_home, int testid, const string& userprofile, vector<string>& vec_report);
EXTERNC HC_DATA_DLL_API unsigned int  HCDIGetDefaultConfigType(int etype, int id_pool);
HC_DATA_DLL_API string HCDIGetLatestUserSubProfile(unsigned int solvercode);

#endif //HCDI_MV_DESCRIPTOR_H




