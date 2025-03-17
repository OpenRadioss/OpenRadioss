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
#ifndef MV_CFG_KERNEL_H
#define MV_CFG_KERNEL_H

#include "mv_data_cfg.h"
#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>
#include <KERNEL_BASE/Structure_various.h>
#include <KERNEL_BASE/GlobalApplication_enum.h>
#include "mv_subtype.h"
#include "mv_keywords.h"
#include "mv_dimension.h"
#include "mv_file_format.h"
#include <vector>

class MvPreDatasHierarchy_t;
class IDescriptor;
class MvDescriptorMap_t;
class MvPreDatasHierarchy_t;
class MvDescriptorMapArray_t;
class LocUserPreDescriptorMapArray_t;
class MvPreDescriptor_t;
class MvSubtype_t;
class MvFullType_t;
class MvDescriptor_t;
class MvDataUncondScalarFeature_t;
class MvFullTypeSet_t;

typedef const MvPreDatasHierarchy_t* LocPreDatasHierarchyPtr_t;
typedef map<MvDataTreeType_e, LocPreDatasHierarchyPtr_t>  LocPreDataHierarchyMap_t;


typedef const MvPreDatasHierarchy_t* LocPreDatasHierarchyPtr_t;
typedef map<MvDataTreeType_e, LocPreDatasHierarchyPtr_t>  LocPreDataHierarchyMap_t;

class CUserNameTypeInfo
{
public:
    CUserNameTypeInfo(obj_type_e type, int flags, size_t keysize, MvKeywordSet_t &optlst,
        const IDescriptor* descrp = nullptr, string username = "", unsigned int idpool = 0) : pdescrp(descrp)
    {
        obj_type = type;
        myAllPossibleflags = flags;

        keyword_size = keysize;
        myStripSubStrings = optlst;
        myusername = username;
        myidpool = idpool;
    }
    CUserNameTypeInfo() {  }
    ~CUserNameTypeInfo() { }

public:
    obj_type_e              obj_type;
    int                     myAllPossibleflags;
    vector<string>          myValidStripedStrComputed;
    size_t                  keyword_size;
    MvKeywordSet_t          myStripSubStrings;
    const IDescriptor*      pdescrp;
    string                  myusername;
    unsigned int            myidpool;
};

namespace cfgkernel
{
    // Basic variant-like type to hold different data types
    class Variant {
    public:
        enum class ValueType { NONE, INTEGER, UNSIGNED_INTEGER, DOUBLE, STRING, BOOL };

        Variant(bool value) : mytype(ValueType::BOOL), myboolValue(value) {}
        Variant(int value) : mytype(ValueType::INTEGER), myintValue(value) {}
        Variant(unsigned int value) : mytype(ValueType::UNSIGNED_INTEGER), myuintValue(value) {}
        Variant(double value) : mytype(ValueType::DOUBLE), mydoubleValue(value) {}
        Variant(const std::string& value) : mytype(ValueType::STRING), mystringValue(value) {}
        Variant() : mytype(ValueType::NONE) {}
        // custom copy constructor
        Variant(const Variant& other) : mytype(other.mytype) {
            switch (mytype) {
            case ValueType::INTEGER:
                myintValue = other.myintValue;
                break;
            case ValueType::UNSIGNED_INTEGER:
                myuintValue = other.myuintValue;
                break;
            case ValueType::DOUBLE:
                mydoubleValue = other.mydoubleValue;
                break;
            case ValueType::STRING:
                new(&mystringValue) std::string(other.mystringValue);
                break;
            case ValueType::BOOL:
                myboolValue = other.myboolValue;
                break;
            }
        }

        // custom copy assignment operator
        Variant& operator=(const Variant& other) {
            if (this != &other) {
                // Destruct current value
                if (mytype == ValueType::STRING) {
                    mystringValue.~basic_string();
                }

                mytype = other.mytype;

                // copy  value
                switch (mytype) {
                case ValueType::INTEGER:
                    myintValue = other.myintValue;
                    break;
                case ValueType::UNSIGNED_INTEGER:
                    myuintValue = other.myuintValue;
                    break;
                case ValueType::DOUBLE:
                    mydoubleValue = other.mydoubleValue;
                    break;
                case ValueType::STRING:
                    new(&mystringValue) std::string(other.mystringValue);
                    break;
                case ValueType::BOOL:
                    myboolValue = other.myboolValue;
                    break;
                case ValueType::NONE:
                    break;
                }
            }
            return *this;
        }

        ~Variant() {
            if (mytype == ValueType::STRING) {
                mystringValue.~basic_string();
            }
        }

        ValueType getType() const { return mytype; }

        int getIntValue() const
        {
            if (mytype == ValueType::INTEGER)
                return myintValue;

            //throw std::invalid_argument(std::string("Variant does not hold a ") + "Int" + " value.");
            return 0;
        }
        unsigned int getUIntValue() const
        {
            if (mytype == ValueType::UNSIGNED_INTEGER)
                return myuintValue;
            return 0;
        }
        double getdoubleValue() const
        {
            if (mytype == ValueType::DOUBLE)
                return mydoubleValue;
            return 0.0;
        }
        const std::string& getStringValue() const
        {
            static const std::string emptystr = "";
            if (mytype == ValueType::STRING)
                return mystringValue;

            return emptystr;
        }
        bool getBoolValue() const
        {
            if (mytype == ValueType::BOOL)
                return myboolValue;

            return false;
        }
        

    private:
        ValueType mytype;
        union {
            int             myintValue;
            unsigned int    myuintValue;
            double          mydoubleValue;
            bool            myboolValue;
            std::string     mystringValue;
        };
    };
}


class SyntaxInfo {
public:


    using ArrayValue = std::vector<cfgkernel::Variant>;

    void addAttribute(const std::string& key, const cfgkernel::Variant& value) {
        attributes[key] = value;
    }

    void addArrayAttribute(const std::string& key, const ArrayValue& value) {
        arrayAttributes[key] = value;
    }

    cfgkernel::Variant getAttribute(const std::string& key) const {
        auto it = attributes.find(key);
        if (it != attributes.end()) {
            return it->second;
        }
        return cfgkernel::Variant(); // default value if key is not found
    }

    ArrayValue getArrayAttribute(const std::string& key) const {
        auto it = arrayAttributes.find(key);
        if (it != arrayAttributes.end()) {
            return it->second;
        }
        return ArrayValue(); // default value if key is not found
    }
    bool isKeyDefined(const std::string& key) const {
        return (attributes.find(key) != attributes.end()) || (arrayAttributes.find(key) != arrayAttributes.end());
    }
private:
    std::map<std::string, cfgkernel::Variant> attributes;
    std::map<std::string, ArrayValue> arrayAttributes;
};



typedef  std::pair<std::string, CUserNameTypeInfo>  StringTypeInfoPair;

class HC_DATA_DLL_API CFGKernel {

public: /** @name Constructor and destructor */
    //@{

    /// Constructor
    CFGKernel();

    CFGKernel(const string& patHome, const string& profile, const string& version, const string& userDefinedDHFilename ="");

    CFGKernel(const string& patHome, const string& profile, const string& version, const vector<string>& allowed_flags, const string& userDefinedDHFilename = "");
    /// Destructor
    virtual ~CFGKernel();
    //@}

private:  /** @methods . */
    //@{
    const MvKeywordMap_t&           get_keyword_map(object_type_e type) const;

    LocPreDataHierarchyMap_t*       loc_get_cfg_map_ptr(int option = 0) const;

    MvTypeUserIdSubtypeMap_t&       get_user_subtype_map();

    MvHMConfigTypeSubtypeMap_t&     get_hm_config_type_subtype_map() ;

    MvHMTypeSubtypeMap_t&           get_hm_type_subtype_map() ;

    const MvDescriptorMapArray_t*   loc_manage_map_array(int option=0) const;

    LocUserPreDescriptorMapArray_t* loc_get_pdma_ptr(int option = 0) const;

    const MvDescriptorMap_t*        loc_manage_map(int option=0) ;

    const MvDirList_t&              get_config_dirs(); 

    MvDirList_t&                    get_version_dirs(); 

    void                            init_trees_cfg();

    const MvPreDatasHierarchy_t*    get_tree_cfg(MvDataTreeType_e tree_type, const string& title = "") const;

    const MvPreDescriptor_t* get_user_pre_descriptor(object_type_e obj_type, int id) const;

    const MvPreDescriptor_t* get_user_pre_descriptor(object_type_e obj_type, int hm_config_type, int hm_type) const;

    const MvPreDescriptor_t* get_user_pre_descriptor(object_type_e obj_type, int hm_config_type, int hm_type, const string& keyword) const;

    void close_trees_cfg();

    void close_model_descriptors();
public:
    MvTypeKeywordSubtypeMap_t&      get_subtype_map();

    const MvPreDatasHierarchy_t*    get_datastreehierarchy(MvDataTreeType_e tree_type) const;

    int get_ikeyword(object_type_e obj_type, const string& skeyword) const;

    void add_user_pre_descriptor(object_type_e obj_type, MvPreDescriptor_t* pre_descr_p, const PseudoStringList_t* user_name_list_p);

    void add_user_pre_descriptor(object_type_e obj_type, MvPreDescriptor_t* pre_descr_p, const string& user_name);

private:

/// Getting a descriptor from object's full type
    const MvDescriptor_t* get_descriptor(const MvFullType_t& fulltype) const;

    /// Getting a descriptor from object's type and subtype
    const MvDescriptor_t* get_descriptor(object_type_e type, const MvSubtype_t* subtype_p) const;

    /// Getting a descriptor from object's type and keyword
    const MvDescriptor_t* get_descriptor(object_type_e type, const string& keyword) const;

    /// Getting a descriptor from string
    const MvDescriptor_t* get_descriptor(const string& fulltype) const;

    /// Getting a user descriptor from id
    const MvDescriptor_t* get_user_descriptor(object_type_e type, int user_id) const;

    
    const MvDescriptor_t* get_user_descriptor(object_type_e type, int hm_config_type, int hm_type) const;

    /// Getting a user descriptor from keyword
    const MvDescriptor_t* get_user_descriptor(object_type_e type, const string& keyword) const;

    const MvDescriptor_t* get_user_descriptor_userkeyword(object_type_e type, int hm_config_type, int hm_type, const string& keyword, string& first_user_keyword) const;

    void get_user_descriptor_userkeyword(object_type_e type, int config_type, int hm_type, string& first_user_keyword) const;

    void init_descriptors();  

    void close_descriptors(); 

//
public:
/// Getting subtype from keyword
    const MvSubtype_t* get_subtype(object_type_e obj_type, const string& keyword, connection_type_e econtype = CONNECTION_UNKNOWN) const ;/*MV_get_subtype*/

    /// Getting subtype of connection from connection_type_e
    const MvSubtype_t* get_subtype_connection(object_type_e obj_type, connection_type_e econtype) const;

    /// Getting user subtype from user id
    const MvSubtype_t* get_config_type_subtype(object_type_e obj_type, int hm_config_type, int hm_type) const;

    /// Getting user subtype from user id
    const MvSubtype_t* get_config_type_subtype(const string& obj_type, int hm_config_type, int hm_type) const;

    /// Getting user subtype from user id
    const MvSubtype_t* get_user_subtype(object_type_e obj_type, int user_id) const;

    /// Getting user subtype from user id
    const MvSubtype_t* get_user_subtype(const string& obj_type, int user_id) const;

    const MvSubtype_t* get_subtype(const string &fulltype) const;

    /// Adding user subtype
    const MvSubtype_t* add_user_subtype(object_type_e obj_type, int user_id, int hm_config_type, int hm_type, short int idpool, string& cardImage,
        const PseudoStringList_t* user_name_list_p, const PseudoStringList_t* optional_header_string_list_p,
        bool add_to_keyword_map = true);
 
    /// Adding user subtype
    const MvSubtype_t* add_user_subtype(object_type_e obj_type, int user_id, int hm_config_type, int hm_type, short int idpool, string& cardImage,
        const string& user_name, const string& optional_header_string) ;
   
 
    /// 
    bool MV_is_subtyped(object_type_e obj_type) const;

public:
    /// Returns the subtypes of a given type
    MvSubtypePtrSet_t* get_subtypes(object_type_e otype, MvSubtypePtrSet_t* subtypes_p = NULL) const;

private:
    void delete_all_subtypes();

    void init_model_descriptors();

    void init_pre_descriptors();

    void close_pre_descriptors();
//
public:
    bool is_user(object_type_e obj_type) const;

private:
    const MvObjectTypeSet_t& get_object_type_set();
    /*const MvObjectTypeSet_t& MV_get_user_type_set();*/

    void close_kernel();

    void mv_init_kernel();

    int LoadCFGKernel();

    int LoadUserDHTree();

    void InitAttributeSolverNames();

    void UpdateSolverInfoRecursively(obj_type_e etype, const MvPreDatasHierarchy_t* data_cfg_p, std::map<string, CUserNameTypeInfo>& objectsolverinfo, MvStringList_t& ulist) const;
public: 
    //@}
    MvPreDatasHierarchy_t*  get_userdefined_hierarchy(int option, string filename="") const;
 /** **/
    //@{

    IDescriptor* GetDescriptorHandle(const char* fulltype) const;

    IDescriptor* GetDescriptorHandleFromKeyword(int type, const string& keyword) const;

    IDescriptor* GetDescriptorHandleUserKeywordFromKeyword(int type, int config_type, int hm_type, const string& keyword, string& first_user_keyword) const;

    void GetUserKeywordFromConfigHmType(int type, int config_type, int hm_type, string& first_user_keyword) const;

    IDescriptor* GetDescriptorHandleFromUserID(int type, int user_id) const;

    IDescriptor* GetDescriptorHandleFromHMConfigHMType(int type, int hm_config_type, int hm_type) const;

    IDescriptor* GetDescriptorHandleFromType(int type) const;

    IDescriptor* GetDescriptorHandleFromFullType(const MvFullType_t& fulltype) const;

    object_type_e get_entitytype(const string& keyword) const;

    const string& get_entitystringtype(int entity_type) const;

    int get_all_domains() const;

    //dir_type_e get_direction(const char* dir, int is_extended);

    //const char* get_direction_str(dir_type_e dir, int is_extended);

    //int  get_directions(const char* dir, int* xdir_p, int* ydir_p, int* zdir_p);

    ///** Gets a string from direction flags */
    //const char* get_directions_str(int xdir, int ydir, int zdir);

    void getAllConfigTypes(int etype, vector< std::pair<unsigned int, string> >& aListConfig) const;

    void GetEntityDescriptors(int etype, vector< std::pair<string, const IDescriptor*> >& aListConfigIDescriptor) const;

    void GetAllTypesTitle(vector< std::pair<obj_type_e, string> >& vec_type_title) const;

    void GetUserDiscreteListFromType(unsigned int etype, map < string, vector< string> >& mapDiscreteList) const;

    void GetInternalDiscreteListFromType(unsigned int etype, map < string, vector< string> >& mapDiscreteList) const;

    void GetDiscreteListFromType(unsigned int etype, bool isuser, map < string, vector< string> >& mapDiscreteList) const;

    void GetConfigHMTypeCardImageFromKeyword(unsigned int etype, string& keyword, int* configType, int* hmType, string& cardImage) const;

    MvKeywordSet_t* getKeywords(object_type_e otype, const MvSubtype_t* subtype_p, MvKeywordSet_t* keywords_p) const;

    void getIdPool(int etype, int hm_config_type, int hm_type, string& cardimage, int* id_pool) const;

    void getChildKeywordListForGivenKeyword(int etype, string& keyword, vector<string>& keyword_list) const;

        /* make sure etype has cardimage*/
    void getChildCardImageListForGivenKeyword(int etype, string& card_image, vector<string>& card_image_list);

    void getKeywordTypeMap(map<string, obj_type_e>& keyword_type_map) const;

    bool getTypeHasSubtype(int etype) const;

    bool GetFirstUserNameForGvnEtypeUserID(const string& etype, int userid, string& firstusername) const;

    bool GetFirstUserNameForGvnEtypeConfigHMType(const string& etype, int config, int hmtype, string& firstusername) const;

    bool GetFirstUserNameForGvnEtypeKeyword(int etype, string& cardimage, string& firstusername) const;

    void GetMultiObjectTypes(const IDescriptor* descrp, int ikeyword, MvFullTypeSet_t& set) const;

    bool hasIdPool(int etype) const;

    string mv_get_cfg_file(const string& relative_path) const;

    // function to retrieve file path at pos, position in dir list
    string mv_get_cfg_file_at_pos(const string& relative_path, int pos) const;

    MvFileFormat_e  getSubUserProfile() const  {  return p_subUserProfile;  }

    ApplicationMode_e  getUserProfile() const  { return p_userProfile; }

    void setLatestVersion(string& version) { p_latestVersion = version; }

    const string& getLatestVersion() const { return p_latestVersion;  }

    /*Responsibility of the user to deallocate the memory*/
    MvDataUncondScalarFeature_t* GetAllocatedUnCondScalarFeatureHandle(const string& title, int  ikeyword, MvDimension_e dimension);

    /*Writing of descriptor contents*/
    void WriteDataHierarchyContents(string& filepath) const;

    void WriteDataHierarchyContentsRecursively(FILE* fp, const MvPreDatasHierarchy_t* data, int* tab_counter) const;

    void WriteDescrContent(obj_type_e a_type, string& keyword, string& filename) const;

    void PrintToFileDescriptor(obj_type_e etype, string& keyword, std::ostream& os) const;

    void PrintToFileAllDescriptors(int etype, std::ostream& os) const;

    int CheckUniqueness(const string& attrib, vector<string>& my_vectm, obj_type_e otype= HCDI_OBJ_TYPE_NULL) const;
    void getlKeywordListInfo(obj_type_e etype, const MvPreDatasHierarchy_t* data_cfg_p, std::map<std::string, CUserNameTypeInfo>& keywordlist) const;
    void getlKeywordListInfo(std::map<std::string, CUserNameTypeInfo>& keywordlist) const;
    void getDiscreteKeywordInfoMap(std::map<std::string, std::vector<CUserNameTypeInfo>>& descrete_lst) const;
    void getBaseStringKeywordSingleTypeMap(std::map<std::string, CUserNameTypeInfo>& resultMap) const;
    //@}

public:
    /*get/set functions for FLAGS*/
    int get_data_hierarchy_bitmask(const string& bit_string) const;

    int set_default_data_hierarchy_flag(const string& bit_string);

    int get_default_bit() const;
    void getlUserNamesSolverInfo(vector<pair<string, CUserNameTypeInfo>> (&objectsolverinfo)[HCDI_OBJ_TYPE_HC_MAX], bool add_user_subtype = false) const;
    void getlUserNamesSolverInfo(map<string, CUserNameTypeInfo>(&objectsolverinfo)[HCDI_OBJ_TYPE_HC_MAX], bool add_user_subtype = false) const;
    void getEntityOptionalStrings(int etype, const set<string>** p_opt_strings) const;
    void setSyntaxInfo(SyntaxInfo* pinfo) { p_syantaxInfo = pinfo; }
    SyntaxInfo* getSyntaxInfo() const { return p_syantaxInfo; }
private:
    mutable MvDescriptorMapArray_t*     p_descr_map_array_p   = nullptr;
    MvKeywords_t                        p_mv_keywords;
    const MvDescriptorMap_t*            p_descriptors_p = nullptr;

    mutable LocPreDataHierarchyMap_t*           p_cfg_map_p      = nullptr;
    mutable LocUserPreDescriptorMapArray_t*     p_pdma_p         = nullptr;
    MvTypeKeywordSubtypeMap_t           p_subtype_map;
    MvTypeUserIdSubtypeMap_t            p_user_subtype_map;
    MvHMConfigTypeSubtypeMap_t          p_hm_config_type_subtype_map;
    MvHMTypeSubtypeMap_t                p_hm_type_subtype_map;
    MvObjectTypeSet_t                   p_objectTypeSet;
   /* MvObjectTypeSet_t                   p_userTypeSet;*/

    MvDirList_t                         p_cfg_dirs;
    MvDirList_t                         p_version_dirs;
    mutable MvPreDatasHierarchy_t*              p_pUserDefinedHierarchy = nullptr;


    MvFileFormat_e                      p_subUserProfile = FF_UNKNOWN;
    ApplicationMode_e                   p_userProfile = HCDI_SOLVER_NONE;
    const string                        p_pathHome;
    string                              p_latestVersion;
    string                              p_cfgDirPath = "";       
    string                              p_unitsDirPath = "";     
    string                              p_verDirVar = "";        /* VER_DIR_VAR      */
    string                              p_userDefinedDHFilename;
    const string                        p_profile;          /*Incoming profile information*/
    /* map stores the flag string and allocated bit relationship */
    map<string, int> p_flag_bit_map;
    /* counter gets incremented everytime a new bit is assigned */
    int p_next_counter_bit = -1;
    /* default bit storage based on enabled flags */
    int p_default_bit = 0;
    SyntaxInfo* p_syantaxInfo = nullptr;
};
void GetSolverFolderHierachy(map<ApplicationMode_e, string>& appfolderhierachy, map<ApplicationMode_e, string> *append_map=nullptr);
HC_DATA_DLL_API MvFileFormat_e CFGKernelGetFileFormatFromString(ApplicationMode_e app, const string& partial_ff);

#endif
