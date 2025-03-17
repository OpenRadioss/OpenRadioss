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


#ifndef MV_MODEL_SCANNER_H
#define MV_MODEL_SCANNER_H

#include "meci_model_scanner.h"
#include <KERNEL_BASE/Structure_types.h>
#include <KERNEL_BASE/Structure_container.h>
#include <MUNITS/mu_unit_converter.h>
#include "hcioi_solverinf.h"
/// Base class for scanning a model
class HCIO_DATA_DLL_API MvModelScanner_t : public MECIModelScanner {

 public: /** @name Constructors & destructor */
  //@{
  /// Constructor
  MvModelScanner_t(void *model, const string& full_name, int moving_include_policy=0, bool is_merge_main=false,int mode=0,int component_moving_policy=0); 
  //MvModelScanner_t(hwEDI::ModelViewRead *model_p, const string& full_name); 
  MvModelScanner_t(hwHCSolverInf *p_solverinf, const string& full_name, int moving_include_policy = 0, bool is_merge_main = false, int mode = 0, int component_moving_policy = 0);

  /// Destructor
  virtual ~MvModelScanner_t();
  //@}

public: /** @name Managing local and global ID (Offset)*/
    //@{
    /// Gets the current offset for a given type
    int virtual GetCurrentOffset(const char* otype) const;
    //@}


public: /** @name Include files and components */
  //@{
  /// Initialize absolutes and relatives path according to include_policy (include files move with main files or not)  
  virtual void InitFilesPath(const string& main_file_path ,int keep_include_policy);
  virtual void InitFilesPathForDyna(const string& main_file_path ,int keep_include_policy);
  /// Gets the number of files (including main file)
  virtual int GetNbFiles() const;
 /// Gets the absolute name of the file of given index
  virtual const char *GetFileAbsoluteName(int file_ind) const; 
  /// Gets the relative name of the file of given index
  virtual const char *GetFileRelativeName(int file_ind) const;
  /// Gets the parent index of the file of given index
  virtual int GetFileParentIndex(int file_ind) const;
  /// Gets the component index of the file of given index
  virtual int GetFileComponentIndex(int file_ind) const;
  /// Gets the status of the file of given index 
  virtual int GetFileStatus(int file_ind) const;
    /// Sets the file status of a given index
  virtual void SetFileStatus(int file_ind, int file_status); // 
  /// Gets the number of sub-files of the file of given index
  virtual int GetNbSubFiles(int file_ind) const;
  /// Gets the number of sub-files of the file of given index
  virtual int GetSubFileIndex(int file_ind,int subfile_ind) const;
  /// Gets the number of components (including global component (id=0))
  virtual int GetNbComponents() const; 
  /// Gets the component by his index
  virtual MECComponent* GetComponent(int index)const;    
  /// Gets the next sub-component index of a component of given index 
  virtual int GetNextSubComponent(int index) const;
  /// Gets the set of subcomponents in a component of given index
  virtual set<int> GetSubComponent(int index) const;

  virtual bool IsInclude(int file_ind) const ;
  virtual bool IsFileReferenced(int file_ind) const; 
  virtual   void  getTransfoInfo(int file_ind, int *idoffset,double * fct,string &fcttem,int *incout,int *tranid);
  virtual void  getIncludeOtherOptions(int file_ind,int *include_type, int *set_option, int *option, int * option_card, 
      int  *pid ,int  *thick ,int  *pstrn ,int  *strain ,int  *stress ,int  *incout ,double *rmax , 
      int  *ns_i ,int  *nc_i ,int  *tensor ,double * thkscl,
      int *isym ,  int *iafter ,double *xyz01 ,double *xyz02,double *xyz03 ,
      int *beamdf, int *shelldf,int *soliddf,
      double *r1,double*xp,double *r2,double*yp,double *r3,double *zp);  
  //@}

 public: /** @name Model data (control cards) */
  //@{
  /// Getting model data
  virtual IMECPreObject *GetModelData(const char *cc_type,IMECPreObject *pre_object_p=NULL) const;
  //@}

 public: /** @name Object data */
  //@{
  /// Getting the number of objects of given type for a given component
  virtual int  GetNbObjects(const char *otype, int comp_ind) const;  
  /// Geting the number of part with orpan elements
  virtual int GetNbPartsOrphanElements(int comp_index) const; 
  /// Getting the object of given index in the given component
  virtual IMECPreObject *GetObjectData(const char *otype,int comp_index ,int i,IMECPreObject *pre_object_p=NULL,int IfApplyUnit =-1) const; 
   virtual IMECPreObject *GetIniCondData(const char *otype,int comp_index ,int i,IMECPreObject *pre_object_p=NULL, char **prop_kwd=NULL) const; 
 /// Getting the object of given id
 virtual IMECPreObject *GetObjectData(const char *otype,int a_id,IMECPreObject *pre_object_p=NULL) const; 

 virtual IMECPreObject *GetObjectDataExt(const char *otype,int i,IMECPreObject *pre_object_p) const ;

  /// Getting the node of given index
  virtual void GetNode(int comp_index, int i,int *id_p,int *unit_id_p,  
		       double *x_p,double *y_p,double *z_p,
		       int *file_ind_p) const;
  /// Returns true if the node is a cnode
  virtual bool IsNodeCNode(int comp_index, int node_index) const; 
  /// Getting the part of given index (now takes into account the Virtual thickness for RADIOSS 10.0 or greater)
  virtual void GetPart(int comp_index, int i,int *id_p,char *title,int title_length_max, 
		       int *prop_id_p,int *mat_id_p,int *subset_id_p,
		       int *file_ind_p, int *has_thickness_p, double *thickness_p,bool *is_unresolved,
                               int *is_connection_part_out) const;
  /// Getting the part with orphan element's information 
  virtual void GetPartOrphanElements(int comp_index, int i, int *part_id, int* part_comp_index, int* part_orphan_index) const; 
  /// Getting the part id for element type
  virtual int GetPartId(int comp_index, int i, const char *otype) const;
  /// Getting the number of elements of given type inside the part of given index
  virtual int  GetPartNbElements(int comp_index, int i,const char *otype) const; 
  /// Getting the element of given type and index inside the part of given index

  
  /// Getting the part of given index
  virtual int  GetNbConnexion(int comp_index) const ; 
  virtual int  GetConnexionIdBegin(void) ;
  virtual int  GetConnexionNbElements(int comp_index, int i,int prop_index, int nb_prop, const char *otype) const ; 
  virtual void GetConnexion(int comp_index, int i,int *nbr_part, int **id_p,char ***title,int title_length_max, 
		       int **prop_id_p,int **mat_id_p,int **subset_id_p,
		       int **file_ind_p, int *part_id_max) ;
  virtual void GetConnexionElement(int comp_index, int i,const char *otype,int i_elt, int prop_index, int nb_prop, 
			      int *id_p,int *nb_nodes_p,int *node_ids,
			      int *file_ind_p,void *more_data_p=NULL, void * shell_angle_p=NULL) const; 
   
  
  /// Getting the XELEM of given index inside the part of given index
  virtual IMECPreObject *GetPartXElement(int comp_index, int i,int i_elt,IMECPreObject *pre_object_p=NULL) const; 
  
  /// Getting the subset of given index
  virtual void GetSubset(int comp_index, int i,int *id_p,char *title,int title_length_max, 
			 int *nb_children_p,int *child_ids,
			 int *file_ind_p) const;
  /// Getting number of child subsets of the subset of given index
  virtual int  GetSubsetNbChildSubsets(int comp_index, int i) const; 
   /// Getting the object index (in MODEL) from his type and his local index in component
  virtual int GetObjectIndex(int comp_index, int i, const char *otype) const; 
  
  /// Getting one object's associated object number
  virtual int GetObjectNbAssociatedObjects(const IMECPreObject &object,const char *ass_otype) const;
  /// Getting one object's associated object
  virtual bool GetAssociatedObject(const IMECPreObject &object,const char *ass_otype,int i,IMECPreObject *ass_object_p) const;    
  
  /// Apply inverse transformation to a given component
   virtual void UntransformNodes(int comp_ind); 
   virtual void TransformNodes(int comp_index); 


  /// Apply inverse transformation to functions
  virtual void UntransformFunctions() const;
  virtual void TransformFunctions() const; 
  
  //@}

 public: /** @name Groups */
  //@{

  /// Flags the given group
  void FlagGroup(obj_type_e a_group_otype,int a_group_ind=-1);

  /// Returns true if the given sub-group of a given object is flagged
  virtual bool IsSubgroupFlaggedOrNull(int comp_index, const IMECPreObject &object,
				       const char *skeyword,int ind=-1) const;
  /// Returns true if the given subgroup (type BOX) of a given object is flagged
  virtual bool IsDefineBoxFlaggedOrNull(int comp_index, const IMECPreObject &object,
				       const char *skeyword,int ind=-1) const;

  // Flags the sub group of a given type and index
   virtual void FlagGroup(const char* otype, int index); 

  // Flags the sub group (type BOX) of a given type and index
   virtual void FlagBox(int index,int index_comp); 
  
  /// Return true if a given group is flagged
  virtual bool IsGroupFlagged(const char* otype, int index_comp, int index); 
   
  /// Return true if a given group (type BOX) is flagged
  virtual bool IsDefineBoxFlagged(int index_comp,int index);

  /// Gets the given sub-group of a given object
  virtual bool GetSubgroupData(const IMECPreObject &object, int comp_ind, 
				    const char *skeyword,IMECPreObject *subgroup_p) const;
  /// Gets the given sub-group of a given object
  virtual bool GetSubgroupData(const IMECPreObject &object, int comp_ind, 
				    const char *skeyword,int ind,IMECPreObject *subgroup_p) const;
  /// Gets the number of unflagged groups of given type
  virtual int  GetNbUnflaggedGroups(const char *otype, int index_comp) const; 
  /// Gets the number of unflagged groups of given type
  virtual void GetUnflaggedGroupData(const char *otype, int index_comp, int i,IMECPreObject *group_p) const; 
  //@}
 
  /*---------Adhesive---------------*/
 public: /** @name Adhesives */
  //@{

  /// Flags the given Adhesive object
  void FlagAdhesive(obj_type_e a_otype,int a_ind=-1);
  /// Flags the given  Adhesive object
  virtual void FlagAdhesive(const char* otype, int index=-1) ;
 /// Return true if a given  Adhesive object is flagged
  virtual bool IsAdhesiveFlagged(const char* otype, int index_comp,int index=-1);
  //@

  /*---------Part id for Connection---------------*/
 public: /** @name Part id for Connection */
  //@{
  /// Flags the given  Part id
  virtual void FlagPartId(const int id) const ;
 /// Return true if a given  Part id object is flagged
  virtual bool IsPartIdFlagged(const int id) const;
  //@}

  
 public: /** @name Crypting keys */
  //@{
  /// Gets the number of crypting keys
  virtual int GetNbCryptingKeys() const;
  /// Gets the crypting key reference of given index
  virtual const char *GetCryptingKeyRef(int i) const;
  /// Gets the crypting key of given index
  virtual const char *GetCryptingKey(int i) const;
  /// Gets the crypting key of given reference
  virtual const char *GetCryptingKey(const char *crypt_ref) const;
  
  virtual const char *GetCryptingKeyCost(int i) const;
  virtual const char *GetCryptingKeyCost(const char *crypt_ref) const;
  
  virtual const char *GetCryptingKeyFeature(int i) const;
  virtual const char *GetCryptingKeyFeature(const char *crypt_ref) const;
  virtual const char *GetCryptingKeyModule(int i) const;
  virtual const char *GetCryptingKeyModule(const char *crypt_ref) const;
  //@}
  

  
 public: /** @name Units */
  //@{
  /// Gets the number of quantities
  virtual int GetNbQuantities() const;
  /// Gets the quantity for the given index
  virtual const char *GetQuantity(int i) const;
  /// Gets the unit name for the given index
  virtual const char *GetUnitName(int i) const; 
  /// Gets the unit coeff for the given index
  virtual double GetUnitCoeff(int i) const;     
  /// Gets the unit offset for the given index
  virtual double GetUnitOffset(int i) const;    
  //@}
  public: /** @name Solver Work Units */
  //@{
  /// Gets the number of quantities
  virtual int GetNbWorkQuantities() const;
  /// Gets the quantity for the given index
  virtual const char *GetWorkQuantity(int i) const;
  /// Gets the unit name for the given index
  virtual const char *GetWorkUnitName(int i) const; 
  /// Gets the unit coeff for the given index
  virtual double GetWorkUnitCoeff(int i) const;     
  /// Gets the unit offset for the given index
  virtual double GetWorkUnitOffset(int i) const;    
  //@}
  
  
 public: /** @name Model Description */
  //@{
  /// Gets the number of foreign object
  virtual int GetNbForeignObjects() const;
  /// Gets the number of Model Description object
  virtual int GetNbModelDescription() const;
  /// Gets the Model Description object key of given index itab
  virtual void GetModelDescription(int itab, int *nbr_line , char ***line) const;
  /// Gets the Parameter Name
  virtual  string GetParameterIdName(const char *otype,int comp_index ,int i) const;
  //@}
  
  
 public: /** @name unit converters */
  //@{
  /// Get unit converter pointer using id of local unit object
  MuUnitConverter_t *GetConverterByUnitId(int id) {return myConverterList[id];}
  /// Get unit un-converter pointer using id of local unit object
  MuUnitConverter_t *GetUnConverterByUnitId(int id) {return myUnConverterList[id];}
  /// Initialize the map of id(local unit object) and unit converter pointers
  virtual void InitializeConverterList(void *a_model);
  /// Get unit converter pointer from unit object
  virtual void GetConverter(void *unit_ent_ptr, MuUnitConverter_t **converter, MuUnitConverter_t **unconverter=NULL) const;
  //@}

 private: /* Object data (private) */
  // Getting /SURF/SEG or /LINE/SEG data
  void getSegmentGroupData(const void *group_ent_ptr,IMECPreObject *pre_object_p) const;
  // Getting object of given type, given index, and given component index. If comp_index==-1 returns the object with the given index in the model 
  //MvObject_t getObject(obj_type_e obj_type,int comp_index ,int index_in_array) const; 
  void getObjectData(const void *a_object, int a_domains, obj_type_e a_otype, IMECPreObject *pre_object_p) const;

  private:
      class MyFile_t {
      public:
          inline MyFile_t(const string& full_name = "", const string& relative_name = "",
              int parent_index = 0, int a_comp_index = 0,
              int status_val = 1, bool isInclude = true, bool isReferenced = false) :
              myFullName(full_name),
              myRelativeName(relative_name),
              myParentIndex(parent_index),
              myComponentIndex(a_comp_index),
              myStatus(status_val), 
              myIsInclude(isInclude),
              myIsReferenced(isReferenced), 
              include_type(0),
              pid(0),
              thick(0),
              pstrn(0),
              strain(0),
              stress(0),
              incout_1(0),
              rmax(0.),
              tensor(0),
              thkscl(0.),
              isym(0),
              iafter(0),
              beamdf(0),
              shelldf(0),
              soliddf(0),
              xp(0.),
              yp(0.),
              zp(0.),
              option(0),
              set_option(0),
              option_card(0),
              incout(0),
              tranid(0)
          {
              ns_i[0] = 0;
              ns_i[1] = 0;
              ns_i[2] = 0;
              nc_i[0] = 0;
              nc_i[1] = 0;
              nc_i[2] = 0;

              xyz01[0] = 0.;
              xyz01[1] = 0.;
              xyz01[2] = 0.;
              xyz02[0] = 0.;
              xyz02[1] = 0.;
              xyz02[2] = 0.;
              xyz03[0] = 0.;
              xyz03[1] = 0.;
              xyz03[2] = 0.;
              r1[0] = 0.;
              r1[1] = 0.;
              r1[2] = 0.;
              r2[0] = 0.;
              r2[1] = 0.;
              r2[2] = 0.;
              r3[0] = 0.;
              r3[1] = 0.;
              r3[2] = 0.;

              idoffset[0] = 0;
              idoffset[1] = 0;
              idoffset[2] = 0;
              idoffset[3] = 0;
              idoffset[4] = 0;
              idoffset[5] = 0;
              idoffset[6] = 0;
              idoffset[7] = 0;
              fct[0] = 0.;
              fct[1] = 0.;
              fct[2] = 0.;
          }
  public:
      string        myFullName;
      string        myRelativeName;
      int           myParentIndex;
      int           myComponentIndex;
      int           myStatus;             
      bool          myIsReferenced;  

      int include_type;  
      int  pid ;
      int  thick ;
      int  pstrn ;
      int  strain ;
      int  stress ;
      int  incout_1 ;
      double rmax ;
      /* card 3 */
      int  ns_i[3] ;
      int  nc_i[3] ;
      int  tensor ;
      double  thkscl ;
      /* card 4 */
      int isym ;
      int iafter ;
      double xyz01[3] ;
      double xyz02[3] ;
      double xyz03[3] ;
      /*** Fields for sub-option NASTRAN ***/
      int beamdf;
      int shelldf;
      int soliddf;
      /*matrix*/
      double r1[3];
      double xp;
      double r2[3];
      double yp;
      double r3[3];
      double zp;               
      int option;
      int set_option;
      int option_card;                

          
          bool         myIsInclude;
          int          idoffset[8];
          double       fct[3];
          string        fcttem;
          int           incout;
          int          tranid;
         
	};

  typedef vector<MyFile_t> MyFileVec_t;


	private:
	  class MyFlaggedGroupIndex_t 
	{
	public: 
	  inline MyFlaggedGroupIndex_t()  {myCurrentGroupIndex=-1; myCurrentCompUnflaggedGroupIndex=-1;}
	  
	public:
	  int				myCurrentGroupIndex;
	  int				myCurrentCompUnflaggedGroupIndex;
	};
 
	typedef vector<MyFlaggedGroupIndex_t> MyComponentFlaggedGroupIndex_t;

 public: /* Tools */
  inline void *getModel() const { return myModelPtr; }
  virtual hwHCSolverInf  *getHmSolverInf() const { return mysolverinf; }

 private: /* Tools */
  obj_type_e              getOtype(const char *otype) const;
  int                     getFileIndex(const void *ent_object) const;
  int                     getComponentIndex(const void *ent_object) const; 
  void                    sortParts(flag_sort_e sort_flag); 
  int                     getCurrentGroupIndex(int comp_index) const;                
  void                    setCurrentGroupIndex(int comp_index, int a_index) const;   
  int                     getCurrentUnflaggedGroupIndex(int comp_index) const;       
  void                    setCurrentUnflaggedGroupIndex(int comp_index, int a_index) const;  

   
 private: /* Types */
  typedef vector<int>                       MyFlagArray_t;
  typedef map<obj_type_e, MyFlagArray_t>  MyGroupFlags_t;
  typedef map<int,MyFlagArray_t>  MyDefineBoxFlags_t;
  typedef const char                       *MyConstCharPtr_t;


  typedef vector<MECComponent*>            MyComponentVect_t;        
  typedef map<int, MuUnitConverter_t*>     MyUnitConverterList_t;
  typedef map<int,int>                     MyMapFlags_t;
 private:
 
    int part_id_max; //save the max part id while creating and writting the connections

private:
  void                           *myModelPtr;
  mutable MyConstCharPtr_t        myCurrentOTypeStr;
  mutable obj_type_e              myCurrentOType;
  //
  mutable int                     myCurrentPartInd;

  mutable int                     myCurrentComponentPartInd;      

  MyGroupFlags_t                  myGroupFlags;
  MyDefineBoxFlags_t              myDefineBoxFlags; 
  mutable int                     myCurrentUnflaggedGroupInd;
  mutable int                     myCurrentGroupInd;
  //
  string                          myD0ARelativeName;

  MyUnitConverterList_t           myConverterList;
  MyUnitConverterList_t           myUnConverterList;
  //
   //string                          myD0ARelativeName; 
  //

  MyComponentVect_t               myComponentVect;     
  MyFileVec_t                     myFileVect;        
  mutable MyComponentFlaggedGroupIndex_t  myComponentGroupFlagIndex; 
  mutable MyFlagArray_t           myCurrentOrphanPartVectIndex; 
  MyGroupFlags_t                  myAdhesiveFlags;
  typedef vector<int>             MyAllPartIds_t;
  public:
  mutable MyAllPartIds_t          myAllPartIds;
  mutable MyMapFlags_t            myPartIgdsGlobalFlag;
  hwHCSolverInf                   *mysolverinf;
};


#endif //MV_MODEL_SCANNER_H
