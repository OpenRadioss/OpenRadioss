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


#ifndef MECI_MODEL_SCANNER_H
#define MECI_MODEL_SCANNER_H

#include <HCDI/hcdi_mec_pre_object.h>
#include "mec_component.h" 
#include "mec_offset.h"     

#include <UTILS/set_utils.h> 
#include <HCDI/hcdi_mec_pre_object.h>
#include "hcio.h"
#include "hcioi_solverinf.h"
/// Base class for creating a model
class HCIO_DATA_DLL_API MECIModelScanner {

 public: /** @name Constructors & destructor */
  //@{
  /// Constructor
  inline MECIModelScanner(bool is_merge_main=false,bool is_remove_submodel_main=false)
  {
     myCurrentOffsetPtr = NULL; 
     myIsMergeMain=is_merge_main; 
     myIsRemoveSubmodel=is_remove_submodel_main;
  } 
  /// Destructor
  virtual ~MECIModelScanner()
  { 
     if (myCurrentOffsetPtr)
         delete myCurrentOffsetPtr;
  } 
  //@}


  public: /** @name Managing current Offset)*/
  //@{
	  /// Gets the current offset for a given type
	  int virtual GetCurrentOffset(const char* otype) const=0;
	  /// Sets the current offset for a given type
	  void inline SetCurrentOffset(MECOffset* a_offset) {myCurrentOffsetPtr = a_offset;}
  //@}
	  

 public: /** @name Include files and components */
  //@{
  /// Initialize absolutes and relatives path according to include_policy (include files move with main files or not)  
  virtual void InitFilesPath(const string& main_file_path, int keep_include_policy) =0;
  virtual void InitFilesPathForDyna(const string& main_file_path ,int keep_include_policy) =0;
  /// Gets the number of include files (including main file)
  virtual int GetNbFiles() const=0;
  /// Gets the absolute name of the file of given index
  virtual const char *GetFileAbsoluteName(int file_ind) const=0; 
  /// Gets the relative name of the file of given index
  virtual const char *GetFileRelativeName(int file_ind) const=0;
   /// Gets the parent index of the file of given index
  virtual int GetFileParentIndex(int file_ind) const=0;
  /// Gets the component index of the file of given index
  virtual int GetFileComponentIndex(int file_ind) const =0;
  /// Gets the status of the file of given index 
  virtual int GetFileStatus(int file_ind) const =0;   
  /// Sets the file status of a given index
  virtual void SetFileStatus(int file_ind, int file_status)=0; // 
  /// Gets the number of sub-files of the file of given index
  virtual int GetNbSubFiles(int file_ind) const=0;
  /// Gets the number of sub-files of the file of given index
  virtual int GetSubFileIndex(int file_ind,int subfile_ind) const=0;
  /// Gets the number of components (including global component (id=0))
  virtual int GetNbComponents() const=0; 
   /// Gets the component by his index
  virtual MECComponent* GetComponent(int index)const=0;    
  /// Gets the next sub-component index of a component of given index 
  virtual int GetNextSubComponent(int index) const=0;     
  /// Gets the set of subcomponents of a component of given index 
  virtual set<int> GetSubComponent(int index) const=0;
  /// Gets the "IsMainMerge" property
  inline bool GetIsMergeMain() const {return myIsMergeMain;} 
  inline bool GetIsRemoveSubmodel() const {return myIsRemoveSubmodel;} 
  virtual bool IsInclude(int file_ind) const =0;
  virtual bool IsFileReferenced(int file_ind) const =0;

  virtual void  getIncludeOtherOptions(int file_ind,int *include_type, int *set_option,int *option, int * option_card, 
      int  *pid ,int  *thick ,int  *pstrn ,int  *strain ,int  *stress ,int  *incout ,double *rmax , 
      int  *ns_i ,int  *nc_i ,int  *tensor ,double  *thkscl ,
      int *isym ,  int *iafter ,double *xyz01 ,double *xyz02,double *xyz03 ,
      int *beamdf, int *shelldf,int *soliddf,
      double *r1,double*xp,double *r2,double*yp,double *r3,double*zp)=0;

  virtual   void  getTransfoInfo(int file_ind, int *idoffset,double * fct,string &fcttem,int *incout,int *tranid)=0;
    virtual  string GetParameterIdName(const char *otype,int comp_index ,int i) const=0;
  //@}

 public: /** @name Model data (control cards) */
  //@{
  /// Getting model data
  virtual IMECPreObject *GetModelData(const char *cc_type,IMECPreObject *pre_object_p=NULL) const=0;
  //@}

 public: /** @name Object data */
  //@{
  /// Getting the number of objects of given type for a given component
  virtual int  GetNbObjects(const char *otype, int comp_index) const=0; 
  /// Geting the number of part with orpan elements
  virtual int GetNbPartsOrphanElements(int comp_index) const=0; 
  /// Getting the object of given index in the given component 
  virtual IMECPreObject *GetObjectData(const char *otype,int comp_index,int i,IMECPreObject *pre_object_p=NULL,int IfApplyUnit =-1) const=0;
virtual IMECPreObject *GetIniCondData(const char *otype,int comp_index,int i,IMECPreObject *pre_object_p=NULL, char **prop_kwd=NULL) const=0;
  /// Getting the object of given index 
 virtual IMECPreObject *GetObjectData(const char *otype,int a_id,IMECPreObject *pre_object_p=NULL) const=0; 
 virtual IMECPreObject *GetObjectDataExt(const char *otype,int i,IMECPreObject *pre_object_p) const=0 ;
 /// Getting the node of given index and a given component index 
  virtual void GetNode(int comp_ind, int i,int *id_p,int *unit_p, 
		       double *x_p,double *y_p,double *z_p,
		       int *file_ind_p) const=0;
  /// Returns true if the node is a cnode
  virtual bool IsNodeCNode(int comp_index, int node_index) const=0; 
  /// Getting the part of given index
  virtual void GetPart(int comp_index, int i,int *id_p,char *title,int title_length_max, 
		       int *prop_id_p,int *mat_id_p,int *subset_id_p,
		       int *file_ind_p, int *has_thickness_p, double *thickness_p,bool *is_unresolved,
                               int *is_connection_part_out) const=0;
  /// Getting the part with orphan element's information of given index 
  virtual void GetPartOrphanElements(int comp_index, int i, int *part_id, int* part_comp_index, int* part_orphan_index) const=0; 
  
  /// Getting the connexion of given index
  virtual int  GetNbConnexion(int comp_index) const=0 ; 
  virtual int  GetConnexionIdBegin(void) =0 ;
  virtual int  GetConnexionNbElements(int comp_index, int i,int prop_index, int nb_prop, const char *otype) const=0 ; 
  virtual void GetConnexion(int comp_index, int i,int *nbr_part, int **id_p,char ***title,int title_length_max, 
		       int **prop_id_p,int **mat_id_p,int **subset_id_p,
		       int **file_ind_p, int *part_id_max) =0;
  virtual void GetConnexionElement(int comp_index, int i,const char *otype,int i_elt, int prop_index, int nb_prop, 
			      int *id_p,int *nb_nodes_p,int *node_ids,
			      int *file_ind_p,void *more_data_p=NULL,void * shell_angle_p=NULL) const=0; 
  
  /// Getting the part id for element type
  virtual int GetPartId(int comp_index, int i, const char *otype) const=0;
  /// Getting the number of elements of given type inside the part of given index
  virtual int  GetPartNbElements(int comp_index, int i,const char *otype) const=0; 
  /// Getting the element of given type and index inside the part of given index
  
   
  /// Getting the XELEM of given index inside the part of given index
  virtual IMECPreObject *GetPartXElement(int comp_index, int i,int i_elt,IMECPreObject *pre_object_p=NULL) const=0; 
  
  /// Getting the subset of given index
  virtual void GetSubset(int comp_index, int i,int *id_p,char *title,int title_length_max, 
			 int *nb_children_p,int *child_ids,
			 int *file_ind_p) const=0;
  /// Getting the number of child subsets of the subset of given index
  virtual int  GetSubsetNbChildSubsets(int comp_index, int i) const=0; 
    /// Getting the object index (in MODEL) from his type and his local index in component
  virtual int GetObjectIndex(int comp_index, int i, const char *otype) const=0; 
    
  /// Getting one object's associated object number
  virtual int GetObjectNbAssociatedObjects(const IMECPreObject &object,const char *ass_otype) const=0;
  /// Getting one object's associated object
  virtual bool GetAssociatedObject(const IMECPreObject &object,const char *ass_otype,int i,IMECPreObject *ass_object_p) const=0;
   
  /// Apply inverse transformation to a given component
  virtual void UntransformNodes(int comp_index) =0; 
  virtual void TransformNodes(int comp_index) =0; 

  /// Apply inverse transformation to functions
  virtual void UntransformFunctions() const=0;
  virtual void TransformFunctions() const=0;
  
  //@}

 public: /** @name Groups */
  //@{
  /// Returns true if the given sub-group of a given object is flagged
  virtual bool IsSubgroupFlaggedOrNull( int comp_index,               
						const IMECPreObject &object,
				       const char *skeyword,int ind=-1) const=0;

  virtual bool IsDefineBoxFlaggedOrNull( int comp_index,               
						const IMECPreObject &object,
				       const char *skeyword,int ind=-1) const=0;
  /// Flags the given sub-group of a given object
  void FlagSubgroup(const IMECPreObject &object,
			      const char *skeyword,int ind=-1); 
  void FlagDefineBox(const IMECPreObject &object,
			      const char *skeyword,int index_comp,int ind=-1);

  /// Flags the sub group of a given type and index
  virtual void FlagGroup(const char* otype, int index) =0; 
  virtual void FlagBox(int index,int index_comp) =0;
  /// Return true if a given group is flagged
  virtual bool IsGroupFlagged(const char* otype, int index_comp, int index)=0; 
  
  virtual bool IsDefineBoxFlagged(int index_comp,int index)=0;
  
  /// Gets the given sub-group of a given object
  virtual bool GetSubgroupData(const IMECPreObject &object, int comp_ind, 
			       const char *skeyword,IMECPreObject *subgroup_p) const=0;
  /// Gets the given sub-group of a given object
  virtual bool GetSubgroupData(const IMECPreObject &object, int comp_ind, 
				    const char *skeyword,int ind,IMECPreObject *subgroup_p) const=0;
  /// Gets the number of unflagged groups of given type
  virtual int  GetNbUnflaggedGroups(const char *otype, int index_comp) const=0; 
  /// Gets the number of unflagged groups of given type
  virtual void GetUnflaggedGroupData(const char *otype, int index_comp, int i,  IMECPreObject *group_p) const=0; 
  //@}
   /*---------Adhesive---------------*/
 public: /** @name Adhesives */
  //@{

  /// Flags the given  Adhesive object
  virtual void FlagAdhesive(const char* otype, int index=-1)=0;
 /// Return true if a given  Adhesive object is flagged
  virtual bool IsAdhesiveFlagged(const char* otype, int index_comp,int index=-1)=0;
  //@}
  /*---------Part id for Connection---------------*/
 public: /** @name Part id for Connection */
  //@{

  /// Flags the given  Part id
  virtual void FlagPartId(const int id) const=0 ;
 /// Return true if a given  Part id object is flagged
  virtual bool IsPartIdFlagged(const int id)const=0;
  //@}

  
 public: /** @name Crypting keys */
  //@{
  /// Gets the number of crypting keys
  virtual int GetNbCryptingKeys() const=0;
  /// Gets the crypting key reference of given index
  virtual const char *GetCryptingKeyRef(int i) const=0;
  /// Gets the crypting key of given index
  virtual const char *GetCryptingKey(int i) const=0;
  /// Gets the crypting key of given reference
  virtual const char *GetCryptingKey(const char *crypt_ref) const=0;
  
  virtual const char *GetCryptingKeyCost(int i) const=0;
  virtual const char *GetCryptingKeyCost (const char *crypt_ref) const=0;
  
  virtual const char *GetCryptingKeyFeature(int i) const=0;
  virtual const char *GetCryptingKeyFeature (const char *crypt_ref) const=0;
  virtual const char *GetCryptingKeyModule(int i) const=0;
  virtual const char *GetCryptingKeyModule (const char *crypt_ref) const=0;
  //@}
  

  
 public: /** @name Units */
  //@{
  /// Gets the number of quantities
  virtual int GetNbQuantities() const=0;
  /// Gets the quantity for the given index
  virtual const char *GetQuantity(int i) const=0;
  /// Gets the unit name for the given index
  virtual const char *GetUnitName(int i) const=0; 
  /// Gets the unit coeff for the given index
  virtual double GetUnitCoeff(int i) const=0;     
  /// Gets the unit offset for the given index
  virtual double GetUnitOffset(int i) const=0;    
  //@}
  public: /** @name Solver Work Units */
  //@{
  /// Gets the number of quantities
  virtual int GetNbWorkQuantities() const=0;
  /// Gets the quantity for the given index
  virtual const char *GetWorkQuantity(int i) const=0;
  /// Gets the unit name for the given index
  virtual const char *GetWorkUnitName(int i) const=0; 
  /// Gets the unit coeff for the given index
  virtual double GetWorkUnitCoeff(int i) const=0;     
  /// Gets the unit offset for the given index
  virtual double GetWorkUnitOffset(int i) const=0;    
  //@}
  

  
 public: /** @name Model Description */
  //@{
  /// Gets the number of foreign object
  virtual int GetNbForeignObjects() const=0;
  /// Gets the number of Model Description object
  virtual int GetNbModelDescription() const=0;
  /// Gets the Model Description object key of given index itab
  virtual void GetModelDescription(int itab, int *nbr_line , char ***line) const=0;

  virtual hwHCSolverInf* getHmSolverInf() const = 0;

  //@}
  

 
  protected:
	  MECOffset*   myCurrentOffsetPtr;
	   

	  bool myIsMergeMain; //RAR_HC_123_04_09_2008
      bool myIsRemoveSubmodel; //OC TC-HC-HM

};


#endif //MECI_MODEL_SCANNER_H
