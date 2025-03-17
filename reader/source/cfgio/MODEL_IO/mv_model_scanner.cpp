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
 
#include <UTILS/str_utils.h>
#include <UTILS/file_utils.h> 
#include <UTILS/mv_string.h>
#include <KERNEL/mv_type.h>
#include <KERNEL/mv_full_type.h>
#include <HCDI/hcdi_mv_descriptor.h>

#include <cstring>
#include "mv_model_scanner.h"


static void sort_array_indexes ( int obj_indexes_tab[], int n, void *iModel );

static void PostTreatPropTshell(string a_kftype, IMECPreObject *a_pre_object_p) ;

const char *remove_initial_blanks(const char *str)
{
  const char *a_char_p = str;
  while(*a_char_p==' ') ++a_char_p;
  return a_char_p;
}

static void sort_array_indexes ( int obj_indexes_tab[], int n, void *iModel )
{
    //int i, tmp, permutation = 1;
    //int order1 = -1, order2 = -1;
    //while (permutation==1)
    //{
    //    permutation = 0;
    //    for (i=0; i<n-1; i++)
    //    {
    //        MvObject_t Transform1 = iModel.getObject ( MCDS_TRANSFORM, obj_indexes_tab[i] );
    //        MvObject_t Transform2 = iModel.getObject ( MCDS_TRANSFORM, obj_indexes_tab[i+1] );
    //        order1 = Transform1.getIntAttribute ("ORDER");
    //        order2 = Transform2.getIntAttribute ("ORDER");
    //        //if ( obj_indexes_tab[i]>obj_indexes_tab[i+1] )
    //        if ( order1>order2 )
    //        {
    //            tmp = obj_indexes_tab[i];
    //            obj_indexes_tab[i] = obj_indexes_tab[i+1];
    //            obj_indexes_tab[i+1]= tmp;
    //            permutation = 1;
    //        }
    //    }
    //}
}



/* --------- Constructors & destructor --------- */

MvModelScanner_t::MvModelScanner_t(void *model, const string& full_name, int include_moving_policy, bool is_merge_main,int mode, int component_moving_policy) :  

MECIModelScanner(is_merge_main), 
myModelPtr(model),
myCurrentOTypeStr(NULL),
myCurrentOType(HCDI_OBJ_TYPE_NULL),
myCurrentPartInd(-1),
myCurrentComponentPartInd(-1),                
myGroupFlags(),
myAdhesiveFlags(),
part_id_max(0),
myCurrentUnflaggedGroupInd(-1),
myCurrentGroupInd(-1),
mysolverinf(NULL)
{
    //myComponentGroupFlagIndex= 0; //model->getNbComponents()
    //myComponentVect=0;// model->getNbComponents();
}

MvModelScanner_t::MvModelScanner_t(hwHCSolverInf *p_solverinf, const string& full_name, int include_moving_policy, bool is_merge_main, int mode, int component_moving_policy) :  
                                                                                                                                                                                    
    MECIModelScanner(is_merge_main),
    mysolverinf(p_solverinf),
    myCurrentOTypeStr(NULL),
    myCurrentOType(HCDI_OBJ_TYPE_NULL),
    myCurrentPartInd(-1),
    myCurrentComponentPartInd(-1),
    myGroupFlags(),
    myAdhesiveFlags(),
    part_id_max(0),
    myModelPtr(NULL),
    myCurrentUnflaggedGroupInd(-1),
    myCurrentGroupInd(-1)
{
    //myComponentGroupFlagIndex= 0; //model->getNbComponents()
    //myComponentVect=0;// model->getNbComponents();
}

MvModelScanner_t::~MvModelScanner_t() {
    sortParts(SORT_PTR); 
}


/* --------- Managing local and global ID  -------- */

int MvModelScanner_t::GetCurrentOffset(const char* otype) const
{
    if(myCurrentOffsetPtr != NULL)
        return myCurrentOffsetPtr->GetOffset(otype);
    return -1;
}

void MvModelScanner_t::InitFilesPathForDyna(const string& new_main_file_path ,int include_moving_policy)
{

}
/* --------- Include files and components--------- */

// Initialize absolutes and relatives path according to include_policy (include files move with main files or not) 
// include_moving_policy 0   -> Main file did not move -> nothing to do
// include_moving_policy 1   -> Main file moved but Include files stay in the directory set in HC Session 
// include_moving_policy 2   -> Main file moved and Include files move at the same location
void MvModelScanner_t::InitFilesPath(const string& new_main_file_path, int include_moving_policy)
{
    //const MvModel_t      &a_model   = getModel();
    const string a_old_main_path = 0;//a_model.getFilePathName(0);   
    int a_nb_files = 0;//a_model.getNbFiles();

    if(include_moving_policy == 3) 
        myIsMergeMain = true;
    else
        myIsMergeMain = false;

    // if all the files are merged in main while exporting, treat only the main
    if(myIsMergeMain)
        a_nb_files=1;

    myFileVect.reserve(a_nb_files);
    //
    // according to keep_include_dir_policy,  init absolute and relative files data
    for( int ifile=0; ifile<a_nb_files;ifile++)
    {
        int parent_index = 0;//a_model.getFileParent(ifile);
        int comp_index = 0;//a_model.getFileComponentIndex(ifile);
        int file_status=0;//a_model.getFileStatus(ifile); 
        bool is_referenced = false;//a_model.isReferenced(ifile,MCDS_INCLUDE); 

        //
        string a_new_file_full_name =  "";//a_model.getFileName(ifile);   /* name of the file to be opened */
        string a_new_file_path_name =  "";//a_model.getFilePathName(ifile);  /* name of the file in parent include :  #include a_new_file_path_name */
        const string an_old_file_full_name ="";// a_model.getFileName(ifile);
        const string an_old_file_path_name = "";//a_model.getFilePathName(ifile);
        //
        
        // it is important to understand that: 
        // a_new_file_full_name will be the name of the file that will be "open" to write
        // a_new file path_name will be the reference of the file in it parent (#include .....)
        // ->  
        // -  a_new_file_full_name must be an absolute path
        // -  a_new_file_path_name and a_new_file_full_name must refer to some files!

        // Main is treated independantly of include_moving_policy
        if(ifile==0)
        {
            if(include_moving_policy!=4)
            {
            a_new_file_full_name = new_main_file_path;
            a_new_file_path_name = new_main_file_path;
                // if not in single expot case, status is set to 0
            file_status = 0; 
        }
        }
        else
        {
            //
            bool is_relative = my_is_file_path_relative(an_old_file_path_name);
            bool is_file_export = true;//(a_model.getFileStatus(ifile)<2) ? true: false;
            bool is_file_read_only =false;//(a_model.getFileAccess(ifile)==4) ? true:false;

            bool do_not_move_includes = false;
            switch(include_moving_policy)
            {
            case 0: //-> Main file did not move 
                {
                    do_not_move_includes = true;

                    // file with export status and read write will be really export
                    if(is_file_export && !is_file_read_only)
                        file_status=1;
                    else
                        file_status=2;
                }
                break;
            case 1: //  -> Main file moved but Include files stay in the directory set in HC Session 
                {
                    string new_main_path = my_get_dir_path(new_main_file_path);
                    new_main_path = my_get_dir_modify_end_path(new_main_path, true);
                    if(is_file_export)
                    {
                        // non data_base include file stay at their location but if the path was relative it becomes absolute
                        if(is_relative)
                        {
                            // modify old full name (concatenation of a_old_main_path and an_old_file_full_name)
#ifdef WIN32
                            bool is_unix=false;
#else
                            bool is_unix=true;
#endif
                            string a_tmp_string=  "";//my_file_full_name_modify_with_absolute(an_old_file_full_name, new_main_path, is_unix);
                            string new_dir_path = my_get_dir_path(a_tmp_string);
                            new_dir_path = my_get_dir_modify_end_path(new_dir_path, true);
                            //if(!my_dir_exists(new_dir_path))
                            //    my_create_file_path(new_dir_path, "", false);
                            a_new_file_full_name = a_tmp_string;
                            a_new_file_path_name =  an_old_file_full_name;
                        }
                        else
                        {
                            a_new_file_full_name = a_new_file_full_name;
                            a_new_file_path_name = a_new_file_full_name;
                        }
                    }
                    else
                    {
                        // the data_base include files won't be written but they need to be declared in parent header
                        // in absolute path
                        if(is_relative)
                        {
                            string a_base_name = my_get_file_basename(an_old_file_full_name);

                            for(int i=0; i< new_main_path.size(); i++)
                            {
                                if(new_main_path[i] == '\\')
                                    new_main_path[i] = '/';
                            }
                            for(int i=0; i< a_new_file_path_name.size(); i++)
                            {
                                if(a_new_file_path_name[i] == '\\')
                                    a_new_file_path_name[i] = '/';
                            }

                            // get full path(new main + new file path)
                            string a_tmp_string=  my_file_full_name_modify_with_relative(new_main_path, a_new_file_path_name);
                            // convert to relative
                            //a_new_file_full_name = my_file_get_relative_path_from_main(a_tmp_string, new_main_path);

                            if(a_new_file_full_name == ".\\" || a_new_file_full_name == "./")
                                a_new_file_full_name ="";
                            a_new_file_full_name=my_file_full_name_append(a_new_file_full_name, a_base_name);
                            a_new_file_path_name = a_new_file_full_name;
                        }
                        else
                        {
                            a_new_file_path_name = a_new_file_full_name;
                        }
                    }
                    // file with export status and read write will be really export
                    if(is_file_export && !is_file_read_only)
                        file_status=1;
                    else
                        file_status=2;
                }
                break;
            case 2:
                {
                    
                    // if file is exported or if it is not exported because it is a referenced
                    if(is_file_export||(!is_file_export&&is_referenced))
                    {
                        // all exported include file move with main and becomes absolutee
                        a_new_file_path_name = my_get_file_basename(an_old_file_full_name);
                        if(ifile==0)
                            a_new_file_full_name = new_main_file_path;
                        else
                        {
                            string file_full_name = my_get_dir_path(new_main_file_path);
                            string file_full_name_modif = my_get_dir_modify_end_path(file_full_name,true);
                            a_new_file_full_name = my_file_full_name_append(file_full_name_modif,a_new_file_path_name);
                        }
                    }
                    
                    else
                    {
                        do_not_move_includes = true;
                    }
                    //
                    
                    break;
                }
            case 4:
                {
                    // the file that is not referenced and not in do not export status will
                    // be modified
                    if(is_file_export && !is_referenced)
                        a_new_file_full_name = new_main_file_path;
                    if(!is_file_export && is_referenced)
                        a_new_file_path_name = a_new_file_full_name;
                    break;
                }

            default:
                break;
            }

            if(do_not_move_includes)
            {
                // base_name
                string a_base_name = my_get_file_basename(an_old_file_full_name);
                // whatever the file is (relative / absolute), 
                // to obtain  a_new_file_path_name, append  old path name and  basename
                a_new_file_path_name = my_file_full_name_append(an_old_file_path_name, a_base_name);

                // if the file is relative,
                if(is_relative)
                {
                    // the tmp path name is obtain by relative modification of the old main path
                    string a_tmp_string=  my_file_full_name_modify_with_relative(a_old_main_path, an_old_file_path_name);
                    //
                    //to obtain a_new_file_full_name, append of a_tmp_string and a_base_name 
                    a_new_file_full_name=my_file_full_name_append(a_tmp_string, a_base_name); 
                }
                // if absolute, a_new file_full_name won't change

                //if data_base the path must be absolute: ie set to a_new_file_full_name
                if(!is_file_export)
                    a_new_file_path_name = a_new_file_full_name;
            }
        }


#if 0	
        cout << "FILE["   << ifile << "] = {" << endl
            << "  a_new_file_full_name = \"" << a_new_file_full_name << "\";" << endl
            << "  a_new_file_path_name = \"" << a_new_file_path_name << "\";" << endl
            << "  parent_index  = "   << parent_index  << ";" << endl
            << "  comp_index    = "     << comp_index  << ";" << endl
            << "}" << endl;
#endif

        
        
        // if file status is not already 2 (do not export) , modify it if newly written file is protected
        if(file_status<2)
        {
            int an_acces = my_get_file_access("",a_new_file_full_name);
            if(an_acces==-1)
            {
                string path_dir = my_get_dir_path(a_new_file_full_name);
                //if(!my_is_dir_writable(path_dir))
                    file_status=2;
            }
            else if(an_acces!=2) //W_OK = 2 
                file_status=2;
        }
        
        myFileVect.push_back(MyFile_t(a_new_file_full_name, a_new_file_path_name, parent_index, comp_index, file_status,true, is_referenced));
        
    }
}


int MvModelScanner_t::GetNbFiles() const {
    
    int nb_files= (int)myFileVect.size();
    return nb_files;
    
}


const char *MvModelScanner_t::GetFileAbsoluteName(int file_ind) const {
    
    int nb_files = GetNbFiles();
    const char* a_file_name = NULL;
    if(file_ind>=0 && file_ind<nb_files)
        a_file_name = myFileVect[file_ind].myFullName.c_str();
    return a_file_name;
    
}


const char *MvModelScanner_t::GetFileRelativeName(int file_ind) const {
    int nb_files = GetNbFiles();
    const char* a_file_name = NULL;
    if(file_ind>=0 && file_ind<nb_files)
        a_file_name = myFileVect[file_ind].myRelativeName.c_str();
    return a_file_name;
}



int MvModelScanner_t::GetFileParentIndex(int file_ind) const {
    int index_parent = -1;
    int nb_files = GetNbFiles();
    if(file_ind>=0 && file_ind<nb_files)
        index_parent = myFileVect[file_ind].myParentIndex;
    return index_parent;
}


bool MvModelScanner_t::IsInclude(int file_ind) const {
    bool isinclude = true;
    int nb_files = GetNbFiles();
    if(file_ind>=0 && file_ind<nb_files)
        isinclude = myFileVect[file_ind].myIsInclude;
    return isinclude;
}


bool MvModelScanner_t::IsFileReferenced(int file_ind) const {
    bool is_referenced = true;
    int nb_files = GetNbFiles();
    if(file_ind>=0 && file_ind<nb_files)
        is_referenced = myFileVect[file_ind].myIsReferenced;
    return is_referenced;
}



void MvModelScanner_t::getTransfoInfo(int file_ind, int *idoffset,double * fct,string &fcttem,int *incout,int *tranid)
{
    int nb_files = GetNbFiles();
    if(file_ind>=0 && file_ind<nb_files){
        for(int i=0;i<8;i++)
            idoffset[i] = myFileVect[file_ind].idoffset[i];

        for(int i=0;i<3;i++)
            fct[i] = myFileVect[file_ind].fct[i];

        fcttem = myFileVect[file_ind].fcttem;
        *incout = myFileVect[file_ind].incout;
        *tranid = myFileVect[file_ind].tranid;
    }
}

void  MvModelScanner_t::getIncludeOtherOptions(int file_ind,int *include_type, int *set_option, int *option, int * option_card, 
      int  *pid ,int  *thick ,int  *pstrn ,int  *strain ,int  *stress ,int  *incout ,double *rmax , 
      int  *ns_i ,int  *nc_i ,int  *tensor ,double * thkscl,
      int *isym ,  int *iafter ,double *xyz01 ,double *xyz02,double *xyz03 ,
      int *beamdf, int *shelldf,int *soliddf,
      double *r1,double *xp,double *r2,double *yp,double *r3,double *zp)
{
    int nb_files = GetNbFiles();
    if(file_ind>=0 && file_ind<nb_files){
      *include_type = myFileVect[file_ind].include_type;
      *set_option = myFileVect[file_ind].set_option;
      *option = myFileVect[file_ind].option;
      *option_card = myFileVect[file_ind].option_card;
                  
      if(*include_type ==1||*include_type ==3)
      {  
         *pid = myFileVect[file_ind].pid ;
         *thick = myFileVect[file_ind].thick ;
         *pstrn = myFileVect[file_ind].pstrn ;
         *strain = myFileVect[file_ind].strain ;
         *stress = myFileVect[file_ind].stress ;
         *incout = myFileVect[file_ind].incout_1 ;
         *rmax = myFileVect[file_ind].rmax ;
         
      if(*option ==2||*option ==3||*option ==6||*option ==7)
      {
         for(int i=0;i<3;i++) 
            r1[i] = myFileVect[file_ind].r1[i];
         for(int i=0;i<3;i++) 
            r2[i] = myFileVect[file_ind].r2[i];         
         for(int i=0;i<3;i++)
            r3[i] = myFileVect[file_ind].r3[i];                   
         *xp = myFileVect[file_ind].xp;
         *yp = myFileVect[file_ind].yp;
         *zp = myFileVect[file_ind].zp;
      }
      else
      {         
         /* card 3 */
         for(int i=0;i<3;i++)         
            ns_i[i] = myFileVect[file_ind].ns_i[i];
            
         for(int i=0;i<3;i++)   
            nc_i[i] = myFileVect[file_ind].nc_i[i] ;
            
         *tensor = myFileVect[file_ind].tensor ;
         *thkscl = myFileVect[file_ind].thkscl;

       }
         /* card 4 */
      if(*option_card==1)
      {
         *isym = myFileVect[file_ind].isym ;
         *iafter = myFileVect[file_ind].iafter;
         
         for(int i=0;i<3;i++)
         xyz01[i] = myFileVect[file_ind].xyz01[i];
         
         for(int i=0;i<3;i++)         
         xyz02[i] = myFileVect[file_ind].xyz02[i];
         
         for(int i=0;i<3;i++)         
         xyz03[i] = myFileVect[file_ind].xyz03[i];
      }       
      }
      else if(*include_type ==2)
      {
         /*** Fields for sub-option NASTRAN ***/
         *beamdf = myFileVect[file_ind].beamdf;
         *shelldf = myFileVect[file_ind].shelldf;
         *soliddf = myFileVect[file_ind].soliddf; 
      } 
    }
}



int MvModelScanner_t::GetFileComponentIndex(int file_ind) const { 
    int index_comp = -1;
    int nb_files = GetNbFiles();
    if(file_ind>=0 && file_ind<nb_files)
        index_comp = myFileVect[file_ind].myComponentIndex;
    return index_comp;
}



int MvModelScanner_t::GetFileStatus(int file_ind) const {
    int file_status=-1;
    int nb_files = GetNbFiles();
    if(file_ind>=0 && file_ind<nb_files)
        file_status = myFileVect[file_ind].myStatus;
    return file_status;
}



void  MvModelScanner_t::SetFileStatus(int file_ind, int file_status) {
    int nb_files = GetNbFiles();
    if(file_ind>=0 && file_ind<nb_files)
        myFileVect[file_ind].myStatus=file_status;
}


int MvModelScanner_t::GetNbSubFiles(int file_ind) const {
    if(file_ind!=0) return 0;
    return GetNbFiles()-1;
}

int MvModelScanner_t::GetSubFileIndex(int file_ind,int subfile_ind) const {
    if(file_ind!=0) return -1;
    return subfile_ind+1;
}

int MvModelScanner_t::GetNbComponents() const {
    int nb_components=0;
    nb_components= (int)myComponentVect.size();
    return nb_components;
}



MECComponent* MvModelScanner_t::GetComponent(int index)const {
    MECComponent* a_comp_p = NULL;
    int nb_components = GetNbComponents();
    if(index>=0 && index<nb_components)
        a_comp_p = myComponentVect[index];
    return a_comp_p;
}



int MvModelScanner_t::GetNextSubComponent(int index)const{

    //const MvModel_t      &a_model   = getModel();
    return 0;//a_model.getNextSubComponent(index);
}

set<int> MvModelScanner_t::GetSubComponent(int index) const
{
    //const MvModel_t      &a_model   = getModel();
    set<int> a_set;
    return a_set;//a_model.getSubComponent(index);
}




/* --------- Model data (control cards) --------- */

IMECPreObject *MvModelScanner_t::GetModelData(const char *cc_type,IMECPreObject *pre_object_p) const {
    IMECPreObject *a_pre_object_p = NULL;
    return a_pre_object_p;
}


/* --------- Object data --------- */

int  MvModelScanner_t::GetNbObjects(const char *otype, int comp_index) const {
    int nb_obj = 0;
    return nb_obj;
}



/// Geting the number of part with orpan elements
int MvModelScanner_t::GetNbPartsOrphanElements(int comp_index) const
{
    return (int)myCurrentOrphanPartVectIndex.size();
}
 

    
IMECPreObject *MvModelScanner_t::GetObjectData(const char *otype,int comp_index ,int i,IMECPreObject *pre_object_p,int IfApplyUnit) const { 
    IMECPreObject *a_pre_object_p = NULL;
    return a_pre_object_p;
    
}
IMECPreObject *MvModelScanner_t::GetIniCondData(const char *otype,int comp_index ,int i,IMECPreObject *pre_object_p, char **prop_kwd) const { 
    IMECPreObject *a_pre_object_p = NULL;
    return a_pre_object_p;
}

IMECPreObject *MvModelScanner_t::GetObjectData(const char *otype,int a_id ,IMECPreObject *pre_object_p) const { 
    IMECPreObject *a_pre_object_p = NULL;
    return a_pre_object_p;
}


IMECPreObject *MvModelScanner_t::GetObjectDataExt(const char *otype,int i,IMECPreObject *pre_object_p) const 
{ 
    IMECPreObject *a_pre_object_p = NULL;
    return a_pre_object_p;
}

void MvModelScanner_t::GetNode(int comp_ind,  
                               int i,int *id_p,int *unit_id_p,
                               double *x_p,double *y_p,double *z_p,
                               int *file_ind_p)const
{

}


bool MvModelScanner_t::IsNodeCNode(int comp_index, int node_index)  const
{
    bool is_cnode = false;

    return is_cnode;
}


void MvModelScanner_t::GetPart(int comp_index, int i,int *id_p,char *title,int title_length_max,  
                               int *prop_id_p,int *mat_id_p,int *subset_id_p,
                               int *file_ind_p, int *has_thickness_p, double *thickness_p,
                               bool *is_unresolved,
                               int *is_connection_part_out) const
{
    if(i >= myAllPartIds.size())
        return;

    return ;
}


 /// Getting the part with orphan element's information 
void MvModelScanner_t::GetPartOrphanElements(int comp_index, int i, int *part_id, int* part_comp_index, int* part_orphan_index) const
{

  
}


/// Getting the part id for element type
int MvModelScanner_t::GetPartId(int comp_index, int i, const char *otype) const {

    int part_id = 0;
    return part_id;
}


int MvModelScanner_t::GetPartNbElements(int comp_index, int i,const char *otype) const {
    int            a_nb_elts = 0;
    return a_nb_elts;
}




IMECPreObject *MvModelScanner_t::GetPartXElement(int comp_index, int i,int i_elt,IMECPreObject *pre_object_p) const {
  return NULL;
}



int MvModelScanner_t::GetNbConnexion(int comp_index) const  
{
    return GetNbObjects("CONNECTION",comp_index);
}


int MvModelScanner_t::GetConnexionIdBegin(void)
{

    return (++part_id_max) ;
}

void MvModelScanner_t::GetConnexion(int comp_index, int i, int *nbr_part ,int **id_p,char ***title,int title_length_max,  
                                    int **prop_id_p,int **mat_id_p,int **subset_id_p,
                                    int **file_ind_p, int *part_id_max) 
{
    return ;
}
int MvModelScanner_t::GetConnexionNbElements(int comp_index, int i,int prop_index, int nb_part, const char *otype) const { 
    int      a_nb_elts = 0 ;
    return a_nb_elts;
}
void MvModelScanner_t::GetConnexionElement(int comp_index, int i,const char *otype,int i_elt, int prop_index, int nb_part, 
                                           int *id_p,int *nb_nodes_p,int *node_ids,
                                           int *file_ind_p,void *more_data_p, void * shell_angle_p) const 
{
 
}


void MvModelScanner_t::GetSubset(int comp_index, 
                                 int i,int *id_p,char *title,int title_length_max,
                                 int *nb_children_p,int *child_ids,
                                 int *file_ind_p) const
{
 
}

int MvModelScanner_t::GetSubsetNbChildSubsets(int comp_index, int i) const { 
    int a_nb_child_subsets=0;
    return a_nb_child_subsets;
}


// Retrieve the object index in model from the index of component and the local index of the object in the component
int MvModelScanner_t::GetObjectIndex(int comp_index, int i, const char *otype) const 
{
    return -1;
}



int MvModelScanner_t::GetObjectNbAssociatedObjects(const IMECPreObject &object,
                                                   const char *ass_otype) const
                                                   //++////////////////////////////////////////////////////////////////////////////
                                                   //Function Descriptions:
                                                   //
                                                   //      This function used to get one object's associated objects number
                                                   //Parameters:
                                                   //      const IMECPreObject &object: the object that we want to get its 
                                                   //      associated object number 
                                                   //      const char *ass_otype: associated object's type
                                                   //
                                                   //Return value:
                                                   //      int,Number of the associated object 
                                                   //Modification History:
                                                   //
                                                   
                                                   //--////////////////////////////////////////////////////////////////////////////
{//Begin -- MvModelScanner_t::GetObjectNbAssociatedObjects

    //const MvModel_t   &a_model    = getModel();
    //int                   a_id    = object.GetId();
    //string         a_full_type    = object.GetKernelFullType();
    //MvFullType_t  my_full_type    = MvFullType_t(a_full_type);
    //object_type_e        otype    = my_full_type.getType();
    //int              a_id_offset = object.GetIdOffset();

    //find object  
    //MvObject_t        a_object    = a_model.searchObject(otype,a_id+a_id_offset); 

    //get object's associated object map
    string ass_otype_string = ass_otype;
    obj_type_e assotype = MV_get_type(ass_otype_string); 

    
    int a_nb_objects = 0;//a_object.getConnectedObjectsNumber(assotype); 
    return a_nb_objects;
}//End -- MvModelScanner_t::GetObjectNbAssociatedObjects
 





void MvModelScanner_t::UntransformNodes(int index_comp)
{
    
    //MvModel_t     &a_model     = getModel();
    
    /*int ind_next_subcomponent = a_model.getNextSubComponent(index_comp);
    if(ind_next_subcomponent !=-1)
    UntransformNodes(ind_next_subcomponent);
    */
    
    // apply  inverse transfo in order inverse that  the reading order
    bool do_reverse=true;
    //a_model.applyAllTransforms(index_comp, do_reverse);
    
}


void MvModelScanner_t::TransformNodes(int index_comp)
{
    //MvModel_t     &a_model     = getModel();
    
    /*int ind_next_subcomponent = a_model.getNextSubComponent(index_comp);
    if(ind_next_subcomponent !=-1)
    TransformNodes(ind_next_subcomponent);
    */
    
    // apply  inverse transfo in order of the reading order
    bool do_reverse=false;
    //a_model.applyAllTransforms(index_comp, do_reverse);
}




bool MvModelScanner_t::GetAssociatedObject(const IMECPreObject &object,
                                           const char *ass_otype,
                                           int i,
                                           IMECPreObject *ass_object_p) const
                                           //++////////////////////////////////////////////////////////////////////////////
                                           //Function Descriptions:
                                           //      This function used to get one object's associated objects
                                           //Parameters:
                                           //      const IMECPreObject &object: the object that we want to get its 
                                           //      associated objects
                                           //      const char *ass_otype: associated object's type
                                           //      int i: index of the assoicated objected which we need
                                           //      IMECPreObject *ass_object_p: pointer of the assoicated objected which we need
                                           //Return value:
                                           //      
                                           //      NONE 
                                           //Modification History:
                                           
                                           //--////////////////////////////////////////////////////////////////////////////
{//Begin -- MvModelScanner_t::GetNbAssociatedObject

    return true;
}//End -- MvModelScanner_t::GetNbAssociatedObject
    





void MvModelScanner_t::UntransformFunctions() const
{

}



void MvModelScanner_t::TransformFunctions() const
{

}
/* --------- Groups --------- */


void MvModelScanner_t::FlagGroup(obj_type_e a_group_otype,int a_group_ind)
{
    if(a_group_ind<0)
    {
        return ;
    }
    myGroupFlags[a_group_otype][a_group_ind]=1;
}


bool MvModelScanner_t::IsSubgroupFlaggedOrNull(int comp_index, const IMECPreObject &object,
                                               const char *skeyword,int ind) const
{
    string                           a_kftype       = object.GetKernelFullType();
    const IDescriptor                *a_descr_p      = HCDI_GetDescriptorHandle((char *)a_kftype.c_str());
    int                              a_ikeyword     = a_descr_p->getIKeyword(skeyword);
    object_type_e                    a_group_otype  = a_descr_p->getObjectType(a_ikeyword);
    IMECPreObject::MyAttributeType_e  a_attrib_atype = (ind<0 ? IMECPreObject::ATY_SINGLE : IMECPreObject::ATY_ARRAY);
    int                              a_attrib_index = object.GetIndex(a_attrib_atype,IMECPreObject::VTY_OBJECT,skeyword);
    MYOBJ_INT                        a_group_id     = (ind<0 ? object.GetObjectId(a_attrib_index) : object.GetObjectId(a_attrib_index,ind));
    if(a_group_id==0) return true;
    //
    
    int a_offset_group = GetCurrentOffset(MV_get_type(a_group_otype).c_str());
    int group_id_to_search =0;
    if(a_offset_group==-1)
    {
       // MCDS_get_model_tab_component_global_offset(getModel().getModelPtr(),comp_index, a_group_otype, &a_offset_group);
    }
    if(a_group_id>0) 
        group_id_to_search=a_group_id+a_offset_group;

    
    //const MvModel_t                &a_model     = getModel();
    //MvObject_t                      a_group     = a_model.searchObject(a_group_otype,group_id_to_search);
    //
    int                             a_group_ind = 0;//a_group.getGlobalIndex();
    MyGroupFlags_t::const_iterator  a_it        = myGroupFlags.find(a_group_otype);
    const MyFlagArray_t            &a_flags     = (*a_it).second;
    //
    if(a_group_ind<0)
    {
        return 0;
    }
    return a_flags[a_group_ind]!=0;
}



bool MvModelScanner_t::IsDefineBoxFlaggedOrNull(int comp_index, const IMECPreObject &object,
                                               const char *skeyword,int ind) const
{
    string                           a_kftype       = object.GetKernelFullType();
    const IDescriptor               *a_descr_p      = HCDI_GetDescriptorHandle((char *)a_kftype.c_str());
    int                              a_ikeyword     = a_descr_p->getIKeyword(skeyword);
    object_type_e                    a_group_otype  = a_descr_p->getObjectType(a_ikeyword);
    IMECPreObject::MyAttributeType_e  a_attrib_atype = (ind<0 ? IMECPreObject::ATY_SINGLE : IMECPreObject::ATY_ARRAY);
    int                              a_attrib_index = object.GetIndex(a_attrib_atype,IMECPreObject::VTY_OBJECT,skeyword);
    MYOBJ_INT                        a_group_id     = (ind<0 ? object.GetObjectId(a_attrib_index) : object.GetObjectId(a_attrib_index,ind));
    if(a_group_id==0) 
        return true;

    int nb_obj = (int)myDefineBoxFlags.size();
    if(nb_obj==0)
        return false;

    MyDefineBoxFlags_t::const_iterator  a_it        = myDefineBoxFlags.find(comp_index);
    MyDefineBoxFlags_t::const_iterator a_it_end   = myDefineBoxFlags.end();
    if(a_it==a_it_end)
        return false;

    const MyFlagArray_t            &a_flags     = (*a_it).second;
    //
    if(!a_flags.size())
         return false;
    //
    
    int a_offset_group = GetCurrentOffset(MV_get_type(a_group_otype).c_str());
    int group_id_to_search =0;
    if(a_offset_group==-1)
    {
        //MCDS_get_model_tab_component_global_offset(getModel().getModelPtr(),comp_index, a_group_otype, &a_offset_group);
    }
    if(a_group_id>0) 
        group_id_to_search=a_group_id+a_offset_group;

    
    //const MvModel_t                &a_model     = getModel();
    //MvObject_t                      a_group     = a_model.searchObject(a_group_otype,group_id_to_search);
    //
    int                             a_group_ind = 0;//a_group.getGlobalIndex();
    //
    if(a_group_ind<0 || a_flags.size() <= a_group_ind)
    {
        return false;
    }
    return a_flags[a_group_ind]!=0;
}









void MvModelScanner_t::FlagGroup(const char* otype, int index)  
{
    if(index>=0)
    {
        obj_type_e a_group_otype = getOtype(otype);
        myGroupFlags[a_group_otype][index]=1;
    }
}

void MvModelScanner_t::FlagBox(int index,int index_comp)  
{
    if(index>=0)
    {
        int nb_obj_g = (int)myDefineBoxFlags.size();

        int nb_obj = (int)myDefineBoxFlags[index_comp].size();
        if(nb_obj < (index+1))
            myDefineBoxFlags[index_comp].resize(index+1);
        

        myDefineBoxFlags[index_comp][index]=1;
    }
}
    
bool MvModelScanner_t::IsDefineBoxFlagged(int index_comp,int index)
{
    bool is_flagged = false;
    if(index>=0)
    {
        int nb_obj_g = (int)myDefineBoxFlags.size();
        if(nb_obj_g==0)
            return is_flagged;

        MyDefineBoxFlags_t::const_iterator  a_it      = myDefineBoxFlags.find(index_comp);
        MyDefineBoxFlags_t::const_iterator a_it_end   = myDefineBoxFlags.end();
        if(a_it==a_it_end)
            return is_flagged;

        const MyFlagArray_t            &a_flags               = (*a_it).second;
        //MvObject_t a_def_box =  getObject(DEFINE_BOX, index_comp, index);

        //if(a_def_box.isEmpty())
        //    return is_flagged;
        //int global_index = a_def_box.getGlobalIndex(); 
        //if(global_index >=0 && global_index < a_flags.size())
        //    is_flagged=(a_flags[global_index]!=0);
    }
    return is_flagged;
}


bool MvModelScanner_t::IsGroupFlagged(const char* otype, int index_comp,int index)
{
    bool is_flagged = false;
    if(index>=0)
    {
        obj_type_e a_group_otype = getOtype(otype);
        MyGroupFlags_t::const_iterator  a_it                  = myGroupFlags.find(a_group_otype);
        const MyFlagArray_t            &a_flags               = (*a_it).second;
        //MvObject_t a_group = getObject(a_group_otype, index_comp, index);
        
        //if(a_group.isEmpty())
        //    return is_flagged;
        int global_index_group = 0;//a_group.getGlobalIndex(); 
        if(global_index_group>=0)
            is_flagged=(a_flags[global_index_group]!=0);
        

    }
    return is_flagged;
}


bool MvModelScanner_t::GetSubgroupData(const IMECPreObject &object,  int comp_ind, 
                                       const char *skeyword,IMECPreObject *subgroup_p) const
{
    return GetSubgroupData(object,comp_ind,skeyword,-1,subgroup_p);
}

bool MvModelScanner_t::GetSubgroupData(const IMECPreObject &object, int comp_ind, 
                                       const char *skeyword,int ind,IMECPreObject *subgroup_p) const
{
    string                           a_kftype          = object.GetKernelFullType();
    const IDescriptor               *a_descr_p         = HCDI_GetDescriptorHandle((char *)a_kftype.c_str());
    int                              a_ikeyword        = a_descr_p->getIKeyword(skeyword);
    object_type_e                    a_group_otype     = a_descr_p->getObjectType(a_ikeyword);
    const string                    &a_group_otype_str = MV_get_type(a_group_otype);
    IMECPreObject::MyAttributeType_e  a_attrib_atype    = (ind<0 ? IMECPreObject::ATY_SINGLE : IMECPreObject::ATY_ARRAY);
    int                              a_attrib_index    = object.GetIndex(a_attrib_atype,IMECPreObject::VTY_OBJECT,skeyword);
    MYOBJ_INT                        a_group_id        = (ind<0 ? object.GetObjectId(a_attrib_index) : object.GetObjectId(a_attrib_index,ind));
    //
    if(a_group_id==0) return false;
    //RAR#MGEN_DEV_2006_171#24_09_2006 (BEG)
    int a_offset = GetCurrentOffset(MV_get_type(a_group_otype).c_str());
    if(a_offset==-1)
    {
        //MCDS_get_model_tab_component_global_offset(getModel().getModelPtr(),comp_ind, a_group_otype, &a_offset);
    }
    int a_id_tmp = a_group_id+a_offset;
    GetObjectData(a_group_otype_str.c_str(), a_id_tmp,subgroup_p); 
    if(subgroup_p!=NULL&&subgroup_p->GetId()<=0)
         return false;
    //RAR#MGEN_DEV_2006_171#24_09_2006 (END)
    return true;
}

//RAR#MGEN_DEV_2006_171#24_09_2006 (BEG)
int MvModelScanner_t::GetNbUnflaggedGroups(const char *otype, int comp_index) const { 

     
    if((myCurrentOTypeStr == NULL) ||
        ((myCurrentOTypeStr != NULL) &&
        (strcmp(otype,myCurrentOTypeStr)!=0)))
    {
        setCurrentGroupIndex(comp_index, -1);
        setCurrentUnflaggedGroupIndex(comp_index, -1);
    }
    int current_group_ind = getCurrentGroupIndex(comp_index);
    int current_unflagged_group_ind = getCurrentUnflaggedGroupIndex(comp_index);

    //
    object_type_e                   a_group_otype         = getOtype(otype);
    MyGroupFlags_t::const_iterator  a_it                  = myGroupFlags.find(a_group_otype);
    const MyFlagArray_t            &a_flags               = (*a_it).second;
    
    int                             a_nb_comp_groups           = GetNbObjects(otype, comp_index);
    int                             a_nb_comp_unflagged_groups = 0;
    //
    for(int i_group_comp=0;i_group_comp<a_nb_comp_groups;++i_group_comp) {
       // MvObject_t a_group = getObject(a_group_otype, comp_index, i_group_comp);
        int global_index_group = 0;//a_group.getGlobalIndex(); 
        bool a_is_flagged=(a_flags[global_index_group]!=0);
        //
        if(!a_is_flagged) {
            ++a_nb_comp_unflagged_groups;
            if(current_group_ind<0) {
                current_unflagged_group_ind = 0;
                current_group_ind          = i_group_comp;
            }
        }    
    }
    //
    setCurrentGroupIndex(comp_index, current_group_ind);
    setCurrentUnflaggedGroupIndex(comp_index, current_unflagged_group_ind);


    return a_nb_comp_unflagged_groups;
}
//RAR#MGEN_DEV_2006_171#24_09_2006 (END)

//RAR#MGEN_DEV_2006_171#24_09_2006 (BEG)
void MvModelScanner_t::GetUnflaggedGroupData(const char *otype, int comp_index ,int i,IMECPreObject *group_p) const {
    int current_group_ind = getCurrentGroupIndex(comp_index);
    int current_unflagged_group_ind = getCurrentUnflaggedGroupIndex(comp_index);

    int if_same_than_previous = 0;
    if((myCurrentOTypeStr != NULL) &&
        (strcmp(otype,myCurrentOTypeStr)==0))
    {
        if_same_than_previous = 1;
    }

     
    if((if_same_than_previous == 1)  && (current_group_ind>=0)) {
        obj_type_e a_group_otype = getOtype(otype);
        MyGroupFlags_t::const_iterator  a_it          = myGroupFlags.find(a_group_otype);
        const MyFlagArray_t            &a_flags       = (*a_it).second;
        int                             a_increm      = (i<current_unflagged_group_ind ? -1 : 1);
        //
        while(current_unflagged_group_ind!=i) {
            current_group_ind+=a_increm;
            //MvObject_t a_group = getObject(a_group_otype, comp_index, current_group_ind);
            int global_index_group = 0;//a_group.getGlobalIndex(); 

            bool a_is_flagged=(a_flags[global_index_group]!=0);
            if(!a_is_flagged) 
                current_unflagged_group_ind+=a_increm;
        }
    } else {
        obj_type_e a_group_otype = getOtype(otype);
        MyGroupFlags_t::const_iterator  a_it          = myGroupFlags.find(a_group_otype);
        const MyFlagArray_t            &a_flags       = (*a_it).second;
        int j = 0;
        //
        current_unflagged_group_ind=-1;
        for(j=0;current_unflagged_group_ind<i;j++) {
           // MvObject_t a_group = getObject(a_group_otype, comp_index, j);
            current_group_ind = 0;//a_group.getGlobalIndex(); 

            bool a_is_flagged=(a_flags[current_group_ind]!=0);
            if(!a_is_flagged) 
                ++current_unflagged_group_ind;      
        }
    }
    //
    setCurrentGroupIndex(comp_index, current_group_ind);
    setCurrentUnflaggedGroupIndex(comp_index, current_unflagged_group_ind);


    GetObjectData(otype,comp_index,current_group_ind,group_p);
}
//RAR#MGEN_DEV_2006_171#24_09_2006 (END)

/*---------Adhesive---------------*/
void MvModelScanner_t::FlagAdhesive(object_type_e a_otype,int a_ind)
{
    if(a_ind<0)
    {
        return ;
    }
    myAdhesiveFlags[a_otype][a_ind]=1;
}


void MvModelScanner_t::FlagAdhesive(const char* otype, int index)  
{
    if(index>=0)
    {
        obj_type_e a_otype = getOtype(otype);
        myAdhesiveFlags[a_otype][index]=1;
    }
}

bool MvModelScanner_t::IsAdhesiveFlagged(const char* otype, int index_comp,int index)
{
    bool is_flagged = false;
    if(index>=0)
    {
        obj_type_e a_otype = getOtype(otype);
        MyGroupFlags_t::const_iterator  a_it                  = myAdhesiveFlags.find(a_otype);
        const MyFlagArray_t            &a_flags               = (*a_it).second;
        //MvObject_t a_obj = getObject(a_otype, index_comp, index);
        
        //if(a_obj.isEmpty())
        //    return is_flagged;
        //int index_group = a_obj.getGlobalIndex(); 
        //if(index_group>=0)
        //    is_flagged=(a_flags[index_group]!=0);
       

    }
    return is_flagged;
}

/*------------------Part id for Connection----------------------*/
void MvModelScanner_t::FlagPartId(const int id) const 
{
    if(id>=0)
    {
        myPartIgdsGlobalFlag[id]=1;
    }
}

bool MvModelScanner_t::IsPartIdFlagged(const int id) const
{
    bool is_flagged = false;
    if(id>=0)
    {
        is_flagged=(myPartIgdsGlobalFlag[id]!=0);
    }
    return is_flagged;
}


/* --------- Crypting keys --------- */


int MvModelScanner_t::GetNbCryptingKeys() const {
   // const MvModel_t &a_model=getModel();
    //
    return 0;//a_model.getIntAttribute(MODEL_CRYPT_NB_KEYS);
}



const char *MvModelScanner_t::GetCryptingKeyRef(int i) const {
    //const MvModel_t &a_model        = getModel();
    const char      *a_crypting_ref = NULL;
    //
    //MCDS_get_model_tab(a_model.getModelPtr(),MODEL_CRYPT_REF,i,&a_crypting_ref);
    return a_crypting_ref;
}



const char *MvModelScanner_t::GetCryptingKey(int i) const {
    //const MvModel_t &a_model        = getModel();
    const char      *a_crypting_key = NULL;
    //
    //MCDS_get_model_tab(a_model.getModelPtr(),MODEL_CRYPT_KEY,i,&a_crypting_key);
    return a_crypting_key;
}



const char *MvModelScanner_t::GetCryptingKeyCost(int i) const {
    //const MvModel_t &a_model        = getModel();
    const char      *a_crypting_cost = NULL;
    //
    //MCDS_get_model_tab(a_model.getModelPtr(),MODEL_CRYPT_COST,i,&a_crypting_cost);
    return a_crypting_cost;
}


const char *MvModelScanner_t::GetCryptingKeyFeature(int i) const {
    //const MvModel_t &a_model        = getModel();
    const char      *a_crypting_feature = NULL;
    //
    //MCDS_get_model_tab(a_model.getModelPtr(),MODEL_CRYPT_FEATURE,i,&a_crypting_feature);
    return a_crypting_feature;
}
const char *MvModelScanner_t::GetCryptingKeyModule(int i) const {
    //const MvModel_t &a_model        = getModel();
    const char      *a_crypting_module = NULL;
    //
    //MCDS_get_model_tab(a_model.getModelPtr(),MODEL_CRYPT_MODULE,i,&a_crypting_module);
    return a_crypting_module;
}


const char *MvModelScanner_t::GetCryptingKey(const char *crypt_ref) const {
    const char *a_crypt_key     = NULL;
    int         a_nb_crypt_keys = GetNbCryptingKeys();
    //
    for(int i=0;i<a_nb_crypt_keys && a_crypt_key==NULL;++i) {
        const char *a_crypt_ref=GetCryptingKeyRef(i);
        if(!strcmp(a_crypt_ref,crypt_ref)) a_crypt_key=GetCryptingKey(i);
    }
    //
    return a_crypt_key;
}



const char *MvModelScanner_t::GetCryptingKeyCost(const char *crypt_ref) const {
    const char      *a_crypt_key_cost = NULL;
    int         a_nb_crypt_keys = GetNbCryptingKeys();
    //
    for(int i=0;i<a_nb_crypt_keys ;++i) {
        const char *a_crypt_ref=GetCryptingKeyRef(i);
        if(!strcmp(a_crypt_ref,crypt_ref)) a_crypt_key_cost = GetCryptingKeyCost(i);
    }
    //
    return a_crypt_key_cost;
}


const char *MvModelScanner_t::GetCryptingKeyFeature(const char *crypt_ref) const {
    const char      *a_crypt_key_feature = NULL;
    int         a_nb_crypt_keys = GetNbCryptingKeys();
    //
    for(int i=0;i<a_nb_crypt_keys ;++i) {
        const char *a_crypt_ref=GetCryptingKeyRef(i);
        if(!strcmp(a_crypt_ref,crypt_ref)) a_crypt_key_feature = GetCryptingKeyFeature(i);
    }
    //
    return a_crypt_key_feature;
}
const char *MvModelScanner_t::GetCryptingKeyModule(const char *crypt_ref) const {
    const char      *a_crypt_key_module = NULL;
    int         a_nb_crypt_keys = GetNbCryptingKeys();
    //
    for(int i=0;i<a_nb_crypt_keys ;++i) {
        const char *a_crypt_ref=GetCryptingKeyRef(i);
        if(!strcmp(a_crypt_ref,crypt_ref)) a_crypt_key_module = GetCryptingKeyModule(i);
    }
    //
    return a_crypt_key_module;
}



/* --------- Units --------- */


int MvModelScanner_t::GetNbQuantities() const {
    //const MvModel_t &a_model    = getModel();
    //
    return 0;//a_model.getIntAttribute(MODEL_UNIT_NB_QUANTITIES);  
}



const char *MvModelScanner_t::GetQuantity(int i) const {
    //const MvModel_t &a_model    = getModel();
    const char      *a_quantity = NULL;
    //
    //MCDS_get_model_tab(a_model.getModelPtr(),MODEL_UNIT_QUANTITY,i,&a_quantity);
    return a_quantity;
}




const char *MvModelScanner_t::GetUnitName(int i) const {
    //const MvModel_t &a_model    = getModel();
    const char      *a_unit     = NULL;
    //
    //MCDS_get_model_tab(a_model.getModelPtr(),MODEL_UNIT_UNIT,i,&a_unit);
    return a_unit;
}




double MvModelScanner_t::GetUnitCoeff(int i) const {
    //const MvModel_t &a_model = getModel();
    double           a_value = 1.;
    //
    //MCDS_get_model_tab(a_model.getModelPtr(),MODEL_UNIT_COEFF,i,&a_value);
    return a_value;
}



double MvModelScanner_t::GetUnitOffset(int i) const {
    //const MvModel_t &a_model = getModel();
    double           a_value = 0.;
    //
    //MCDS_get_model_tab(a_model.getModelPtr(),MODEL_UNIT_OFFSET,i,&a_value);
    return a_value;
}

int MvModelScanner_t::GetNbWorkQuantities() const 
{
    //const MvModel_t &a_model    = getModel();
    //
    return 0;//a_model.getIntAttribute(MODEL_WORK_UNIT_NB_QUANTITIES);  
}



const char *MvModelScanner_t::GetWorkQuantity(int i) const 
{
    //const MvModel_t &a_model    = getModel();
    const char      *a_quantity = NULL;
    //
    //MCDS_get_model_tab(a_model.getModelPtr(),MODEL_WORK_UNIT_QUANTITY,i,&a_quantity);
    return a_quantity;
}




const char *MvModelScanner_t::GetWorkUnitName(int i) const 
{
    //const MvModel_t &a_model    = getModel();
    const char      *a_unit     = NULL;
    //
    //MCDS_get_model_tab(a_model.getModelPtr(),MODEL_WORK_UNIT_UNIT,i,&a_unit);
    return a_unit;
}




double MvModelScanner_t::GetWorkUnitCoeff(int i) const 
{
    //const MvModel_t &a_model = getModel();
    double           a_value = 1.;
    //
   // MCDS_get_model_tab(a_model.getModelPtr(),MODEL_WORK_UNIT_COEFF,i,&a_value);
    return a_value;
}



double MvModelScanner_t::GetWorkUnitOffset(int i) const 
{
    //const MvModel_t &a_model = getModel();
    double           a_value = 0.;
    //
   // MCDS_get_model_tab(a_model.getModelPtr(),MODEL_WORK_UNIT_OFFSET,i,&a_value);
    return a_value;
}

/* --------- Model Description --------- */


int  MvModelScanner_t::GetNbForeignObjects(void) const {
    //const MvModel_t &a_model = getModel();
    int            nbr_unknown = 0;

    //MCDS_get_tab_size(a_model.getModelPtr(), MCDS_UNKNOBJ, &nbr_unknown) ;
    return nbr_unknown;
}

int  MvModelScanner_t::GetNbModelDescription(void) const {
    //const MvModel_t &a_model = getModel();
    int            nbr_unknown = 0;
    int            NbModelDescription = 0;
    
    return NbModelDescription;
}

void  MvModelScanner_t::GetModelDescription(int itab, int *nbr_line , char ***line) const {

}



void MvModelScanner_t::InitializeConverterList(void *a_model)
{
    //int nb_unit = a_model.getNbObjects(LOCAL_UNIT_SYSTEM);
    //for(int j= 0;j<nb_unit ;j++)
    //{
    //    MvObject_t a_unit_obj = a_model.getObject(LOCAL_UNIT_SYSTEM, j);
    //    int id = a_unit_obj.getGlobalId();/*offset?*/
    //    MuUnitConverter_t *converter = NULL;
    //    MuUnitConverter_t *unconverter = NULL;
    //    GetConverter(a_unit_obj, &converter,&unconverter);
    //    myConverterList[id]= converter;
    //    myUnConverterList[id]= unconverter;
    //}
}

void MvModelScanner_t::GetConverter(void  *unit_obj, MuUnitConverter_t **converter, MuUnitConverter_t **unconverter) const
{
}

/* --------- Object data (private) --------- */

void MvModelScanner_t::getSegmentGroupData(const void *group_ent_ptr,
                                           IMECPreObject    *pre_object_p) const
{

}


void MvModelScanner_t::getObjectData(const void *a_object, int a_domains, obj_type_e a_otype, IMECPreObject *a_pre_object_p) const
{

}


/* --------- Tools --------- */

obj_type_e MvModelScanner_t::getOtype(const char *otype) const {
    
    // This method was only safe when called with the output of 
    // (MV_get_type(object_type_e)).c_str().
    // Now it can be called with any char*.

    // First we test a very fast condition, which is true whenever we call this
    // method twice with the same object type, as retrieved with
    // MV_get_type(object_type_e).
    // We are in this case very often, so it has to be fast!
    if(otype==myCurrentOTypeStr) return myCurrentOType;

    // Now we really do a string comparison in order to detect when we call this
    // method twice with the same object type, but this time NOT retrieved with
    // MV_get_type(object_type_e).
    if((myCurrentOTypeStr != NULL) &&
        (strcmp(otype,myCurrentOTypeStr)==0))
    {
        return myCurrentOType;
    }

    // If we get here, we have a new object type, so we store it as current
    myCurrentOType    = MV_get_type(otype);
    // In myCurrentOTypeStr, we must not directly store otype, because we don't
    // know whether it points to memory that will stay valid after we return
    // from this method!
    // We use the "global" string instead.
    myCurrentOTypeStr = MV_get_type(myCurrentOType).c_str();

    return myCurrentOType;
}



int MvModelScanner_t::getFileIndex(const void *ent_object) const {
    
    if(myIsMergeMain)
        return 0;
    
    
    int a_file_index = 0; ///object.getFileIndex(&getModel());

    return a_file_index;
    
}


int MvModelScanner_t::getComponentIndex(const void *ent_object) const {

    int        a_nb_component = 0;
    //a_nb_component= getModel().getNbObjects(MCDS_SUBMODEL);
    
    if(a_nb_component<2) return 0;
    int a_comp_index = 0;//object.getComponentIndex();
    return a_comp_index;
}



void MvModelScanner_t::sortParts(flag_sort_e sort_flag) {

}



int MvModelScanner_t::getCurrentGroupIndex(int comp_index) const
{
    return myComponentGroupFlagIndex[comp_index].myCurrentGroupIndex;
}



void MvModelScanner_t::setCurrentGroupIndex(int comp_index, int a_index) const
{
    myComponentGroupFlagIndex[comp_index].myCurrentGroupIndex = a_index;

}



int MvModelScanner_t::getCurrentUnflaggedGroupIndex(int comp_index) const
{
    return myComponentGroupFlagIndex[comp_index].myCurrentCompUnflaggedGroupIndex;
}



void MvModelScanner_t::setCurrentUnflaggedGroupIndex(int comp_index, int a_index) const
{
    myComponentGroupFlagIndex[comp_index].myCurrentCompUnflaggedGroupIndex = a_index;
}

string MvModelScanner_t::GetParameterIdName(const char *otype,int comp_index ,int i) const
{
    //const MvModel_t &a_model   = getModel();
    obj_type_e    a_otype   = getOtype(otype);
    //MvObject_t       a_object  = getObject(a_otype, comp_index, i);
    return "";//a_object.GetParameterIdName();
}

static void PostTreatPropTshell(string a_kftype, IMECPreObject *a_pre_object_p)
{

}
