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
#ifndef MV_PRE_DESCRIPTOR_H
#define MV_PRE_DESCRIPTOR_H


#include <UTILS/mv_string.h>
#include <MESSAGE/mv_messages.h>


class MvDescriptor_t;


class MvPreDescriptor_t {
public:
  
  MvPreDescriptor_t(const string &filename,
            object_type_e obj_type=HCDI_OBJ_TYPE_NULL,const string &keyword="", int user_id=-1,int hm_config_type=0,int hm_type=0, short int id_pool=0, char* card_image = NULL);
  
  MvPreDescriptor_t();
  ~MvPreDescriptor_t();
public:
  const MvDescriptor_t *getDescriptorPtr(void* cfgkernel) const;
  inline bool           isUser()     const { return myUserId>=0; }
  inline int            getUserId()  const { return myUserId; }
  inline int            getHmConfigType()  const { return myHmConfigType; }
  inline int            getHmType()  const { return myHmType; }
  inline short int      getIdPool()  const { return myIdPool; }
  inline const string  &getKeyword() const { return myKeyword; }
  inline char*          getCardImage() const { return myCardImage; }
  void  deleteDescriptor();/* { if (myDescriptorPtr != NULL) { delete myDescriptorPtr; myDescriptorPtr = NULL; } } */
  inline const string& getFileName() const { return myFileName; }
private:
  inline const char    *getMsg(int i) const { return MV_get_msg_array(MSGT_KERNEL)[i]; }
private:
  object_type_e           myObjectType;
  string                  myKeyword;
  char*                   myCardImage;
  int                     myUserId;
  int                     myHmConfigType;
  int                     myHmType;
  string                  myFileName;
  mutable MvDescriptor_t *myDescriptorPtr;
  short int               myIdPool;
};
typedef map<int, MvPreDescriptor_t*>    LocUserPreDescriptorIdMap_t;
typedef map<string, MvPreDescriptor_t*> LocUserPreDescriptorStrMap_t;
typedef map<int, vector<MvPreDescriptor_t*> >    LocUserVecPreDescriptorIdMap_t;
typedef map<short int, MvPreDescriptor_t*>    LocUserPreDescriptorShortIdMap_t;
class LocUserPreDescriptorMapArray_t {
public:
    LocUserPreDescriptorMapArray_t();
    ~LocUserPreDescriptorMapArray_t(); 
public:
    void addPreDescriptor(object_type_e type, MvPreDescriptor_t* pre_descr_p,
        const PseudoStringList_t* user_name_list_p);
    const MvPreDescriptor_t* getPreDescriptorPtr(object_type_e type, int id) const;
    const MvPreDescriptor_t* getPreDescriptorPtr(object_type_e type, int hm_config_type, int hm_type) const;
    const MvPreDescriptor_t* getPreDescriptorPtr(object_type_e type, int hm_config_type, int hm_type, const string& keyword) const;
    void deleteDescrData(MvPreDescriptor_t* pre_descr_p, int i);
public:
    void display(object_type_e type) const;
private:
    LocUserPreDescriptorIdMap_t  myUserPreDescriptorIdMapArray[MV_NB_MCDS_TYPES];
    LocUserPreDescriptorStrMap_t myUserPreDescriptorStrMapArray[MV_NB_MCDS_TYPES];
    LocUserPreDescriptorIdMap_t  myUserPreDescriptorHMConfigTypeMapArray[MV_NB_MCDS_TYPES];
    LocUserPreDescriptorIdMap_t  myUserPreDescriptorHMTypeMapArray[MV_NB_MCDS_TYPES];
    LocUserVecPreDescriptorIdMap_t  myUserPreDescriptorHMConfigHMTypeMapArray[MV_NB_MCDS_TYPES];
    LocUserPreDescriptorShortIdMap_t  myUserPreDescriptorIdPoolMapArray[MV_NB_MCDS_TYPES];
};
typedef map<const MvSubtype_t*, MvPreDescriptor_t*> MvPreDescriptorMap_t;
class MvDescriptorMapArray_t {
public:
    MvDescriptorMapArray_t();
    ~MvDescriptorMapArray_t();
public:
    const MvDescriptor_t* getDescriptorPtr(void* cfgkernel, object_type_e type, const MvSubtype_t* subtype_p) const;
private:
    MvPreDescriptor_t* addPreDescriptor(void* cfgkernel, const string& filename, object_type_e type, const string& keyword = "", int user_id = -1, int hm_config_type = -1, int hm_type = -1, short int idpool = -1, string cardImage = "", const string& optional_header_string = "");

private:
    MvPreDescriptorMap_t myPreDescriptorMapArray[MV_NB_MCDS_TYPES];
};


//void MV_init_pre_descriptors();
//void MV_close_pre_descriptors();


//void add_user_pre_descriptor(object_type_e obj_type,MvPreDescriptor_t *pre_descr_p,
//				const PseudoStringList_t *user_name_list_p=NULL);

//void add_user_pre_descriptor(object_type_e obj_type,MvPreDescriptor_t *pre_descr_p,
//				const string &user_name);

#endif //MV_PRE_DESCRIPTOR_H




