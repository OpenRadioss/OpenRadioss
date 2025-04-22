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



#include <UTILS/mv_string.h>
#include <UTILS/mv_stl_various.h>
#include <UTILS/str_utils.h>      


#include "mv_file_format.h"

typedef map<string,MvFileFormat_e> LocStrFileFormatMap_t;
typedef map<MvFileFormat_e,string> LocFileFormatStrMap_t;
typedef set<MvFileFormat_e>        LocFileFormatSet_t;    

class MvFileFormatMap_t {
public:
  MvFileFormatMap_t();
  ~MvFileFormatMap_t();
public:
  MvFileFormat_e  getFileFormat(const string &keyword)        const;
  const string   &getFileFormat(MvFileFormat_e domain)        const;
  const string   &getRadiossVersion(MvFileFormat_e format)    const; 
  MvFileFormat_e  getRadiossFileFormat(const string &version) const; 
  bool            isRadiossFileFormat(MvFileFormat_e format)  const; 
  bool            isLsDynaFileFormat(MvFileFormat_e format) const;
  void            setFileFormat(string& format) 
  { 
      if (myStrFileFormatMap.find(format) != myStrFileFormatMap.end())
          return;
      static int count = FF_CDRI;
      if (count >= FF_LAST)
      {
          //assert(0);
          return;
      }
      myFileFormatStrMap[(MvFileFormat_e)(count++)] = format;
      myStrFileFormatMap[format] = (MvFileFormat_e)(count);
      count++;
  }
private:
  void InitStrFileFormatMap();
  void InitFileFormatStrMap();
private:
  void InitFormat2VersionMap();  
  void InitVersion2FormatMap();  
  void InitRadiossFileFormats(); 
  void InitLsDynaFileFormats();
private:
  LocStrFileFormatMap_t myStrFileFormatMap;
  LocFileFormatStrMap_t myFileFormatStrMap;
private:
  LocFileFormatStrMap_t myFormat2VersionMap;  
  LocStrFileFormatMap_t myVersion2FormatMap;  
  LocFileFormatSet_t    myRadiossFileFormats; 
  LocFileFormatSet_t    myLsDynaFileFormats; 
};

static const MvFileFormatMap_t& get_file_format_map(string insert_subprofile = "");


MvFileFormatMap_t::MvFileFormatMap_t() {
  InitStrFileFormatMap();
  InitFileFormatStrMap();
  InitFormat2VersionMap();  
  InitVersion2FormatMap();  
  InitRadiossFileFormats(); 
  InitLsDynaFileFormats();
}

MvFileFormatMap_t::~MvFileFormatMap_t() {
}

MvFileFormat_e MvFileFormatMap_t::getFileFormat(const string &keyword) const {
  LocStrFileFormatMap_t::const_iterator it=myStrFileFormatMap.find(keyword);
  if(it!=myStrFileFormatMap.end()) return (*it).second;
  return FF_UNKNOWN;
}

const string &MvFileFormatMap_t::getFileFormat(MvFileFormat_e domain) const {
  LocFileFormatStrMap_t::const_iterator it=myFileFormatStrMap.find(domain);
  if(it!=myFileFormatStrMap.end()) return (*it).second;
  return (*(myFileFormatStrMap.find(FF_UNKNOWN))).second;
}


const string &MvFileFormatMap_t::getRadiossVersion(MvFileFormat_e format) const {
  LocFileFormatStrMap_t::const_iterator a_it=myFormat2VersionMap.find(format);
  if(a_it!=myFormat2VersionMap.end()) return (*a_it).second;
  return (*(myFormat2VersionMap.find(FF_UNKNOWN))).second;
}



MvFileFormat_e MvFileFormatMap_t::getRadiossFileFormat(const string &version) const {
  LocStrFileFormatMap_t::const_iterator a_it=myVersion2FormatMap.find(version);
  if(a_it!=myVersion2FormatMap.end()) return (*a_it).second;
  return FF_UNKNOWN;
}



bool MvFileFormatMap_t::isRadiossFileFormat(MvFileFormat_e format) const {
  LocFileFormatSet_t::const_iterator a_it=myRadiossFileFormats.find(format);
  return (a_it!=myRadiossFileFormats.end());
}

bool MvFileFormatMap_t::isLsDynaFileFormat(MvFileFormat_e format) const {
  LocFileFormatSet_t::const_iterator a_it=myLsDynaFileFormats.find(format);
  return (a_it!=myLsDynaFileFormats.end());
}



void MvFileFormatMap_t::InitStrFileFormatMap() {
  myStrFileFormatMap["UNKNOWN"]       = FF_UNKNOWN;
  myStrFileFormatMap["radioss41"]     = FF_D00_41;
  myStrFileFormatMap["radioss41F"]    = FF_D00_41F;
  myStrFileFormatMap["radioss41B"]    = FF_D00_41B;
  myStrFileFormatMap["radioss42"]     = FF_D00_42;
  myStrFileFormatMap["radioss43"]     = FF_D00_43;     
  myStrFileFormatMap["radioss44"]     = FF_D00_44;     
  myStrFileFormatMap["radioss4X"]     = FF_D00_4X;
  myStrFileFormatMap["radioss51"]     = FF_D00_51;     
  myStrFileFormatMap["radioss52"]     = FF_D00_52;     
  myStrFileFormatMap["radioss5X"]     = FF_D00_5X;     
  myStrFileFormatMap["radioss90"]     = FF_D00_90;     
  myStrFileFormatMap["radioss9X"]     = FF_D00_9X;     
  myStrFileFormatMap["radioss100"]    = FF_D00_100;     
  myStrFileFormatMap["radioss10X"]    = FF_D00_10X;     
  myStrFileFormatMap["radioss110"]    = FF_D00_110;     
  myStrFileFormatMap["radioss11X"]    = FF_D00_11X;     
  myStrFileFormatMap["radioss120"]    = FF_D00_120;     
  myStrFileFormatMap["radioss12X"]    = FF_D00_12X;     
  myStrFileFormatMap["radioss130"]    = FF_D00_130;     
  myStrFileFormatMap["radioss13X"]    = FF_D00_13X;     
  myStrFileFormatMap["radioss140"]    = FF_D00_140;     
  myStrFileFormatMap["radioss14X"]    = FF_D00_14X;     
  myStrFileFormatMap["radioss2017"]    = FF_D00_20170;     
  myStrFileFormatMap["radioss2017X"]    = FF_D00_2017X;     
  myStrFileFormatMap["radioss2018"]    = FF_D00_20180;     
  myStrFileFormatMap["radioss2018X"]    = FF_D00_2018X;     
  myStrFileFormatMap["radioss2019"]    = FF_D00_20190;     
  myStrFileFormatMap["radioss2019X"]    = FF_D00_2019X;     
  myStrFileFormatMap["radioss2020"]    = FF_D00_20200;     
  myStrFileFormatMap["radioss2020X"]    = FF_D00_2020X;     
  myStrFileFormatMap["radioss2021"] = FF_D00_2021;
  myStrFileFormatMap["radioss2022"] = FF_D00_2022;
  myStrFileFormatMap["radioss2023"] = FF_D00_2023;
  myStrFileFormatMap["radioss2024"] = FF_D00_2024;
  myStrFileFormatMap["radioss2025"] = FF_D00_2025;
  myStrFileFormatMap["radioss2026"] = FF_D00_2026;
  myStrFileFormatMap["D00_LAST"]   = FF_D00_LAST;
  myStrFileFormatMap["DYNA"]       = FF_DYNA;
  myStrFileFormatMap["Keyword970"]    = FF_971R4; 
  myStrFileFormatMap["Keyword971"]    = FF_971R5;       
  myStrFileFormatMap["Keyword971_R6.1"] = FF_971R6;       
  myStrFileFormatMap["Keyword971_R7.1"] = FF_971R7;       
  myStrFileFormatMap["Keyword971_R8.0"]       = FF_971R8;       
  myStrFileFormatMap["Keyword971_R9.0"]       = FF_971R9;       
  myStrFileFormatMap["Keyword971_R9.3"]       = FF_971R93;       
  myStrFileFormatMap["Keyword971_R10.1"]      = FF_971R101;       
  myStrFileFormatMap["Keyword971_R11.0"]      = FF_971R11;  
  myStrFileFormatMap["Keyword971_R11.1"]      = FF_971R111;
  myStrFileFormatMap["Keyword971_R11.2"]      = FF_971R112;    
  myStrFileFormatMap["Keyword971_R12.0"]      = FF_971R12;
  myStrFileFormatMap["Keyword971_R13.0"]      = FF_971R13;
  myStrFileFormatMap["Keyword971_R13.1"]      = FF_971R131;
  myStrFileFormatMap["Keyword971_R14.1"]      = FF_971R141;
  myStrFileFormatMap["Keyword971_R15.0"]      = FF_971R15;  
  myStrFileFormatMap["Keyword971_R16.0"]      = FF_971R16;

  myStrFileFormatMap["LAST"]       = FF_LAST;
}

void MvFileFormatMap_t::InitFileFormatStrMap() {
  for(LocStrFileFormatMap_t::const_iterator it=myStrFileFormatMap.begin();it!=myStrFileFormatMap.end();++it) {
    myFileFormatStrMap[(*it).second]=(*it).first;
  }
}


void MvFileFormatMap_t::InitFormat2VersionMap() {
  myFormat2VersionMap[FF_UNKNOWN] = "UNKNOWN";
  myFormat2VersionMap[FF_D00_41] = "radioss41";
  myFormat2VersionMap[FF_D00_41F] = "radioss41f";
  myFormat2VersionMap[FF_D00_41B] = "radioss41b";
  myFormat2VersionMap[FF_D00_42]  = "radioss42";
  myFormat2VersionMap[FF_D00_43]  = "radioss43";
  myFormat2VersionMap[FF_D00_44]  = "radioss44";
  myFormat2VersionMap[FF_D00_4X]  = "radioss4x";
  myFormat2VersionMap[FF_D00_51]  = "radioss51";
  myFormat2VersionMap[FF_D00_52]  = "radioss52";
  myFormat2VersionMap[FF_D00_5X]  = "radioss5x";
  myFormat2VersionMap[FF_D00_90]  = "radioss90";
  myFormat2VersionMap[FF_D00_9X]  = "radioss9x";
  myFormat2VersionMap[FF_D00_100]  = "radioss100";
  myFormat2VersionMap[FF_D00_10X]  = "radioss10x";
  myFormat2VersionMap[FF_D00_110]  = "radioss110";
  myFormat2VersionMap[FF_D00_11X]  = "radioss11x";
  myFormat2VersionMap[FF_D00_120]  = "radioss120";
  myFormat2VersionMap[FF_D00_12X]  = "radioss12x";
  myFormat2VersionMap[FF_D00_130]  = "radioss130";
  myFormat2VersionMap[FF_D00_13X]  = "radioss13x";
  myFormat2VersionMap[FF_D00_140]  = "radioss140";
  myFormat2VersionMap[FF_D00_14X]  = "radioss14x";
  myFormat2VersionMap[FF_D00_20170]  = "radioss2017";
  myFormat2VersionMap[FF_D00_2017X]  = "radioss2017x";
  myFormat2VersionMap[FF_D00_20180]  = "radioss2018";
  myFormat2VersionMap[FF_D00_2018X]  = "radioss2018x";
  myFormat2VersionMap[FF_D00_20190]  = "radioss2019";
  myFormat2VersionMap[FF_D00_2019X]  = "radioss2019x";
  myFormat2VersionMap[FF_D00_20200]  = "radioss2020";
  myFormat2VersionMap[FF_D00_2020X]  = "radioss2020x";
  myFormat2VersionMap[FF_D00_2021]   = "radioss2021";
  myFormat2VersionMap[FF_D00_2022]   = "radioss2022";
  myFormat2VersionMap[FF_D00_2023]   = "radioss2023";
  myFormat2VersionMap[FF_D00_2024]   = "radioss2024";
  myFormat2VersionMap[FF_D00_2025]   = "radioss2025";
  myFormat2VersionMap[FF_D00_2026]   = "radioss2026";
}



void MvFileFormatMap_t::InitVersion2FormatMap() {
  LocFileFormatStrMap_t::iterator a_it_begin = myFormat2VersionMap.begin();
  LocFileFormatStrMap_t::iterator a_it_end   = myFormat2VersionMap.end();
  LocFileFormatStrMap_t::iterator a_it;
  for(a_it=a_it_begin;a_it!=a_it_end;++a_it) myVersion2FormatMap[(*a_it).second] = (*a_it).first;
  //
  myVersion2FormatMap["radioss41"]  = FF_D00_41;
  myVersion2FormatMap["radioss41f"] = FF_D00_41F;
  myVersion2FormatMap["radioss41F"]  = FF_D00_41F;
  myVersion2FormatMap["radioss41b"] = FF_D00_41B;
  myVersion2FormatMap["radioss41B"]  = FF_D00_41B;
  myVersion2FormatMap["radioss4X"]   = FF_D00_4X;
  myVersion2FormatMap["radioss5X"]   = FF_D00_5X;
  myVersion2FormatMap["radioss9X"]   = FF_D00_9X;
  myVersion2FormatMap["radioss10X"]   = FF_D00_10X;
  myVersion2FormatMap["radioss11X"]   = FF_D00_11X;
  myVersion2FormatMap["radioss12X"]   = FF_D00_12X;
  myVersion2FormatMap["radioss13X"]   = FF_D00_13X;
  myVersion2FormatMap["radioss14X"]   = FF_D00_14X;
  myVersion2FormatMap["radioss2017X"]   = FF_D00_2017X;
  myVersion2FormatMap["radioss2018X"]   = FF_D00_2018X;
  myVersion2FormatMap["radioss2019X"]   = FF_D00_2019X;
  myVersion2FormatMap["radioss2020X"]   = FF_D00_2020X;
  myVersion2FormatMap["radioss2021"] = FF_D00_2021;
  myVersion2FormatMap["radioss2022"] = FF_D00_2022;
  myVersion2FormatMap["radioss2023"] = FF_D00_2023;
  myVersion2FormatMap["radioss2024"] = FF_D00_2024;
  myVersion2FormatMap["radioss2025"] = FF_D00_2025;
  myVersion2FormatMap["radioss2026"] = FF_D00_2026;
}



void MvFileFormatMap_t::InitRadiossFileFormats() {
  LocStrFileFormatMap_t::iterator a_it_begin = myVersion2FormatMap.begin();
  LocStrFileFormatMap_t::iterator a_it_end   = myVersion2FormatMap.end();
  LocStrFileFormatMap_t::iterator a_it;
  for(a_it=a_it_begin;a_it!=a_it_end;++a_it) myRadiossFileFormats.insert((*a_it).second);
}

void MvFileFormatMap_t::InitLsDynaFileFormats()
{
    myLsDynaFileFormats.insert(FF_DYNA);
    myLsDynaFileFormats.insert(FF_971R4);
    myLsDynaFileFormats.insert(FF_971R5);
    myLsDynaFileFormats.insert(FF_971R6);
    myLsDynaFileFormats.insert(FF_971R7);
    myLsDynaFileFormats.insert(FF_971R8);
    myLsDynaFileFormats.insert(FF_971R9);
    myLsDynaFileFormats.insert(FF_971R93);
    myLsDynaFileFormats.insert(FF_971R101);
    myLsDynaFileFormats.insert(FF_971R11);
    myLsDynaFileFormats.insert(FF_971R111);
    myLsDynaFileFormats.insert(FF_971R112);
    myLsDynaFileFormats.insert(FF_971R12);
    myLsDynaFileFormats.insert(FF_971R13);
    myLsDynaFileFormats.insert(FF_971R131);
    myLsDynaFileFormats.insert(FF_971R14);
    myLsDynaFileFormats.insert(FF_971R141);
    myLsDynaFileFormats.insert(FF_971R15);
    myLsDynaFileFormats.insert(FF_971R16);
}


const MvFileFormatMap_t &get_file_format_map(string insert_subprofile) {
  static  MvFileFormatMap_t MV_FILE_FORMAT_MAP;

  if (insert_subprofile != "")
      MV_FILE_FORMAT_MAP.setFileFormat(insert_subprofile);
  return MV_FILE_FORMAT_MAP;
}
void HCDI_SetFileFormat(string subprofile) {
    get_file_format_map(subprofile);
}

MvFileFormat_e MV_get_file_format(const string &keyword) {
  return get_file_format_map().getFileFormat(keyword);
}

const string &MV_get_file_format(MvFileFormat_e domain) {
  return get_file_format_map().getFileFormat(domain);
}


bool MV_is_radioss_file_format(MvFileFormat_e format) {
  return get_file_format_map().isRadiossFileFormat(format);
}

bool MV_is_lsdyna_file_format(MvFileFormat_e format) {
  return get_file_format_map().isLsDynaFileFormat(format);
}


bool MV_is_radioss_block_file_format(MvFileFormat_e format) {
  return (MV_is_radioss_file_format(format) && (format!=FF_D00_41F) );
}


MvFileFormat_e MV_get_radioss_file_format(int version,int ifix) {
  string a_version;
  a_version = str_printf("radioss%d",version);
  
  if(version==41) { 
    if(ifix) a_version+="radiossf"; else a_version+="radiossb";
  }
  
  return MV_get_radioss_file_format(a_version);
}



MvFileFormat_e MV_get_radioss_file_format(const string &version) {
  return get_file_format_map().getRadiossFileFormat(version);
}




