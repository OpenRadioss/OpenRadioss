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


#include "GlobalModelSDI.h"
#include <buildmapping.h>

#include <radiossblk.h>
#include <dynamain.h>

#include <HCDI/hcdi_mv_descriptor.h>

#include <boost/unordered_map.hpp>
#include <tuple>


#include <dyna2rad/dyna2rad.h>

#include <stdarg.h>
#include <string>
#include <fstream>
#include <string.h>
#include <dll_settings.h>

#include <typedef.h>
#include <sdiModelView.h>
using namespace sdi;
using namespace std;

//===========================================================
typedef std::pair<int,std::string>  MappingKey ;
typedef std::tuple<int,std::string,std::string> MappingValue;
typedef std::tuple<std::string,int,std::string> NameIdTitle;

// hash function for a pair of integer + a string
// When the key of an unordored_map is a pair, we must define the hash function
struct pair_hash{
    std::size_t operator()( const MappingKey  &my_pair) const 
    {
        size_t seed = 0;
//  If boost need to be removed:
//  the two following lines are equivalent (same magic numbers)  of using boost:hash_combine
//      seed std::hash<int>{}(my_pair.first) + 0x9e3779b9 + (seed<<6) + (seed >>2); 
//      seed std::hash<std::string>{}(my_pair.second) + 0x9e3779b9 + (seed<<6) + (seed >>2);
        boost::hash_combine(seed,my_pair.first);
        boost::hash_combine(seed,my_pair.second);
        return seed;
    } 
};
//Give a short name for the type tyat will contain the dyna2rad mapping
//use boost for performance reason only, can safely be replaced by std
typedef boost::unordered_map<MappingKey,MappingValue,pair_hash> R2DMapping; 
// R2D is a pair of MappingKeys (i.e. a couple fof triplets {id,name,tittre})
typedef std::pair<NameIdTitle,NameIdTitle> R2DCouple;
//D2RMapping is a vector of R2D couples
typedef std::vector<R2DCouple> D2RMapping; 

// declare global variables containing the mapping
R2DMapping g_rad2dyna;
D2RMapping g_dyna2rad;
// Comment: Here I decided to save the information twice
//  + g_dyna2rad can be cleared after the print of log
//  + good time efficiency
//  - poor memory efficiency (x2) 


// Radioss Treatments for error messages
// -------------------------------------
// Warning & Error message pass Global Option Name + UID
// Converter give : FULLOptionNAME + IUD    
// Need 2nd Mapping "Global Option" + "UserID" => "Radioss Option" + !UID
// Ex : /MAT + MatUID => /MAT/LAWXX, USERID
typedef std::pair<int,std::string>  RadOptKey ;
typedef std::pair<int,std::string>  RadOpt ;


// hash function for a pair of integer + a string for RadOptKey
struct pair_hash_rd{
    std::size_t operator()( const RadOptKey &my_pair) const 
    {
        size_t seed = 0;
        boost::hash_combine(seed,my_pair.first);
        boost::hash_combine(seed,my_pair.second);
        return seed;
    } 
};

typedef boost::unordered_map<RadOptKey,RadOpt,pair_hash_rd> RADMapping; 

// STD:: version
//typedef std::pair<RadOptKey,RadOpt> RADCouple;
//typedef std::vector<RADCouple> RADMapping; 

RADMapping g_rdmapping;

//===========================================================


// ===========================================================================================================
//  Fill g_rad2dyna mapping (global variable) + RADCouple
// ===========================================================================================================
CDECL void BuildMapping(sdiConvert::LogQueryHandle &g_conversionLog,ModelViewEdit* pDynaModelViewSDI )
{

    if (g_conversionLog.size())
    {
        for (auto readPair : g_conversionLog)
        {
            auto VectItrObj = readPair.second;
            // One radioss => vector of dyna options
            for (auto itrObj : VectItrObj)
            {
                if (itrObj.IsValid())
                {
//                  Dyna values
                    EntityRead itrEntityDyna(pDynaModelViewSDI, itrObj);
                    auto dynKeyword =  itrEntityDyna.GetKeyword();
                    int dynId = itrEntityDyna.GetId();    
                    auto  dynTitle =  itrEntityDyna.GetName();

//                  Radioss values
                    auto itrObj2 = readPair.first;
                    EntityRead itrEntityRadioss(g_pModelViewSDI, itrObj2);
                    auto radKeyword =  itrEntityRadioss.GetKeyword();
                    int radId = itrEntityRadioss.GetId();    
                    auto radTitle = itrEntityRadioss.GetName(); 

//                  One radioss option -> one dyna option
//                  g_rad2dyna[ (radioss Id, Keyworrd) ] = (dyna ID, Dyna Keyword, Dyna title) 
                    g_rad2dyna[std::make_pair(radId,radKeyword)] = std::make_tuple(dynId,dynKeyword,dynTitle);

//                    std::cout<<radId<<" "<<radKeyword<<" -> "<<dynId<<" "<<dynKeyword<<std::endl;

//                  same dyna option or radioss option can appear may times
//                  vector of pairs of triplets
                    g_dyna2rad.push_back(std::make_pair(std::make_tuple(dynKeyword,dynId,dynTitle),std::make_tuple(radKeyword,radId,radTitle)));


//                  Create table : Option + UID -> FullOptionName + UID 
                    std::string rdopt = radKeyword ; 
                    rdopt.erase(0,1);                       // remove first "/"
                    std::string delimiter = "/";
                    std::string RadOpt = rdopt.substr(0, rdopt.find(delimiter));
                    RadOpt = "/" + RadOpt;
                    g_rdmapping[std::make_pair(radId,RadOpt)] = std::make_pair(radId,radKeyword);
//                    g_rdmapping.push_back(std::make_pair(std::make_pair(radId,RadOpt),std::make_pair(radId,radKeyword)));
//                    cout << "Radopt:" << rdopt << " -- " << RadOpt << "\n";

                }
            }
        }
        // sort the vector of [dyna tuple , radioss tuple]
        std::sort(g_dyna2rad.begin(),g_dyna2rad.end());
    }
}


void PrintLog(const char *outfilename)
{
/*
    ofstream logFile;
    logFile.open(outfilename,std::ofstream::app);
    logFile << "************************************************************************\n";
    logFile << "*KEYWORD MAPPING TO RADIOSS PROCESS \n";
    logFile << "************************************************************************\n\n";


    std::string ColumnName1("OPTION");
    std::string ColumnName2("ID");
    std::string ColumnName3("TITLE");

    size_t wKeyword = ColumnName1.size();
    size_t wTitle = ColumnName2.size();

    // find maximum lengths of option name and title
    for(auto const & p : g_dyna2rad)
    {
        wKeyword = std::max(wKeyword,(std::get<0>(p.first) ).size());
        wKeyword = std::max(wKeyword,(std::get<0>(p.second)).size());
        wTitle   = std::max(wTitle  ,(std::get<2>(p.first)).size());
        wTitle   = std::max(wTitle  ,(std::get<2>(p.second)).size());
    }

// Header
    logFile << std::left << string(wTitle+wKeyword +13,' ')<<"DYNA : RADIOSS"<<std::endl;
    logFile << std::left << std::setw(wKeyword)  << ColumnName1 ;
    logFile << " (" <<std::setw(11) << ColumnName2 << ") "; 
    logFile << std::left << std::setw(wTitle) << ColumnName3 << " ";
    logFile << "  :  ";
    logFile << std::left << std::setw(wKeyword)  << ColumnName1 ;
    logFile << " (" <<std::setw(11) << ColumnName2 << ") "; 
    logFile << std::left << std::setw(wTitle) << ColumnName3 << " ";
    logFile << std::endl;

    for(auto const & p : g_dyna2rad)
    {
         // p.first: dyna tuple 
         logFile << std::left << std::setw(wKeyword)  << std::get<0>(p.first) ;
         logFile << " (" <<std::setw(11) << std::get<1>(p.first) << ") "; 
         logFile << std::left << std::setw(wTitle) << std::get<2>(p.first) << " ";
         logFile << "  :  ";
         // p.second: radioss tuple
         logFile << std::left<<  std::setw(wKeyword) <<  std::get<0>(p.second);
         logFile << " (" << std::setw(11) << std::get<1>(p.second) << ") "; 
         logFile << std::left << std::setw(wTitle) << std::get<2>(p.second) << " ";
         logFile << std::endl;
         // logFile << "\n" can be used here to improve perfs for long files
        // because endl involves a flush
    }
    logFile.close();
*/

    ofstream logFile;
    logFile.open(outfilename,std::ofstream::app);
    logFile << "*KEYWORD MAPPING TO RADIOSS PROCESS \n";
    logFile << "************************************************************************\n\n";
    for(auto const & p : g_dyna2rad)
    {
        logFile << std::get<0>(p.first);
        logFile << " (" << std::get<1>(p.first) << ")  " << std::get<2>(p.first) << "\n";
        logFile << "    is mapped to\n";
        logFile << std::get<0>(p.second);
        logFile << " (" << std::get<1>(p.second) << ")  " << std::get<2>(p.second) << "\n";
        logFile << endl;
    }
    logFile.close();



//  g_dyna2rad.clear();
}

extern "C" 
{

CDECL void cpp_find_dyna_(char *name, int *size, int *id, char *TEXT, int *STEXT)
{
    char *cname;
    int cname_len;
    int i;
    cname_len = *size + 1;
    cname=(char*) malloc(sizeof(char)*cname_len);
    for(i=0;i<*size;i++){
      cname[i] = name[i];
    }
    cname[*size]='\0';

    //ofstream logFile;
    //logFile.open("ConversionMessage.txt");
    //logFile << "************************************************************************\n";
    //logFile << "*KEYWORD MAPPING TO RADIOSS PROCESS \n";
    //logFile << "************************************************************************\n\n";


    // Define key to look for in the hash table
    std::string sname;
    sname.assign(cname,(*size));
    MappingKey key = std::make_pair(*id,sname);

    auto got = g_rad2dyna.find(key);
    if( got == g_rad2dyna.end() )
    {
       cout<<"Error "<<sname<<" Not found"<<endl; 

    }else{
      auto id = get<0>(got->second);
      auto keyword = get<1>(got->second).c_str();
      auto name = get<2>(got->second).c_str();
      sprintf(TEXT,"From: %s (%d) %s",keyword,id,name,'\0');
      *STEXT = strlen(TEXT);
    }
}

CDECL void CPP_FIND_DYNA(char *name, int *size, int *id, char *TEXT, int *STEXT)
{cpp_find_dyna_ (name,size,id,TEXT,STEXT);}

CDECL void cpp_find_dyna__ (char *name, int *size, int *id, char *TEXT, int *STEXT)
{cpp_find_dyna_ (name,size,id,TEXT,STEXT);}

CDECL void cpp_find_dyna (char *name, int *size, int *id, char *TEXT, int *STEXT)
{cpp_find_dyna_ (name,size,id,TEXT,STEXT);}


CDECL void cpp_print_dyna_(char *name, int *size)
{
    char *cname;
    int cname_len;
    int i;
    cname_len = *size + 1;
    cname=(char*) malloc(sizeof(char)*cname_len);
    for(i=0;i<*size;i++){
      cname[i] = name[i];
    }
    cname[*size]='\0';
    PrintLog(cname);

}

CDECL void CPP_PRINT_DYNA(char *name, int *size)
{cpp_print_dyna_ (name,size);}

CDECL void cpp_print_dyna__ (char *name, int *size)
{cpp_print_dyna_ (name,size);}

CDECL void cpp_print_dyna (char *name, int *size)
{cpp_print_dyna_ (name,size);}


/* --------------------------------------------------------
    Interrogates the dyna Hashtable Radioss -> Dyna Option 
    Dedicated for Message writing
   -------------------------------------------------------- */
CDECL void cpp_find_dyna_mess_(char *name, int *size, int *id, char *Option, int *SOption,int *ncount,char * Title, int *STitle)
{
    char *cname;
    int cname_len;
    int i;
    cname_len = *size + 1;
    cname=(char*) malloc(sizeof(char)*cname_len);
    for(i=0;i<*size;i++){
      cname[i] = name[i];
    }
    cname[*size]='\0';

    //ofstream logFile;
    //logFile.open("ConversionMessage.txt");
    //logFile << "************************************************************************\n";
    //logFile << "*KEYWORD MAPPING TO RADIOSS PROCESS \n";
    //logFile << "************************************************************************\n\n";

    std::string optname="\0";

    std::string nname(cname,*size);
    RadOptKey rkey = std::make_pair(*id,nname);
    auto rgot = g_rdmapping.find(rkey);

    if(rgot != g_rdmapping.end() )
    { 
      optname = get<1>(rgot->second).c_str();
    }

//    std::string nname(cname,*size);
//    std::string optname;
//    for(auto const & p : g_rdmapping)
//    {
//        if (nname == std::get<1>(p.first) && *id == std::get<0>(p.first)) {
//          int OptId = std::get<0>(p.first) ; 
//          optname = get<1>(p.second);
//      }
//    }


    // Define key to look for in the hash table
    std::string sname;
    sname.assign(optname);
    MappingKey key = std::make_pair(*id,sname);
    auto got = g_rad2dyna.find(key);

    if( got != g_rad2dyna.end() )
    {
      char c_string[1024];
      int current_length;

      auto id = get<0>(got->second);
      auto keyword = get<1>(got->second).c_str();
      auto name = get<2>(got->second).c_str();

      /* 
         C Style Charachter handling to be pass to Fortran
         string contains 3 lines (formated to be inserted in Fortran):
         mapped from :
           Dyna_Option ID: DynaID
           Dyna_Option Title: DynaOptionTitle
       */
      Option[0]='\0';

      //mapped from line
#if defined(_WIN64)
      strcat(Option,"mapped from:\r\n");
#else
      strcat(Option,"mapped from:\n");
#endif
      current_length=strlen(Option);

      // [spaces]Dyna_Option ID: DynaID
      for (i=current_length;i < current_length+*ncount;i++) Option[i]=' ' ;  //add spaces for format
      Option[current_length+*ncount]='\0';

#if defined(_WIN64)
      sprintf(c_string,"%s ID: %i \r\n",keyword,id,'\0');
#else
      sprintf(c_string,"%s ID: %i \n",keyword,id,'\0');
#endif

      strcat(Option,c_string);
      current_length=strlen(Option);

      // [spaces]Dyna_Option Title: DynaOptionTitle
      for (i=current_length;i < current_length+*ncount;i++) Option[i]=' ' ;  //add spaces for format
      Option[current_length+*ncount]='\0';

      sprintf(c_string,"%s Title: %s",keyword,name,'\0');
      strcat(Option,c_string);

      *SOption=strlen(Option);
    }
    else
    {
      Option[0]='\0';
#if defined(_WIN64)
      strcat(Option," \r\n");
      strcat(Option," \r\n");
#else
      strcat(Option," \n");
      strcat(Option," \n");
#endif
      strcat(Option," \0");
      *SOption=strlen(Option);
    }
    


}

CDECL void CPP_FIND_DYNA_MESS(char *name, int *size, int *id, char *Option, int *SOption,int *ID,char * Title, int *STitle)
{cpp_find_dyna_mess_ (name,size,id, Option, SOption, ID,Title,STitle);}

CDECL void cpp_find_dyna__mess__ (char *name, int *size, int *id, char *Option, int *SOption,int *ID,char * Title, int *STitle)
{cpp_find_dyna_mess_ (name,size,id, Option, SOption, ID,Title,STitle);}

CDECL void cpp_find_dyna_mess (char *name, int *size, int *id, char *Option, int *SOption,int *ID,char * Title, int *STitle)
{cpp_find_dyna_mess_ (name,size,id, Option, SOption, ID,Title,STitle);}




}


