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



#include <UTILS/mv_cstdlib.h> 
#include <UTILS/mv_cstdio.h>
#include <UTILS/mv_cstdarg.h>
#include <UTILS/error.h>


#include "mv_parser_base.h"

// @@ MPOST R.A 18/10/2004 MP_DEV_2004_83: Zipp Param File
MvParserBase_t::MvParserBase_t(const string &fullname,bool is_gzipped, bool use_local_msg) : 
  MvReadBase_t(fullname,is_gzipped, use_local_msg) 
{
  // Nothing to do
}

MvParserBase_t::~MvParserBase_t() {
  // Nothing to do
}



bool MvParserBase_t::seof(bool do_strict) {
  if(!do_strict) while(!eof()) {
    char c=MvReadBase_t::readChar();
    if(c=='/' && !eof()) {
      char c1=MvReadBase_t::readChar();
      if(c1=='/') {
	if(eof()) return true;
	gotoNextLine();
      } else if(c1=='*') {
	bool comment=true;
	c1=MvReadBase_t::readChar();
	while(comment) {
	  if(c1=='*') {
	    c1=MvReadBase_t::readChar();
	    if(c1=='/') comment=false;
	  } else {
	    c1=MvReadBase_t::readChar();
	  }
	}
	if(eof()) return true;
      } else {
	unreadChar();
	return false;
      }
    } else if(!isBlank(c)) {
      unreadChar();
      return false;
    }
    
    else if(c=='#' && !eof()){
      gotoNextLine();
    }
    
  }
  //
  return eof(); 
}


// Taking comments into account
char MvParserBase_t::readChar() {
  char c=MvReadBase_t::readChar();
  if(c=='/' && !eof()) {
    char c1=MvReadBase_t::readChar();
    if(c1=='/') {
      gotoNextLine();
      return readChar();
    } else if(c1=='*') {
      bool comment=true;
      c1=MvReadBase_t::readChar();
      while(comment) {
        if(c1=='*') {
          c1=MvReadBase_t::readChar();
          if(c1=='/') comment=false;
        } else {
          c1=MvReadBase_t::readChar();
        }
      }
      return readChar();
    } else {
      unreadChar();
      return c;
    }
  }
  
  else if(c=='#' && !eof()){
    gotoNextLine();
    return readChar();
  }
  
  return c;
}

// Next not blank
char MvParserBase_t::getNextChar() {
  char c=readChar();
  while(isBlank(c)) c=readChar();
  return c;
}

// Next before a separator, a blank, an operator, a comparator, or a logical operator
string MvParserBase_t::getNextString(bool withspace) {
  
  string a_string      = "";
  bool   a_do_continue = true;
  char   a_char        = getNextChar();
  //
  do {
    if(isComparatorBegin(a_char) || isLogicalOperatorBegin(a_char)) {
      a_do_continue=false;
    } else if((!withspace && isBlank(a_char)) || isSeparator(a_char) || isOperator(a_char) || isBoundary(a_char)) {
      a_do_continue=false;
    } else {
      a_string+=a_char;
    }
    //
    if(a_do_continue) a_char=readChar();
  } while(a_do_continue);
  //
  unreadChar();
  return a_string;  
  
}

string MvParserBase_t::getNextQuotedString(char comma) {
  string s="";
  char c=getNextChar();
  if(c!=comma) {
    if(useLocalMsg()) {
      throwError("Quoted string not found");
    } else {
      throwError(getMsg(2));
    }
  }
  while((c=MvReadBase_t::readChar())!=comma) { 
    if(c=='\\') {
      char cc=MvReadBase_t::readChar();
      if(cc=='\"' || cc=='\\') {
        c=cc;
      } else if(cc=='n') {
        c='\n';
      } else if(cc=='t') {
        c='\t';
      } else {
        s+=c; c=cc;
      }
    }
    s+=c;
  }  
  return s;
}

bool MvParserBase_t::getNextBool() {
  bool var = false;
  string a_str=getNextString();
  if(a_str != "TRUE" && a_str != "FALSE") throwError(getMsg(7),a_str.c_str());
  if(a_str == "TRUE")
     var = true;
  return var;
}

int MvParserBase_t::getNextInt() {
  string a_str=getNextString();
  char *a_res;
  int an_int=(int)strtol(a_str.c_str(),&a_res,10);
  if(a_res==a_str.c_str()) {
    if(useLocalMsg()) {
      throwError("\"%s\" is not an integer",a_str.c_str());
    } else {
      throwError(getMsg(4),a_str.c_str()); 
    }
  }
  return an_int;
}

double MvParserBase_t::getNextFloat() {
  string a_str=getNextString();
  char *a_res;
  double a_double=strtod(a_str.c_str(),&a_res);
  if(a_res==a_str.c_str()) {
    if(useLocalMsg()) {
      throwError("\"%s\" is not a double",a_str.c_str());
    } else {
      throwError(getMsg(5),a_str.c_str()); 
    }
  }
  return a_double;
}

string MvParserBase_t::getNextComparator() {
  string s="";
  char c=getNextChar();
  if(!isComparatorBegin(c)) {
    unreadChar();
    s=getNextString();
  } else {
    s+=c;
    
    c=readChar();
    if(isComparator(s+c)) s+=c; else unreadChar();
    
  }
  if(!isComparator(s)) throwError(getMsg(6),s.c_str()); 
  return s;
}


string MvParserBase_t::getNextLogicalOperator() {
  string s="";
  char c=getNextChar();
  if(!isLogicalOperatorBegin(c)) {
    unreadChar();
    s=getNextString();
  } else {
    s+=c;
    if(!isLogicalOperator(s)) {
      c=readChar();
      if(isLogicalOperator(s+c)) s+=c; else unreadChar();
    }
  }
  if(!isLogicalOperator(s)) throwError(getMsg(6),s.c_str()); 
  return s;
}


void MvParserBase_t::throwError(const char *format,...) const {
  static char buffer[500];
  va_list args;
  va_start(args,format);
  vsprintf(buffer,format,args);
  va_end(args);
  //
  if(useLocalMsg()) {
    MvError_t err("In file \"%s\", at line %d:\nERROR: %s",getFileName().c_str(),getCurrentLine(),buffer);
    throw err;
  } else {
    MvError_t err(getMsg(3),getFileName().c_str(),getCurrentLine(),buffer);
    throw err;
  }
}

void MvParserBase_t::throwError(const string &msg) const {
  if(useLocalMsg()) {
    MvError_t err("In file \"%s\", at line %d:\nERROR: %s",getFileName().c_str(),getCurrentLine(),msg.c_str());
    throw err;
  } else {
    MvError_t err(getMsg(3),getFileName().c_str(),getCurrentLine(),msg.c_str());
    throw err;
  }
}




