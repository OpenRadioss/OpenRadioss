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




////////////////////////////////////////////////////////////////////////////////////

#if !defined(SDITYPEMAPPER__INCLUDED_)
#define SDITYPEMAPPER__INCLUDED_

#include <sdiDefs.h>

#include <algorithm>
#include <vector>
#include <map>
#include <set>

namespace sdi
{

class SDITypeCorrespondence
{
public:
    SDITypeCorrespondence(const sdiString& keyword = "", EntityType type = 0,
                          unsigned int idPool = 0) :
        p_keyword(keyword), p_type(type), p_idPool(idPool)
    {}
    SDITypeCorrespondence(const sdiString& keyword, EntityType type,
                             EntityType typeParent,
                             const sdiString& datanameSubobject) :
        p_keyword(keyword), p_type(type),
        p_typeParent(typeParent), p_datanameSubobject(datanameSubobject)
    {}
    SDITypeCorrespondence(const sdiString& keyword, EntityType type,
                          unsigned int elemConfig, unsigned int subType,
                          EntityType typeParent = ENTITY_TYPE_NONE, const sdiString& datanameSubobject = "",
                          bool hasIdPools = false) :
        p_keyword(keyword), p_type(type), p_elemConfig(elemConfig), p_subType(subType),
        p_typeParent(typeParent), p_datanameSubobject(datanameSubobject), p_hasIdPools(hasIdPools)
    {}

    sdiString p_keyword;
    EntityType p_type;
    unsigned int p_elemConfig = 0;
    unsigned int p_subType = 0;
    EntityType p_typeParent = ENTITY_TYPE_NONE;
    unsigned int p_idPool = 0;
    sdiString p_datanameSubobject;
    bool p_hasIdPools = false;
};

/// Class which describes the mapping between SDI and DB types
class SDITypeMapper
{
protected:
    struct myKeywordInfo
    {
        sdiString mykeyword;
        unsigned int myDBtype;
        unsigned int myidpool;
        unsigned int myconfigType;
        unsigned int mysubType;
        unsigned int mysubkeywordcount = 0;
        myKeywordInfo(const sdiString& keyword, unsigned int DBtype, int idpool=0,
                      int configType=0, int subType=0) :
            mykeyword(keyword), myDBtype(DBtype)
        {
            myidpool = idpool > 0 ? idpool : 0;
            myconfigType = configType > 0 ? configType : 0;
            mysubType = subType > 0 ? subType : 0;
        }
    };

public:
    // Constructors and destructor
    SDITypeMapper(const sdiString& keywordprefix = "",
                  char keywordseparator = '_') :
        p_keywordprefix(keywordprefix),
        p_keywordseparator(keywordseparator)
    {}

    SDITypeMapper(const SDITypeMapper& other) :
        p_typesbykeyword(other.p_typesbykeyword),
        p_typesbyidpool(other.p_typesbyidpool),
        p_DBtypebylasttype(other.p_DBtypebylasttype),
        p_lasttypebyDBtype(other.p_lasttypebyDBtype),
        p_keywordseparator(other.p_keywordseparator),
        p_elemtypemax(other.p_elemtypemax),
        p_keywordprefix(other.p_keywordprefix),
        p_idpools(other.p_idpools),
        p_keywordbyidpool(other.p_keywordbyidpool),
        p_elemDBtype(other.p_elemDBtype),
        p_subobjectDBtype(other.p_subobjectDBtype),
        p_maxDBtype(other.p_maxDBtype)
    {
        unsigned int maxtype = 0;
        for (auto it = p_typesbykeyword.begin(); it != p_typesbykeyword.end(); ++it)
            if(it->second.p_type > maxtype) maxtype = it->second.p_type;
        p_typesbytype.resize(maxtype + 1, NULL);
        for (auto it = p_typesbykeyword.begin(); it != p_typesbykeyword.end(); ++it)
        {
            p_typesbytype[it->second.p_type] = &(it->second);
            if(0 != it->second.p_elemConfig)
            {
                p_typesbyelemconfig[std::make_pair(it->second.p_elemConfig, it->second.p_subType)] = &(it->second);
            }
        }
    }

    // improvement TBD: move constructor

private:
    SDITypeMapper& operator= (const SDITypeMapper& other); // hidden until needed...

public:
    virtual ~SDITypeMapper() {}

protected: // to be used by constructors of derived classes (also used by constructor of base class here)
    void Init()
    {
        Pretreat();
        AddElements();
        Add1To1();
        AddManyTo1();
        AddSubobjects();
        Posttreat();
    }

    void Reserve(unsigned int type)
    {
        p_tmptypesdonelist.insert(type);
    }

    // First step: Elements
    void AddElement(const sdiString& keyword, EntityType type, unsigned int idPool,
                    unsigned int elemConfig, unsigned int subType)
    {
        assert(p_tmptypevsDBtype.size() == 0); // many SDI types to one DB type not started
        assert(ENTITY_TYPE_NONE == p_tmptypemax); // subobjects to be added in the last place!

        p_typesbykeyword[keyword] = SDITypeCorrespondence(keyword, type, elemConfig, subType);
        if(p_elemtypemax < type) p_elemtypemax = type;

        if(0 != idPool)
        {
            if(p_typesbyidpool.size() <= idPool) p_typesbyidpool.resize(idPool + 1, ENTITY_TYPE_NONE);
            p_typesbyidpool[idPool] = type;
        }
    }

    void AddElement(const sdiString& keyword, int idPool, unsigned int elemConfig, unsigned int subType)
    {
        if(0 == p_elemtypemax) p_elemtypemax = p_maxDBtype + 1;
        else                   ++p_elemtypemax;
        AddElement(keyword, p_elemtypemax, idPool, elemConfig, subType);
    }

    void AddElement(const sdiString& keyword, EntityType type, unsigned int idPool=0)
    {
        AddElement(keyword, type, idPool, 0, 0);
    }

    // Second step: 1 to 1 mappings
    void Add(const sdiString& keyword, unsigned int type)
    {
        assert(p_tmptypevsDBtype.size() == 0); // many SDI types to one DB type not started
        assert(ENTITY_TYPE_NONE == p_tmptypemax); // subobjects to be added in the last place!

        p_typesbykeyword[keyword] = SDITypeCorrespondence(keyword, (EntityType) type,
                                                          ENTITY_TYPE_NONE, "");
        p_tmptypesdonelist.insert(type);
    }

    // Third step: Many SDI types to one DB type, or a shifted one
    void Start(unsigned int DBtype, EntityType minType = ENTITY_TYPE_NONE)
    {
        assert(ENTITY_TYPE_NONE == p_tmptypemax); // subobjects to be added in the last place!

        EntityType type = ENTITY_TYPE_NONE;
        if(!p_tmptypevsDBtype.empty())
        {
            type = p_tmptypevsDBtype[p_tmptypevsDBtype.size() - 1].first;
        }
        else
        {
            type = (EntityType) p_elemtypemax + 1;
        }
        if(minType > type) type = minType;
        p_tmptypevsDBtype.push_back(std::make_pair(type, DBtype));
    }
    void Add(const sdiString& keyword)
    {
        AddWithIdPool(keyword, 0);
    }
    void AddWithIdPool(const sdiString& keyword, unsigned int idPool)
    {
        assert(p_tmptypevsDBtype.size() > 0); // at least the current one
        assert(ENTITY_TYPE_NONE == p_tmptypemax); // subobjects to be added in the last place!

        size_t current = p_tmptypevsDBtype.size() - 1;
        (p_tmptypevsDBtype[current].first)++;
        EntityType type = p_tmptypevsDBtype[current].first;
        p_typesbykeyword[keyword] = SDITypeCorrespondence(keyword, type, idPool);
        if(0 != idPool)
        {
            if(p_typesbyidpool.size() <= idPool) p_typesbyidpool.resize(idPool + 1, 0);
            p_typesbyidpool[idPool] = type;
        }
    }

    // Fourth step: Subobjects
    void Add(const sdiString& keywordSubobject, const sdiString& keywordParent, const sdiString& datanameSubobject)
    {
        if(ENTITY_TYPE_NONE == p_tmptypemax)
        {
            // first call
            assert(p_tmptypevsDBtype.size() > 1);
            size_t current = p_tmptypevsDBtype.size() - 1;
            p_tmptypemax = p_tmptypevsDBtype[current].first;
        }

        ++p_tmptypemax;
        assert(GetEntityType(keywordParent) != ENTITY_TYPE_NONE);
        p_typesbykeyword[keywordSubobject] = 
            SDITypeCorrespondence(
                keywordSubobject, p_tmptypemax,
                GetEntityType(keywordParent), datanameSubobject);
    }

    // utility
    void AssureKeywordPrefix(sdiString& keyword)
    {
        if(p_keywordprefix.size() > 0)
        {
            if(keyword.compare(0, p_keywordprefix.size(), p_keywordprefix) != 0 &&
               '#' != keyword[0]) // hack for Radioss #include
            {
                keyword.insert(0, p_keywordprefix);
            }
        }
    }

protected: // virtual methods called in Init().
    // Might work as required if p_tmpkeywordlist is populated by derived class before it
    // calls Init(), but can be redefined if necessary.

    virtual void SortTmpKeywordList()
    {
        std::sort(p_tmpkeywordlist.begin(), p_tmpkeywordlist.end(), [](const auto& lhs, const auto& rhs)
                  { return lhs.mykeyword < rhs.mykeyword; });
    }

    virtual void Pretreat()
    {
        // remove wrong subobject entries. They all contain '_', but we are getting here also
        // false ones, where the '_' have been replaced by '/'
        int i = 0;
        while(i < p_tmpkeywordlist.size())
        {
            if(p_subobjectDBtype == p_tmpkeywordlist[i].myDBtype &&
               p_tmpkeywordlist[i].mykeyword.find('_') == p_tmpkeywordlist[i].mykeyword.npos)
            {
                p_tmpkeywordlist.erase(p_tmpkeywordlist.begin() + i);
            }
            else
            {
                ++i;
            }
        }

        // prepend p_keywordprefix if not present
        for(auto& keywordlistitem : p_tmpkeywordlist)
        {
            AssureKeywordPrefix(keywordlistitem.mykeyword);
        }

        // sort
        SortTmpKeywordList();

        // join subsequent duplicates
        i = 0;
        while(i < p_tmpkeywordlist.size())
        {
            // special case: subobjects are skipped here:
            if(p_subobjectDBtype == p_tmpkeywordlist[i].myDBtype)
            {
                ++i;
                continue;
            }

            size_t pos = p_tmpkeywordlist[i].mykeyword.size();
            bool doJoin = true;
            while (pos != p_tmpkeywordlist[i].mykeyword.npos && pos != 0) 
            {
                sdiString base = p_tmpkeywordlist[i].mykeyword.substr(0, pos);
                // first check whther all subsequent keyword duplicates have same type and idpool
                int j = i + 1;
                while(j < p_tmpkeywordlist.size() &&
                      p_tmpkeywordlist[j].mykeyword.compare(0, base.size(), base) == 0)
                {
                    if(p_tmpkeywordlist[i].myDBtype != p_tmpkeywordlist[j].myDBtype ||
                       p_tmpkeywordlist[i].myidpool != p_tmpkeywordlist[j].myidpool)
                    {
                        doJoin = false;
                        break;
                    }
                    ++j;
                }
                // if the following ones could be joined, make sure there are no preceeding ones that couldn't
                if(doJoin)
                {
                    j = i - 1;
                    while(j >= 0 &&
                          p_tmpkeywordlist[j].mykeyword.compare(0, base.size(), base) == 0)
                    {
                        if(p_tmpkeywordlist[i].myDBtype != p_tmpkeywordlist[j].myDBtype ||
                           p_tmpkeywordlist[i].myidpool != p_tmpkeywordlist[j].myidpool)
                        {
                            doJoin = false;
                            break;
                        }
                        --j;
                    }

                }
                // one more try: find a common single keyword for a type and idpool
                if(!doJoin)
                {
                    for(j = 0; j < p_tmpkeywordlist.size(); ++j)
                    {
                        if(j < i && p_tmpkeywordlist[j].mykeyword == base)
                        {
                            doJoin = false; // base is already taken
                            break;
                        }
                        if(j != i && p_tmpkeywordlist[j].myDBtype == p_tmpkeywordlist[i].myDBtype)
                        {
                           if(p_tmpkeywordlist[j].myidpool != p_tmpkeywordlist[i].myidpool ||
                              p_tmpkeywordlist[j].mykeyword.compare(0, base.size(), base) != 0)
                           {
                               doJoin = false; // mismatch
                               break;
                           }
                           else
                           {
                               doJoin = true; // there is one to join
                           }
                        }
                    }
                }
                // if ok, join them and set common keyword in remaining one
                if(doJoin)
                {
                    // forward...
                    j = i + 1;
                    while(j < p_tmpkeywordlist.size() &&
                          p_tmpkeywordlist[j].mykeyword.compare(0, base.size(), base) == 0)
                    {
                        if(p_tmpkeywordlist[i].myDBtype == p_tmpkeywordlist[j].myDBtype)
                        {
                            p_tmpkeywordlist.erase(p_tmpkeywordlist.begin() + j);
                            ++p_tmpkeywordlist[i].mysubkeywordcount;
                        }
                        else
                        {
                            ++j;
                        }
                    }
                    // ... and backward
                    while(i >= 1 &&
                          p_tmpkeywordlist[i - 1].mykeyword.compare(0, base.size(), base) == 0 &&
                          p_tmpkeywordlist[i].myDBtype == p_tmpkeywordlist[i - 1].myDBtype &&
                          p_tmpkeywordlist[i].myidpool == p_tmpkeywordlist[i - 1].myidpool)
                    {
                        p_tmpkeywordlist.erase(p_tmpkeywordlist.begin() + i - 1);
                        --i;
                        ++p_tmpkeywordlist[i].mysubkeywordcount;
                    }
                    p_tmpkeywordlist[i].mykeyword = base;
                }

                pos = p_tmpkeywordlist[i].mykeyword.rfind(p_keywordseparator, pos - 1);
            }
            ++i;
        }

        p_tmptypesdonelist.insert(p_subobjectDBtype); // handled in AddSubobjects() only
    }

    // element mapping
    virtual void AddElements()
    {
        size_t i = 0;
        while(i < p_tmpkeywordlist.size())
        {
            if(p_elemDBtype == p_tmpkeywordlist[i].myDBtype)
            {
                AddElement(p_tmpkeywordlist[i].mykeyword, p_tmpkeywordlist[i].myidpool,
                           p_tmpkeywordlist[i].myconfigType, p_tmpkeywordlist[i].mysubType);
                p_tmpkeywordlist.erase(p_tmpkeywordlist.begin() + i);
            }
            else
            {
                ++i;
            }
        }
        // if no elements added, make sure subsequent calls of Start() work correctly:
        if(ENTITY_TYPE_NONE == p_elemtypemax) p_elemtypemax = p_maxDBtype - 1;
    }

    // 1 to 1 mapping
    virtual void Add1To1()
    {
        // make sure that no DB types are already reserved by re-implementations of this method
        size_t i = 0;
        while(i < p_tmpkeywordlist.size())
        {
            if(p_subobjectDBtype != p_tmpkeywordlist[i].myDBtype &&
               p_tmptypesdonelist.count(p_tmpkeywordlist[i].myDBtype) > 0)
            {
                // should use messaging system rather than printf
                /* Intermediately we have new data_hierarchy (for common data reader) but old reader, so no message
                printf("%s is reserved and cannot be used!\n",
                       GetDBTypeName(p_tmpkeywordlist[i].myDBtype));
                       */
                p_tmpkeywordlist.erase(p_tmpkeywordlist.begin() + i);
            }
            else
            {
                ++i;
            }
        }

        for(i = 0; i < p_tmpkeywordlist.size(); ++i)
        {
            if(p_tmptypesdonelist.count(p_tmpkeywordlist[i].myDBtype) == 0)
            {
                size_t j = 0;
                for(; j < p_tmpkeywordlist.size(); ++j)
                {
                    if(i != j && p_tmpkeywordlist[i].myDBtype == p_tmpkeywordlist[j].myDBtype) break;
                }
                if(j == p_tmpkeywordlist.size())
                {
                    // above loop has not been left by break => type is only present once
                    Add(p_tmpkeywordlist[i].mykeyword, p_tmpkeywordlist[i].myDBtype);
                    p_tmptypesdonelist.insert(p_tmpkeywordlist[i].myDBtype);
                }
            }
        }
    }

    // many to 1 mapping
    virtual void AddManyTo1()
    {
        for(size_t i = 0; i < p_tmpkeywordlist.size(); ++i)
        {
            if(p_tmptypesdonelist.count(p_tmpkeywordlist[i].myDBtype) == 0)
            {
                Start(p_tmpkeywordlist[i].myDBtype);
                p_tmptypesdonelist.insert(p_tmpkeywordlist[i].myDBtype);
                for(size_t j = i; j < p_tmpkeywordlist.size(); ++j)
                {
                    if(p_tmpkeywordlist[i].myDBtype == p_tmpkeywordlist[j].myDBtype)
                    {
                        AddWithIdPool(p_tmpkeywordlist[j].mykeyword, p_tmpkeywordlist[j].myidpool);
                    }
                }
            }
        }
    }

    virtual void AddSubobjects()
    {
        for(size_t i = 0; i < p_tmpkeywordlist.size(); ++i)
        {
            if(p_subobjectDBtype == p_tmpkeywordlist[i].myDBtype)
            {
                const sdiString& str = p_tmpkeywordlist[i].mykeyword;
                sdiString keywordSubobject, keywordParent, datanameSubobject;
                size_t pos1 = str.find(' ');
                if(str.size() > pos1)
                {
                    keywordSubobject = str.substr(0, pos1);
                    AssureKeywordPrefix(keywordSubobject);
                    pos1 = str.find_first_not_of(' ', pos1 + 1);
                    if(str.size() > pos1)
                    {
                        size_t pos2 = str.find(' ', pos1);
                        if(str.size() > pos2)
                        {
                            keywordParent = str.substr(pos1, pos2 - pos1);
                            AssureKeywordPrefix(keywordParent);
                            pos1 = str.find_first_not_of(' ', pos2 + 1);
                            if(str.size() > pos1)
                            {
                                datanameSubobject = str.substr(pos1, str.find(' ', pos1) - pos1);
                                Add(keywordSubobject, keywordParent, datanameSubobject);
                            }
                        }
                    }
                }
            }
        }
    }

    virtual void Posttreat()
    {
        unsigned int maxtype = 0;
        for (auto it = p_typesbykeyword.begin(); it != p_typesbykeyword.end(); ++it)
            if(it->second.p_type > maxtype) maxtype = it->second.p_type;
        p_typesbytype.resize(maxtype + 1, NULL);
        for (auto it = p_typesbykeyword.begin(); it != p_typesbykeyword.end(); ++it)
        {
            p_typesbytype[it->second.p_type] = &(it->second);
        }
        for (auto it = p_tmptypevsDBtype.begin(); it != p_tmptypevsDBtype.end(); ++it)
        {
            p_DBtypebylasttype[it->first] = it->second;
            p_lasttypebyDBtype[it->second] = it->first;
        }
        p_tmptypevsDBtype.resize(0);

        for (auto it = p_typesbykeyword.begin(); it != p_typesbykeyword.end(); ++it)
        {
            p_typesbytype[it->second.p_type] = &(it->second);
            if(0 != it->second.p_elemConfig)
            {
                p_typesbyelemconfig[std::make_pair(it->second.p_elemConfig, it->second.p_subType)] = &(it->second);
            }
        }

        // build table of id pools for many to 1 mapping
        p_idpools.resize(p_typesbytype.size());
        std::map<unsigned int, std::set<EntityType>> tmpidpools;
        EntityType type = p_elemtypemax + 1;
        unsigned int DBtypelast = 0;
        while(type <= p_typesbytype.size())
        {
            const SDITypeCorrespondence* typeinfo = nullptr;
            unsigned int DBtypenew = DBtypelast;
            if(type < p_typesbytype.size())
            { // "true" iteration
                typeinfo = p_typesbytype[type];
                if(nullptr != typeinfo) DBtypenew = GetDBType(type);
            }
            else
            { // "fake" iteration to flush tmpidpools after the last type
                DBtypenew = 0;
            }

            // "flush" tmpidpools of previous DBtype
            if(DBtypenew != DBtypelast)
            {
                for(auto tmpidpool : tmpidpools)
                {
                    if(tmpidpool.second.size() > 1)
                    {
                        for(EntityType type1 : tmpidpool.second)
                        {
                            for(EntityType type2 : tmpidpool.second)
                            {
                                if(type1 != type2) p_idpools[type1].insert(type2);
                            }
                        }
                    }
                }
                tmpidpools.clear();
                DBtypelast = DBtypenew;
            }

            if(nullptr != typeinfo) tmpidpools[typeinfo->p_idPool].insert(typeinfo->p_type);
            ++type;
        }

        
        //DumpToFile("C:/tmp/SDITypeMapper.txt");
    }

    
    virtual const char* GetDBTypeName(unsigned int DBType)
    {
        static char emptyString[] = "";
        return emptyString;
    }

public:
    void DumpToFile(const char* filename)
    {
        FILE *file = fopen(filename, "w");
        if (nullptr == file) return;

        fprintf(file, "1 to 1 mapping\n");
        std::vector<myKeywordInfo> p_tmpkeywordlist;
        for(EntityType type = ENTITY_TYPE_NONE; type < p_maxDBtype; ++type)
        {
            if(!GetKeyword(type).empty())
            {
                p_tmpkeywordlist.push_back(myKeywordInfo(GetKeyword(type), (unsigned int) type));
                // sorted by type
                fprintf(file, "%s   %s   (%u)\n",
                        GetKeyword(type).c_str(),
                        GetDBTypeName(type),
                        (unsigned int) type);

            }
        }
        // sorted alphabetically
        sort(p_tmpkeywordlist.begin(), p_tmpkeywordlist.end(), [](const myKeywordInfo& lhs, const myKeywordInfo& rhs)
             {
                 return lhs.mykeyword < rhs.mykeyword;
             });
        for(auto keyword : p_tmpkeywordlist)
        {
            /*
            fprintf(file, "%s   %s   (%u)\n",
                    keyword.mykeyword.c_str(),
                    GetDBTypeName(keyword.myDBtype),
                    (unsigned int) keyword.myDBtype);
             */
        }

        fprintf(file, "elements\n");
        for(EntityType type = p_maxDBtype; type <= p_elemtypemax; ++type)
        {
            if(!GetKeyword(type).empty())
            {
                fprintf(file, "%s   (%u)\n", GetKeyword(type).c_str(), (unsigned int) type);
            }
        }

        fprintf(file, "many to 1 mapping\n");
        EntityType type = p_elemtypemax + 1;
        unsigned int DBtypelast = 0;
        while(type <= GetMaxEntityType())
        {
            if(!GetKeyword(type).empty())
            {
                if(GetDBType(type) != DBtypelast)
                {
                    if(GetEntityTypeParent(type) == ENTITY_TYPE_NONE)
                    {
                        fprintf(file, "// %s (%u)\n",
                                GetDBTypeName(GetDBType(type)), GetDBType(type));
                    }
                    else
                    {
                        fprintf(file, "// Subobjects of %s (%u)\n",
                                GetDBTypeName(GetDBType(type)), GetDBType(type));
                    }
                    DBtypelast = GetDBType(type);
                }
                fprintf(file, "   %s   (%u)", GetKeyword(type).c_str(), (unsigned int) type);
                if(GetIdPoolTypes(type).size() > 0)
                {
                    fprintf(file, "   (Id pool with");
                    for(EntityType othertype : GetIdPoolTypes(type))
                    {
                        fprintf(file, " %s (%u)", GetKeyword(othertype).c_str(), (unsigned int)othertype);
                    }
                    fprintf(file, ")");
                }
                fprintf(file, "\n");
            }
            ++type;
        }

        fclose(file);
    }

protected:
    EntityType GetEntityType(const sdiString& keyword, bool doRecursive) const
    {
        if(keyword.size() == 0)
        {
            return ENTITY_TYPE_NONE;
        }
        std::map<sdiString, SDITypeCorrespondence>::const_iterator it =
            p_typesbykeyword.upper_bound(keyword);
        if(p_typesbykeyword.begin() == it)
        {
            return ENTITY_TYPE_NONE;
        }
        --it;  // upper_bound returns the first one that is greater, so the preceding one might be it
        if(keyword.compare(0, it->first.size(), it->first) == 0)
        {
            return it->second.p_type;
        }
        // we might have one type for a more general keyword, but a different type for a more specialized
        // keyword. Example: We have "key1" but a different "key1_b". "key1_a" or "key1_c" should both be
        // mapped to "key1". So, we build sub-keyword(s) of the keyword and try again.
        if(doRecursive)
        {
            sdiString subkeyword(keyword);
            size_t pos = subkeyword.rfind(p_keywordseparator);
            while(pos != subkeyword.npos)
            {
                subkeyword.resize(pos);
                EntityType type = GetEntityType(subkeyword, false);
                if(ENTITY_TYPE_NONE != type) return type;
                pos = subkeyword.rfind(p_keywordseparator);
            }
        }
        // if not found, return invalid type
        return ENTITY_TYPE_NONE;
    }

public: // queries
    EntityType GetEntityType(const sdiString& keyword) const
    {
        return GetEntityType(keyword, true);
    }

    bool HasIdPools(EntityType type) const
    {
        if(type >= p_typesbytype.size()) return false;
        if(NULL != p_typesbytype[type]) return p_typesbytype[type]->p_hasIdPools;
        return false;
    }

    const sdiString & GetKeyword(EntityType type) const
    {
        if(type >= p_typesbytype.size()) return EMPTYSTRING;
        if(NULL != p_typesbytype[type]) return p_typesbytype[type]->p_keyword;
        return EMPTYSTRING;
    }

    EntityType GetMaxEntityType() const
    {
        if(0 == p_typesbytype.size()) return ENTITY_TYPE_NONE; // shouldn't happen
        return (EntityType) p_typesbytype.size() - 1;
    }

    virtual unsigned int GetDBType(EntityType type) const
    {
        // 1 to 1 mapping
        if(p_maxDBtype > type) return (unsigned int) type;
        // elements
        if(p_elemtypemax >= type) return p_elemDBtype;
        // many SDI types to one DB type
        auto it = p_DBtypebylasttype.lower_bound(type);
        if(p_DBtypebylasttype.end() != it) return it->second;
        // subobjects
        if(type < p_typesbytype.size() && NULL != p_typesbytype[type])
        {
            return GetDBType(p_typesbytype[type]->p_typeParent);
        }
        return 0;
    }

    EntityType GetEntityType(unsigned int DBtype, bool& is1to1) const
    {
        if(p_elemDBtype == DBtype)
        {
            is1to1 = false;
            return p_elemtypemax;
        }
        if(p_lasttypebyDBtype.count(DBtype) > 0)
        {
            is1to1 = false; // we might have to check, but for the time being we do simple
            return p_lasttypebyDBtype.at(DBtype);
        }
        is1to1 = true;
        return (EntityType) DBtype;
    }

    EntityType GetEntityTypeParent(EntityType type) const
    {
        if(type >= p_typesbytype.size()) return ENTITY_TYPE_NONE;
        if(NULL != p_typesbytype[type]) return p_typesbytype[type]->p_typeParent;
        return ENTITY_TYPE_NONE;
    }

    const sdiString & GetDataNameSubobject(EntityType type) const
    {
        if(type >= p_typesbytype.size()) return EMPTYSTRING;
        if(NULL != p_typesbytype[type]) return p_typesbytype[type]->p_datanameSubobject;
        return EMPTYSTRING;
    }

    unsigned int GetIdPool(EntityType type) const
    {
        if(type >= p_typesbytype.size()) return 0;
        if(NULL != p_typesbytype[type]) return p_typesbytype[type]->p_idPool;
        return 0;
    }

    unsigned int GetConfig(EntityType type) const
    {
        if(type >= p_typesbytype.size()) return 0;
        if(NULL != p_typesbytype[type]) return p_typesbytype[type]->p_elemConfig;
        return 0;
    }

    unsigned int GetDBSubtype(EntityType type) const
    {
        if(type >= p_typesbytype.size()) return 0;
        if(NULL != p_typesbytype[type]) return p_typesbytype[type]->p_subType;
        return 0;
    }

    // Gets the entity type from the id pool number.
    // Calling this only makes sense if the id pool maps to a single entity type.
    // Example: Elements
    EntityType GetEntityTypeFromIdPool(unsigned int idPool) const
    {
        if(idPool >= p_typesbyidpool.size()) return ENTITY_TYPE_NONE;
        return p_typesbyidpool[idPool];
    }

    const std::set<EntityType>& GetIdPoolTypes(EntityType type) const
    {
        assert(p_idpools.size() > type);
        return p_idpools[type];
    }

    const std::vector<std::set<EntityType>>& GetIdPools() const
    {
        return p_idpools;
    }

    EntityType GetEntityTypeFromConfig(unsigned int config, unsigned int subType=0) const
    {
        std::map<std::pair<unsigned int, unsigned int>, const SDITypeCorrespondence*>::const_iterator it =
            p_typesbyelemconfig.upper_bound(std::make_pair(config, subType));
        if(p_typesbyelemconfig.begin() == it) return 0;
        --it;  // upper_bound returns the first one that is greater, so the preceding one might be it
        if(it->first.first == config)
        {
            if( it->first.second == subType || // config+subtype match or
               it->first.second == 0)         // config matches, subtype not set
            {
                return it->second->p_type;
            }
        }
        return ENTITY_TYPE_NONE;
    }

    const sdiString & GetKeywordPrefix() const
    {
        return p_keywordprefix;
    }

//private:
protected:
    // persistent data
    std::map<sdiString, SDITypeCorrespondence> p_typesbykeyword;
    std::vector<const SDITypeCorrespondence*> p_typesbytype;
    std::map<std::pair<unsigned int, unsigned int>, const SDITypeCorrespondence*> p_typesbyelemconfig;
    std::vector<EntityType> p_typesbyidpool;
    std::map<EntityType, unsigned int> p_DBtypebylasttype;
    std::map<unsigned int, EntityType> p_lasttypebyDBtype;
    EntityType p_elemtypemax = ENTITY_TYPE_NONE;
    sdiString p_keywordprefix;
    char p_keywordseparator = '_';
    // Id pool information: For each entity type (outer vector) we store the other
    // entity types of the id pool.
    std::vector<std::set<EntityType>> p_idpools;
    std::map<sdiString, sdiString> p_keywordbyidpool;
protected:
    // temp data used for population
    std::vector<std::pair<EntityType, unsigned int>> p_tmptypevsDBtype;
    EntityType p_tmptypemax = ENTITY_TYPE_NONE;
    std::vector<myKeywordInfo> p_tmpkeywordlist;
    std::set<unsigned int> p_tmptypesdonelist;
    // DB types, to be set by constructor of derived class
    unsigned int p_elemDBtype = 0;
    unsigned int p_subobjectDBtype = 0;
    unsigned int p_maxDBtype = 0;
};

} // namespace sdi

#endif //! !defined(SDITYPEMAPPER__INCLUDED_)
