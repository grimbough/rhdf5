#include "common.h"
#include <time.h>

struct member_list {
  SEXP members;
  int  count;
};

struct apply_state {
  SEXP  subset;		       
  SEXP  funcall;
  SEXP  result;
};


int HDF_print_iterator(hid_t group_id, const char* member_name, 
		       void* data) 
{
    H5G_stat_t	buf;
    static char	value[1024];
    struct tm	when;
    
    H5Gget_objinfo(group_id,member_name,0,&buf);
    when = *localtime(&buf.mtime);
    
    
    Rprintf("[%d]\t%s",++(*(int*)data),member_name);
    switch(buf.type) {
    case H5G_GROUP:
	Rprintf("\tGroup\t");
	break;
    case H5G_DATASET:
	Rprintf("\tDataset\t");
	Rprintf("%.2d/%.2d/%.4d %.2d:%.2d",
		when.tm_mday, when.tm_mon+1, when.tm_year+1900,
		when.tm_hour, when.tm_min); 
	break;
    default:
    /* maybe we should do something here - SDR */
    break;
    }
    value[0] = '\0';
    H5Gget_comment(group_id,member_name,1024,value);
    if(strlen(value) > 0)
	Rprintf("\t%s\n",value);
    else
	Rprintf("\n");
    return 0;
}

SEXP HDF_group_print(SEXP group)
{
    int count;
    if(!isFILE(group) && !isGROUP(group)) {
	error("not a file or group");
	return R_NilValue;
    }
    Rprintf("  \tName\tType\tDate\t\tComment\n");
    count=0;
    H5Giterate(HID(group),".",NULL,HDF_print_iterator,(void*)&count);
    return R_NilValue;
}

int HDF_subgroup_iterator(hid_t group_id, const char* member_name,
			  void* data) 
{
    if(strcmp((char *)data, member_name) == 0)
	return 1;
    else
	return 0;
}

int HDF_has_subgroup(SEXP group, SEXP name)
{
    int ret;
    ret = H5Giterate(HID(group), ".",  NULL, HDF_subgroup_iterator,
		     (void*)STR(name)); 
    return ret;
}

/*
  positive if there is a subgroup/element with the name, zero
  otherwise 
*/
int HDF_hassubgroup(hid_t group, char *name)
{
    int ret;

    ret = H5Giterate(group, ".",  NULL, HDF_subgroup_iterator, (void*)
		     name); 
    return ret;
}

int HDF_count_iterator(hid_t group_id, const char* member_name, void*
		       data) 
{
    (*(int*)data)++;
    return 0;
}

int HDF_count_members(SEXP group)
{
    int ret = 0;
    H5Giterate(HID(group), ".", NULL, HDF_count_iterator,
	       (void*)&ret);
    return ret;
}

int HDF_member_iterator(hid_t group_id, const char* member_name, void*
			data) 
{
    struct member_list *d = (struct member_list*)data;
    SET_STRING_ELT(d->members, d->count++, mkChar(member_name));
    return 0;
}

SEXP HDF_group_members(SEXP group)
{
    SEXP ans;
    int  count;
    struct member_list list;
    
    count = HDF_count_members(group);

    PROTECT(ans = allocVector(STRSXP,count));
    list.members = ans;
    list.count   = 0;
    H5Giterate(HID(group),".",NULL,HDF_member_iterator,(void*)&list);
    UNPROTECT(1);
    return(ans);
}

SEXP HDF_group_set_comment(SEXP group,SEXP name,SEXP comment)
{
    if(!isFILE(group) && !isGROUP(group)) {
	error("not a file or group");
	return R_NilValue;
    }
    H5Gset_comment(HID(group),STR(name),STR(comment));
    return R_NilValue;
}

SEXP HDF_group_get_comment(SEXP group,SEXP name)
{
    SEXP	ans;
    char	buffer[1024];
    
    if(!isFILE(group) && !isGROUP(group)) {
	error("not a file or group");
	return R_NilValue;
    }
    
    H5Gget_comment(HID(group),STR(name),1024,buffer);
    PROTECT(ans = allocVector(STRSXP,1));
    SET_STRING_ELT(ans,0,mkChar(buffer));
    UNPROTECT(1);
    return(ans);
}

/* return R_NilValue if name is not a subgroup of group */
SEXP HDF_group_get_info(SEXP group, SEXP name)
{
    SEXP ans, names, temp;
    H5G_stat_t	info;

    if(!isFILE(group) && !isGROUP(group)) {
	error("not a file or group");
	return R_NilValue;
    }
    if(!HDF_has_subgroup(group, name))
	return R_NilValue;
    
    if (H5Gget_objinfo(HID(group),STR(name),1,&info) < 0)
	error("unable to get object information");
    PROTECT(ans = allocVector(VECSXP,3));
    PROTECT(names = allocVector(STRSXP,3));
    addClass(ans,"hdf5.info");
    
    PROTECT(temp = allocVector(REALSXP,1));
    REAL(temp)[0] = (double)info.nlink;
    SET_VECTOR_ELT(ans,0,temp);
    
    PROTECT(temp = allocVector(STRSXP,1));
    switch(info.type)
    {
    case H5G_GROUP:
	SET_STRING_ELT(temp, 0, mkChar("group"));
	break;
    case H5G_DATASET:
	SET_STRING_ELT(temp, 0, mkChar("dataset"));
	break;
    case H5G_LINK:
	SET_STRING_ELT(temp, 0, mkChar("link"));
	break;
    case H5G_TYPE:
	SET_STRING_ELT(temp, 0, mkChar("type"));
	break;
    default:
	SET_STRING_ELT(temp, 0, mkChar("default"));
    }
    SET_VECTOR_ELT(ans,1,temp);
    
    PROTECT(temp = allocVector(REALSXP,1));
    REAL(temp)[0] = (double)info.mtime;
    SET_VECTOR_ELT(ans,2,temp);
    
    SET_STRING_ELT(names,0,mkChar("links"));
    SET_STRING_ELT(names,1,mkChar("type"));
    SET_STRING_ELT(names,2,mkChar("modification.time"));
    setAttrib(ans,R_NamesSymbol,names);
    
    UNPROTECT(5);
    return(ans);
}

SEXP HDF_group_get_group(SEXP group, SEXP name)
{
	hid_t	grp;

	if(!isFILE(group) && !isGROUP(group))
		error("not a file or group");

	if(HDF_has_subgroup(group,name))
	    grp = H5Gopen(HID(group),STR(name));
	else
	    return R_NilValue;

	if(grp < 0)
		error("unable to open group");
	return H5Gsexp(grp);
}

SEXP HDF_group_get_dataset(SEXP group,SEXP name)
{
	hid_t	ds;

	if(!isFILE(group) && !isGROUP(group))
		error("not a file or group");
        if(HDF_has_subgroup(group,name))
	    ds = H5Dopen(HID(group),STR(name));
	else
	    return R_NilValue;

	if(ds < 0)
		error("unable to open dataset");
	return H5Dsexp(ds);
}

SEXP HDF_group_mkgroup(SEXP group, SEXP name)
{
    hid_t	grp;
    
    if(!isFILE(group) && !isGROUP(group)) 
	error("not a file or group");

    if( !isString(name) )
      error("name must be a string");

    if( length(name) != 1 )
      error("name must have length 1");
    
    /* Open the group if it already exists */
    if(HDF_has_subgroup(group, name)) {
	grp = H5Gopen(HID(group),STR(name));
	return H5Gsexp(grp);
    }
    
    /* Otherwise create a new group */
    grp = H5Gcreate(HID(group),STR(name),0);
    if(grp < 0)
	error("unable to create a group here");
    return H5Gsexp(grp);
}

/* C level interface to the functionality */
hid_t HDF_groupmkgroup(hid_t group, char *name)
{
    hid_t grp;
    H5I_type_t ftype;

    ftype = H5Iget_type(group);

    if(ftype != H5I_FILE && ftype != H5I_GROUP)
	error("bad hid type, cannot open %s", name);

    if(HDF_hassubgroup(group, name))
	grp = H5Gopen(group, name);
    else
	grp = H5Gcreate(group, name, 0);
    if( grp < 0 )
	error("unable to create group %s", name);
    return grp;
}



SEXP HDF_group_delete(SEXP group, SEXP name)
{
    if(!isFILE(group) && !isGROUP(group)) 
	error("first argument is not a file or group");
    
    if( !isString(name) || length(name) > 1)
      error("invalid name");

    if( !HDF_has_subgroup(group, name) )
      error("the group has no element named %s", STR(name));


    if(H5Gunlink(HID(group),STR(name)) < 0)
	warning("unable to delete \"%s\"",STR(name));
    return R_NilValue;
}

