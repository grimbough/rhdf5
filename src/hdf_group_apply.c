#include "common.h"
/**
 * We need to do a lot of work to apply properly 
 */
struct apply_state {
  int   index;
  SEXP  dataset;
  SEXP  funcall;
  SEXP  results;
};

struct masked_state {
  int mask;
  int count;
};

#define MASKED_DATASET 0x01
#define MASKED_GROUP   0x02
#define MASKED_LINK    0x04
#define MASKED_TYPE    0x08
#define MASKED_ANY (MASKED_DATASET|MASKED_GROUP|MASKED_LINK|MASKED_TYPE)


/*
 * We find ourselves having to count the number of a particular type
 * since the apply function could encounter a mixture of things within
 * a particular group
 */
int HDF_masked_count(hid_t group_id,const char* member_name,void* data)
{
  H5G_stat_t           buf;
  struct masked_state *state = (struct masked_state*)data;
  H5Gget_objinfo(group_id,member_name,0,&buf);
  switch(buf.type) {
  case H5G_GROUP:if(state->mask & MASKED_GROUP) state->count++;break;
  case H5G_DATASET:if(state->mask & MASKED_DATASET) state->count++;break;
  case H5G_LINK:if(state->mask & MASKED_LINK) state->count++;break;
  case H5G_TYPE:if(state->mask & MASKED_TYPE) state->count++;break;
  }
  return 0;
}
int HDF_count(hid_t group,int mask) 
{
  struct masked_state state;
  state.count = 0;
  state.mask  = mask;
  H5Giterate(group,".",NULL,HDF_masked_count,(void*)&state);
  return state.count;
}

int HDF_group_apply_iterator(hid_t group_id,const char* member_name,void* userdata)
{
  H5G_stat_t buf;
  hid_t      dataset;
  struct     apply_state *apply = (struct apply_state *)userdata;
  
  H5Gget_objinfo(group_id,member_name,0,&buf);
  if(buf.type == H5G_DATASET) {
    dataset = H5Dopen(group_id,member_name);
    SETCAR(apply->dataset,H5Dsexp(dataset));
    SET_VECTOR_ELT(apply->results,apply->index,eval(apply->funcall,R_GlobalEnv));
    apply->index++;
  }
  return 0;
}

SEXP HDF_group_apply(SEXP group,SEXP FUN,SEXP region,SEXP args)
{
  int i,n;
  struct apply_state state;
  SEXP tmp,subscripts,myargs;

  if(!isFunction(FUN))
    error("FUN must be a function");
  
  /* Construct a function */
  myargs = R_NilValue;
  n = length(args);
  for(i=0;i<n;i++) myargs = LCONS(VECTOR_ELT(args,i),myargs);
  if(region != R_NilValue) {
    n = length(region);
    subscripts = R_NilValue;
    for(i=0;i<n;i++) {
      if(VECTOR_ELT(region,i) == R_NilValue)
	subscripts = LCONS(R_MissingArg,subscripts);
      else
	subscripts = LCONS(VECTOR_ELT(region,i),subscripts);
    }
    PROTECT(tmp = LCONS(R_BracketSymbol,LCONS(R_NilValue,subscripts)));
    PROTECT(state.funcall = LCONS(FUN,LCONS(tmp,myargs)));
    state.dataset = CDR(tmp);
  } else {
    PROTECT(state.funcall = LCONS(FUN,LCONS(R_NilValue,myargs)));
    state.dataset = CDR(state.funcall);
  }

  /* Call the iterator */
  n = HDF_count(HID(group),MASKED_DATASET);
  PROTECT(state.results = allocVector(VECSXP,n));
  state.index = 0;
  H5Giterate(HID(group),".",NULL,HDF_group_apply_iterator,(void*)&state);
  UNPROTECT(2);
  if(region != R_NilValue) UNPROTECT(1);
  return state.results;
}
