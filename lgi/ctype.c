/*
 * Dynamic Lua binding to GObject using dynamic gobject-introspection.
 *
 * Copyright (c) 2012 Pavel Holejsovsky
 * Licensed under the MIT license:
 * http://www.opensource.org/licenses/mit-license.php
 *
 * Implements type-related utilites, mainly conversion between C and
 * Lua types.
 */

#include <string.h>
#include <ffi.h>
#include "lgi.h"

/* Lightuserdata is key of 'ctype' table in the registry. */
static int ctype_api;

enum {
  CTYPE_BASE          = 0x0f,
  CTYPE_BASE_VOID     = 0x00,
  CTYPE_BASE_BOOLEAN  = 0x01,
  CTYPE_BASE_INT      = 0x02,
  CTYPE_BASE_UINT     = 0x03,
  CTYPE_BASE_FLOAT    = 0x04,
  CTYPE_BASE_GTYPE    = 0x05,
  CTYPE_BASE_STRING   = 0x06,
  CTYPE_BASE_COMPOUND = 0x07,
  CTYPE_BASE_ENUM     = 0x08,
  CTYPE_BASE_ARRAY    = 0x09,
  CTYPE_BASE_LIST     = 0x0a,
  CTYPE_BASE_HASH     = 0x0b,
  CTYPE_BASE_CARRAY   = 0x0c,
  CTYPE_BASE_CALLABLE = 0x0d,

  CTYPE_VARIANT       = 0x30,
  CTYPE_VARIANT_SHIFT = 8,
  CTYPE_TRANSFER      = 0x40,
  CTYPE_OPTIONAL      = 0x80,
  CTYPE_POINTER       = 0x100,

  CTYPE_VARIANT_INT_8           = 0x00,
  CTYPE_VARIANT_INT_16          = 0x10,
  CTYPE_VARIANT_INT_32          = 0x20,
  CTYPE_VARIANT_INT_64          = 0x30,
  CTYPE_VARIANT_FLOAT_FLOAT     = 0x00,
  CTYPE_VARIANT_FLOAT_DOUBLE    = 0x10,
  CTYPE_VARIANT_STRING_UTF8     = 0x00,
  CTYPE_VARIANT_STRING_FILENAME = 0x10,
  CTYPE_VARIANT_ARRAY_ARRAY     = 0x00,
  CTYPE_VARIANT_ARRAY_PTRARRAY  = 0x10,
  CTYPE_VARIANT_ARRAY_BYTEARRAY = 0x20,
  CTYPE_VARIANT_ARRAY_FIXEDC    = 0x30,
  CTYPE_VARIANT_LIST_SLIST      = 0x00,
  CTYPE_VARIANT_LIST_LIST       = 0x10,
};

/* Very similar to GIArgument, but contains also ffi_arg and ffi_sarg
   to handle return values well. */
typedef union _CTypeValue
{
  gboolean v_boolean;
  gint8 v_int8;
  guint8 v_uint8;
  gint16 v_int16;
  guint16 v_uint16;
  gint32 v_int32;
  guint32 v_uint32;
  gint64 v_int64;
  guint64 v_uint64;
  gfloat v_float;
  gdouble v_double;
  GType v_gtype;
  gpointer v_pointer;
  ffi_arg v_uarg;
  ffi_sarg v_sarg;
} CTypeValue;

/* Throws an error related to given type and argument.  If 'extra' is
   NULL, assumes that 'bad type' error is requested, otherwise detail
   part is fromatted using 'extra'. */
static int
ctype_error (lua_State *L, int nti, int ntipos, int dir, int narg,
	     const char *extra, ...)
{
  int level, args = 3;

  /* Preparations, and get 'ctype' api table on the stack. */
  luaL_checkstack (L, 3, NULL);
  narg = lua_absindex (L, narg);
  nti = lua_absindex (L, nti);
  lua_rawgetp (L, LUA_REGISTRYINDEX, &ctype_api);

  /* Increase context.level, in order to skip this C-frame. */
  lua_getfield (L, -1, "context");
  lua_getfield (L, -1, "level");
  level = lua_tonumber (L, -1);
  lua_pushnumber (L, level + 1);
  lua_setfield (L, -3, "level");
  lua_pop (L, 3);

  /* Invoke method from 'type' table with proper arguments. */
  lua_getfield (L, -1, "error");
  lua_pushvalue (L, nti);
  lua_pushnumber (L, ntipos);
  lua_pushnumber (L, dir);
  lua_pushnumber (L, narg);
  if (extra != NULL)
    {
      va_list vl;
      va_start (vl, extra);
      lua_pushvfstring (L, extra, vl);
      va_end (vl);
      args++;
    }
  lua_call (L, args, 0);
  return luaL_error (L, "ctype.error() should not return");
}

typedef enum _GuardScope {
  GUARD_SCOPE_BOTH,
  GUARD_SCOPE_ROLLBACK,
  GUARD_SCOPE_COMMIT
} GuardScope;

typedef enum _GuardItemType {
  GUARD_TYPE_FREE,
  GUARD_TYPE_COMPOUND_OWN,
  GUARD_TYPE_COMPOUND_UNOWN,
  GUARD_TYPE_GARRAY,
  GUARD_TYPE_GPTRARRAY,
  GUARD_TYPE_GBYTEARRAY
} GuardItemType;

typedef void (*CTypeDestroyNotify)(lua_State *L, gpointer user_data);

static void
ctype_destroy_free (lua_State *L, gpointer user_data)
{
  (void) L;
  g_free (user_data);
}

static void
ctype_destroy_compound_own (lua_State *L, gpointer user_data)
{
  lgi_compound_2lua (L, 0, user_data, 1, 0);
  lua_pop (L, 1);
}

static void
ctype_destroy_compound_unown (lua_State *L, gpointer user_data)
{
  lgi_compound_2lua (L, 0, user_data, -1, 0);
  lua_pop (L, 1);
}

static void
ctype_destroy_garray (lua_State *L, gpointer user_data)
{
  (void) L;
  g_array_unref (user_data);
}

static void
ctype_destroy_gptrarray (lua_State *L, gpointer user_data)
{
  (void) L;
  g_ptr_array_unref (user_data);
}

static void
ctype_destroy_gbytearray (lua_State *L, gpointer user_data)
{
  (void) L;
  g_byte_array_unref (user_data);
}

static const CTypeDestroyNotify guard_destroy[] = {
  ctype_destroy_free, /* GUARD_TYPE_FREE */
  ctype_destroy_compound_own, /* GUARD_TYPE_COMPOUND_OWN */
  ctype_destroy_compound_unown, /* GUARD_TYPE_COMPOUND_UNOWN */
  ctype_destroy_garray, /* GUARD_TYPE_GARRAY */
  ctype_destroy_gptrarray, /* GUARD_TYPE_GPTRARRAY */
  ctype_destroy_gbytearray /* GUARD_TYPE_GBYTEARRAY */
};

typedef struct _GuardItem {
  guint type : 6;
  guint destroy_on_commit : 1;
  guint destroy_on_gc : 1;
  gpointer user_data;
} GuardItem;

typedef struct _LgiCTypeGuard
{
  GuardItem *items;
  int n_items;
  int items_alloced;
  GuardItem first_item[1];
} LgiCTypeGuard;

/* Lightuserdata of address of this is key in registry to guard
   metatable. */
int guard_mt;

static int
ctype_guard_gc (lua_State *L)
{
  int n;
  GuardItem *item;
  LgiCTypeGuard *guard = lua_touserdata (L, 1);

  /* Go through all non-committed items and invoke them. */
  for (n = guard->n_items, item = guard->items; n-- > 0; item++)
    if (item->destroy_on_gc)
      guard_destroy[item->type] (L, item->user_data);

  /* Deallocate the block, if it was allocated in external memory. */
  if (guard->items != guard->first_item)
    g_free (guard->items);

  return 0;
}

LgiCTypeGuard *
lgi_ctype_guard_create (lua_State *L, int n_items)
{
  LgiCTypeGuard *guard = NULL;

  if (n_items == 0)
    /* When no items are needed, do not create guard at all. */
    lua_pushnil (L);
  else
    {
      /* Create and initialize guard, allocate initial space in the
	 userdata space itself to avoid excessive fragmentation. */
      guard = lua_newuserdata (L, G_STRUCT_OFFSET (LgiCTypeGuard,
						   first_item) +
			       n_items * sizeof (GuardItem));
      guard->items = guard->first_item;
      guard->n_items = 0;
      guard->items_alloced = n_items;
      memset (guard->items, 0, sizeof (GuardItem) * n_items);
      lua_rawgetp (L, LUA_REGISTRYINDEX, &guard_mt);
      lua_setmetatable (L, -2);
    }

  return guard;
}

void
lgi_ctype_guard_commit (lua_State *L, LgiCTypeGuard *guard)
{
  /* Go through the guard and null (deactivate) all commitable
     entries, so that upon __gc they will not be destroyed. */
  if (guard != NULL)
    {
      int n;
      GuardItem *item;
      for (n = guard->n_items, item = guard->items; n-- > 0; item++)
	{
	  if (item->destroy_on_commit)
	    guard_destroy[item->type] (L, item->user_data);

	  /* Commit phase passed, do not destroy this object on
	     rollback any more. */
	  item->destroy_on_gc = FALSE;
	}
    }
}

static void
ctype_guard_add (lua_State *L, LgiCTypeGuard *guard, GuardItemType type,
		 GuardScope scope, gpointer user_data)
{
  GuardItem *item;

  if (G_UNLIKELY (guard == NULL))
    {
      /* Invoke destruction directly, but only for commit guards,
	 ignore rollback-only guards. */
      if (scope != GUARD_SCOPE_ROLLBACK)
	guard_destroy[type] (L, user_data);

      return;
    }

  if (G_UNLIKELY (guard->n_items == guard->items_alloced))
    {
      /* Reallocate old contents, double the count of allocated
	 items. */
      guard->items_alloced *= 2;
      GuardItem *new_items = g_new0 (GuardItem, guard->items_alloced);
      memcpy (new_items, guard->items, sizeof (GuardItem) * guard->n_items);
      if (guard->items != guard->first_item)
	g_free (guard->items);
      guard->items = new_items;
    }

  /* Add new item to the list. */
  item = &guard->items[++guard->n_items];
  item->type = type;
  item->destroy_on_commit = (scope != GUARD_SCOPE_ROLLBACK);
  item->destroy_on_gc = (scope != GUARD_SCOPE_COMMIT);
  item->user_data = user_data;
}

void
lgi_ctype_query (lua_State *L, int nti, int *ntipos,
		 gsize *size, gsize *align)
{
  guint ctype, variant;
  gboolean is_pointer;

#define GET_INFO(type)						\
  do {								\
      struct AlignTest { char offender; type testee; };		\
      *align = G_STRUCT_OFFSET (struct AlignTest, testee);	\
      *size = sizeof (type);					\
      return;							\
  } while (0)


  *size = *align = 0;
  luaL_checkstack (L, 3, NULL);
  lua_rawgeti (L, nti, (*ntipos)++);
  ctype = lua_tonumber (L, -1);
  lua_pop (L, 1);
  variant = ctype & CTYPE_VARIANT;
  is_pointer = (ctype & CTYPE_POINTER) != 0;
  switch (ctype & CTYPE_BASE)
    {
    case CTYPE_BASE_VOID:
      if (is_pointer)
	GET_INFO (gpointer);
      return;

    case CTYPE_BASE_BOOLEAN:
      GET_INFO (gboolean);

    case CTYPE_BASE_INT:
    case CTYPE_BASE_UINT:
      switch (variant)
	{
	case CTYPE_VARIANT_INT_8:
	  GET_INFO (gint8);
	case CTYPE_VARIANT_INT_16:
	  GET_INFO (gint16);
	case CTYPE_VARIANT_INT_32:
	  GET_INFO (gint32);
	case CTYPE_VARIANT_INT_64:
	  GET_INFO (gint64);
	}

    case CTYPE_BASE_FLOAT:
      if (variant == CTYPE_VARIANT_FLOAT_FLOAT)
	GET_INFO (gfloat);
      else
	GET_INFO (gdouble);

    case CTYPE_BASE_GTYPE:
      GET_INFO (GType);

    case CTYPE_BASE_STRING:
      GET_INFO (const char *);

    case CTYPE_BASE_COMPOUND:
      {
	int pos = (*ntipos)++;
	if (is_pointer)
	  GET_INFO (gpointer);

	/* Get information from the typetable. */
	lua_rawgeti (L, nti, pos);
	lua_getfield (L, -1, "_size");
	*size = lua_tonumber (L, -1);
	lua_getfield (L, -2, "_align");
	*align = lua_tonumber (L, -2);
	lua_pop (L, 3);
	return;
      }

    case CTYPE_BASE_ENUM:
      {
	/* Get type from the typetable and query base ctype from it. */
	int pos = 1;
	lua_rawgeti (L, nti, (*ntipos)++);
	lua_getfield (L, -1, "_type");
	lgi_ctype_query (L, -1, &pos, size, align);
	lua_pop (L, 2);
	return;
      }

    case CTYPE_BASE_ARRAY:
    case CTYPE_BASE_LIST:
    case CTYPE_BASE_HASH:
      /* Skip the information about argument. */
      {
	int i;
	for (i = (ctype & CTYPE_BASE) == CTYPE_BASE_HASH ? 2 : 1; i != 0; i--)
	  lgi_ctype_query (L, nti, ntipos, size, align);
	GET_INFO (gpointer);
      }

    case CTYPE_BASE_CARRAY:
      if (is_pointer)
	GET_INFO (gpointer);
      else
	{
	  /* Fixed-size C array case. */
	  int count;
	  lua_rawgeti (L, nti, (*ntipos)++);
	  count = lua_tonumber (L, -1);
	  lua_pop (L, 1);
	  lgi_ctype_query (L, nti, ntipos, size, align);
	  *size *= count;
	  return;
	}

    case CTYPE_BASE_CALLABLE:
    default:
      luaL_error (L, "bad typeinfo");
    }

#undef GET_INFO
}

static gboolean
ctype_2c_int (lua_State *L, guint ctype, int nti, int ntipos, int dir,
	      int ntiarg, int narg, CTypeValue *v)
{
  gboolean is_pointer = (ctype & CTYPE_POINTER) != 0;
  gboolean is_return = (dir == 0);

  if (!lua_isnumber (L, narg))
    return FALSE;

  switch (ctype & (CTYPE_BASE | CTYPE_VARIANT))
    {
#define HANDLE_INT(ct, size, tfield, ptrconv, pct, retfield,		\
		   val_min, val_max)					\
      case CTYPE_BASE_ ## ct | (size << CTYPE_VARIANT_SHIFT):		\
	{								\
	  lua_Number val = lua_tonumber (L, narg);			\
	  if (G_UNLIKELY (val < val_min || val > val_max))		\
	    ctype_error (L, nti, ntipos, dir, ntiarg,			\
			 "%f is out of <%f, %f>",			\
			 val, val_min, val_max);			\
	  if (G_UNLIKELY (sizeof (v->v_ ## tfield) <= sizeof (gint32))	\
	      && is_pointer)						\
	    v->v_pointer = G ## ptrconv ## _TO_POINTER ((pct) val);	\
	  else if (G_UNLIKELY (sizeof (v->v_ ## tfield) <=		\
			       sizeof (v->v_ ## retfield) &&		\
			       is_return))				\
	    v->v_ ## retfield = val;					\
	  else								\
	    v->v_ ## tfield = val;					\
	}								\
	break

      HANDLE_INT (INT, 0, int8, INT, gint, sarg,
		  -0x80, 0x7f);
      HANDLE_INT (UINT, 0, uint8, UINT, guint, uarg,
		  0, 0xff);
      HANDLE_INT (INT, 1, int16, INT, gint, sarg,
		  -0x8000, 0x7fff);
      HANDLE_INT (UINT, 1, uint16, UINT, guint, uarg,
		  0, 0xffff);
      HANDLE_INT (INT, 2, int32, INT, gint, sarg,
		  -0x80000000LL, 0x7fffffffLL);
      HANDLE_INT (UINT, 2, uint32, UINT, guint, uarg,
		  0, 0xffffffffUL);
      HANDLE_INT (INT, 3, int64, INT, gint, sarg,
		  ((lua_Number) -0x7f00000000000000LL) - 1,
		  0x7fffffffffffffffLL);
      HANDLE_INT (UINT, 3, uint64, UINT, guint, uarg,
		  0, 0xffffffffffffffffULL);

    default:
      g_assert_not_reached ();
#undef HANDLE_INT
    }

  return TRUE;
}

static void
ctype_2lua_int (lua_State *L, guint ctype, int dir, CTypeValue *v)
{
  lua_Number val;
  gboolean is_pointer = (ctype & CTYPE_POINTER) != 0;
  gboolean is_return = (dir == 0);

  switch (ctype & (CTYPE_BASE | CTYPE_VARIANT))
    {
#define HANDLE_INT(ct, size, tfield, ptrconv, retfield)		\
      case CTYPE_BASE_ ## ct | (size << CTYPE_VARIANT_SHIFT):	\
	if (G_UNLIKELY ((sizeof (v->v_ ## tfield)		\
			 <= sizeof (gint32)) && is_pointer))	\
	  val = GPOINTER_TO_  ## ptrconv (v->v_pointer);	\
	else if (G_UNLIKELY (sizeof (v->v_ ## tfield) <=	\
			       sizeof (v->v_ ## retfield) &&	\
			     is_return))			\
	  val = v->v_ ## retfield;				\
	else							\
	  val = v->v_ ## tfield;				\
	lua_pushnumber (L, val);				\
	break

      HANDLE_INT (INT, 0, int8, INT, sarg);
      HANDLE_INT (UINT, 0, uint8, UINT, uarg);
      HANDLE_INT (INT, 1, int16, INT, sarg);
      HANDLE_INT (UINT, 1, uint16, UINT, uarg);
      HANDLE_INT (INT, 2, int32, INT, sarg);
      HANDLE_INT (UINT, 2, uint32, UINT, uarg);
      HANDLE_INT (INT, 3, int64, INT, sarg);
      HANDLE_INT (UINT, 3, uint64, UINT, uarg);

#undef HANDLE_INT

    default:
      g_assert_not_reached ();
    }
}

static gboolean
ctype_2c_string (lua_State *L, LgiCTypeGuard *guard, guint ctype,
		 int narg, CTypeValue *val)
{
  if (lua_isnoneornil (L, narg))
    {
      if ((ctype & CTYPE_OPTIONAL) != 0)
	{
	  val->v_pointer = NULL;
	  return TRUE;
	}
    }
  else if (lua_isstring (L, narg))
    {
      gboolean transfer = (ctype & CTYPE_TRANSFER) != 0;
      char *str = (char *) lua_tostring (L, narg);
      if ((ctype & CTYPE_VARIANT) != 0)
	{
	  /* Convert to filename encoding. */
	  str = g_filename_from_utf8 (str, -1, NULL, NULL, NULL);
	  ctype_guard_add (L, guard, GUARD_TYPE_FREE,
			   transfer ? GUARD_SCOPE_ROLLBACK : GUARD_SCOPE_BOTH,
			   str);
	}
      else if (transfer)
	{
	  /* Transfer needed, so duplicate the string and protect
	     by guard. */
	  str = g_strdup (str);
	  ctype_guard_add (L, guard, GUARD_TYPE_FREE, GUARD_SCOPE_ROLLBACK,
			   str);
	}
      val->v_pointer = str;
      return TRUE;
    }

  return FALSE;
}

static void
ctype_2lua_string (lua_State *L, LgiCTypeGuard *guard, guint ctype,
		   CTypeValue *val)
{
  char *str = val->v_pointer;
  gboolean transfer = (ctype & CTYPE_TRANSFER) != 0;
  if ((ctype & CTYPE_VARIANT) != 0)
    {
      /* Convert to filename encoding and add proper guard. */
      str = g_filename_to_utf8 (str, -1, NULL, NULL, NULL);
      ctype_guard_add (L, guard, GUARD_TYPE_FREE,
		       transfer ? GUARD_SCOPE_ROLLBACK : GUARD_SCOPE_BOTH,
		       str);
    }
  else if (transfer)
    /* Add guard which frees transferred data unless committed. */
    ctype_guard_add (L, guard, GUARD_TYPE_FREE, GUARD_SCOPE_COMMIT, str);

  /* Finally, store the string to the Lua stack. */
  lua_pushstring (L, str);
}

static gboolean
ctype_2c_enum (lua_State *L, int nti, int ntipos, int dir,
	       int narg, CTypeValue *v)
{
  guint ctype;
  lua_Number num;
  gboolean ok;

  /* Get the enum typetable. */
  lua_rawgeti (L, nti, ntipos + 1);
  if (lua_type (L, narg) == LUA_TNUMBER)
    num = lua_tonumber (L, narg);
  else
    {
      /* Call the enum type, its 'constructor' converts any symbolic
	 values to number. */
      lua_pushvalue (L, -1);
      lua_pushvalue (L, narg);
      lua_call (L, 1, 1);
      num = lua_tonumber (L, -1);
      lua_pop (L, 1);
    }

  /* Get type describing underlying numeric type of the enum. */
  lua_getfield (L, -1, "_type");
  ctype = lua_tonumber (L, -1);
  lua_pop (L, 1);

  /* Get number from Lua. */
  lua_pushnumber (L, num);
  ok = ctype_2c_int (L, ctype, nti, ntipos, dir, narg, -1, v);
  lua_pop (L, 1);
  return ok;
}

static void
ctype_2lua_enum (lua_State *L, int nti, int ntipos, int dir, CTypeValue *v)
{
  guint ctype;

  /* Get the enum typetable. */
  lua_rawgeti (L, nti, ntipos + 1);

  /* Get the ctype fromat from the table and retrieve number from
     source */
  lua_getfield (L, -1, "_type");
  ctype = lua_tonumber (L, -1);
  lua_pop (L, 1);
  ctype_2lua_int (L, ctype, dir, v);

  /* Convert number on the stack to symbolic value. Then remove enum
     table from the stack. */
  lua_gettable (L, -2);
  lua_remove (L, -2);
}

static gboolean
ctype_2c_compound (lua_State *L, LgiCTypeGuard *guard,
		   guint ctype, int nti, int ntipos,
		   int dir, int narg, gpointer target)
{
  int size = 0;
  CTypeValue *v = NULL;
  lua_rawgeti (L, nti, ntipos + 1);
  if (G_UNLIKELY (ctype && CTYPE_POINTER) != 0)
    {
      /* Get size of the structure if it is not passed by pointer. */
      lua_getfield (L, -1, "_size");
      size = lua_tonumber (L, -1);
      lua_pop (L, 1);
      if (G_UNLIKELY (size == 0))
	ctype_error (L, nti, ntipos, dir, narg, "cannot make copy");
    }
  else
    /* Remember pointer to store address to. */
    v = target;

  /* Check for nil case. */
  if ((ctype & CTYPE_OPTIONAL) != 0 && lua_isnoneornil (L, narg))
    {
      /* Pop type table. */
      lua_pop (L, 1);
      if (G_LIKELY (v != NULL))
	v->v_pointer = NULL;
      else
	memset (target, 0, size);
    }
  else
    {
      gpointer ptr;

      /* Get the compound typetable to the stack and get pointer to C
	 compound. This also gets rid of the type table from the
	 stack. */
      ptr = lgi_compound_2c (L, narg, -1);
      if (G_UNLIKELY (ptr == NULL))
	return FALSE;

      /* Store target location according to byref/byvalue indicator. */
      if (G_UNLIKELY (v == NULL))
	/* Byte-copy compound to the target location. */
	memcpy (target, ptr, size);
      else
	{
	  v->v_pointer = ptr;

	  /* Add guard in case that we need it. */
	  if ((ctype & CTYPE_TRANSFER) != 0)
	    {
	      /* Remove ownership from the object, because ownership
		 is transferred. */
	      if (G_UNLIKELY (!lgi_compound_own (L, narg, -1)))
		ctype_error (L, nti, ntipos, dir, narg,
			     "cannot transfer ownership");

	      /* Add guard on rollback which readds compound's
		 ownership back. */
	      ctype_guard_add (L, guard, GUARD_TYPE_COMPOUND_OWN,
			       GUARD_SCOPE_ROLLBACK, ptr);
	    }
	}
    }

  return TRUE;
}

static void
ctype_2lua_compound (lua_State *L, LgiCTypeGuard *guard, guint ctype,
		     int nti, int ntipos, int parent, gpointer src)
{
  if (src == NULL)
    /* NULL is translated as nil. */
    lua_pushnil (L);
  else
    {
      gboolean transfer = (ctype & CTYPE_TRANSFER) != 0;

      /* If we actually have pointer to the target, dereference it
	 first. */
      if ((ctype & CTYPE_POINTER) != 0)
	src = ((CTypeValue *) src)->v_pointer;

      /* Get the typetable and convert pointer to the compound object. */
      lua_rawgeti (L, nti, ntipos + 1);
      lgi_compound_2lua (L, -1, src, transfer, parent);
      lua_remove (L, -2);
      lua_remove (L, -2);

      /* If we took ownership, make sure that if not committed,
	 ownership is removed. */
      if (transfer)
	ctype_guard_add (L, guard, GUARD_TYPE_COMPOUND_UNOWN,
			 GUARD_SCOPE_ROLLBACK, src);
    }
}

/* Advances *ntipos to element type, calculates element size and
   element count for array marshaling.  Sets up ntipos to the position
   of the argument type. */
static void
ctype_2c_array_info (lua_State *L, int ctype,
		     int nti, int *ntipos, int *endpos, int dir,
		     int narg, gsize *size, int *count)
{
  gsize unused;
  int ltype, sourcecount, basepos = (*ntipos);

  /* Parse and skip fixed-size indicator if present. */
  *count = 0;
  (*ntipos)++;
  if ((ctype & CTYPE_VARIANT) == CTYPE_VARIANT_ARRAY_FIXEDC)
    {
      lua_rawgeti (L, nti, (*ntipos)++);
      *count = lua_tonumber (L, -1);
      lua_pop (L, 1);
    }

  /* Get element size. */
  *endpos = *ntipos;
  lgi_ctype_query (L, nti, endpos, size, &unused);

  /* Check the type of argument. */
  ltype = lua_type (L, narg);
  if ((ltype == LUA_TNIL || ltype == LUA_TNONE)
      && (ctype & CTYPE_OPTIONAL) != 0)
    return;
  if (ltype == LUA_TTABLE)
    sourcecount = luaL_len (L, narg);
  else if (*size == 1 && (ltype == LUA_TSTRING
			  || luaL_testudata (L, narg, LGI_BYTES_BUFFER)))
    sourcecount = luaL_len (L, narg);
  else
    /* Signalize an error, source argument is not suitable. */
    ctype_error (L, nti, basepos, dir, narg, NULL);

  if (*count == 0)
    *count = sourcecount;
  else if (*count < sourcecount)
    ctype_error (L, nti, basepos, dir, narg,
		 "expecting array size %d, got %d",
		 (int) *count, (int) sourcecount);
}

static void
ctype_2c_flatarray (lua_State *L, LgiCTypeGuard *guard,
		    int nti, int ntipos, int dir, int narg,
		    gpointer array, gsize eltsize, int count)
{
  int pos, ltype = lua_type (L, narg), i;
  char *dest;

  /* Handle special case of accepting strings and byte arrays for
     arrays with eltsize==1. */
  if (eltsize == 1)
    {
      /* It is possible to accept string or byte buffers as source for
	 1byte-elements arrays. */
      gpointer src = (ltype == LUA_TSTRING)
	? (gpointer) lua_tostring (L, narg)
	: luaL_testudata (L, narg, LGI_BYTES_BUFFER);
      if (src != NULL)
	{
	  memcpy (array, src, count);
	  return;
	}
    }

  /* Otherwise go through the input table and marshal
     element-by-element. */
  dest = array;
  for (i = 0; i < count; i++, dest += eltsize)
    {
      lua_pushnumber (L, i + 1);
      lua_gettable (L, narg);
      pos = ntipos;
      lgi_ctype_2c (L, guard, nti, &pos, dir, -1, dest);
      lua_pop (L, 1);
    }
}

static gboolean
ctype_2c_array (lua_State *L, LgiCTypeGuard *guard, guint ctype,
		int nti, int *ntipos, int dir, int narg,
		gpointer val)
{
  gsize size;
  int endpos, count;
  gpointer raw_array;
  GuardScope scope = (ctype & CTYPE_TRANSFER) != 0
    ? GUARD_SCOPE_ROLLBACK : GUARD_SCOPE_BOTH;

  /* Get information about the array (size, elementcount, type
     description positions). */
  ctype_2c_array_info (L, ctype, nti, ntipos, &endpos, dir,
		       narg, &size, &count);

  /* Check nil case. */
  if (lua_isnoneornil (L, narg) && (ctype & CTYPE_OPTIONAL) != 0)
    {
      *(gpointer *) val = NULL;
      *ntipos = endpos;
      return TRUE;
    }

  /* Allocate the array, according to the type. */
  switch (ctype & CTYPE_VARIANT)
    {
    case CTYPE_VARIANT_ARRAY_ARRAY:
      {
	GArray *array = g_array_sized_new (FALSE, TRUE, size, count);
	raw_array = array->data;
	ctype_guard_add (L, guard, GUARD_TYPE_GARRAY, scope, array);
	*(gpointer *) val = array;
	break;
      }

    case CTYPE_VARIANT_ARRAY_PTRARRAY:
      {
	GPtrArray *array = g_ptr_array_sized_new (count);
	raw_array = array->pdata;
	ctype_guard_add (L, guard, GUARD_TYPE_GPTRARRAY, scope, array);
	*(gpointer *) val = array;
	break;
      }

    case CTYPE_VARIANT_ARRAY_BYTEARRAY:
      {
	GByteArray *array = g_byte_array_sized_new (count);
	raw_array = array->data;
	ctype_guard_add (L, guard, GUARD_TYPE_GBYTEARRAY, scope, array);
	*(gpointer *) val = array;
	break;
      }

    case CTYPE_VARIANT_ARRAY_FIXEDC:
      {
	if (ctype & CTYPE_POINTER)
	  {
	    raw_array = g_malloc0_n (count, size);
	    ctype_guard_add (L, guard, GUARD_TYPE_FREE, scope, raw_array);
	    *(gpointer *) val = raw_array;
	  }
	else
	  raw_array = val;
	break;
      }

    default:
      g_assert_not_reached ();
    }

  /* Marshal the contents of the array. */
  ctype_2c_flatarray (L, guard, nti, *ntipos, dir,
		      narg, raw_array, size, count);

  /* Advance ntipos to the end of elementtype description. */
  *ntipos = endpos;
  return TRUE;
}

static void
ctype_2lua_array (lua_State *L, LgiCTypeGuard *guard, guint ctype,
		  int nti, int *ntipos, gpointer src)
{
  gpointer rawarray;
  int count, typepos;
  gsize size, align;
  gboolean transfer = (ctype & CTYPE_TRANSFER) != 0;

  /* Convert NULL to nil. */
  if (src == NULL)
    {
      lua_pushnil (L);
      return;
    }

  /* Get pointer to raw array and count to marshal. */
  (*ntipos)++;
  switch (ctype & CTYPE_VARIANT)
    {
    case CTYPE_VARIANT_ARRAY_ARRAY:
      {
	GArray *array = src;
	rawarray = array->data;
	count = array->len;
	if (transfer)
	  ctype_guard_add (L, guard, GUARD_TYPE_GARRAY,
			   GUARD_SCOPE_COMMIT, array);
	break;
      }

    case CTYPE_VARIANT_ARRAY_PTRARRAY:
      {
	GPtrArray *array = src;
	rawarray = array->pdata;
	count = array->len;
	if (transfer)
	  ctype_guard_add (L, guard, GUARD_TYPE_GPTRARRAY,
			   GUARD_SCOPE_COMMIT, array);
	break;
      }

    case CTYPE_VARIANT_ARRAY_BYTEARRAY:
      {
	GByteArray *array = src;
	rawarray = array->data;
	count = array->len;
	if (transfer)
	  ctype_guard_add (L, guard, GUARD_TYPE_GBYTEARRAY,
			   GUARD_SCOPE_COMMIT, array);
	break;
      }

    case CTYPE_VARIANT_ARRAY_FIXEDC:
      {
	lua_rawgeti(L, nti, (*ntipos)++);
	count = lua_tonumber (L, -1);
	lua_pop (L, 1);
	if (ctype & CTYPE_POINTER)
	  {
	    rawarray = *(gpointer *) src;
	    if (transfer)
	      ctype_guard_add (L, guard, GUARD_TYPE_FREE,
			       GUARD_SCOPE_COMMIT, rawarray);
	  }
	else
	  rawarray = src;
	break;
      }

    default:
      g_assert_not_reached ();
    }

  /* Query the type to get element size and endpos. */
  typepos = *ntipos;
  lgi_ctype_query (L, nti, ntipos, &size, &align);

  if (size == 1)
    {
      /* Marshal byte-sized units into bytes.buffer. */
      memcpy (lua_newuserdata (L, count), data, count);
      luaL_getmetatable (L, LGI_BYTES_BUFFER);
      lua_setmetatable (L, -2);
    }
  else
    {
      int i;
      lua_createtable (L, count, 0);
      char *source = rawarray;
      for (i = 0; i < count; i++, source += size)
	{
	  *ntipos = typepos;
	  lgi_ctype_2lua (L, guard, nti, ntipos, dir, narg, source);
	  lua_rawseti (L, -2, i + 1);
	}
    }
}

void
lgi_ctype_2c (lua_State *L, LgiCTypeGuard *guard, int nti, int *ntipos,
	      int dir, int narg, gpointer target)
{
  guint ctype;
  int ltype, basepos = *ntipos;
  CTypeValue *val = target;

  luaL_checkstack (L, 3, NULL);
  nti = lua_absindex (L, nti);
  narg = lua_absindex (L, narg);

  /* Get basic kind of ctype. */
  lua_rawgeti (L, nti, (*ntipos)++);
  ctype = lua_tonumber (L, -1);
  lua_pop (L, 1);
  switch (ctype & CTYPE_BASE)
    {
    case CTYPE_BASE_VOID:
      if ((ctype & CTYPE_POINTER) != 0)
	{
	  /* We accept quite a big range of types of sources for
	     generic ptr.  It is not exactly safe, but allows a lot of
	     freedom in assorted overrides. */
	  ltype = lua_type (L, narg);
	  if (ltype == LUA_TNIL)
	    val->v_pointer = NULL;
	  if (ltype == LUA_TLIGHTUSERDATA)
	    val->v_pointer = lua_touserdata (L, narg);
	  else if (ltype == LUA_TSTRING)
	    val->v_pointer = (gpointer) lua_tostring (L, narg);
	  else if (ltype == LUA_TUSERDATA)
	    {
	      val->v_pointer = lgi_udata_test (L, narg, LGI_BYTES_BUFFER);
	      if (val->v_pointer == NULL)
		{
		  val->v_pointer = lgi_compound_2c (L, narg, 0);
		  if (val->v_pointer == NULL)
		    break;
		}
	    }
	}
      return;

    case CTYPE_BASE_BOOLEAN:
      val->v_boolean = lua_toboolean (L, narg);
      return;

    case CTYPE_BASE_INT:
    case CTYPE_BASE_UINT:
      if (ctype_2c_int (L, ctype, nti, basepos, dir, narg, narg, target))
	return;
      break;

    case CTYPE_BASE_FLOAT:
      if (lua_isnumber (L, narg))
	{
	  lua_Number n = lua_tonumber (L, narg);
	  if ((ctype & CTYPE_VARIANT) != 0)
	    val->v_double = n;
	  else
	    val->v_float = n;

	  return;
	}
      break;

    case CTYPE_BASE_GTYPE:
      /* Get either string or repotable containing '_gtype' field with
	 the string. */
      ltype = lua_type (L, narg);
      if (ltype == LUA_TTABLE)
	{
	  lua_getfield (L, narg, "_gtype");
	  ltype = lua_type (L, -1);
	  narg = -1;
	}
      if (ltype == LUA_TSTRING)
	{
	  val->v_gtype = g_type_from_name (lua_tostring (L, narg));
	  if (narg == -1)
	    lua_pop (L, 1);
	  return;
	}
      break;

    case CTYPE_BASE_STRING:
      if (ctype_2c_string (L, guard, ctype, narg, val))
	return;
      break;

    case CTYPE_BASE_ENUM:
      if (ctype_2c_enum (L, nti, basepos, dir, narg, val))
	{
	  (*ntipos)++;
	  return;
	}
      break;

    case CTYPE_BASE_COMPOUND:
      if (ctype_2c_compound (L, guard, ctype, nti, basepos, dir, narg, val))
	{
	  (*ntipos)++;
	  return;
	}
      break;

    case CTYPE_BASE_ARRAY:
      *ntipos = basepos;
      if (ctype_2c_array (L, guard, ctype, nti, ntipos, dir, narg, val))
	  return;
      break;

    case CTYPE_BASE_LIST:
    case CTYPE_BASE_HASH:

    case CTYPE_BASE_CARRAY:
    case CTYPE_BASE_CALLABLE:
      luaL_error (L, "automatic marshal of %d not supported.", ctype);
    }

  /* Failure, report error properly. */
  ctype_error (L, nti, basepos, dir, narg, NULL);
}

void
lgi_ctype_2lua (lua_State *L, LgiCTypeGuard *guard, int nti, int *ntipos,
		int dir, int parent, gpointer source)
{
  guint ctype;
  CTypeValue *val = source;
  int basepos = *ntipos;

  luaL_checkstack (L, 3, NULL);
  nti = lua_absindex (L, nti);
  parent = lua_absindex (L, parent);

  /* Get basic kind of ctype. */
  lua_rawgeti (L, nti, (*ntipos)++);
  ctype = lua_tonumber (L, -1);
  lua_pop (L, 1);
  switch (ctype & CTYPE_BASE)
    {
    case CTYPE_BASE_VOID:
      if ((ctype & CTYPE_POINTER) != 0)
	lua_pushlightuserdata (L, val->v_pointer);
      return;

    case CTYPE_BASE_BOOLEAN:
      lua_pushboolean (L, val->v_boolean);
      return;

    case CTYPE_BASE_INT:
    case CTYPE_BASE_UINT:
      ctype_2lua_int (L, ctype, dir, source);
      return;

    case CTYPE_BASE_FLOAT:
      lua_pushnumber (L, (ctype & CTYPE_VARIANT) != 0
		      ? val->v_double : val->v_float);
      return;

    case CTYPE_BASE_GTYPE:
      lua_pushstring (L, g_type_name (val->v_gtype));
      return;

    case CTYPE_BASE_STRING:
      ctype_2lua_string (L, guard, ctype, source);
      return;

    case CTYPE_BASE_ENUM:
      ctype_2lua_enum (L, nti, basepos, dir, source);
      (*ntipos)++;
      return;

    case CTYPE_BASE_COMPOUND:
      ctype_2lua_compound (L, guard, ctype, nti, basepos, parent, source);
      (*ntipos)++;
      return;

    case CTYPE_BASE_ARRAY:
      ctype_2lua_array (L, guard, ctype, nti, &basepos, source);
      *ntipos = basepos;
      return;

    case CTYPE_BASE_LIST:
    case CTYPE_BASE_HASH:

    case CTYPE_BASE_CARRAY:
    case CTYPE_BASE_CALLABLE:
      luaL_error (L, "automatic marshal of %d not supported.", ctype);
    }
}

static const struct luaL_Reg ctype_api_reg[] = {
  { NULL, NULL }
};

void
lgi_ctype_init (lua_State *L)
{
  /* Register guard metatable. */
  lua_newtable (L);
  lua_pushcfunction (L, ctype_guard_gc);
  lua_setfield (L, -2, "__gc");
  lua_rawsetp (L, LUA_REGISTRYINDEX, &guard_mt);

  /* Create 'type' API table in main core API table. */
  lua_newtable (L);
  luaL_setfuncs (L, ctype_api_reg);
  lua_newtable (L);
  lua_setfield (L, -1, "context");
  lua_pushlightuserdata (L, &ctype_api);
  lua_pushvalue (L, -2);
  lua_rawset (L, LUA_REGISTRYINDEX);
  lua_setfield (L, -2, "ctype");
}
