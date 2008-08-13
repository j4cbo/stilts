/* This is a standalone version of the real mysql.h, to avoid parsing problems
   caused by GCC extensions in system header files. See README.
*/

typedef unsigned long long my_ulonglong;
typedef unsigned long my_ulonglong_returned;
typedef int my_bool;
typedef struct st_mysql MYSQL;
typedef struct st_mysql_res MYSQL_RES;

enum enum_field_types { MYSQL_TYPE_DECIMAL, MYSQL_TYPE_TINY,
                        MYSQL_TYPE_SHORT,  MYSQL_TYPE_LONG,
                        MYSQL_TYPE_FLOAT,  MYSQL_TYPE_DOUBLE,
                        MYSQL_TYPE_NULL,   MYSQL_TYPE_TIMESTAMP,
                        MYSQL_TYPE_LONGLONG,MYSQL_TYPE_INT24,
                        MYSQL_TYPE_DATE,   MYSQL_TYPE_TIME,
                        MYSQL_TYPE_DATETIME, MYSQL_TYPE_YEAR,
                        MYSQL_TYPE_NEWDATE, MYSQL_TYPE_VARCHAR,
                        MYSQL_TYPE_BIT,
                        MYSQL_TYPE_NEWDECIMAL=246,
                        MYSQL_TYPE_ENUM=247,
                        MYSQL_TYPE_SET=248,
                        MYSQL_TYPE_TINY_BLOB=249,
                        MYSQL_TYPE_MEDIUM_BLOB=250,
                        MYSQL_TYPE_LONG_BLOB=251,
                        MYSQL_TYPE_BLOB=252,
                        MYSQL_TYPE_VAR_STRING=253,
                        MYSQL_TYPE_STRING=254,
                        MYSQL_TYPE_GEOMETRY=255

};

typedef struct st_mysql_field {
  char *name;                 /* Name of column */
  char *org_name;             /* Original column name, if an alias */
  char *table;                /* Table of column if column was a field */
  char *org_table;            /* Org table name, if table was an alias */
  char *db;                   /* Database for table */
  char *catalog;              /* Catalog for table */
  char *def;                  /* Default value (set by mysql_list_fields) */
  unsigned long length;       /* Width of column (create length) */
  unsigned long max_length;   /* Max width for selected set */
  unsigned int name_length;
  unsigned int org_name_length;
  unsigned int table_length;
  unsigned int org_table_length;
  unsigned int db_length;
  unsigned int catalog_length;
  unsigned int def_length;
  unsigned int flags;         /* Div flags */
  unsigned int decimals;      /* Number of decimals in field */
  unsigned int charsetnr;     /* Character set */
  enum enum_field_types type; /* Type of field. See mysql_com.h for types */
} MYSQL_FIELD;

typedef unsigned int MYSQL_FIELD_OFFSET;
typedef char ** MYSQL_ROW;
typedef void * MYSQL_ROW_OFFSET;
typedef void MY_CHARSET_INFO;

void my_init();
my_ulonglong_returned mysql_affected_rows(MYSQL *mysql);
my_bool mysql_autocommit(MYSQL *mysql, my_bool mode);
my_bool mysql_change_user(MYSQL *mysql, const char *user, const char *password, const char *db);
const char *mysql_character_set_name(MYSQL *mysql);
void mysql_close(MYSQL *mysql);
my_bool mysql_commit(MYSQL *mysql);
/* XXX sml/nj does not support unsigned long long
void mysql_data_seek(MYSQL_RES *result, my_ulonglong offset);
*/
void mysql_debug(const char *debug);
int mysql_dump_debug_info(MYSQL *mysql);
my_bool mysql_eof(MYSQL_RES *result);
unsigned int mysql_errno(MYSQL *mysql);
const char *mysql_error(MYSQL *mysql);
MYSQL_FIELD *mysql_fetch_field(MYSQL_RES *result);
MYSQL_FIELD *mysql_fetch_field_direct(MYSQL_RES *result, unsigned int fieldnr);
MYSQL_FIELD *mysql_fetch_fields(MYSQL_RES *result);
unsigned long *mysql_fetch_lengths(MYSQL_RES *result);
MYSQL_ROW mysql_fetch_row(MYSQL_RES *result);
unsigned int mysql_field_count(MYSQL *mysql);
MYSQL_FIELD_OFFSET mysql_field_seek(MYSQL_RES *result, MYSQL_FIELD_OFFSET offset);
MYSQL_FIELD_OFFSET mysql_field_tell(MYSQL_RES *result);
void mysql_free_result(MYSQL_RES *result);
void mysql_get_character_set_info(MYSQL *mysql, MY_CHARSET_INFO *cs);
const char *mysql_get_client_info(void);
unsigned long mysql_get_client_version(void);
const char *mysql_get_host_info(MYSQL *mysql);
unsigned int mysql_get_proto_info(MYSQL *mysql);
const char *mysql_get_server_info(MYSQL *mysql);
unsigned long mysql_get_server_version(MYSQL *mysql);
const char *mysql_get_ssl_cipher(MYSQL *mysql);
unsigned long mysql_hex_string(char *to, const char *from, unsigned long length);
const char *mysql_info(MYSQL *mysql);
MYSQL *mysql_init(MYSQL *mysql);
my_ulonglong_returned mysql_insert_id(MYSQL *mysql);
my_bool mysql_more_results(MYSQL *mysql);
int mysql_next_result(MYSQL *mysql);
unsigned int mysql_num_fields(MYSQL_RES *result);
my_ulonglong_returned mysql_num_rows(MYSQL_RES *result);
int mysql_options(MYSQL *mysql, enum mysql_option option, const void *arg);
int mysql_ping(MYSQL *mysql);
int mysql_query(MYSQL *mysql, const char *stmt_str);
MYSQL *mysql_real_connect(MYSQL *mysql, const char *host, const char *user, const char *passwd, const char *db, unsigned int port, const char *unix_socket, unsigned long client_flag);
unsigned long mysql_real_escape_string(MYSQL *mysql, char *to, const char *from, unsigned long length);
int mysql_real_query(MYSQL *mysql, const char *stmt_str, unsigned long length);
int mysql_refresh(MYSQL *mysql, unsigned int options);
my_bool mysql_rollback(MYSQL *mysql);
MYSQL_ROW_OFFSET mysql_row_seek(MYSQL_RES *result, MYSQL_ROW_OFFSET offset);
MYSQL_ROW_OFFSET mysql_row_tell(MYSQL_RES *result);
int mysql_select_db(MYSQL *mysql, const char *db);
int mysql_set_character_set(MYSQL *mysql, const char *csname);
int mysql_set_server_option(MYSQL *mysql, enum enum_mysql_set_option option);
int mysql_shutdown(MYSQL *mysql, enum mysql_enum_shutdown_level shutdown_level);
const char *mysql_sqlstate(MYSQL *mysql);
my_bool mysql_ssl_set(MYSQL *mysql, const char *key, const char *cert, const char *ca, const char *capath, const char *cipher);
const char *mysql_stat(MYSQL *mysql);
MYSQL_RES *mysql_store_result(MYSQL *mysql);
unsigned long mysql_thread_id(MYSQL *mysql);
MYSQL_RES *mysql_use_result(MYSQL *mysql);
unsigned int mysql_warning_count(MYSQL *mysql);
