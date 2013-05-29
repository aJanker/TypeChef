struct sockaddr
  {
    char sa_data[14];		/* Address data.  */
  };
typedef struct len_and_sockaddr {
	union {
	    struct sockaddr sa;
		int i;
	} u;
} len_and_sockaddr;
enum {
	LSA_LEN_SIZE = __builtin_offsetof (len_and_sockaddr, u),
		LSA_SIZEOF_SA = sizeof(
    		union {
                struct sockaddr sa;
    		}
    	)
};

void main(){}