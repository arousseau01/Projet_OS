#include <sys/types.h>

#ifndef KAHN_INCLUDED
#define KAHN_INCLUDED

typedef enum { 
    KAHN_INT, 
    KAHN_DOUBLE,
    KAHN_LONG_DOUBLE,
} Kahn_Datatype;

static int _kahn_data_size[] = {4, 8, 16};

typedef void (*process)();

    pid_t *pid_list;

typedef struct Channel {
    int fd_in;
    int fd_out;
} channel;

channel *new_channel();

void put(void *value, channel *chan_out, Kahn_Datatype dtype);
void get(void *value, channel *chan_in, Kahn_Datatype dtype);

void put_array(void *value, int cnt, channel *chan_out, Kahn_Datatype dtype);
void get_array(void *value, int cnt, channel *chan_in, Kahn_Datatype dtype);

void doco(int nb_proc, process processes[], void* arguments[]);

#endif
