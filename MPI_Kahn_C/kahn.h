#include <sys/types.h>

#ifndef KAHN_INCLUDED
#define KAHN_INCLUDED

typedef enum { 
    KAHN_INT, 
    KAHN_FLOAT,
    KAHN_DOUBLE,
    KAHN_LONG_DOUBLE,
} Kahn_Datatype;

static int _kahn_data_size[] = {
    sizeof(int),
    sizeof(float),
    sizeof(double),
    sizeof(long double)
    };

typedef void (*process)();

    pid_t *pid_list;

typedef struct Channel {
    int fd_in;
    int fd_out;
} channel;

channel *new_channel();

void put(void *value, int cnt, channel *chan_out, Kahn_Datatype dtype);
void get(void *value, int cnt, channel *chan_in, Kahn_Datatype dtype);

void doco(int nb_proc, process processes[], void* arguments[]);

#endif
