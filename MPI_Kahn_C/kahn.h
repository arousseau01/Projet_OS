#include <sys/types.h>

#ifndef KAHN_INCLUDED
#define KAHN_INCLUDED

#define BUFFER_SIZE 20000

typedef void (*process)();

int nb_proc2;
pid_t *pid_list;

typedef struct Channel {
    int fd_in;
    int fd_out;
} channel;

channel *new_channel();

// Communication
void put_int(int value, channel *chan_out);
int get_int(channel *chan_in);
void put_double(double value, channel *chan_out);
double get_double(channel *chan_in);

void doco(int nb_proc, process processes[], void* arguments[]);

#endif
