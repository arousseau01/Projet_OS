#include <stdlib.h>
#include <stdio.h>
#include "kahn.h"

/* Test basique de l'implémentation de KPN: un processus envoi la suite des entiers naturels sur un canal, un autre processus les récupère et les imprime sur la sortie standard */

void *integers(channel *Q) {
    for (int i=1;;i++) {
        put(&i, 1, Q, KAHN_INT);
    }
}

void *output(channel *Q) {
    int out;
    for (;;) {
        get(&out, 1, Q, KAHN_INT);
        printf("%d\n", out);
    }
}

int main() {

    channel *Q = new_channel();
    int nb_proc = 2;
    process p1 = {integers, Q};
    process p2 = {output, Q};
    process *processes[] = {&p1, &p2};
    doco(nb_proc, processes);
    return 0;
}