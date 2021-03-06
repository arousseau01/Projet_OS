# Projet d'implémentation de réseaux de processus de Kahn (KPN)

## Repertoires:

### Implémentation de KPN

- UnixVersion: implémentation basée sur des processus UNIX communiquant via des pipes (OCaml)
- NetworkVersion: implémentation basée sur des processus UNIX communiquant via des stream sockets (OCaml)
- SequentialVersion: version séquentielle à parallélisme simulée
- C_Version: implémentation basée sur des processus Unix communiquant par des pipes (C)

### Implémentation de MPI

- MPI_Kahn : implémentation de MPI utilisant l'implémentation UnixVersion (OCaml) (non fonctionnel)
- MPI_Kahn_C : implémentation de MPI utilisant l'implémentation de KPN C_Version (C)

  - Exemples de scripts:
    - test_hello.c : démonstration fonctionalités implémentées
    - test_pi.c: calcul parallèle de pi
    - FEM_MPI.c: résolution parallèle de l'équation de d'Alembert 2D

  La compilation du fichier FEM_MPI_REF.c (destiné à la comparaison) nécéessite d'avoir une implémentation de MPI installée (changer le include PATH dans le Makefile)
