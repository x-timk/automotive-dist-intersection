# erlDIM
A distributed Intersection Simulation written in Erlang

## Compilare ed eseguire il progetto
Aprire shell:
 - git clone https://github.com/xpicox/erlDIM.git
 - Entrare nella root del progetto: 
   - #shell:~ $ cd erlDIM
 - Compilare i sorgenti erlang: 
   - #shell:~ erlDIM$ erl -make
 - Entrare nel path ebin:
   - #shell:~ erlDIM$ cd ebin
 - Lanciare shell erlang specificando nome e cookie:
   - #shell:~ erlDIM$ erl -name 'envnode@192.168.1.100' -setcookie "guitar"
 - Avviare environment:
   -  (envnode@192.168.1.100)1> dim_env:go().
  - Compilare la gui java (è un progetto maven) o in alternativa utilizzare il jar precompilato.
  - Inserire il nodo e l'ip con cui è stata avviata la shell erlang in precedenza.
  - Scegliere un nome qualsiasi per il nodo erlang java e selezionare indirizzo ip
  - Se i dati sono corretti l'interfaccia partirà correttamente (in caso contrario verranno segnalati degli errori.
   
