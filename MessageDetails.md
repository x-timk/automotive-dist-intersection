# Elenco messaggi/azioni
Vengono quì specificati i possibili messaggi/eventi e le relative azioni intraprese per ogni modulo
## Modulo ENVIRONMENT
I Messaggi che l'environment riceve hanno la stessa struttura di base: {tipologia_messaggio,payload}
Elenco possibili messaggi:
 - {spawn_car, {CarName, CarDesc, StartPos, EndPos, Speed}}: L'environment genera una nuova auto. Di seguito una descrizione dei parametri
   - CarName: Nome Macchina. Usare un atom() in questo caso. L'atom viene registrato dalla vm erlang; Si utilizzerà il CarName invece del pid per inviare messaggi all'auto
   - CarDesc: Una stringa di descrizione per l'auto
   - StartPos: posizione di partenza dell'auto. I nomi dei nodi del grafo sono atomi.
   - EndPos: posizione alla quale l'auto desidera arrivare. Da StartPos ed EndPos verrà automaticamente calcolato il percorso più breve (si utilizza dijkstra)
   - Speed: Velocità di movimento dell'auto. Es: se speed vale 2000 allora l'auto tenterà di muoversi in avanti ogni 2 secondi
 - {disc,FromCar}: L'environment riceve un disc dall'auto FromCar. Dovrà simulare il fatto che la FromCar manda un broadcast alle auto a portata wifi. Vengono quindi trovate le auto nelle vicinanze di FromCar (funzione get_car_neighboorhood) e quindi per ogni vicino rigiro il messaggio {disc, FromCar} così com'è.
 - {posfree, {FromCar,V}}: L'environment riceve una richiesta da sensore di prossimità dell'auto FromCar che chiede se la posizione V è libera. Viene inviata all'auto una risposta che può essere {posfree_resp, true} se V è occupato o {posfree_resp, false} se V è libero.
 - {move, {FromCar, {From, To}}}: L'auto FromCar si muove da nodo From a nodo To. In questo caso l'env deve solamente aggiornare il grafo in base allo spostamento dell'auto.

## Modulo VEHICLE
Il veicolo è una macchina a stati finiti.
I Messaggi che un veicolo può ricevere e le azioni intraprese di conseguenza dipendono dall stato in cui il veicolo si trova.
I messaggi possono essere di due tipi:
 - {info, {Message, MessageData}}: Messaggi ricevuti da processi esterni
 - {timeout, EVENTO_TIMEOUT}: Eventi di timeout generici scatenati internamente
 - {state_timeout, EVENTO_TIMEOUT}: Eventi di timeout di stato scatenati internamente

Di seguito per ogni stato l'elenco dei possibili messaggi/eventi e relative azioni:
### Stato inqueue:
#### Messaggi tipo info:
 - {posfree_resp, true/false} :  
   - Se ricevo false e mi trovo in un nodo tail posso avanzare
   - Se ricevo false e mi trovo in un nodo top non posso avanzare; passo allo stato discover e mando in broadcast un disc ai miei vicini.
  - Se ricevo true stò fermo e rimango in stato inqueue resettando timeout CAR_MV
 - ontop: non usato
 - ontail: non usato
 - disc: se ricevo un disc quando sono in stato inqueue ignoro la richiesta semplicemente.
#### Messaggi tipo state_timeout: 
 - {timeout, CAR_MV}: evento scatenato quando scade timeout generico per movimento auto. In questo caso invio a sensore prossimità richiesta all'env per sapere se ho qualcuno davanti
### Stato discover:
#### Messaggi tipo info:
 - {disc, FromCar}: Ho ricevuto un messaggio broadcast da un mio vicino; Mi registro il suo id tra i miei vicini conosciuti, e mando come risposta un messaggio {hello, MIOID} per segnalare "ci sono"
 - {hello, FromCar}: Dopo aver mandato un disc in broadcast posso ricevere 0 o più messaggi di questo tipo dalle auto a portata. Quando ricevo un messaggio di questo tipo mi salvo il FromCar tra la lista dei miei vicini e basta.
 - {wait, _}: Se ricevo un messaggio di questo tipo rimango in discover e resetto il timeout di stato DISC_TM
 - {bully_elect,_} e altri messaggi da implementare.
#### Messaggi tipo state_timeout:
 - {state_timeout, DISC_TM}: Evento scatenato quando scade timeout di stato discover. In questo caso passo allo stato election, e (da implementare) inizio elezione.

### Stato election: 
TODO praticamente tutto, ho solo messo un timeout di stato che mi riporta in discover

