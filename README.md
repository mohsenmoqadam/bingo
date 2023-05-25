EPMD and EDP
============

Suppose we have an Erlang cluster with any count of nodes in this cluster. Each node in the startup stage selects a random port number and other nodes can send their requests to this node. But here is one central question: How can other nodes inform what is the opened port number of the target node? (for creating a TCP connection to that port and sending a request through it)? The answer to this question is the role of Erlang Port Mapper Daemon or EPMD. 

The EPMD can be started explicitly or automatically as a result of the Erlang node startup and listen on TCP-Port: `4369` all Erlang nodes know about this port and connect through this to EPMD and then use Erlang Distribution Protocol or EDP to inform EPMD of some information like `PortNo`, `NodeType`, `HighestVersion`, `LowestVersion` and other things. Furthermore, the Erlang node gets and updates its information based on its requirements with the EDP protocol. 

For example, When one node wants to connect to another node it starts with a `PORT_PLEASE2_REQ` request to the EPMD on the host where the node resides to get the distribution port that the node listens to and EPMD sends the result back with the message: `PORT2_RESP`, if you require more information please visit this. 

Now we know that we have a daemon with the name EPMD that we familiarized with their responsibility and furthermore, we know that EDP is a protocol that defines some messages and has some specifications (like it listens on TCP port 4369), this Daemon and Protocol help Erlang nodes to find each other in the cluster. If we want to get some information about clustered node from EPMD this shell command can help us (please care 127.0.0.1):  

``` bash
    nmap -sV -Pn -n -T4 -p 4369 --script epmd-info 127.0.0.1
    PORT     STATE SERVICE VERSION
    4369/tcp open  epmd    Erlang Port Mapper Daemon
    | epmd-info: 
    |   epmd_port: 4369
    |   nodes: 
    |     bingo-dev: 36963
    |     bingo-node-1: 41881
    |_    bingo-node-2: 33417
```
If we haven't `nmap`, we can send `NAMESREQ` to EPMD (this message has one byte and this byte value is 110) with `nc`:     
``` bash
    echo -n -e "\x00\x01\x6e" | nc -vn 127.0.0.1 4369
    Connection to 127.0.0.1 4369 port [tcp/*] succeeded!
    name bingo-dev at port 36963
    name bingo-node-1 at port 41881
    name bingo-node-2 at port 33417
```

Now we know how Erlang nodes find and identify each other in the Erlang cluster, after this step each node can create a TCP connection to others (like a mesh network) and can exchange data with these nodes. For example, distributed ETS or Mnesia use these connections for their purposes. Until now all the things are ok, and we have a good state but if we want to use these connections for our purposes or application purposes may we lose that good state and if our traffic requires real-time behaviour, things may go from a good state to a bad state and if the number of nodes in the cluster increase, may we have experience worse state.

Bingo
=====
Bingo helps us to redirect our application traffic between nodes from general and common channel to a specific channel and let us control the capacity and behaviour of this channel based on our application requirements. We can use Bingo's channel to send our `cast` and `call` request from the local node to the remote node or simultaneously send `cast` and `call` to more than one remote node, Bingo provides `multi_cast` and `multi_call` API for these purposes.

When we require one `call`, `cast`, `multi_call` or `multi_cast` to target node, Bingo create some TCP connection to that node and then sends our request to target nodde(s) and (if require) it gathers the results and return the replies. Send request and return reply is a feature like RPC, but they have small differences in details. Channel is the abstract concept that represents direct access between two Erlang nodes. Each channel may have one or more TCP connections and other settings that we can set its properties through Bingo configuration options:

``` erlang
{conn, #{ ip => "127.0.0.1"
        , port => {25000, 50000}
        , acceptors => 2
        , conns => 2
        , workers => 4
        }
}
```

`ip` and `port`: Tells Bingo listening on which `IP:PORT` for creating a new connection. 
`acceptors`: How many `TCP ACCEPTOR` must exist on each channel for accepting new `TCP CONNECTIONS`. Each acceptor is one Erlang Actor. 
`conns`: How many `TCP CONNECTION` must exist on each channel. Each `TCP CONNECTION` is one Erlang Actor that holds a `SOCKET` for sending and receiving requests. 
`workers`: How many workers must exist on each channel. `worker` gets a request from one `conn` and process it and prepare the result, then send the result as a reply to `conn` for sending to the requested node.

Here arise two main question: 
#### 1. Why do we have workers and why we must not process the incoming request with actors that hold a socket?
If we process each incoming request with `conn`, the `conn` actor during process the received request is blocked and other received requests are queued in `conn`'s inbox, and this approach increases response time for each request. Bingo use a scalable approach and trying to decrease response time. When one `conn` receive a request, first it selects a worker randomly and send that request to that worker. After selected worker process one request, it sends the result to `conn` (because `conn` hold the socket). `conn` convey the result to requested node as reply.

#### 2. How Bingo does `multi_call` and `multi_cast` requests?
For responding to this question, first we must be familiar with `multi_cast` and `multi_call` anatomy! With `multi_x` API Bingo, send specific request to a list of nodes in cluster and gather results and then return replies. Bingo hire a set of worker and named them: `mc.workers`. When Bingo receive `multi_call` or `multi_cast` request, it selects on `mc_worker` and send that request to selected worker. This worker sends `call` or `cast` requests to all requested remote nodes simultaneously and waits for gathering results and prepare reply and return it.
Bingo provide below options to configure `multicast` and `multi call` behaviours:

``` erlang
, {'mc.timeout', 2000}
, {'mc.workers', 4}   
```

`mc.workers`: How many `multi_x` worker must exist on one channel.

`mc.timeout`: How much time one worker must be waiting for gathering results from all nodes.

Configuration Options
=====================
Below snippet shows all configuration options and if we want to use Bingo in our application these options must set carefully based on application requirements.
``` erlang
{bingo, [ {conn, #{ ip => "127.0.0.1"
                   , port => {25000, 50000}
                   , acceptors => 2
                   , conns => 2
                   , workers => 4
                   }
           }
         , {'cluster.erpc.timeout', 2000}
         , {'mc.timeout', 2000}
         , {'mc.workers', 4}
         ]
 }
```

‌Be aware...
===========
This documentary is not complete and other parts of it will be added in the future.
