"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[5229],{3905:(e,t,a)=>{a.d(t,{Zo:()=>c,kt:()=>h});var n=a(67294);function o(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function r(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,n)}return a}function i(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?r(Object(a),!0).forEach((function(t){o(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):r(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function s(e,t){if(null==e)return{};var a,n,o=function(e,t){if(null==e)return{};var a,n,o={},r=Object.keys(e);for(n=0;n<r.length;n++)a=r[n],t.indexOf(a)>=0||(o[a]=e[a]);return o}(e,t);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);for(n=0;n<r.length;n++)a=r[n],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(o[a]=e[a])}return o}var l=n.createContext({}),p=function(e){var t=n.useContext(l),a=t;return e&&(a="function"==typeof e?e(t):i(i({},t),e)),a},c=function(e){var t=p(e.components);return n.createElement(l.Provider,{value:t},e.children)},u="mdxType",d={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},m=n.forwardRef((function(e,t){var a=e.components,o=e.mdxType,r=e.originalType,l=e.parentName,c=s(e,["components","mdxType","originalType","parentName"]),u=p(a),m=o,h=u["".concat(l,".").concat(m)]||u[m]||d[m]||r;return a?n.createElement(h,i(i({ref:t},c),{},{components:a})):n.createElement(h,i({ref:t},c))}));function h(e,t){var a=arguments,o=t&&t.mdxType;if("string"==typeof e||o){var r=a.length,i=new Array(r);i[0]=m;var s={};for(var l in t)hasOwnProperty.call(t,l)&&(s[l]=t[l]);s.originalType=e,s[u]="string"==typeof e?e:o,i[1]=s;for(var p=2;p<r;p++)i[p]=a[p];return n.createElement.apply(null,i)}return n.createElement.apply(null,a)}m.displayName="MDXCreateElement"},23681:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>l,contentTitle:()=>i,default:()=>d,frontMatter:()=>r,metadata:()=>s,toc:()=>p});var n=a(87462),o=(a(67294),a(3905));const r={},i="Networking",s={unversionedId:"dev/architecture/networking",id:"dev/architecture/networking",title:"Networking",description:"This page provides details about the Hydra networking layer, which encompasses",source:"@site/docs/dev/architecture/networking.md",sourceDirName:"dev/architecture",slug:"/dev/architecture/networking",permalink:"/head-protocol/docs/dev/architecture/networking",draft:!1,editUrl:"https://github.com/cardano-scaling/hydra/tree/master/docs/docs/dev/architecture/networking.md",tags:[],version:"current",frontMatter:{},sidebar:"developerDocumentation",previous:{title:"Architecture",permalink:"/head-protocol/docs/dev/architecture/"},next:{title:"Handling rollbacks",permalink:"/head-protocol/docs/dev/rollbacks/"}},l={},p=[{value:"Questions",id:"questions",level:2},{value:"Investigations",id:"investigations",level:2},{value:"Network resilience",id:"network-resilience",level:3},{value:"Ouroboros",id:"ouroboros",level:3},{value:"Cardano networking",id:"cardano-networking",level:3},{value:"Implementations",id:"implementations",level:2},{value:"Current state",id:"current-state",level:3},{value:"Gossip diffusion network",id:"gossip-diffusion-network",level:3}],c={toc:p},u="wrapper";function d(e){let{components:t,...r}=e;return(0,o.kt)(u,(0,n.Z)({},c,r,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("h1",{id:"networking"},"Networking"),(0,o.kt)("p",null,"This page provides details about the Hydra networking layer, which encompasses\nthe network of Hydra nodes where heads can be opened."),(0,o.kt)("h2",{id:"questions"},"Questions"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"What's the expected topology of the transport layer?",(0,o.kt)("ul",{parentName:"li"},(0,o.kt)("li",{parentName:"ul"},"Are connected peers a subset, superset, or identical set of the head parties?"))),(0,o.kt)("li",{parentName:"ul"},"Do we need the delivery ordering and reliability guarantees TCP provides?",(0,o.kt)("ul",{parentName:"li"},(0,o.kt)("li",{parentName:"ul"},"TCP provides full-duplex, stream-oriented, persistent connections between nodes"),(0,o.kt)("li",{parentName:"ul"},"The Hydra networking layer is based on asynchronous message passing, which seems better suited to UDP"))),(0,o.kt)("li",{parentName:"ul"},"Do we need to consider nodes being reachable through firewalls?",(0,o.kt)("ul",{parentName:"li"},(0,o.kt)("li",{parentName:"ul"},"This responsibility could be delegated to end users, allowing them to configure their firewalls/NATs to align with Hydra node requirements"),(0,o.kt)("li",{parentName:"ul"},"This may be more manageable for business, corporate, or organizational parties than for individual end-users"))),(0,o.kt)("li",{parentName:"ul"},"Do we want ",(0,o.kt)("em",{parentName:"li"},"privacy")," within a head?",(0,o.kt)("ul",{parentName:"li"},(0,o.kt)("li",{parentName:"ul"},"Transactions' details should be opaque to outside observers, with only the final outcome of the head's fanout being observable"))),(0,o.kt)("li",{parentName:"ul"},"How do we identify/discover peers/parties?",(0,o.kt)("ul",{parentName:"li"},(0,o.kt)("li",{parentName:"ul"},"The paper assumes a ",(0,o.kt)("em",{parentName:"li"},"setup")," phase where:",(0,o.kt)("blockquote",{parentName:"li"},(0,o.kt)("p",{parentName:"blockquote"},"To create a head-protocol instance, an initiator invites a set of participants ","{","p1,...,pn","}"," (including themselves) to join by announcing protocol parameters: the participant list, parameters of the (multi-)signature scheme, etc.\nEach party subsequently establishes pairwise authenticated channels with all other parties involved."))))),(0,o.kt)("li",{parentName:"ul"},"What constitutes a ",(0,o.kt)("em",{parentName:"li"},"list of participants"),"? Should each participant be uniquely identifiable? If so, what identification method should be used \u2014 naming scheme, IP: port address, public key, certificate?",(0,o.kt)("ul",{parentName:"li"},(0,o.kt)("li",{parentName:"ul"},"What do 'pairwise authenticated channels' entail? Are these actual TCP/TLS connections, or do they operate at the Transport (layer 4) or Session (layer 5) level?"))),(0,o.kt)("li",{parentName:"ul"},"How open do we want our network protocol to be?",(0,o.kt)("ul",{parentName:"li"},(0,o.kt)("li",{parentName:"ul"},"Currently leveraging the Ouroboros stack with CBOR message encoding, integrating other tools into the Hydra network may pose challenges.")))),(0,o.kt)("h2",{id:"investigations"},"Investigations"),(0,o.kt)("h3",{id:"network-resilience"},"Network resilience"),(0,o.kt)("p",null,"In August 2024 we added some network resilience tests, implemented as a GitHub\naction step in ",(0,o.kt)("a",{parentName:"p",href:"https://github.com/cardano-scaling/hydra/blob/master/.github/workflows/network-test.yaml"},"network-test.yaml"),"."),(0,o.kt)("p",null,"The approach is to use ",(0,o.kt)("a",{parentName:"p",href:"https://github.com/alexei-led/pumba"},"Pumba")," to inject\nnetworking faults into a docker-based setup. This is effective, because of the\n",(0,o.kt)("a",{parentName:"p",href:"https://srtlab.github.io/srt-cookbook/how-to-articles/using-netem-to-emulate-networks.html"},"NetEm"),"\ncapability that allows for very powerful manipulation of the networking stack\nof the containers."),(0,o.kt)("p",null,"Initially, we have set up percentage-based loss on some very specific\nscenarios; namely a three-node setup between ",(0,o.kt)("inlineCode",{parentName:"p"},"Alice"),", ",(0,o.kt)("inlineCode",{parentName:"p"},"Bob")," and ",(0,o.kt)("inlineCode",{parentName:"p"},"Carol"),"."),(0,o.kt)("p",null,"With this setup, we tested the following scenarios:"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},'Three nodes, 900 transactions ("scaling=10"):'),(0,o.kt)("ul",{parentName:"li"},(0,o.kt)("li",{parentName:"ul"},"1% packet loss to both peers: \u2705 Success"),(0,o.kt)("li",{parentName:"ul"},"2% packet loss to both peers: \u2705 Success"),(0,o.kt)("li",{parentName:"ul"},"3% packet loss to both peers: \u2705 Success"),(0,o.kt)("li",{parentName:"ul"},"4% packet loss to both peers: \u2705 Success"),(0,o.kt)("li",{parentName:"ul"},"5% packet loss to both peers: Sometimes works, sometimes fails"),(0,o.kt)("li",{parentName:"ul"},"10% packet loss to both peers: Sometimes works, sometimes fails"),(0,o.kt)("li",{parentName:"ul"},"20% packet loss to both peers: \u274cFailure"))),(0,o.kt)("li",{parentName:"ul"},(0,o.kt)("p",{parentName:"li"},'Three nodes, 4500 transactions ("scaling=50"):'),(0,o.kt)("ul",{parentName:"li"},(0,o.kt)("li",{parentName:"ul"},"1% packet loss to both peers: \u2705 Success"),(0,o.kt)("li",{parentName:"ul"},"2% packet loss to both peers: \u2705 Success"),(0,o.kt)("li",{parentName:"ul"},"3% packet loss to both peers: \u2705 Success"),(0,o.kt)("li",{parentName:"ul"},"4% packet loss to both peers: Sometimes works, sometimes fails"),(0,o.kt)("li",{parentName:"ul"},"5% packet loss to both peers: Sometimes works, sometimes fails"),(0,o.kt)("li",{parentName:"ul"},"10% packet loss to both peers: \u274cFailure"),(0,o.kt)("li",{parentName:"ul"},"20% packet loss to both peers: \u274cFailure")))),(0,o.kt)("p",null,'"Success" here means that ',(0,o.kt)("em",{parentName:"p"},"all"),' transactions were processed; "Failure" means\none or more transactions did not get confirmed by all participants within a\nparticular timeframe.'),(0,o.kt)("p",null,"The main conclusion here is ... there's a limit to the amount of packet loss\nwe can sustain, it's related to how many transactions we are trying to send\n(naturally, ",(0,o.kt)("a",{parentName:"p",href:"http://www.voiptroubleshooter.com/indepth/burstloss.html"},"given the percent of failure is per\npacket"),".)"),(0,o.kt)("p",null,"You can keep an eye on the runs of this action here: ",(0,o.kt)("a",{parentName:"p",href:"https://github.com/cardano-scaling/hydra/actions/workflows/network-test.yaml"},"Network fault\ntolerance"),"."),(0,o.kt)("p",null,"The main things to note are:"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"Overall, the CI job will succeed even if every scenario fails. This is,\nultimately, due to a bug in ",(0,o.kt)("a",{parentName:"li",href:"https://github.com/actions/runner/issues/2347"},"GitHub\nactions")," that prevents one\nfrom declaring an explicit pass-or-fail expectation per scenario. The impact\nis that you should manually check this job on each of your PRs."),(0,o.kt)("li",{parentName:"ul"},"It's okay to see certain configurations fail, but it's certainly not\nexpected to see them ",(0,o.kt)("em",{parentName:"li"},"all")," fail; certainly not the zero-loss cases. Anything\nthat looks suspcisious should be investigated.")),(0,o.kt)("h3",{id:"ouroboros"},"Ouroboros"),(0,o.kt)("p",null,"We held a meeting with the networking team on February 14, 2022, to explore the integration of the Ouroboros network stack into Hydra. During the discussion, there was a notable focus on performance, with Neil Davies providing insightful performance metrics."),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"World circumference: 600ms"),(0,o.kt)("li",{parentName:"ul"},"Latency w/in 1 continent: 50-100ms"),(0,o.kt)("li",{parentName:"ul"},"Latency w/in DC: 2-3ms"),(0,o.kt)("li",{parentName:"ul"},"Subsecond roundtrip should be fine wherever the nodes are located"),(0,o.kt)("li",{parentName:"ul"},"Basic reliability of TCP connections decreases w/ distance:",(0,o.kt)("ul",{parentName:"li"},(0,o.kt)("li",{parentName:"ul"},"w/in DC connection can last forever"),(0,o.kt)("li",{parentName:"ul"},"outside DC: it's hard to keep a single TCP cnx up forever; if a reroute occurs because some intermediate node is down, it takes 90s to resettle a route"),(0,o.kt)("li",{parentName:"ul"},"this implies that as the number of connections goes up, the probability of having at least one connection down at all times increases"))),(0,o.kt)("li",{parentName:"ul"},"Closing of the head must be dissociated from network connections => a TCP cnx disappearing =/=> closing the head"),(0,o.kt)("li",{parentName:"ul"},"Within the Cardano network, propagation of a single empty block takes 400ms (to reach 10K nodes)",(0,o.kt)("ul",{parentName:"li"},(0,o.kt)("li",{parentName:"ul"},"the Ouroboros network should withstand 1000s of connections (there are some system-level limits)"))),(0,o.kt)("li",{parentName:"ul"},"Modelling the Hydra network",(0,o.kt)("ul",{parentName:"li"},(0,o.kt)("li",{parentName:"ul"},"a logical framework for modelling the performance of network associate CDF with time for a message to appear at all nodes (this is what is done in the ",(0,o.kt)("a",{parentName:"li",href:"https://github.com/input-output-hk/hydra-sim"},"hydra-sim")),(0,o.kt)("li",{parentName:"ul"},"we could define a layer w/ the semantics we expect; for example, Snocket = PTP connection w/ ordered guaranteed messages delivery \u2013 do we need that in Hydra?"))),(0,o.kt)("li",{parentName:"ul"},"How about ",(0,o.kt)("a",{parentName:"li",href:"https://wireguard.io"},"Wireguard"),"? It's a very interesting approach, with some shortcomings:",(0,o.kt)("ul",{parentName:"li"},(0,o.kt)("li",{parentName:"ul"},"no global addressing scheme"),(0,o.kt)("li",{parentName:"ul"},"there is one ",(0,o.kt)("inlineCode",{parentName:"li"},"eth")," interface/connection"),(0,o.kt)("li",{parentName:"ul"},"on the plus side, it transparently manages IP address changes"),(0,o.kt)("li",{parentName:"ul"},"does not help w/ Firewalls, eg NAT needs to be configured on each node.")))),(0,o.kt)("h3",{id:"cardano-networking"},"Cardano networking"),(0,o.kt)("p",null,"See ",(0,o.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra.wiki/blob/master/Networking.md#L1"},"this Wiki page")," for detailed notes about how the Cardano network works and uses Ouroboros."),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"Cardano is a global network spanning thousands of nodes, with nodes constantly joining and leaving, resulting in a widely varying topology. Its primary function is block propagation: blocks produced by certain nodes according to consensus rules must reach every node in the network within 20 seconds."),(0,o.kt)("li",{parentName:"ul"},"Nodes cannot maintain direct connections to all other nodes; instead, block diffusion occurs through a form of ",(0,o.kt)("em",{parentName:"li"},"gossiping"),". Each node is connected to a limited set of peers with whom it exchanges blocks."),(0,o.kt)("li",{parentName:"ul"},"Nodes must withstand adversarial behavior from peers and other nodes, necessitating control over the amount and rate of data they ingest. Hence, a ",(0,o.kt)("em",{parentName:"li"},"pull-based")," messaging layer is essential."),(0,o.kt)("li",{parentName:"ul"},"Producer nodes, which require access to signing keys, are considered sensitive assets. They are typically operated behind ",(0,o.kt)("em",{parentName:"li"},"relay nodes")," to enhance security and mitigate the risks of DoS attacks or other malicious activities."),(0,o.kt)("li",{parentName:"ul"},"Nodes often operate behind ADSL or cable modems, firewalls, or in other complex networking environments that prevent direct addressing. Therefore, nodes must initiate connections to externally reachable ",(0,o.kt)("em",{parentName:"li"},"relay nodes"),", and rely on a ",(0,o.kt)("em",{parentName:"li"},"pull-based")," messaging approach.")),(0,o.kt)("h2",{id:"implementations"},"Implementations"),(0,o.kt)("h3",{id:"current-state"},"Current state"),(0,o.kt)("ul",null,(0,o.kt)("li",{parentName:"ul"},"Hydra nodes form a network of pairwise connected ",(0,o.kt)("em",{parentName:"li"},"peers")," using point-to-point (eg, TCP) connections that are expected to remain active at all times:",(0,o.kt)("ul",{parentName:"li"},(0,o.kt)("li",{parentName:"ul"},"Nodes use ",(0,o.kt)("a",{parentName:"li",href:"https://github.com/input-output-hk/ouroboros-network/"},"Ouroboros")," as the underlying network abstraction, which manages connections with peers via a reliable point-to-point stream-based communication framework known as a ",(0,o.kt)("inlineCode",{parentName:"li"},"Snocket")),(0,o.kt)("li",{parentName:"ul"},"All messages are ",(0,o.kt)("em",{parentName:"li"},"broadcast")," to peers using the PTP connections"),(0,o.kt)("li",{parentName:"ul"},"Due to the nature of the Hydra protocol, the lack of a connection to a peer halts any progress of the head."))),(0,o.kt)("li",{parentName:"ul"},"A ",(0,o.kt)("inlineCode",{parentName:"li"},"hydra-node")," can only open a head with ",(0,o.kt)("em",{parentName:"li"},"all")," its peers and exclusively with them. This necessitates that nodes possess prior knowledge of the topology of both peers and heads they intend to establish."),(0,o.kt)("li",{parentName:"ul"},"Connected nodes implement basic ",(0,o.kt)("em",{parentName:"li"},"failure detection")," through heartbeats and monitoring exchanged messages."),(0,o.kt)("li",{parentName:"ul"},"Messages exchanged between peers are signed using the party's Hydra key and validated upon receiving.")),(0,o.kt)("h3",{id:"gossip-diffusion-network"},"Gossip diffusion network"),(0,o.kt)("p",null,"The following diagram illustrates one possible implementation of a pull-based messaging system for Hydra, developed from discussions with IOG\u2019s networking engineers:"),(0,o.kt)("p",null,(0,o.kt)("img",{alt:"Hydra pull-based network",src:a(9968).Z,width:"5827",height:"2745"})))}d.isMDXComponent=!0},9968:(e,t,a)=>{a.d(t,{Z:()=>n});const n=a.p+"assets/images/hydra-pull-based-network-82c3d214f8e8d9b2054a23a4fadd48db.jpg"}}]);