"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[9726],{3905:(e,t,a)=>{a.d(t,{Zo:()=>c,kt:()=>m});var n=a(67294);function r(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function i(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var n=Object.getOwnPropertySymbols(e);t&&(n=n.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,n)}return a}function o(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?i(Object(a),!0).forEach((function(t){r(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):i(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function l(e,t){if(null==e)return{};var a,n,r=function(e,t){if(null==e)return{};var a,n,r={},i=Object.keys(e);for(n=0;n<i.length;n++)a=i[n],t.indexOf(a)>=0||(r[a]=e[a]);return r}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(n=0;n<i.length;n++)a=i[n],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(r[a]=e[a])}return r}var d=n.createContext({}),s=function(e){var t=n.useContext(d),a=t;return e&&(a="function"==typeof e?e(t):o(o({},t),e)),a},c=function(e){var t=s(e.components);return n.createElement(d.Provider,{value:t},e.children)},p="mdxType",h={inlineCode:"code",wrapper:function(e){var t=e.children;return n.createElement(n.Fragment,{},t)}},u=n.forwardRef((function(e,t){var a=e.components,r=e.mdxType,i=e.originalType,d=e.parentName,c=l(e,["components","mdxType","originalType","parentName"]),p=s(a),u=r,m=p["".concat(d,".").concat(u)]||p[u]||h[u]||i;return a?n.createElement(m,o(o({ref:t},c),{},{components:a})):n.createElement(m,o({ref:t},c))}));function m(e,t){var a=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var i=a.length,o=new Array(i);o[0]=u;var l={};for(var d in t)hasOwnProperty.call(t,d)&&(l[d]=t[d]);l.originalType=e,l[p]="string"==typeof e?e:r,o[1]=l;for(var s=2;s<i;s++)o[s]=a[s];return n.createElement.apply(null,o)}return n.createElement.apply(null,a)}u.displayName="MDXCreateElement"},70721:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>d,contentTitle:()=>o,default:()=>h,frontMatter:()=>i,metadata:()=>l,toc:()=>s});var n=a(87462),r=(a(67294),a(3905));const i={slug:30,title:"30. Use CBOR in external representation of Cardano transactions\n",authors:["abailly"],tags:["Accepted"]},o=void 0,l={permalink:"/head-protocol/adr/30",source:"@site/adr/2023-12-06_030-use-cbor-for-tx.md",title:"30. Use CBOR in external representation of Cardano transactions\n",description:"Status",date:"2023-12-06T00:00:00.000Z",formattedDate:"December 6, 2023",tags:[{label:"Accepted",permalink:"/head-protocol/adr/tags/accepted"}],readingTime:2.925,hasTruncateMarker:!1,authors:[{name:"Arnaud Bailly",title:"Lead Architect",url:"https://github.com/abailly-iohk",imageURL:"https://github.com/abailly-iohk.png",key:"abailly"}],frontMatter:{slug:"30",title:"30. Use CBOR in external representation of Cardano transactions\n",authors:["abailly"],tags:["Accepted"]},prevItem:{title:"29. EventSource & EventSink abstractions\n",permalink:"/head-protocol/adr/29"}},d={authorsImageUrls:[void 0]},s=[{value:"Status",id:"status",level:2},{value:"Context",id:"context",level:2},{value:"Decision",id:"decision",level:2},{value:"Consequences",id:"consequences",level:2}],c={toc:s},p="wrapper";function h(e){let{components:t,...a}=e;return(0,r.kt)(p,(0,n.Z)({},c,a,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("h2",{id:"status"},"Status"),(0,r.kt)("p",null,"Accepted"),(0,r.kt)("h2",{id:"context"},"Context"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"The ",(0,r.kt)("a",{parentName:"li",href:"https://github.com/input-output-hk/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/src/Hydra/Ledger/Cardano.hs#L127"},"Hydra.Ledger.Cardano")," module provides ",(0,r.kt)("inlineCode",{parentName:"li"},"ToJSON/FromJSON")," instances for ",(0,r.kt)("inlineCode",{parentName:"li"},"Tx")," and ",(0,r.kt)("a",{parentName:"li",href:"https://github.com/input-output-hk/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/src/Hydra/Ledger/Cardano/Json.hs#L361"},"AlonzoTx"),(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"We have specified this format as part of ",(0,r.kt)("a",{parentName:"li",href:"https://github.com/cardano-scaling/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/json-schemas/api.yaml#L1473"},"Hydra API")))),(0,r.kt)("li",{parentName:"ul"},"These instances appear in a few places as part of Hydra API:",(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"In the ",(0,r.kt)("a",{parentName:"li",href:"https://github.com/cardano-scaling/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/src/Hydra/API/ServerOutput.hs#L51"},"ServerOutput")," sent by the node to clients"),(0,r.kt)("li",{parentName:"ul"},"In the ",(0,r.kt)("a",{parentName:"li",href:"https://github.com/cardano-scaling/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/src/Hydra/Node.hs#L122"},"HydraNodeLog")," as part of Hydra's logging output"),(0,r.kt)("li",{parentName:"ul"},"In the ",(0,r.kt)("a",{parentName:"li",href:"https://github.com/cardano-scaling/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/src/Hydra/HeadLogic/Outcome.hs#L46"},"StateChanged")," events which are persisted and allow hydra-node to restart gracefully after stopping"))),(0,r.kt)("li",{parentName:"ul"},"In other places the hydra-node produces, expects, or accepts a CBOR-encoded transaction:",(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"In the ",(0,r.kt)("a",{parentName:"li",href:"https://github.com/cardano-scaling/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/src/Hydra/Network/Message.hs#L20"},"Network.Message")," exchanged between the nodes"),(0,r.kt)("li",{parentName:"ul"},"In the ",(0,r.kt)("a",{parentName:"li",href:"https://github.com/cardano-scaling/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/src/Hydra/API/ClientInput.hs#L9"},"ClientInput")," from clients submitting ",(0,r.kt)("inlineCode",{parentName:"li"},"NewTx")," commands"),(0,r.kt)("li",{parentName:"ul"},"In the ",(0,r.kt)("a",{parentName:"li",href:"https://github.com/cardano-scaling/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/src/Hydra/API/HTTPServer.hs#L297"},"HTTPServer")," API"))),(0,r.kt)("li",{parentName:"ul"},"Note that in the latter 2 cases, the hydra-node ",(0,r.kt)("em",{parentName:"li"},"accepts")," a hex-CBOR-encoded ",(0,r.kt)("em",{parentName:"li"},"JSON string")," to represent a transaction and this particular case is handled directly in the ",(0,r.kt)("a",{parentName:"li",href:"https://github.com/input-output-hk/hydra/blob/b2dc5a0da4988631bd2c1e94b66ba6217d5db595/hydra-node/src/Hydra/Ledger/Cardano/Json.hs#L388"},"FromJSON")," instance for transactions where 3 different representations are even accepted:",(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"JSON object detailing the transaction"),(0,r.kt)("li",{parentName:"ul"},"A JSON string representing CBOR-encoding of a transaction"),(0,r.kt)("li",{parentName:"ul"},"Or a ",(0,r.kt)("inlineCode",{parentName:"li"},"TextEnvelope")," which wraps the CBOR transaction in a simple JSON object"))),(0,r.kt)("li",{parentName:"ul"},"Using JSON-based representation of Cardano transactions is problematic because:",(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"The representation we are providing is not ",(0,r.kt)("em",{parentName:"li"},"canonical")," nor widely used, and therefore require maintenance when the underlying cardano-ledger API changes"),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("strong",{parentName:"li"},"More importantly")," the JSON representation contains a ",(0,r.kt)("inlineCode",{parentName:"li"},"txId")," field which is computed from the CBOR encoding of the transaction. When this encoding changes, the transaction id changes even though no other part of the transaction has changed. This implies that we could send and receive transactions with incorrect or inconsistent identifiers."))),(0,r.kt)("li",{parentName:"ul"},"This is true for any content-addressable piece of data, eg. any piece of data whose unique identifier is derived from the data itself, but not of say UTxO which is just data.")),(0,r.kt)("h2",{id:"decision"},"Decision"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},'Drop support of "structured" JSON encoding of transactions in log messages, external APIs, and local storage of a node state'),(0,r.kt)("li",{parentName:"ul"},"Require JSON encoding for transactions that consists in:",(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"A ",(0,r.kt)("inlineCode",{parentName:"li"},"cborHex")," string field containing the base16 CBOR-encoded transaction"),(0,r.kt)("li",{parentName:"ul"},"An optional ",(0,r.kt)("inlineCode",{parentName:"li"},"txId")," string field containing the Cardano transaction id, i.e. the base16 encoded Blake2b256 hash of the transaction body bytes"),(0,r.kt)("li",{parentName:"ul"},"When present, the ",(0,r.kt)("inlineCode",{parentName:"li"},"txId")," MUST be consistent with the ",(0,r.kt)("inlineCode",{parentName:"li"},"cborHex"),". This will be guaranteed for data produced by Hydra, but input data (eg. through a ",(0,r.kt)("inlineCode",{parentName:"li"},"NewTx")," message) that does not respect this constraint will be rejected")))),(0,r.kt)("h2",{id:"consequences"},"Consequences"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},"This is a breaking change and client applications must decode the full transaction CBOR before accessing any part of it",(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"Hydra clients like ",(0,r.kt)("inlineCode",{parentName:"li"},"hydraw"),", ",(0,r.kt)("inlineCode",{parentName:"li"},"hydra-auction"),", ",(0,r.kt)("inlineCode",{parentName:"li"},"hydra-pay"),", ",(0,r.kt)("inlineCode",{parentName:"li"},"hydra-poll")," and hydra-chess` need to be updated"))),(0,r.kt)("li",{parentName:"ul"},"By providing a ",(0,r.kt)("inlineCode",{parentName:"li"},"txId")," field alongside the CBOR encoding, we still allow clients to observe the lifecycle of a transaction inside a Head as it gets validated and confirmed without requiring from them to be able to decode the CBOR body and compute the ",(0,r.kt)("inlineCode",{parentName:"li"},"txId")," themselves",(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},"This is particularly important for monitoring which usually does not care about the details of transactions"))),(0,r.kt)("li",{parentName:"ul"},"We should point users to existing tools for decoding transactions' content in a human-readable format as this can be useful for troubleshooting:",(0,r.kt)("ul",{parentName:"li"},(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("inlineCode",{parentName:"li"},"cardano-cli transaction view --tx-file <path to tx envelope file>")," is one example"))),(0,r.kt)("li",{parentName:"ul"},"We need to ",(0,r.kt)("em",{parentName:"li"},"version")," the data that's persisted and exchanged, e.g the Head state and network messages, in order to ensure nodes can either gracefully migrate stored data or detect explicitly versions inconsistency"),(0,r.kt)("li",{parentName:"ul"},"We should use the ",(0,r.kt)("a",{parentName:"li",href:"https://github.com/CardanoSolutions/cardanonical"},"cardanonical")," schemas should the need arise to represent transaction in JSON again")))}h.isMDXComponent=!0}}]);