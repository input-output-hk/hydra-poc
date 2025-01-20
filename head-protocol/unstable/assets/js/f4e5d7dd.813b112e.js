"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[9560],{61978:(e,n,s)=>{s.r(n),s.d(n,{assets:()=>o,contentTitle:()=>c,default:()=>h,frontMatter:()=>r,metadata:()=>t,toc:()=>a});var t=s(63803),i=s(74848),d=s(28453);const r={slug:29,title:"29. EventSource & EventSink abstractions\n",authors:["cardenaso11","quantumplation","ch1bo"],tags:["Accepted"]},c=void 0,o={authorsImageUrls:[void 0,void 0,void 0]},a=[{value:"Status",id:"status",level:2},{value:"Context",id:"context",level:2},{value:"Consequences",id:"consequences",level:2},{value:"Out of scope / future work",id:"out-of-scope--future-work",level:2}];function l(e){const n={a:"a",code:"code",h1:"h1",h2:"h2",li:"li",p:"p",pre:"pre",strong:"strong",ul:"ul",...(0,d.R)(),...e.components};return(0,i.jsxs)(i.Fragment,{children:[(0,i.jsx)(n.h2,{id:"status",children:"Status"}),"\n",(0,i.jsx)(n.p,{children:"Accepted"}),"\n",(0,i.jsx)(n.h2,{id:"context",children:"Context"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"The Hydra node represents a significant engineering asset, providing layer 1 monitoring, peer to peer consensus, durable persistence, and an isomorphic Cardano ledger. Because of this, it is being eyed as a key building block not just in Hydra based applications, but other protocols as well."}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsxs)(n.p,{children:["Currently the ",(0,i.jsx)(n.code,{children:"hydra-node"})," uses a very basic persistence mechanism for it's internal ",(0,i.jsx)(n.code,{children:"HeadState"}),", that is saving ",(0,i.jsx)(n.code,{children:"StateChanged"})," events to file on disk and reading them back to load and re-aggregate the ",(0,i.jsx)(n.code,{children:"HeadState"})," upon startup."]}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsx)(n.li,{children:"Some production setups would benefit from storing these events to a service like Amazon Kinesis data stream instead of local files."}),"\n"]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsxs)(n.p,{children:["The ",(0,i.jsx)(n.code,{children:"hydra-node"})," websocket-based API is the only available event stream right now and might not fit all purposes."]}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["See also ADR ",(0,i.jsx)(n.a,{href:"/adr/3",children:"3"})," and ",(0,i.jsx)(n.a,{href:"/adr/25",children:"25"})]}),"\n",(0,i.jsxs)(n.li,{children:["Internally, this is realized as a single ",(0,i.jsx)(n.code,{children:"Server"})," handle which can ",(0,i.jsx)(n.code,{children:"sendOutput :: ServerOutput tx -> m ()"})]}),"\n",(0,i.jsxs)(n.li,{children:["These ",(0,i.jsx)(n.code,{children:"ServerOutput"}),"s closely relate to ",(0,i.jsx)(n.code,{children:"StateChanged"})," events and ",(0,i.jsx)(n.code,{children:"ClientEffect"}),"s are yielded by the logic layer often together with the ",(0,i.jsx)(n.code,{children:"StateChanged"}),". For example:"]}),"\n"]}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-hs",children:"onInitialChainAbortTx newChainState committed headId =\n  StateChanged HeadAborted{chainState = newChainState}\n    <> Effects [ClientEffect $ ServerOutput.HeadIsAborted{headId, utxo = fold committed}]\n"})}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsxs)(n.p,{children:["Users of ",(0,i.jsx)(n.code,{children:"hydra-node"})," are interested to add alternative implementations for storing, loading and consuming events of the Hydra protocol."]}),"\n"]}),"\n"]}),"\n",(0,i.jsx)(n.h1,{id:"decision",children:"Decision"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsxs)(n.p,{children:["We create two new interfaces in the ",(0,i.jsx)(n.code,{children:"hydra-node"})," architecture:"]}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsx)(n.li,{children:(0,i.jsx)(n.code,{children:"data EventSource e m = EventSource { getEvents :: m [e] }"})}),"\n",(0,i.jsx)(n.li,{children:(0,i.jsx)(n.code,{children:"data EventSink e m = EventSink { putEvent :: e -> m () }"})}),"\n"]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsxs)(n.p,{children:["We realize our current ",(0,i.jsx)(n.code,{children:"PersistenceIncremental"})," used for persisting ",(0,i.jsx)(n.code,{children:"StateChanged"})," events is both an ",(0,i.jsx)(n.code,{children:"EventSource"})," and an ",(0,i.jsx)(n.code,{children:"EventSink"})]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsxs)(n.p,{children:["We drop the ",(0,i.jsx)(n.code,{children:"persistence"})," from the main handle ",(0,i.jsx)(n.code,{children:"HydraNode tx m"}),", add ",(0,i.jsx)(n.strong,{children:"one"})," ",(0,i.jsx)(n.code,{children:"EventSource"})," and allow ",(0,i.jsx)(n.strong,{children:"many"})," ",(0,i.jsx)(n.code,{children:"EventSinks"})]}),"\n"]}),"\n"]}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-hs",children:"data HydraNode tx m = HydraNode\n  { -- ...\n  , eventSource :: EventSource (StateEvent tx) m\n  , eventSinks :: [EventSink (StateEvent tx) m]\n  }\n"})}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsxs)(n.p,{children:["The ",(0,i.jsx)(n.code,{children:"hydra-node"})," will load events and ",(0,i.jsx)(n.code,{children:"hydrate"})," its ",(0,i.jsx)(n.code,{children:"HeadState"})," using ",(0,i.jsx)(n.code,{children:"getEvents"})," of the single ",(0,i.jsx)(n.code,{children:"eventSource"}),"."]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsxs)(n.p,{children:["The ",(0,i.jsx)(n.code,{children:"stepHydraNode"})," main loop does call ",(0,i.jsx)(n.code,{children:"putEvent"})," on all ",(0,i.jsx)(n.code,{children:"eventSinks"})," in sequence. Any failure will make the ",(0,i.jsx)(n.code,{children:"hydra-node"})," process terminate and require a restart."]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsxs)(n.p,{children:["When loading events from ",(0,i.jsx)(n.code,{children:"eventSource"})," on ",(0,i.jsx)(n.code,{children:"hydra-node"})," startup, it will also re-submit events via ",(0,i.jsx)(n.code,{children:"putEvent"})," to all ",(0,i.jsx)(n.code,{children:"eventSinks"}),"."]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsxs)(n.p,{children:["The default ",(0,i.jsx)(n.code,{children:"hydra-node"})," main loop does use the file-based ",(0,i.jsx)(n.code,{children:"EventSource"})," and a single file-based ",(0,i.jsx)(n.code,{children:"EventSink"})," (using the same file)."]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsxs)(n.p,{children:["We realize that the ",(0,i.jsx)(n.code,{children:"EventSource"})," and ",(0,i.jsx)(n.code,{children:"EventSink"})," handles, as well as their aggregation in ",(0,i.jsx)(n.code,{children:"HydraNode"})," are used as an API by forks of the ",(0,i.jsx)(n.code,{children:"hydra-node"})," and try to minimize changes to it."]}),"\n"]}),"\n"]}),"\n",(0,i.jsx)(n.h2,{id:"consequences",children:"Consequences"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsxs)(n.p,{children:["The default operation of the ",(0,i.jsx)(n.code,{children:"hyda-node"})," remains unchanged."]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsxs)(n.p,{children:["There are other things called ",(0,i.jsx)(n.code,{children:"Event"})," and ",(0,i.jsx)(n.code,{children:"EventQueue(putEvent)"})," right now in the ",(0,i.jsx)(n.code,{children:"hydra-node"}),". This is getting confusing and when we implement this, we should also rename several things first (tidying)."]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsxs)(n.p,{children:["Interface first: Implementations of ",(0,i.jsx)(n.code,{children:"EventSink"})," should specify their format in a non-ambiguous and versioned way, especially when a corresponding ",(0,i.jsx)(n.code,{children:"EventSource"})," exists."]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsxs)(n.p,{children:["The API ",(0,i.jsx)(n.code,{children:"Server"})," can be modelled and refactored as an ",(0,i.jsx)(n.code,{children:"EventSink"}),"."]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"Projects forking the hydra node have dedicated extension points for producing and consuming events."}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsxs)(n.p,{children:['Sundae Labs can build a "Save transaction batches to S3" proof of concept ',(0,i.jsx)(n.code,{children:"EventSink"}),"."]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsxs)(n.p,{children:['Sundae Labs can build a "Scrolls source" ',(0,i.jsx)(n.code,{children:"EventSink"}),"."]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsxs)(n.p,{children:['Sundae Labs can build a "Amazon Kinesis" ',(0,i.jsx)(n.code,{children:"EventSource"})," and ",(0,i.jsx)(n.code,{children:"EventSink"}),"."]}),"\n"]}),"\n"]}),"\n",(0,i.jsx)(n.h2,{id:"out-of-scope--future-work",children:"Out of scope / future work"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsxs)(n.p,{children:["Available implementations for ",(0,i.jsx)(n.code,{children:"EventSource"})," and ",(0,i.jsx)(n.code,{children:"EventSink"})," could be"]}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["configured upon ",(0,i.jsx)(n.code,{children:"hydra-node"})," startup using for example URIs: ",(0,i.jsx)(n.code,{children:"--event-source file://state"})," or ",(0,i.jsx)(n.code,{children:"--event-sink s3://some-bucket"})]}),"\n",(0,i.jsxs)(n.li,{children:["dynamically loaded as plugins without having to fork ",(0,i.jsx)(n.code,{children:"hydra-node"}),"."]}),"\n"]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsxs)(n.p,{children:["The ",(0,i.jsx)(n.code,{children:"Network"})," and ",(0,i.jsx)(n.code,{children:"Chain"})," parts qualify as ",(0,i.jsx)(n.code,{children:"EventSink"}),"s as well or shall those be triggered by ",(0,i.jsx)(n.code,{children:"Effect"}),"s still?"]}),"\n"]}),"\n"]})]})}function h(e={}){const{wrapper:n}={...(0,d.R)(),...e.components};return n?(0,i.jsx)(n,{...e,children:(0,i.jsx)(l,{...e})}):l(e)}},28453:(e,n,s)=>{s.d(n,{R:()=>r,x:()=>c});var t=s(96540);const i={},d=t.createContext(i);function r(e){const n=t.useContext(d);return t.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function c(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(i):e.components||i:r(e.components),t.createElement(d.Provider,{value:n},e.children)}},63803:e=>{e.exports=JSON.parse('{"permalink":"/head-protocol/unstable/adr/29","source":"@site/adr/2023-11-07_029-event-source-sink.md","title":"29. EventSource & EventSink abstractions\\n","description":"Status","date":"2023-11-07T00:00:00.000Z","tags":[{"inline":true,"label":"Accepted","permalink":"/head-protocol/unstable/adr/tags/accepted"}],"readingTime":3.09,"hasTruncateMarker":false,"authors":[{"name":"Elaine Cardenas","title":"Software Engineer","url":"https://github.com/cardenaso11","imageURL":"https://github.com/cardenaso11.png","key":"cardenaso11","page":null},{"name":"Pi Lanningham","title":"Chief Technology Officer, Sundae Labs","url":"https://github.com/quantumplation","imageURL":"https://github.com/quantumplation.png","key":"quantumplation","page":null},{"name":"Sebastian Nagel","title":"Software Engineering Lead","url":"https://github.com/ch1bo","imageURL":"https://github.com/ch1bo.png","key":"ch1bo","page":null}],"frontMatter":{"slug":"29","title":"29. EventSource & EventSink abstractions\\n","authors":["cardenaso11","quantumplation","ch1bo"],"tags":["Accepted"]},"unlisted":false,"prevItem":{"title":"28. Offline mode\\n","permalink":"/head-protocol/unstable/adr/28"},"nextItem":{"title":"30. Use CBOR in external representation of Cardano transactions\\n","permalink":"/head-protocol/unstable/adr/30"}}')}}]);