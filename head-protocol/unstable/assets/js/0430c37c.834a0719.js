"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[5270],{94416:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>l,contentTitle:()=>a,default:()=>h,frontMatter:()=>i,metadata:()=>s,toc:()=>d});var s=n(69386),o=n(74848),r=n(28453);const i={slug:22,title:"22. Test High-level Properties using Model-Based Testing\n",authors:[],tags:["Accepted"]},a=void 0,l={authorsImageUrls:[]},d=[{value:"Status",id:"status",level:2},{value:"Context",id:"context",level:2},{value:"Decision",id:"decision",level:2},{value:"Consequences",id:"consequences",level:2}];function c(e){const t={a:"a",code:"code",em:"em",h2:"h2",li:"li",p:"p",ul:"ul",...(0,r.R)(),...e.components};return(0,o.jsxs)(o.Fragment,{children:[(0,o.jsx)(t.h2,{id:"status",children:"Status"}),"\n",(0,o.jsx)(t.p,{children:"Accepted"}),"\n",(0,o.jsx)(t.h2,{id:"context",children:"Context"}),"\n",(0,o.jsxs)(t.ul,{children:["\n",(0,o.jsxs)(t.li,{children:["We have been experimenting with ",(0,o.jsx)(t.a,{href:"https://hackage.org/packages/quickcheck-dynamic",children:"quickcheck-dynamic"})," for a while, leading to the implementation of basic ",(0,o.jsx)(t.a,{href:"https://github.com/cardano-scaling/hydra/blob/master/hydra-node/test/Hydra/ModelSpec.hs",children:"Model-Based tests"})," for the Hydra Head Protocol"]}),"\n",(0,o.jsxs)(t.li,{children:["These tests fill a gap in our testing strategy, between ",(0,o.jsx)(t.a,{href:"https://github.com/cardano-scaling/hydra/blob/master/hydra-node/test/Hydra/BehaviorSpec.hs",children:"BehaviorSpec"}),' tests which test a "network" of nodes but only at the level of the off-chain Head logic, and ',(0,o.jsx)(t.a,{href:"https://github.com/cardano-scaling/hydra/blob/master/hydra-cluster/test/Test/EndToEndSpec.hs",children:"EndToEndSpec"})," tests which test a full blown network of nodes interconnected through real network connections and to a real cardano-node:","\n",(0,o.jsxs)(t.ul,{children:["\n",(0,o.jsx)(t.li,{children:"The former are fast but do not test the complete lifecycle of a Head. Furthermore, they are only unit tests so do not provide coverage into various corner cases that could arise in practice"}),"\n",(0,o.jsx)(t.li,{children:"The latter exercise the full lifecycle but are very slow and brittle"}),"\n"]}),"\n"]}),"\n",(0,o.jsxs)(t.li,{children:["Because they run in ",(0,o.jsx)(t.a,{href:"https://github.com/input-output-hk/io-sim",children:"io-sim"}),", those Model-based tests are fast and robust as they don't depend on system interactions. Moreover, decoupling the ",(0,o.jsx)(t.em,{children:"System-under-Test"})," from ",(0,o.jsx)(t.code,{children:"IO"}),' makes it easy to simulate an environment that deviates from the "happy path" such as delays from the network, filesystem errors, or even adversarial behaviour from the node, or the chain.']}),"\n"]}),"\n",(0,o.jsx)(t.h2,{id:"decision",children:"Decision"}),"\n",(0,o.jsxs)(t.ul,{children:["\n",(0,o.jsxs)(t.li,{children:["We will maintain and evolve the ",(0,o.jsx)(t.a,{href:"https://github.com/cardano-scaling/hydra/blob/master/hydra-node/test/Hydra/Model.hs",children:"Model"})," over time to cover more features"]}),"\n",(0,o.jsxs)(t.li,{children:["Key properties of the whole system should be written-down as proper ",(0,o.jsx)(t.code,{children:"DynamicLogic"})," properties and thoroughly tested using quickcheck-dynamic. This includes but is not limited to:","\n",(0,o.jsxs)(t.ul,{children:["\n",(0,o.jsx)(t.li,{children:"Liveness of the Head"}),"\n",(0,o.jsx)(t.li,{children:"Consistency of the Head"}),"\n",(0,o.jsx)(t.li,{children:"Soundness of Chain"}),"\n",(0,o.jsx)(t.li,{children:"Completeness of Chain"}),"\n"]}),"\n"]}),"\n"]}),"\n",(0,o.jsx)(t.h2,{id:"consequences",children:"Consequences"}),"\n",(0,o.jsxs)(t.ul,{children:["\n",(0,o.jsx)(t.li,{children:"We need to ensure the Model covers the full lifecycle of a Hydra Head network which at the time of writing this ADR is not the case"}),"\n",(0,o.jsxs)(t.li,{children:["There cannot be ",(0,o.jsx)(t.em,{children:"One Model to Rule Them All"})," so we should refrain from defining different ",(0,o.jsx)(t.code,{children:"StateModel"})," or different ",(0,o.jsx)(t.code,{children:"RunModel"})," depending on what needs to be tested"]}),"\n",(0,o.jsxs)(t.li,{children:["In particular, testing against adversarial conditions will certainly require defining different instances of the ",(0,o.jsx)(t.code,{children:"Network"})," or ",(0,o.jsx)(t.code,{children:"Chain"})," components, for example:","\n",(0,o.jsxs)(t.ul,{children:["\n",(0,o.jsxs)(t.li,{children:["An ",(0,o.jsx)(t.em,{children:"Accepted Adversary"})," that fully the controls the protocol and the parties,"]}),"\n",(0,o.jsxs)(t.li,{children:["A ",(0,o.jsx)(t.em,{children:"Network Adversary"})," that can delay and or drop messages,"]}),"\n",(0,o.jsxs)(t.li,{children:["A ",(0,o.jsx)(t.em,{children:"Faulty Filesystem"})," that can causes exceptions when reading or writing files,"]}),"\n",(0,o.jsx)(t.li,{children:"..."}),"\n"]}),"\n"]}),"\n"]})]})}function h(e={}){const{wrapper:t}={...(0,r.R)(),...e.components};return t?(0,o.jsx)(t,{...e,children:(0,o.jsx)(c,{...e})}):c(e)}},28453:(e,t,n)=>{n.d(t,{R:()=>i,x:()=>a});var s=n(96540);const o={},r=s.createContext(o);function i(e){const t=s.useContext(r);return s.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function a(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(o):e.components||o:i(e.components),s.createElement(r.Provider,{value:t},e.children)}},69386:e=>{e.exports=JSON.parse('{"permalink":"/head-protocol/unstable/adr/22","source":"@site/adr/2022-12-06_022-model-based-testing.md","title":"22. Test High-level Properties using Model-Based Testing\\n","description":"Status","date":"2022-12-06T00:00:00.000Z","tags":[{"inline":true,"label":"Accepted","permalink":"/head-protocol/unstable/adr/tags/accepted"}],"readingTime":1.805,"hasTruncateMarker":false,"authors":[],"frontMatter":{"slug":"22","title":"22. Test High-level Properties using Model-Based Testing\\n","authors":[],"tags":["Accepted"]},"unlisted":false,"prevItem":{"title":"21. Bounded transaction validity on Hydra protocol transactions\\n","permalink":"/head-protocol/unstable/adr/21"},"nextItem":{"title":"23. Local chain state in chain layer\\n","permalink":"/head-protocol/unstable/adr/23"}}')}}]);