"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[9188],{64114:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>r,contentTitle:()=>o,default:()=>d,frontMatter:()=>c,metadata:()=>i,toc:()=>h});var i=t(83112),a=t(74848),s=t(28453);const c={slug:23,title:"23. Local chain state in chain layer\n",authors:[],tags:["Accepted"]},o=void 0,r={authorsImageUrls:[]},h=[{value:"Status",id:"status",level:2},{value:"Context",id:"context",level:2},{value:"Decision",id:"decision",level:2},{value:"Consequences",id:"consequences",level:2}];function l(e){const n={a:"a",code:"code",em:"em",h2:"h2",img:"img",li:"li",ol:"ol",p:"p",ul:"ul",...(0,s.R)(),...e.components};return(0,a.jsxs)(a.Fragment,{children:[(0,a.jsx)(n.h2,{id:"status",children:"Status"}),"\n",(0,a.jsx)(n.p,{children:"Accepted"}),"\n",(0,a.jsx)(n.h2,{id:"context",children:"Context"}),"\n",(0,a.jsxs)(n.ul,{children:["\n",(0,a.jsxs)(n.li,{children:[(0,a.jsx)(n.a,{href:"/adr/18",children:"ADR 18"})," merged both ",(0,a.jsx)(n.code,{children:"headState"})," and ",(0,a.jsx)(n.code,{children:"chainState"})," into one single\nstate in the Hydra node, giving the chain layer a way to ",(0,a.jsx)(n.em,{children:"fetch"})," and update\nthe ",(0,a.jsx)(n.code,{children:"chainState"})," when observing a chain event."]}),"\n",(0,a.jsxs)(n.li,{children:["Having the ",(0,a.jsx)(n.code,{children:"headState"})," containing the ",(0,a.jsx)(n.code,{children:"chainState"})," made persistency easier to\ndeal with: we ensure that we always save cohesive states."]}),"\n",(0,a.jsxs)(n.li,{children:["When opening our first head on mainnet we suffered from a ",(0,a.jsx)(n.a,{href:"https://github.com/cardano-scaling/hydra/issues/784",children:"commit/rollback\nissue"})," that was the\nresult of a race condition in the management of the ",(0,a.jsx)(n.code,{children:"chainState"})," as implemented\nin the context of ",(0,a.jsx)(n.a,{href:"/adr/18",children:"ADR 18"}),"."]}),"\n",(0,a.jsx)(n.li,{children:"Reproducing the issue by introducing rollbacks in the model based tests, we\ndiscovered that, as a client of a hydra-node, we had no idea how to deal with\nthe rollback event as it is defined now."}),"\n",(0,a.jsxs)(n.li,{children:[(0,a.jsx)(n.a,{href:"https://github.com/cardano-scaling/hydra/issues/185",children:"#185"})," plans to improve\nrollback management."]}),"\n"]}),"\n",(0,a.jsx)(n.p,{children:"The following picture details the race condition through an exemple:"}),"\n",(0,a.jsxs)(n.ol,{children:["\n",(0,a.jsxs)(n.li,{children:["\n",(0,a.jsxs)(n.p,{children:["The DirectChain component fetch some ",(0,a.jsx)(n.code,{children:"chainState 0"})," from the ",(0,a.jsx)(n.code,{children:"headState"})]}),"\n"]}),"\n",(0,a.jsxs)(n.li,{children:["\n",(0,a.jsx)(n.p,{children:"The DirectChain component observes a transaction and it"}),"\n"]}),"\n"]}),"\n",(0,a.jsxs)(n.ul,{children:["\n",(0,a.jsx)(n.li,{children:"publishes an event about this observation"}),"\n",(0,a.jsxs)(n.li,{children:["updates the ",(0,a.jsx)(n.code,{children:"headState"})," with some ",(0,a.jsx)(n.code,{children:"chainState 1"})]}),"\n"]}),"\n",(0,a.jsxs)(n.ol,{children:["\n",(0,a.jsxs)(n.li,{children:["The Node processes the event and emits a new ",(0,a.jsx)(n.code,{children:"headState"})," with a\n",(0,a.jsx)(n.code,{children:"previousRecoverableState"})," in case a rollback later happens"]}),"\n"]}),"\n",(0,a.jsxs)(n.p,{children:["The problem is that ",(0,a.jsx)(n.code,{children:"HeadState 2"})," in the figure should point to a previous\nrecoverable head state containing ",(0,a.jsx)(n.code,{children:"chainState 0"})," and not ",(0,a.jsx)(n.code,{children:"chainState 1"}),"."]}),"\n",(0,a.jsx)(n.p,{children:(0,a.jsx)(n.img,{alt:"race condition",src:t(64099).A+"",width:"2310",height:"1731"})}),"\n",(0,a.jsxs)(n.p,{children:["Updating the chain state only in the ",(0,a.jsx)(n.code,{children:"HeadLogic"})," leads to problems when several\ntransactions are in the same block. This can be mitigated by keeping a volatile\nchain state locally while analysing the block. But then it leads to race\nconditions issues if, for some reason, blocks are produced faster than they are\nprocessed by the HeadLogic. Low probability in production but higher when\ntesting."]}),"\n",(0,a.jsx)(n.h2,{id:"decision",children:"Decision"}),"\n",(0,a.jsxs)(n.ul,{children:["\n",(0,a.jsxs)(n.li,{children:["We supersede ",(0,a.jsx)(n.a,{href:"/adr/18",children:"ADR 18"})," with the current ADR."]}),"\n",(0,a.jsx)(n.li,{children:"A local chain state is re-introduced in the chain component, not shared with\nthe head logic."}),"\n",(0,a.jsxs)(n.li,{children:["A copy of the ",(0,a.jsx)(n.code,{children:"chainState"})," is kept in the ",(0,a.jsx)(n.code,{children:"headState"})," to keep the benefits of\n",(0,a.jsx)(n.a,{href:"/adr/18",children:"ADR 18"})," regarding persistency."]}),"\n",(0,a.jsxs)(n.li,{children:["The ",(0,a.jsx)(n.code,{children:"RolledBack"})," output is removed from the API unless actionable by users or\n",(0,a.jsx)(n.a,{href:"https://github.com/cardano-scaling/hydra/issues/185",children:"#185"})," implemented."]}),"\n"]}),"\n",(0,a.jsx)(n.h2,{id:"consequences",children:"Consequences"}),"\n",(0,a.jsxs)(n.ul,{children:["\n",(0,a.jsx)(n.li,{children:"The rollback logic is removed from the HeadLogic and only maintained in the\nchain component."}),"\n",(0,a.jsx)(n.li,{children:"The Rollback event carries the ChainState."}),"\n",(0,a.jsxs)(n.li,{children:["At the node startup, we initialize the chain layer with the persisted\n",(0,a.jsx)(n.code,{children:"chainState"})]}),"\n"]})]})}function d(e={}){const{wrapper:n}={...(0,s.R)(),...e.components};return n?(0,a.jsx)(n,{...e,children:(0,a.jsx)(l,{...e})}):l(e)}},64099:(e,n,t)=>{t.d(n,{A:()=>i});const i=t.p+"assets/images/2023-04-26-023-race-condition-7bc6b62f01470cdeaaaf81ec5227363e.jpg"},28453:(e,n,t)=>{t.d(n,{R:()=>c,x:()=>o});var i=t(96540);const a={},s=i.createContext(a);function c(e){const n=i.useContext(s);return i.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function o(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(a):e.components||a:c(e.components),i.createElement(s.Provider,{value:n},e.children)}},83112:e=>{e.exports=JSON.parse('{"permalink":"/head-protocol/unstable/adr/23","source":"@site/adr/2023-04-26_023-local-chain-state.md","title":"23. Local chain state in chain layer\\n","description":"Status","date":"2023-04-26T00:00:00.000Z","tags":[{"inline":true,"label":"Accepted","permalink":"/head-protocol/unstable/adr/tags/accepted"}],"readingTime":1.99,"hasTruncateMarker":false,"authors":[],"frontMatter":{"slug":"23","title":"23. Local chain state in chain layer\\n","authors":[],"tags":["Accepted"]},"unlisted":false,"prevItem":{"title":"22. Test High-level Properties using Model-Based Testing\\n","permalink":"/head-protocol/unstable/adr/22"},"nextItem":{"title":"24. Persist state changes incrementally\\n","permalink":"/head-protocol/unstable/adr/24"}}')}}]);