"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[9918],{8070:(e,a,n)=>{n.r(a),n.d(a,{assets:()=>d,contentTitle:()=>o,default:()=>h,frontMatter:()=>r,metadata:()=>t,toc:()=>c});const t=JSON.parse('{"id":"managed/index","title":"Managed Hydra head","description":"This document is a work in progress.","source":"@site/topologies/managed/index.mdx","sourceDirName":"managed","slug":"/managed/","permalink":"/head-protocol/unstable/topologies/managed/","draft":false,"unlisted":false,"editUrl":"https://github.com/cardano-scaling/hydra/tree/master/docs/topologies/managed/index.mdx","tags":[],"version":"current","sidebarPosition":3,"frontMatter":{"sidebar_label":"Managed Hydra head","sidebar_position":3},"sidebar":"defaultSidebar","previous":{"title":"Basic Hydra head","permalink":"/head-protocol/unstable/topologies/basic/"},"next":{"title":"Delegated head","permalink":"/head-protocol/unstable/topologies/delegated-head/"}}');var s=n(74848),i=n(28453);const r={sidebar_label:"Managed Hydra head",sidebar_position:3},o="Managed Hydra head",d={},c=[];function l(e){const a={a:"a",admonition:"admonition",code:"code",em:"em",h1:"h1",header:"header",p:"p",...(0,i.R)(),...e.components};return(0,s.jsxs)(s.Fragment,{children:[(0,s.jsx)(a.header,{children:(0,s.jsx)(a.h1,{id:"managed-hydra-head",children:"Managed Hydra head"})}),"\n",(0,s.jsx)(a.admonition,{type:"note",children:(0,s.jsxs)(a.p,{children:["\ud83d\udee0"," This document is a work in progress."]})}),"\n",(0,s.jsxs)(a.p,{children:["This document outlines the ",(0,s.jsx)(a.em,{children:"managed Hydra head"})," topology, which could also be described as ",(0,s.jsx)(a.em,{children:"Hydra as a service"}),"."]}),"\n",(0,s.jsx)("p",{align:"center",children:(0,s.jsx)("img",{src:n(20194).A,alt:"Managed Hydra Head",height:400})}),"\n",(0,s.jsxs)(a.p,{children:["The ",(0,s.jsx)(a.a,{href:"./basic",children:"basic Hydra head setup"})," requires each participant to host an instance of a ",(0,s.jsx)(a.code,{children:"hydra-node"}),", similar to how ",(0,s.jsx)(a.code,{children:"cardano-node"}),"s operate on the Cardano network. In contrast, 'light node' setups allow users to access the blockchain through a hosted API, with light wallets being a common example."]}),"\n",(0,s.jsxs)(a.p,{children:["In this topology, clients do not need to run their own ",(0,s.jsx)(a.code,{children:"hydra-node"}),"s but instead access a ",(0,s.jsx)(a.code,{children:"hydra-node"})," provided by a ",(0,s.jsx)(a.em,{children:"service provider"}),". Client applications, such as light wallets, do not need to be aware of individual ",(0,s.jsx)(a.code,{children:"hydra-node"})," instances. Instead, logical ",(0,s.jsx)(a.em,{children:"Hydra heads"})," are accessible via an API."]}),"\n",(0,s.jsxs)(a.p,{children:["The illustration above depicts three different Hydra heads: two pairwise (yellow and green) and one multi-party (blue). Clients A, B, and C access their heads using the service provider, while client D manages their own hosting.\nFor this setup to be feasible, it is crucial that the ",(0,s.jsx)(a.em,{children:"Hydra keys"})," remain on the client side and that the ",(0,s.jsx)(a.code,{children:"hydra-node"})," serves purely as infrastructure \u2014 it does not take custody of the user's funds."]}),"\n",(0,s.jsxs)(a.p,{children:["As a result, the client must be online for any progress to occur within a Hydra head. This requirement can be cumbersome in multi-party Hydra heads, as they may stall if a lightweight mobile client is offline for a period. However, this setup aligns well with ",(0,s.jsx)(a.em,{children:"two-party Hydra heads"}),", where a transaction is only completed if both parties are online to send, receive, and acknowledge it."]}),"\n",(0,s.jsxs)(a.p,{children:["An example use case for two-party Hydra heads includes payment channels between two ",(0,s.jsx)(a.a,{href:"./../use-cases/payments/pay-per-use-api",children:"machines"})," or ",(0,s.jsx)(a.a,{href:"./../use-cases/payments/inter-wallet-payments",children:"individuals"}),", especially if such multiple channels are logically interconnected, similar to the Lightning Network."]}),"\n",(0,s.jsx)(a.p,{children:"Although access to Hydra heads is facilitated by the service provider, this does not centralize the system. A client can always close a head and recover funds with an alternative provider or even use a transaction created out-of-band (eg, by the client application using another service to submit the transaction)."})]})}function h(e={}){const{wrapper:a}={...(0,i.R)(),...e.components};return a?(0,s.jsx)(a,{...e,children:(0,s.jsx)(l,{...e})}):l(e)}},20194:(e,a,n)=>{n.d(a,{A:()=>t});const t=n.p+"assets/images/managed-hydra-head-2203fcc5a3e69632cdd273a1c1acef21.jpg"},28453:(e,a,n)=>{n.d(a,{R:()=>r,x:()=>o});var t=n(96540);const s={},i=t.createContext(s);function r(e){const a=t.useContext(i);return t.useMemo((function(){return"function"==typeof e?e(a):{...a,...e}}),[a,e])}function o(e){let a;return a=e.disableParentContext?"function"==typeof e.components?e.components(s):e.components||s:r(e.components),t.createElement(i.Provider,{value:a},e.children)}}}]);