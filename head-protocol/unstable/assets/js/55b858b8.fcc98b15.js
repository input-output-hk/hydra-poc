"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[8542],{92272:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>d,contentTitle:()=>l,default:()=>m,frontMatter:()=>c,metadata:()=>s,toc:()=>u});const s=JSON.parse('{"id":"index","title":"Hydra use cases","description":"This section aims to initiate a dialogue with community members interested in exploring how Hydra can be leveraged to address real-world problems or those considering using Hydra as a framework to develop solutions on the Cardano platform. Our goal is to expand this collection of case studies with contributions from the community, making it a valuable reference point, even for those not deeply familiar with the technical feature roadmap.","source":"@site/use-cases/index.md","sourceDirName":".","slug":"/","permalink":"/head-protocol/unstable/use-cases/","draft":false,"unlisted":false,"editUrl":"https://github.com/cardano-scaling/hydra/tree/master/docs/use-cases/index.md","tags":[],"version":"current","sidebarPosition":1,"frontMatter":{"sidebar_label":"Use cases","sidebar_position":1},"sidebar":"defaultSidebar","next":{"title":"Auctions","permalink":"/head-protocol/unstable/use-cases/auctions/"}}');var r=n(74848),o=n(28453),i=n(41830),a=n(60609);const c={sidebar_label:"Use cases",sidebar_position:1},l="Hydra use cases",d={},u=[];function h(e){const t={a:"a",h1:"h1",header:"header",p:"p",...(0,o.R)(),...e.components};return(0,r.jsxs)(r.Fragment,{children:[(0,r.jsx)(t.header,{children:(0,r.jsx)(t.h1,{id:"hydra-use-cases",children:"Hydra use cases"})}),"\n",(0,r.jsxs)(t.p,{children:["This section aims to initiate a dialogue with community members interested in exploring how Hydra can be leveraged to address real-world problems or those considering using Hydra as a framework to develop solutions on the Cardano platform. Our goal is to expand this collection of case studies with contributions from the community, making it a valuable reference point, even for those not deeply familiar with the ",(0,r.jsx)(t.a,{href:"https://github.com/orgs/input-output-hk/projects/21",children:"technical feature roadmap"}),"."]}),"\n",(0,r.jsx)(t.p,{children:"We are eager to highlight the initial use cases where Hydra can significantly enhance value by reducing transaction costs, decreasing confirmation times, and increasing transaction throughput. We actively invite the community to engage in discussions, contribute to the featured use cases on the roadmap, and suggest additional scenarios we may have overlooked."}),"\n",(0,r.jsx)(t.p,{children:"We are currently exploring use cases across various domains, including payments, digital asset auctions, decentralized voting, and decentralized finance. This section will be updated as these use cases are further developed and detailed. So, stay tuned!"}),"\n","\n",(0,r.jsx)(i.A,{items:(0,a.t)().items.filter((({docId:e})=>"index"!=e))})]})}function m(e={}){const{wrapper:t}={...(0,o.R)(),...e.components};return t?(0,r.jsx)(t,{...e,children:(0,r.jsx)(h,{...e})}):h(e)}},41830:(e,t,n)=>{n.d(t,{A:()=>y});n(96540);var s=n(18215),r=n(26972),o=n(28774),i=n(53465),a=n(16654),c=n(21312),l=n(6028);const d={cardContainer:"cardContainer_fWXF",cardTitle:"cardTitle_rnsV",cardDescription:"cardDescription_PWke"};var u=n(74848);function h(e){let{href:t,children:n}=e;return(0,u.jsx)(o.A,{href:t,className:(0,s.A)("card padding--lg",d.cardContainer),children:n})}function m(e){let{href:t,icon:n,title:r,description:o}=e;return(0,u.jsxs)(h,{href:t,children:[(0,u.jsxs)(l.A,{as:"h2",className:(0,s.A)("text--truncate",d.cardTitle),title:r,children:[n," ",r]}),o&&(0,u.jsx)("p",{className:(0,s.A)("text--truncate",d.cardDescription),title:o,children:o})]})}function p(e){let{item:t}=e;const n=(0,r.Nr)(t),s=function(){const{selectMessage:e}=(0,i.W)();return t=>e(t,(0,c.T)({message:"1 item|{count} items",id:"theme.docs.DocCard.categoryDescription.plurals",description:"The default description for a category card in the generated index about how many items this category includes"},{count:t}))}();return n?(0,u.jsx)(m,{href:n,icon:"\ud83d\uddc3\ufe0f",title:t.label,description:t.description??s(t.items.length)}):null}function f(e){let{item:t}=e;const n=(0,a.A)(t.href)?"\ud83d\udcc4\ufe0f":"\ud83d\udd17",s=(0,r.cC)(t.docId??void 0);return(0,u.jsx)(m,{href:t.href,icon:n,title:t.label,description:t.description??s?.description})}function g(e){let{item:t}=e;switch(t.type){case"link":return(0,u.jsx)(f,{item:t});case"category":return(0,u.jsx)(p,{item:t});default:throw new Error(`unknown item type ${JSON.stringify(t)}`)}}function x(e){let{className:t}=e;const n=(0,r.$S)();return(0,u.jsx)(y,{items:n.items,className:t})}function y(e){const{items:t,className:n}=e;if(!t)return(0,u.jsx)(x,{...e});const o=(0,r.d1)(t);return(0,u.jsx)("section",{className:(0,s.A)("row",n),children:o.map(((e,t)=>(0,u.jsx)("article",{className:"col col--6 margin-bottom--lg",children:(0,u.jsx)(g,{item:e})},t)))})}},53465:(e,t,n)=>{n.d(t,{W:()=>l});var s=n(96540),r=n(44586);const o=["zero","one","two","few","many","other"];function i(e){return o.filter((t=>e.includes(t)))}const a={locale:"en",pluralForms:i(["one","other"]),select:e=>1===e?"one":"other"};function c(){const{i18n:{currentLocale:e}}=(0,r.A)();return(0,s.useMemo)((()=>{try{return function(e){const t=new Intl.PluralRules(e);return{locale:e,pluralForms:i(t.resolvedOptions().pluralCategories),select:e=>t.select(e)}}(e)}catch(t){return console.error(`Failed to use Intl.PluralRules for locale "${e}".\nDocusaurus will fallback to the default (English) implementation.\nError: ${t.message}\n`),a}}),[e])}function l(){const e=c();return{selectMessage:(t,n)=>function(e,t,n){const s=e.split("|");if(1===s.length)return s[0];s.length>n.pluralForms.length&&console.error(`For locale=${n.locale}, a maximum of ${n.pluralForms.length} plural forms are expected (${n.pluralForms.join(",")}), but the message contains ${s.length}: ${e}`);const r=n.select(t),o=n.pluralForms.indexOf(r);return s[Math.min(o,s.length-1)]}(n,t,e)}}},28453:(e,t,n)=>{n.d(t,{R:()=>i,x:()=>a});var s=n(96540);const r={},o=s.createContext(r);function i(e){const t=s.useContext(o);return s.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function a(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(r):e.components||r:i(e.components),s.createElement(o.Provider,{value:t},e.children)}}}]);