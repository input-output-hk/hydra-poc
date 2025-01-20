"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[1803],{3905:(e,t,n)=>{n.d(t,{Zo:()=>d,kt:()=>h});var r=n(67294);function a(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function i(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function o(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?i(Object(n),!0).forEach((function(t){a(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function c(e,t){if(null==e)return{};var n,r,a=function(e,t){if(null==e)return{};var n,r,a={},i=Object.keys(e);for(r=0;r<i.length;r++)n=i[r],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(r=0;r<i.length;r++)n=i[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var s=r.createContext({}),l=function(e){var t=r.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):o(o({},t),e)),n},d=function(e){var t=l(e.components);return r.createElement(s.Provider,{value:t},e.children)},u="mdxType",m={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},p=r.forwardRef((function(e,t){var n=e.components,a=e.mdxType,i=e.originalType,s=e.parentName,d=c(e,["components","mdxType","originalType","parentName"]),u=l(n),p=a,h=u["".concat(s,".").concat(p)]||u[p]||m[p]||i;return n?r.createElement(h,o(o({ref:t},d),{},{components:n})):r.createElement(h,o({ref:t},d))}));function h(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var i=n.length,o=new Array(i);o[0]=p;var c={};for(var s in t)hasOwnProperty.call(t,s)&&(c[s]=t[s]);c.originalType=e,c[u]="string"==typeof e?e:a,o[1]=c;for(var l=2;l<i;l++)o[l]=n[l];return r.createElement.apply(null,o)}return r.createElement.apply(null,n)}p.displayName="MDXCreateElement"},90279:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>d,contentTitle:()=>s,default:()=>h,frontMatter:()=>c,metadata:()=>l,toc:()=>u});var r=n(87462),a=(n(67294),n(3905)),i=n(27922),o=n(1116);const c={sidebar_label:"Benchmarks",sidebar_position:1},s="Benchmarks and limitations",l={unversionedId:"index",id:"index",title:"Benchmarks and limitations",description:"This section provides up-to-date data regarding the known limitations of the Hydra Head on-chain protocol. Cardano transactions and blocks are subject to constraints on transaction size, execution cost, and the number of inputs and outputs. These constraints are determined by network parameters and significantly influence the capabilities of the Hydra Head protocol, such as the maximum number of parties that can participate, the amount of UTXOs that can be committed, and the extent to which these UTXOs can be fanned out. As on-chain scripts and transactions are further optimized, and as the underlying Cardano blockchain evolves with expanded parameters and enhanced script execution efficiency, these limitations are expected to evolve.",source:"@site/benchmarks/index.md",sourceDirName:".",slug:"/",permalink:"/head-protocol/benchmarks/",draft:!1,tags:[],version:"current",sidebarPosition:1,frontMatter:{sidebar_label:"Benchmarks",sidebar_position:1},sidebar:"defaultSidebar",next:{title:"Plutus Merkle tree benchmarks",permalink:"/head-protocol/benchmarks/mt-benchmarks"}},d={},u=[],m={toc:u},p="wrapper";function h(e){let{components:t,...n}=e;return(0,a.kt)(p,(0,r.Z)({},m,n,{components:t,mdxType:"MDXLayout"}),(0,a.kt)("h1",{id:"benchmarks-and-limitations"},"Benchmarks and limitations"),(0,a.kt)("p",null,"This section provides up-to-date data regarding the known limitations of the Hydra Head on-chain protocol. Cardano transactions and blocks are subject to constraints on transaction size, execution cost, and the number of inputs and outputs. These constraints are determined by network parameters and significantly influence the capabilities of the Hydra Head protocol, such as the maximum number of parties that can participate, the amount of UTXOs that can be committed, and the extent to which these UTXOs can be fanned out. As on-chain scripts and transactions are further optimized, and as the underlying Cardano blockchain evolves with expanded parameters and enhanced script execution efficiency, these limitations are expected to evolve."),(0,a.kt)("p",null,"The data in this section is ",(0,a.kt)("em",{parentName:"p"},"generated")," through Hydra's ",(0,a.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/hydra/actions/workflows/ci-nix.yaml"},"continuous integration (CI)")," process, ensuring that it accurately reflects the current state of the code."),(0,a.kt)(i.Z,{items:(0,o.V)().items.filter((e=>{let{docId:t}=e;return"index"!=t})).map((e=>("tests"==e.label&&(e.label="Test Results"),e))),mdxType:"DocCardList"}))}h.isMDXComponent=!0},27922:(e,t,n)=>{n.d(t,{Z:()=>b});var r=n(67294),a=n(86010),i=n(53438),o=n(39960),c=n(13919),s=n(95999);const l={cardContainer:"cardContainer_fWXF",cardTitle:"cardTitle_rnsV",cardDescription:"cardDescription_PWke"};function d(e){let{href:t,children:n}=e;return r.createElement(o.Z,{href:t,className:(0,a.Z)("card padding--lg",l.cardContainer)},n)}function u(e){let{href:t,icon:n,title:i,description:o}=e;return r.createElement(d,{href:t},r.createElement("h2",{className:(0,a.Z)("text--truncate",l.cardTitle),title:i},n," ",i),o&&r.createElement("p",{className:(0,a.Z)("text--truncate",l.cardDescription),title:o},o))}function m(e){let{item:t}=e;const n=(0,i.Wl)(t);return n?r.createElement(u,{href:n,icon:"\ud83d\uddc3\ufe0f",title:t.label,description:t.description??(0,s.I)({message:"{count} items",id:"theme.docs.DocCard.categoryDescription",description:"The default description for a category card in the generated index about how many items this category includes"},{count:t.items.length})}):null}function p(e){let{item:t}=e;const n=(0,c.Z)(t.href)?"\ud83d\udcc4\ufe0f":"\ud83d\udd17",a=(0,i.xz)(t.docId??void 0);return r.createElement(u,{href:t.href,icon:n,title:t.label,description:t.description??a?.description})}function h(e){let{item:t}=e;switch(t.type){case"link":return r.createElement(p,{item:t});case"category":return r.createElement(m,{item:t});default:throw new Error(`unknown item type ${JSON.stringify(t)}`)}}function f(e){let{className:t}=e;const n=(0,i.jA)();return r.createElement(b,{items:n.items,className:t})}function b(e){const{items:t,className:n}=e;if(!t)return r.createElement(f,e);const o=(0,i.MN)(t);return r.createElement("section",{className:(0,a.Z)("row",n)},o.map(((e,t)=>r.createElement("article",{key:t,className:"col col--6 margin-bottom--lg"},r.createElement(h,{item:e})))))}}}]);