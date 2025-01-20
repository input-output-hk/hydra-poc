"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[3737],{78525:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>l,contentTitle:()=>c,default:()=>h,frontMatter:()=>o,metadata:()=>a,toc:()=>d});const a=JSON.parse('{"id":"auctions/index","title":"Hydra for auctions","description":"NFT marketplaces on Cardano","source":"@site/use-cases/auctions/index.md","sourceDirName":"auctions","slug":"/auctions/","permalink":"/head-protocol/unstable/use-cases/auctions/","draft":false,"unlisted":false,"editUrl":"https://github.com/cardano-scaling/hydra/tree/master/docs/use-cases/auctions/index.md","tags":[],"version":"current","sidebarPosition":2,"frontMatter":{"sidebar_label":"Auctions","sidebar_position":2},"sidebar":"defaultSidebar","previous":{"title":"Use cases","permalink":"/head-protocol/unstable/use-cases/"},"next":{"title":"Delegated voucher: invitational","permalink":"/head-protocol/unstable/use-cases/auctions/delegated-voucher-invitational/"}}');var r=t(74848),i=t(28453),s=t(41830);const o={sidebar_label:"Auctions",sidebar_position:2},c="Hydra for auctions",l={},d=[{value:"NFT marketplaces on Cardano",id:"nft-marketplaces-on-cardano",level:3},{value:"NFT auctions on Cardano and current constraints",id:"nft-auctions-on-cardano-and-current-constraints",level:3},{value:"Running auctions with Hydra",id:"running-auctions-with-hydra",level:3},{value:"A way forward",id:"a-way-forward",level:3},{value:"Further reading",id:"further-reading",level:3}];function u(e){const n={a:"a",h1:"h1",h3:"h3",header:"header",li:"li",p:"p",ul:"ul",...(0,i.R)(),...e.components};return(0,r.jsxs)(r.Fragment,{children:[(0,r.jsx)(n.header,{children:(0,r.jsx)(n.h1,{id:"hydra-for-auctions",children:"Hydra for auctions"})}),"\n",(0,r.jsx)(n.h3,{id:"nft-marketplaces-on-cardano",children:"NFT marketplaces on Cardano"}),"\n",(0,r.jsx)(n.p,{children:"Cardano simplifies the minting and transferring of non-fungible tokens (NFTs) by integrating the accounting for non-ada tokens directly into the ledger alongside ada, eliminating the need for complex and potentially error-prone custom smart contracts. This streamlined approach has spurred a vibrant NFT ecosystem on Cardano, encompassing art, music, identity, real estate, gaming, service subscriptions, and more."}),"\n",(0,r.jsx)(n.p,{children:"High-quality marketplaces have emerged on Cardano, offering platforms where users can list, view, and purchase a wide array of NFTs. These platforms feature user-friendly interfaces that neatly display images or animations, rarity charts, royalty terms, and other metadata. NFTs can be purchased at the seller\u2019s listed price or through an alternative offer made by a buyer."}),"\n",(0,r.jsx)(n.p,{children:"However, the novelty of tokenized assets on Cardano and the relatively small market size pose challenges for effective price discovery in the NFT sector."}),"\n",(0,r.jsx)(n.h3,{id:"nft-auctions-on-cardano-and-current-constraints",children:"NFT auctions on Cardano and current constraints"}),"\n",(0,r.jsx)(n.p,{children:"Auctions are a proven mechanism for efficient price discovery, especially for novel or unique items such as artworks, complex allocations like radio spectrums, or situations prone to insider trading and collusion, such as bankruptcy fire sales. Unlike traditional marketplaces, where a seller sets a price and the first buyer to match it secures the purchase, auctions allow the price to evolve through competitive bidding, culminating in the sale to the highest bidder."}),"\n",(0,r.jsx)(n.p,{children:"Auctions need to be dynamic and efficient. Bidders often enter auctions spontaneously with limited time and attention, and a slow or cumbersome bidding process can deter participation. Implementing such an auction experience directly on Cardano\u2019s main network (layer 1) poses challenges, as layer 1 transactions can take time to be added to a block (approximately 10\u201360 seconds) and to be confirmed with a low probability of rollback (several minutes to hours)."}),"\n",(0,r.jsx)(n.h3,{id:"running-auctions-with-hydra",children:"Running auctions with Hydra"}),"\n",(0,r.jsx)(n.p,{children:"Auctions' bidding mechanism is ideally suited for scaling on layer 2 technologies such as Hydra, where transactions within a Hydra head are confirmed rapidly and attain immediate finality. An effective layer 2-powered auction service, leveraging Hydra\u2019s capabilities, could significantly enhance the auction experience on Cardano."}),"\n",(0,r.jsx)(n.p,{children:"We envision that a Hydra-based auction framework could become a standard component integrated into NFT marketplaces, games, and other Web 3.0 applications. This integration would enable these platforms to incorporate digital asset auctions seamlessly. Additionally, it could stimulate a new ecosystem for professional scalability providers, offering layer 2 hosting services akin to the existing stake pool and emerging governance ecosystems."}),"\n",(0,r.jsx)(n.h3,{id:"a-way-forward",children:"A way forward"}),"\n",(0,r.jsx)(n.p,{children:"The path to achieving this vision could unfold through a series of feasible milestones, leveraging Hydra and its future enhancements:"}),"\n","\n",(0,r.jsx)(s.A,{}),"\n",(0,r.jsx)(n.h3,{id:"further-reading",children:"Further reading"}),"\n",(0,r.jsxs)(n.ul,{children:["\n",(0,r.jsxs)(n.li,{children:["\n",(0,r.jsxs)(n.p,{children:["Explore the foundational paper by IOG and MLabs on utilizing Hydra for auctions: ",(0,r.jsx)(n.a,{href:"https://iohk.io/en/blog/posts/2023/01/20/implementing-auction-projects-using-hydra/",children:"Implementing auction projects using Hydra"})]}),"\n"]}),"\n",(0,r.jsxs)(n.li,{children:["\n",(0,r.jsxs)(n.p,{children:["Check out the repository for a reference implementation of a delegated voucher auction using Hydra: ",(0,r.jsx)(n.a,{href:"https://github.com/mlabs-haskell/hydra-auction",children:"hydra-auction"}),"."]}),"\n"]}),"\n"]})]})}function h(e={}){const{wrapper:n}={...(0,i.R)(),...e.components};return n?(0,r.jsx)(n,{...e,children:(0,r.jsx)(u,{...e})}):u(e)}},41830:(e,n,t)=>{t.d(n,{A:()=>x});t(96540);var a=t(18215),r=t(26972),i=t(28774),s=t(53465),o=t(16654),c=t(21312),l=t(6028);const d={cardContainer:"cardContainer_fWXF",cardTitle:"cardTitle_rnsV",cardDescription:"cardDescription_PWke"};var u=t(74848);function h(e){let{href:n,children:t}=e;return(0,u.jsx)(i.A,{href:n,className:(0,a.A)("card padding--lg",d.cardContainer),children:t})}function p(e){let{href:n,icon:t,title:r,description:i}=e;return(0,u.jsxs)(h,{href:n,children:[(0,u.jsxs)(l.A,{as:"h2",className:(0,a.A)("text--truncate",d.cardTitle),title:r,children:[t," ",r]}),i&&(0,u.jsx)("p",{className:(0,a.A)("text--truncate",d.cardDescription),title:i,children:i})]})}function m(e){let{item:n}=e;const t=(0,r.Nr)(n),a=function(){const{selectMessage:e}=(0,s.W)();return n=>e(n,(0,c.T)({message:"1 item|{count} items",id:"theme.docs.DocCard.categoryDescription.plurals",description:"The default description for a category card in the generated index about how many items this category includes"},{count:n}))}();return t?(0,u.jsx)(p,{href:t,icon:"\ud83d\uddc3\ufe0f",title:n.label,description:n.description??a(n.items.length)}):null}function f(e){let{item:n}=e;const t=(0,o.A)(n.href)?"\ud83d\udcc4\ufe0f":"\ud83d\udd17",a=(0,r.cC)(n.docId??void 0);return(0,u.jsx)(p,{href:n.href,icon:t,title:n.label,description:n.description??a?.description})}function g(e){let{item:n}=e;switch(n.type){case"link":return(0,u.jsx)(f,{item:n});case"category":return(0,u.jsx)(m,{item:n});default:throw new Error(`unknown item type ${JSON.stringify(n)}`)}}function y(e){let{className:n}=e;const t=(0,r.$S)();return(0,u.jsx)(x,{items:t.items,className:n})}function x(e){const{items:n,className:t}=e;if(!n)return(0,u.jsx)(y,{...e});const i=(0,r.d1)(n);return(0,u.jsx)("section",{className:(0,a.A)("row",t),children:i.map(((e,n)=>(0,u.jsx)("article",{className:"col col--6 margin-bottom--lg",children:(0,u.jsx)(g,{item:e})},n)))})}},53465:(e,n,t)=>{t.d(n,{W:()=>l});var a=t(96540),r=t(44586);const i=["zero","one","two","few","many","other"];function s(e){return i.filter((n=>e.includes(n)))}const o={locale:"en",pluralForms:s(["one","other"]),select:e=>1===e?"one":"other"};function c(){const{i18n:{currentLocale:e}}=(0,r.A)();return(0,a.useMemo)((()=>{try{return function(e){const n=new Intl.PluralRules(e);return{locale:e,pluralForms:s(n.resolvedOptions().pluralCategories),select:e=>n.select(e)}}(e)}catch(n){return console.error(`Failed to use Intl.PluralRules for locale "${e}".\nDocusaurus will fallback to the default (English) implementation.\nError: ${n.message}\n`),o}}),[e])}function l(){const e=c();return{selectMessage:(n,t)=>function(e,n,t){const a=e.split("|");if(1===a.length)return a[0];a.length>t.pluralForms.length&&console.error(`For locale=${t.locale}, a maximum of ${t.pluralForms.length} plural forms are expected (${t.pluralForms.join(",")}), but the message contains ${a.length}: ${e}`);const r=t.select(n),i=t.pluralForms.indexOf(r);return a[Math.min(i,a.length-1)]}(t,n,e)}}},28453:(e,n,t)=>{t.d(n,{R:()=>s,x:()=>o});var a=t(96540);const r={},i=a.createContext(r);function s(e){const n=a.useContext(i);return a.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function o(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(r):e.components||r:s(e.components),a.createElement(i.Provider,{value:n},e.children)}}}]);