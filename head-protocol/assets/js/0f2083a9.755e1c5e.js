"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[4085],{3905:(e,t,i)=>{i.d(t,{Zo:()=>c,kt:()=>m});var a=i(67294);function n(e,t,i){return t in e?Object.defineProperty(e,t,{value:i,enumerable:!0,configurable:!0,writable:!0}):e[t]=i,e}function o(e,t){var i=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),i.push.apply(i,a)}return i}function r(e){for(var t=1;t<arguments.length;t++){var i=null!=arguments[t]?arguments[t]:{};t%2?o(Object(i),!0).forEach((function(t){n(e,t,i[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(i)):o(Object(i)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(i,t))}))}return e}function s(e,t){if(null==e)return{};var i,a,n=function(e,t){if(null==e)return{};var i,a,n={},o=Object.keys(e);for(a=0;a<o.length;a++)i=o[a],t.indexOf(i)>=0||(n[i]=e[i]);return n}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(a=0;a<o.length;a++)i=o[a],t.indexOf(i)>=0||Object.prototype.propertyIsEnumerable.call(e,i)&&(n[i]=e[i])}return n}var l=a.createContext({}),d=function(e){var t=a.useContext(l),i=t;return e&&(i="function"==typeof e?e(t):r(r({},t),e)),i},c=function(e){var t=d(e.components);return a.createElement(l.Provider,{value:t},e.children)},h="mdxType",u={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},p=a.forwardRef((function(e,t){var i=e.components,n=e.mdxType,o=e.originalType,l=e.parentName,c=s(e,["components","mdxType","originalType","parentName"]),h=d(i),p=n,m=h["".concat(l,".").concat(p)]||h[p]||u[p]||o;return i?a.createElement(m,r(r({ref:t},c),{},{components:i})):a.createElement(m,r({ref:t},c))}));function m(e,t){var i=arguments,n=t&&t.mdxType;if("string"==typeof e||n){var o=i.length,r=new Array(o);r[0]=p;var s={};for(var l in t)hasOwnProperty.call(t,l)&&(s[l]=t[l]);s.originalType=e,s[h]="string"==typeof e?e:n,r[1]=s;for(var d=2;d<o;d++)r[d]=i[d];return a.createElement.apply(null,r)}return a.createElement.apply(null,i)}p.displayName="MDXCreateElement"},59316:(e,t,i)=>{i.r(t),i.d(t,{assets:()=>l,contentTitle:()=>r,default:()=>u,frontMatter:()=>o,metadata:()=>s,toc:()=>d});var a=i(87462),n=(i(67294),i(3905));const o={sidebar_label:"Delegated voucher: invitational",sidebar_position:1},r="Delegated voucher auctions (invitational)",s={unversionedId:"auctions/delegated-voucher-invitational/index",id:"auctions/delegated-voucher-invitational/index",title:"Delegated voucher auctions (invitational)",description:"This is the first complete prototype of an auction that can host its bidding process using the Hydra head protocol (layer 2).",source:"@site/use-cases/auctions/delegated-voucher-invitational/index.md",sourceDirName:"auctions/delegated-voucher-invitational",slug:"/auctions/delegated-voucher-invitational/",permalink:"/head-protocol/use-cases/auctions/delegated-voucher-invitational/",draft:!1,editUrl:"https://github.com/cardano-scaling/hydra/tree/master/docs/use-cases/auctions/delegated-voucher-invitational/index.md",tags:[],version:"current",sidebarPosition:1,frontMatter:{sidebar_label:"Delegated voucher: invitational",sidebar_position:1},sidebar:"defaultSidebar",previous:{title:"Auctions",permalink:"/head-protocol/use-cases/auctions/"},next:{title:"Delegated voucher: open",permalink:"/head-protocol/use-cases/auctions/delegated-voucher-open/"}},l={},d=[{value:"Overview",id:"overview",level:2},{value:"Design",id:"design",level:2},{value:"Workflow",id:"workflow",level:2},{value:"Prerequisites from Hydra technical roadmap",id:"prerequisites-from-hydra-technical-roadmap",level:2},{value:"Remaining limitations",id:"remaining-limitations",level:2}],c={toc:d},h="wrapper";function u(e){let{components:t,...o}=e;return(0,n.kt)(h,(0,a.Z)({},c,o,{components:t,mdxType:"MDXLayout"}),(0,n.kt)("h1",{id:"delegated-voucher-auctions-invitational"},"Delegated voucher auctions (invitational)"),(0,n.kt)("p",null,"This is the first complete prototype of an auction that can host its bidding process using the Hydra head protocol (layer 2)."),(0,n.kt)("admonition",{title:"This use case is under development",type:"tip"},(0,n.kt)("p",{parentName:"admonition"},"This project is an ongoing effort in collaboration with MLabs. Source code and documentation are available ",(0,n.kt)("a",{parentName:"p",href:"https://github.com/mlabs-haskell/hydra-auction"},"here"),".")),(0,n.kt)("h2",{id:"overview"},"Overview"),(0,n.kt)("p",null,"This document outlines the first building block of what could, with the support of the Cardano community, develop into a comprehensive Hydra-based auction framework. Among the various design candidates explored in our ",(0,n.kt)("a",{parentName:"p",href:"https://iohk.io/en/blog/posts/2023/01/20/implementing-auction-projects-using-hydra/"},"January 2023 paper"),", the delegated voucher auction optimally utilizes the strengths of both the Cardano mainnet (layer 1) and the Hydra Head protocol (layer 2) while addressing their respective limitations."),(0,n.kt)("p",null,(0,n.kt)("img",{src:i(77618).Z,width:"3600",height:"2577"})),(0,n.kt)("p",null,"Cardano secures its mainnet ledger by broadcasting transactions across a globally distributed network of independent nodes. A slot leader is randomly selected among these nodes to add the next block to the chain, making it extremely challenging to add an invalid transaction, censor a new transaction, or contradict the existence of a stable transaction within the ledger. The Ouroboros consensus protocol ensures robustness to nodes entering and leaving the network, as long as an honest majority of stake is maintained. However, the mainnet has high overhead in terms of cost and latency, as every transaction and block must be broadcast, stored, and validated on every node worldwide."),(0,n.kt)("p",null,"The Hydra Head protocol significantly reduces this cost and latency overhead by distributing its ledger across a smaller network of direct participants and only tracking transactions that interact with a subset of the mainnet UTXOs. However, it relies on the direct participants to maintain the ledger's integrity, and all participants must unanimously agree on each transaction added to the ledger. This requires anyone committing funds to a Hydra head to either participate directly in the protocol or effectively grant custody over the committed funds to the direct participants. Additionally, no action can be taken in the Hydra head without unanimous consent from the direct participants. Moreover, all participants must be online and responsive to each other for the Hydra Head protocol to make progress; otherwise, it will stall."),(0,n.kt)("h2",{id:"design"},"Design"),(0,n.kt)("p",null,"To create a lively and efficient bidding experience, we host the bidding process of our auction on layer 2 instead of layer 1. Since we can't have bidders directly participate in the Hydra head, we designate an independent group of delegates to represent them. These delegates play a role similar to stake pool and light wallet providers on Cardano layer 1. "),(0,n.kt)("p",null,"To avoid granting custody of the bidder\u2019s funds or the seller's NFT auction lot to the delegates, we use the Hydra head purely to manage the informational aspect of the bidding process. The auction lot and the bidders' funds remain on layer 1 at all times. As the bidders' funds are not on the Hydra head ledger, we allow the seller to require fixed security deposits into a smart contract on layer 1 to ensure that winning bidders honor their bids. "),(0,n.kt)("p",null,"However, fixed-size security deposits don't completely protect the seller from dishonorable behavior by bidders, especially if bids unexpectedly rise. This could lead a bidder to dishonor a genuine bid or place a disingenuous bid to sabotage the auction. To mitigate this risk and manage the auction effectively, the seller needs to be able to apply know-your-customer (KYC) and anti-money-laundering (AML) processes to the auction. "),(0,n.kt)("p",null,"For these reasons, we plan to implement an invitational private version of the auction initially, where the seller has absolute discretion over which bidders may participate. The public version of the auction, where bidders can freely participate, will be implemented in a later milestone."),(0,n.kt)("h2",{id:"workflow"},"Workflow"),(0,n.kt)("p",null,"In this prototype, each auction proceeds as follows:"),(0,n.kt)("ol",null,(0,n.kt)("li",{parentName:"ol"},"Delegates set up a Hydra head to host the bidding for an upcoming auction."),(0,n.kt)("li",{parentName:"ol"},"The seller announces the auction, defines the terms (including the security deposit), places the NFT into the auction smart contract, and specifies which Hydra head will host the bidding."),(0,n.kt)("li",{parentName:"ol"},"Potential bidders register by placing security deposits into the auction smart contract."),(0,n.kt)("li",{parentName:"ol"},"As the auction's start time approaches, the bidding process transitions from layer 1 to layer 2."),(0,n.kt)("li",{parentName:"ol"},"The seller invites bidders and commences the auction. The seller has discretion based on security deposits and KYC/AML considerations."),(0,n.kt)("li",{parentName:"ol"},"Bidders submit bids to delegates, who collectively record the bids in the Hydra head ledger and confirm bids to bidders through multi-signed ledger snapshots."),(0,n.kt)("li",{parentName:"ol"},"When the bidding phase ends, the Hydra head closes, and the highest bid goes to layer 1. The winning bidder then purchases the auction lot at the highest bid price."),(0,n.kt)("li",{parentName:"ol"},"The winning bidder has time until the voucher expiration to purchase the auction lot for the highest bid price."),(0,n.kt)("li",{parentName:"ol"},"If the winning bidder does not purchase the auction lot after the voucher expiration, the seller can reclaim it and assert the winning bidder\u2019s security deposit for the auction. Otherwise, the winning bidder can assert their security deposit."),(0,n.kt)("li",{parentName:"ol"},"All other bidders can assert their security deposits at the end of the bidding phase.")),(0,n.kt)("h2",{id:"prerequisites-from-hydra-technical-roadmap"},"Prerequisites from Hydra technical roadmap"),(0,n.kt)("p",null,"During the design and early implementation phase, the following prerequisites were identified to enable using the auction smart contracts on layer 2:"),(0,n.kt)("ul",null,(0,n.kt)("li",{parentName:"ul"},"Add a method to the Hydra node API to commit UTXOs from script addresses."),(0,n.kt)("li",{parentName:"ul"},"Support committing multiple UTXOs per Hydra head participant, so that collateral UTXOs can be committed to the Hydra head for transactions with validator scripts on layer 2."),(0,n.kt)("li",{parentName:"ul"},"Allow time to pass on the Layer 2 ledger (instead of maintaining time fixed at the start time of the Hydra head).")),(0,n.kt)("p",null,"Fortunately, the Hydra core developers implemented all of these features during the course of the milestone. Thank you! \ud83d\ude80"),(0,n.kt)("h2",{id:"remaining-limitations"},"Remaining limitations"),(0,n.kt)("p",null,"Please note the following limitations of the initial prototype auction design:"),(0,n.kt)("ul",null,(0,n.kt)("li",{parentName:"ul"},"The auction implementation is a prototype and may not be ready for production."),(0,n.kt)("li",{parentName:"ul"},"Open auctions, where bidders can freely enter any auction to bid, are not supported."),(0,n.kt)("li",{parentName:"ul"},"Bids are only backed by fixed security deposits from bidders, which may be less than the full bid amount."),(0,n.kt)("li",{parentName:"ul"},"A new Hydra head must be opened for every auction."),(0,n.kt)("li",{parentName:"ul"},"Delegates have the ability to censor bidders from submitting bids to the auction."),(0,n.kt)("li",{parentName:"ul"},"Delegates are responsible for ensuring that the standing bid of the auction can only be replaced by a new bid that exceeds it by the minimum increment defined in the auction terms. However, bidders receive multi-signed proof for every confirmed bid, which can serve as evidence in case of disputes. This could allow for off-chain arbitration mechanisms to resolve disputes and/or an additional smart contract module where delegates could provide deposits that would be slashable if such evidence is provided.")))}u.isMDXComponent=!0},77618:(e,t,i)=>{i.d(t,{Z:()=>a});const a=i.p+"assets/images/running-auctions-on-cardano-d8aa54c6b6919ec90304d5d31899260c.png"}}]);