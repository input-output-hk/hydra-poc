"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[5735],{3905:(e,t,a)=>{a.d(t,{Zo:()=>d,kt:()=>y});var r=a(67294);function n(e,t,a){return t in e?Object.defineProperty(e,t,{value:a,enumerable:!0,configurable:!0,writable:!0}):e[t]=a,e}function i(e,t){var a=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),a.push.apply(a,r)}return a}function o(e){for(var t=1;t<arguments.length;t++){var a=null!=arguments[t]?arguments[t]:{};t%2?i(Object(a),!0).forEach((function(t){n(e,t,a[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(a)):i(Object(a)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(a,t))}))}return e}function s(e,t){if(null==e)return{};var a,r,n=function(e,t){if(null==e)return{};var a,r,n={},i=Object.keys(e);for(r=0;r<i.length;r++)a=i[r],t.indexOf(a)>=0||(n[a]=e[a]);return n}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(r=0;r<i.length;r++)a=i[r],t.indexOf(a)>=0||Object.prototype.propertyIsEnumerable.call(e,a)&&(n[a]=e[a])}return n}var l=r.createContext({}),c=function(e){var t=r.useContext(l),a=t;return e&&(a="function"==typeof e?e(t):o(o({},t),e)),a},d=function(e){var t=c(e.components);return r.createElement(l.Provider,{value:t},e.children)},u="mdxType",p={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},m=r.forwardRef((function(e,t){var a=e.components,n=e.mdxType,i=e.originalType,l=e.parentName,d=s(e,["components","mdxType","originalType","parentName"]),u=c(a),m=n,y=u["".concat(l,".").concat(m)]||u[m]||p[m]||i;return a?r.createElement(y,o(o({ref:t},d),{},{components:a})):r.createElement(y,o({ref:t},d))}));function y(e,t){var a=arguments,n=t&&t.mdxType;if("string"==typeof e||n){var i=a.length,o=new Array(i);o[0]=m;var s={};for(var l in t)hasOwnProperty.call(t,l)&&(s[l]=t[l]);s.originalType=e,s[u]="string"==typeof e?e:n,o[1]=s;for(var c=2;c<i;c++)o[c]=a[c];return r.createElement.apply(null,o)}return r.createElement.apply(null,a)}m.displayName="MDXCreateElement"},79586:(e,t,a)=>{a.r(t),a.d(t,{assets:()=>l,contentTitle:()=>o,default:()=>p,frontMatter:()=>i,metadata:()=>s,toc:()=>c});var r=a(87462),n=(a(67294),a(3905));const i={sidebar_label:"Auctions-as-a-service: multi",sidebar_position:5},o="Always-on delegated auction service (multi-head)",s={unversionedId:"auctions/always-on-service-multi/index",id:"auctions/always-on-service-multi/index",title:"Always-on delegated auction service (multi-head)",description:"The always-on delegated auction service is a persistent service for DApps, providing auctions-as-a-service across multiple Hydra heads to facilitate delegated auctions.",source:"@site/use-cases/auctions/always-on-service-multi/index.md",sourceDirName:"auctions/always-on-service-multi",slug:"/auctions/always-on-service-multi/",permalink:"/head-protocol/use-cases/auctions/always-on-service-multi/",draft:!1,editUrl:"https://github.com/cardano-scaling/hydra/tree/master/docs/use-cases/auctions/always-on-service-multi/index.md",tags:[],version:"current",sidebarPosition:5,frontMatter:{sidebar_label:"Auctions-as-a-service: multi",sidebar_position:5},sidebar:"defaultSidebar",previous:{title:"Auctions-as-a-service: single",permalink:"/head-protocol/use-cases/auctions/always-on-service-single/"},next:{title:"Payments",permalink:"/head-protocol/use-cases/payments/"}},l={},c=[{value:"Overview",id:"overview",level:2},{value:"Features and scope",id:"features-and-scope",level:2},{value:"Remaining limitations",id:"remaining-limitations",level:2},{value:"Prerequisites from Hydra technical roadmap",id:"prerequisites-from-hydra-technical-roadmap",level:2}],d={toc:c},u="wrapper";function p(e){let{components:t,...a}=e;return(0,n.kt)(u,(0,r.Z)({},d,a,{components:t,mdxType:"MDXLayout"}),(0,n.kt)("h1",{id:"always-on-delegated-auction-service-multi-head"},"Always-on delegated auction service (multi-head)"),(0,n.kt)("p",null,"The always-on delegated auction service is a persistent service for DApps, providing auctions-as-a-service across multiple Hydra heads to facilitate delegated auctions."),(0,n.kt)("h2",{id:"overview"},"Overview"),(0,n.kt)("p",null,"The always-on single-head service is suitable for a wide range of DApps, but some use cases demand an increased level of decentralization beyond the group of delegates in a single Hydra head. By allowing DApps to use multiple Hydra heads for their layer 2 processes, we can distribute the influence of any one head\u2019s delegates. This approach also enhances the robustness of the layer 2 bidding process \u2014 if one Hydra head goes down, bidders can continue on the other Hydra heads."),(0,n.kt)("h2",{id:"features-and-scope"},"Features and scope"),(0,n.kt)("p",null,"To be determined."),(0,n.kt)("h2",{id:"remaining-limitations"},"Remaining limitations"),(0,n.kt)("ol",null,(0,n.kt)("li",{parentName:"ol"},"Delegates can censor bidders from submitting bids to the auction. However, if bidders are censored by the delegates of one Hydra head, they can participate in another Hydra head to submit their bids."),(0,n.kt)("li",{parentName:"ol"},"Delegates on layer 2 are responsible for ensuring that the standing bid of the auction can only be replaced by a new bid that exceeds it by the minimum increment defined in the auction terms. However, bidders receive a multi-signed proof for every confirmed bid, which can be used as incontrovertible proof against delegates if there\u2019s any foul play.")),(0,n.kt)("h2",{id:"prerequisites-from-hydra-technical-roadmap"},"Prerequisites from Hydra technical roadmap"),(0,n.kt)("p",null,"To be determined."))}p.isMDXComponent=!0}}]);