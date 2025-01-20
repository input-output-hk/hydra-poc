"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[9182],{3905:(e,n,t)=>{t.d(n,{Zo:()=>c,kt:()=>m});var a=t(67294);function r(e,n,t){return n in e?Object.defineProperty(e,n,{value:t,enumerable:!0,configurable:!0,writable:!0}):e[n]=t,e}function i(e,n){var t=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);n&&(a=a.filter((function(n){return Object.getOwnPropertyDescriptor(e,n).enumerable}))),t.push.apply(t,a)}return t}function o(e){for(var n=1;n<arguments.length;n++){var t=null!=arguments[n]?arguments[n]:{};n%2?i(Object(t),!0).forEach((function(n){r(e,n,t[n])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(t)):i(Object(t)).forEach((function(n){Object.defineProperty(e,n,Object.getOwnPropertyDescriptor(t,n))}))}return e}function s(e,n){if(null==e)return{};var t,a,r=function(e,n){if(null==e)return{};var t,a,r={},i=Object.keys(e);for(a=0;a<i.length;a++)t=i[a],n.indexOf(t)>=0||(r[t]=e[t]);return r}(e,n);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(a=0;a<i.length;a++)t=i[a],n.indexOf(t)>=0||Object.prototype.propertyIsEnumerable.call(e,t)&&(r[t]=e[t])}return r}var d=a.createContext({}),l=function(e){var n=a.useContext(d),t=n;return e&&(t="function"==typeof e?e(n):o(o({},n),e)),t},c=function(e){var n=l(e.components);return a.createElement(d.Provider,{value:n},e.children)},p="mdxType",u={inlineCode:"code",wrapper:function(e){var n=e.children;return a.createElement(a.Fragment,{},n)}},h=a.forwardRef((function(e,n){var t=e.components,r=e.mdxType,i=e.originalType,d=e.parentName,c=s(e,["components","mdxType","originalType","parentName"]),p=l(t),h=r,m=p["".concat(d,".").concat(h)]||p[h]||u[h]||i;return t?a.createElement(m,o(o({ref:n},c),{},{components:t})):a.createElement(m,o({ref:n},c))}));function m(e,n){var t=arguments,r=n&&n.mdxType;if("string"==typeof e||r){var i=t.length,o=new Array(i);o[0]=h;var s={};for(var d in n)hasOwnProperty.call(n,d)&&(s[d]=n[d]);s.originalType=e,s[p]="string"==typeof e?e:r,o[1]=s;for(var l=2;l<i;l++)o[l]=t[l];return a.createElement.apply(null,o)}return a.createElement.apply(null,t)}h.displayName="MDXCreateElement"},66753:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>d,contentTitle:()=>o,default:()=>u,frontMatter:()=>i,metadata:()=>s,toc:()=>l});var a=t(87462),r=(t(67294),t(3905));const i={sidebar_position:5},o="Extend the node with event source and sinks",s={unversionedId:"how-to/event-sinks-and-sources",id:"how-to/event-sinks-and-sources",title:"Extend the node with event source and sinks",description:"There are certain use cases in which multiple features of the Hydra platform are beneficial. However, interfacing with the entire hydra node, particularly with regard to IO, may be impractical. For a use case that requires different persistence requirements than the default Hydra setup, it may initially appear that there are two options available:",source:"@site/docs/how-to/event-sinks-and-sources.md",sourceDirName:"how-to",slug:"/how-to/event-sinks-and-sources",permalink:"/head-protocol/docs/how-to/event-sinks-and-sources",draft:!1,editUrl:"https://github.com/cardano-scaling/hydra/tree/master/docs/docs/how-to/event-sinks-and-sources.md",tags:[],version:"current",sidebarPosition:5,frontMatter:{sidebar_position:5},sidebar:"userDocumentation",previous:{title:"Operate a Hydra node",permalink:"/head-protocol/docs/how-to/operating-hydra"},next:{title:"FAQs",permalink:"/head-protocol/docs/faqs"}},d={},l=[{value:"Deciding between forking and using event sinks",id:"deciding-between-forking-and-using-event-sinks",level:2},{value:"Implementation of event sinks and source",id:"implementation-of-event-sinks-and-source",level:2},{value:"Examples",id:"examples",level:3},{value:"Building event sinks and sources",id:"building-event-sinks-and-sources",level:3},{value:"Offline mode",id:"offline-mode",level:2}],c={toc:l},p="wrapper";function u(e){let{components:n,...t}=e;return(0,r.kt)(p,(0,a.Z)({},c,t,{components:n,mdxType:"MDXLayout"}),(0,r.kt)("h1",{id:"extend-the-node-with-event-source-and-sinks"},"Extend the node with event source and sinks"),(0,r.kt)("p",null,"There are certain use cases in which multiple features of the Hydra platform are beneficial. However, interfacing with the entire ",(0,r.kt)("inlineCode",{parentName:"p"},"hydra node"),", particularly with regard to IO, may be impractical. For a use case that requires different persistence requirements than the default Hydra setup, it may initially appear that there are two options available:"),(0,r.kt)("h2",{id:"deciding-between-forking-and-using-event-sinks"},"Deciding between forking and using event sinks"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},(0,r.kt)("strong",{parentName:"p"},"Forking the Hydra codebase"),". Customizing the Hydra codebase can provide precise control over node behavior but introduces a significant maintenance burden due to the instability and lack of internal code documentation.")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},(0,r.kt)("strong",{parentName:"p"},"Using event sinks and source"),". Running a full ",(0,r.kt)("inlineCode",{parentName:"p"},"hydra node")," requires preparing a persistence file beforehand or parsing the file as it is written. However, this approach has downsides, including reduced control, code duplication, increased resource usage, and reliance on interfacing with an unstable external API (the persistence file on disk)."))),(0,r.kt)("h2",{id:"implementation-of-event-sinks-and-source"},"Implementation of event sinks and source"),(0,r.kt)("p",null,"Hydra introduces alternate event sinks and a single alternate event source to enhance these use cases. Event sinks permanently store new transactions processed at runtime, while the event source provides the initial transactions that a ",(0,r.kt)("inlineCode",{parentName:"p"},"hydra node")," loads upon startup."),(0,r.kt)("p",null,"Multiple event sinks can be utilized simultaneously, whereas only one event source can be used at a time. The event source is loaded only during startup. Each event sink runs for every new transaction. Currently, the order of the event sinks must be specified by customizing the order of the event sink list in the ",(0,r.kt)("inlineCode",{parentName:"p"},"hydra node")," source code, in the ",(0,r.kt)("inlineCode",{parentName:"p"},"eventSinks")," parameter to hydrate, invoked in ",(0,r.kt)("inlineCode",{parentName:"p"},"Hydra.Node.Run.run")," ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/SundaeSwap-finance/hydra/blob/4785bd86a03b92ba8fa8fb34c9d485a1e2f4f7d7/hydra-node/src/Hydra/Node/Run.hs#L104"},"here"),"."),(0,r.kt)("p",null,"The default Hydra file-based persistence is implemented as an event sink and source pair. They can be used independently. For example, you can use the default event source to process previous transactions from a file on disk, along with an event sink that could store new transactions on S3, on several machines, or not at all."),(0,r.kt)("h3",{id:"examples"},"Examples"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},(0,r.kt)("strong",{parentName:"p"},"Simple basic implementation"),". For a basic implementation that sends new transactions over UDP, please refer to ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/ffakenz/hydra/tree/udp-sink"},"this fork"),".")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},(0,r.kt)("strong",{parentName:"p"},"Complex implementation"),". For more advanced implementations using S3 and AWS Kinesis as event sources and sinks, visit the ",(0,r.kt)("inlineCode",{parentName:"p"},"doug_hydra_changes")," branch ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/SundaeSwap-finance/hydra"},"here"),", particularly the ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/SundaeSwap-finance/hydra/blob/f27e51c001e7b64c3679eab4efd9f17f08db53fe/hydra-node/src/Hydra/Events/AWS/Kinesis.hs"},"AWS Kinesis implementation"),"."))),(0,r.kt)("p",null,"Currently, there is no CLI API to toggle which sources and sinks are utilized; this configuration must be manually implemented by the developers of the sources and sinks. Refer to the source and sink configuration example ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/SundaeSwap-finance/hydra/blob/4785bd86a03b92ba8fa8fb34c9d485a1e2f4f7d7/hydra-node/src/Hydra/Node/Run.hs#L97"},"here"),", where the event sinks and source are toggled through CLI options."),(0,r.kt)("h3",{id:"building-event-sinks-and-sources"},"Building event sinks and sources"),(0,r.kt)("ul",null,(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},(0,r.kt)("strong",{parentName:"p"},"Event sink construction"),". Create an ",(0,r.kt)("inlineCode",{parentName:"p"},"EventSink e m")," object, where ",(0,r.kt)("inlineCode",{parentName:"p"},"m")," denotes the monad (such as IO) in which the event sink operates, and ",(0,r.kt)("inlineCode",{parentName:"p"},"e")," represents the event type. The only field in the ",(0,r.kt)("inlineCode",{parentName:"p"},"EventSink")," record corresponds to the monadic action taken upon a new event. See an example with AWS Kinesis ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/SundaeSwap-finance/hydra/blob/598b20fcee9669a196781f70e02e13779967e470/hydra-node/src/Hydra/Events/AWS/Kinesis.hs#L85"},"here"),".")),(0,r.kt)("li",{parentName:"ul"},(0,r.kt)("p",{parentName:"li"},(0,r.kt)("strong",{parentName:"p"},"Event source construction"),". Construct an ",(0,r.kt)("inlineCode",{parentName:"p"},"EventSource e m")," object, where the encapsulated monadic action produces a list of events ",(0,r.kt)("inlineCode",{parentName:"p"},"[e]"),". An example loading events from AWS Kinesis is available ",(0,r.kt)("a",{parentName:"p",href:"https://github.com/SundaeSwap-finance/hydra/blob/598b20fcee9669a196781f70e02e13779967e470/hydra-node/src/Hydra/Events/AWS/Kinesis.hs#L85"},"here"),". Consider implementing delays to manage the rate of event list construction, as the entire list is replayed at node startup, potentially overwhelming any API if not adequately throttled."))),(0,r.kt)("h2",{id:"offline-mode"},"Offline mode"),(0,r.kt)("p",null,"Hydra also supports an offline mode, which allows for disabling the layer 1 interface (that is, the underlying Cardano blockchain, which Hydra heads use to seed and withdraw funds to). This offline mode makes operation only influenced by the HTTP/WebSocket APIs, and the configured event sinks and sources. While separate from the event sinks and event source functionality, disabling layer 1 interactions allows for further customization, enabling use cases that would otherwise require running and configuring an entire layer 1 private devnet. See offline mode documentation ",(0,r.kt)("a",{parentName:"p",href:"/head-protocol/docs/configuration#offline-mode"},"here"),"."))}u.isMDXComponent=!0}}]);