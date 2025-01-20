"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[1473],{3905:(e,t,n)=>{n.d(t,{Zo:()=>d,kt:()=>h});var a=n(67294);function o(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function r(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function l(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?r(Object(n),!0).forEach((function(t){o(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):r(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function i(e,t){if(null==e)return{};var n,a,o=function(e,t){if(null==e)return{};var n,a,o={},r=Object.keys(e);for(a=0;a<r.length;a++)n=r[a],t.indexOf(n)>=0||(o[n]=e[n]);return o}(e,t);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);for(a=0;a<r.length;a++)n=r[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(o[n]=e[n])}return o}var s=a.createContext({}),c=function(e){var t=a.useContext(s),n=t;return e&&(n="function"==typeof e?e(t):l(l({},t),e)),n},d=function(e){var t=c(e.components);return a.createElement(s.Provider,{value:t},e.children)},m="mdxType",u={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},p=a.forwardRef((function(e,t){var n=e.components,o=e.mdxType,r=e.originalType,s=e.parentName,d=i(e,["components","mdxType","originalType","parentName"]),m=c(n),p=o,h=m["".concat(s,".").concat(p)]||m[p]||u[p]||r;return n?a.createElement(h,l(l({ref:t},d),{},{components:n})):a.createElement(h,l({ref:t},d))}));function h(e,t){var n=arguments,o=t&&t.mdxType;if("string"==typeof e||o){var r=n.length,l=new Array(r);l[0]=p;var i={};for(var s in t)hasOwnProperty.call(t,s)&&(i[s]=t[s]);i.originalType=e,i[m]="string"==typeof e?e:o,l[1]=i;for(var c=2;c<r;c++)l[c]=n[c];return a.createElement.apply(null,l)}return a.createElement.apply(null,n)}p.displayName="MDXCreateElement"},93316:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>s,contentTitle:()=>l,default:()=>u,frontMatter:()=>r,metadata:()=>i,toc:()=>c});var a=n(87462),o=(n(67294),n(3905));const r={sidebar_position:3},l="Decommit funds",i={unversionedId:"how-to/incremental-decommit",id:"how-to/incremental-decommit",title:"Decommit funds",description:"To take out some UTxO present in an open head and send it back to the layer one, you need to do a so-called decommit.",source:"@site/docs/how-to/incremental-decommit.md",sourceDirName:"how-to",slug:"/how-to/incremental-decommit",permalink:"/head-protocol/docs/how-to/incremental-decommit",draft:!1,editUrl:"https://github.com/cardano-scaling/hydra/tree/master/docs/docs/how-to/incremental-decommit.md",tags:[],version:"current",sidebarPosition:3,frontMatter:{sidebar_position:3},sidebar:"userDocumentation",previous:{title:"Submit a transaction",permalink:"/head-protocol/docs/how-to/submit-transaction"},next:{title:"Operate a Hydra node",permalink:"/head-protocol/docs/how-to/operating-hydra"}},s={},c=[],d={toc:c},m="wrapper";function u(e){let{components:t,...n}=e;return(0,o.kt)(m,(0,a.Z)({},d,n,{components:t,mdxType:"MDXLayout"}),(0,o.kt)("h1",{id:"decommit-funds"},"Decommit funds"),(0,o.kt)("p",null,"To take out some ",(0,o.kt)("inlineCode",{parentName:"p"},"UTxO")," present in an open head and send it back to the layer one, you need to do a so-called ",(0,o.kt)("inlineCode",{parentName:"p"},"decommit"),"."),(0,o.kt)("p",null,"This how-to assumes that we are in a similar situation as in the ",(0,o.kt)("a",{parentName:"p",href:"../getting-started"},"Getting Started")," or ",(0,o.kt)("a",{parentName:"p",href:"../tutorial"},"Testnet tutorial"),". Depending on who owns something in your head, you might need to update the instructions of this tutorial. In our example we decommit funds owned by Alice from their address:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-shell"},"export WALLET_SK=credentials/alice-funds.sk\nexport WALLET_ADDR=addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z\n")),(0,o.kt)("p",null,"First, we need to find out which ",(0,o.kt)("inlineCode",{parentName:"p"},"UTxO")," we can spend using our address:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-shell"},'curl localhost:4001/snapshot/utxo \\\n  | jq "with_entries(select(.value.address == \\"${WALLET_ADDR}\\"))" \\\n  > utxo.json\n')),(0,o.kt)("details",null,(0,o.kt)("summary",null," Example output"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-json"},'{\n  "f6b004be1cf95dbd3d0abc3daceac40ef6401e502972a919e5e52564b9f5740b#0": {\n    "address": "addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z",\n    "datum": null,\n    "datumhash": null,\n    "inlineDatum": null,\n    "referenceScript": null,\n    "value": {\n      "lovelace": 50000000\n    }\n  },\n  "f6b004be1cf95dbd3d0abc3daceac40ef6401e502972a919e5e52564b9f5740b#1": {\n    "address": "addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z",\n    "datum": null,\n    "datumhash": null,\n    "inlineDatum": null,\n    "referenceScript": null,\n    "value": {\n      "lovelace": 50000000\n    }\n  }\n}\n'))),(0,o.kt)("p",null,"Now, the ",(0,o.kt)("inlineCode",{parentName:"p"},"decommit")," command requires us to build a transaction that proves we can spend what we want to decommit. The outputs of this transaction will be the outputs that are also going to be made available on the main chain."),(0,o.kt)("p",null,"For example, to spend the first UTxO queried above in a transaction sending the same value to Alice's key (so she can spend it on the layer one later):"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-shell"},"LOVELACE=$(jq -r 'to_entries[0].value.value.lovelace' < utxo.json)\ncardano-cli transaction build-raw \\\n  --tx-in $(jq -r 'to_entries[0].key' < utxo.json) \\\n  --tx-out ${WALLET_ADDR}+${LOVELACE} \\\n  --fee 0 \\\n  --out-file decommit.json\n")),(0,o.kt)("p",null,"You can inspect the transaction with"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-shell"},"cardano-cli transaction view --tx-file decommit.json\n")),(0,o.kt)("p",null,"As the transaction spends from Alices funds in the Hydra head, we also need to\nsign it with her key:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-shell"},"cardano-cli transaction sign \\\n  --tx-file decommit.json \\\n  --signing-key-file ${WALLET_SK} \\\n  --out-file alice-decommit-tx-signed.json\n")),(0,o.kt)("p",null,"With the signed decommit transaction, now we can submit it to the ",(0,o.kt)("inlineCode",{parentName:"p"},"/decommit")," endpoint:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-shell"},"curl -X POST 127.0.0.1:4001/decommit \\\n  --data @alice-decommit-tx-signed.json\n")),(0,o.kt)("details",null,(0,o.kt)("summary",null,"Alternative using websocket"),(0,o.kt)("p",null,"We can also submit a ",(0,o.kt)("inlineCode",{parentName:"p"},"Decommit")," client input using a websocket:"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-shell"},'cat alice-decommit-tx-signed.json \\\n  | jq -c \'{tag: "Decommit", decommitTx: .}\' \\\n  | websocat "ws://127.0.0.1:4001?history=no"\n'))),(0,o.kt)("p",null,"If you haven't already, open a websocket session using ",(0,o.kt)("inlineCode",{parentName:"p"},"websocat ws://0.0.0.0:4001")," now."),(0,o.kt)("p",null,"In the message history you will see a ",(0,o.kt)("inlineCode",{parentName:"p"},"DecommitRequested")," message which\nindicates a decommit is requested."),(0,o.kt)("p",null,"After some time, a ",(0,o.kt)("inlineCode",{parentName:"p"},"DecommitFinalized")," can be observed which concludes the decommit process and after which the funds are available on the layer one."),(0,o.kt)("p",null,"To confirm, you can query the funds of the wallet on the layer one from a ",(0,o.kt)("inlineCode",{parentName:"p"},"cardano-node"),":"),(0,o.kt)("pre",null,(0,o.kt)("code",{parentName:"pre",className:"language-shell"},"cardano-cli query utxo \\\n  --address ${WALLET_ADDR} \\\n  --output-json | jq\n")))}u.isMDXComponent=!0}}]);