"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[5818],{3905:(e,t,n)=>{n.d(t,{Zo:()=>p,kt:()=>b});var a=n(67294);function r(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function o(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var a=Object.getOwnPropertySymbols(e);t&&(a=a.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,a)}return n}function i(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?o(Object(n),!0).forEach((function(t){r(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):o(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function s(e,t){if(null==e)return{};var n,a,r=function(e,t){if(null==e)return{};var n,a,r={},o=Object.keys(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||(r[n]=e[n]);return r}(e,t);if(Object.getOwnPropertySymbols){var o=Object.getOwnPropertySymbols(e);for(a=0;a<o.length;a++)n=o[a],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(r[n]=e[n])}return r}var c=a.createContext({}),l=function(e){var t=a.useContext(c),n=t;return e&&(n="function"==typeof e?e(t):i(i({},t),e)),n},p=function(e){var t=l(e.components);return a.createElement(c.Provider,{value:t},e.children)},d="mdxType",u={inlineCode:"code",wrapper:function(e){var t=e.children;return a.createElement(a.Fragment,{},t)}},m=a.forwardRef((function(e,t){var n=e.components,r=e.mdxType,o=e.originalType,c=e.parentName,p=s(e,["components","mdxType","originalType","parentName"]),d=l(n),m=r,b=d["".concat(c,".").concat(m)]||d[m]||u[m]||o;return n?a.createElement(b,i(i({ref:t},p),{},{components:n})):a.createElement(b,i({ref:t},p))}));function b(e,t){var n=arguments,r=t&&t.mdxType;if("string"==typeof e||r){var o=n.length,i=new Array(o);i[0]=m;var s={};for(var c in t)hasOwnProperty.call(t,c)&&(s[c]=t[c]);s.originalType=e,s[d]="string"==typeof e?e:r,i[1]=s;for(var l=2;l<o;l++)i[l]=n[l];return a.createElement.apply(null,i)}return a.createElement.apply(null,n)}m.displayName="MDXCreateElement"},13508:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>c,contentTitle:()=>i,default:()=>u,frontMatter:()=>o,metadata:()=>s,toc:()=>l});var a=n(87462),r=(n(67294),n(3905));const o={sidebar_position:2},i="Submit a transaction",s={unversionedId:"how-to/submit-transaction",id:"how-to/submit-transaction",title:"Submit a transaction",description:"This section describes how to submit a transaction to an already open head using the NewTx command of the WebSocket API.",source:"@site/docs/how-to/submit-transaction.md",sourceDirName:"how-to",slug:"/how-to/submit-transaction",permalink:"/head-protocol/docs/how-to/submit-transaction",draft:!1,editUrl:"https://github.com/cardano-scaling/hydra/tree/master/docs/docs/how-to/submit-transaction.md",tags:[],version:"current",sidebarPosition:2,frontMatter:{sidebar_position:2},sidebar:"userDocumentation",previous:{title:"Commit using a blueprint",permalink:"/head-protocol/docs/how-to/commit-blueprint"},next:{title:"Decommit funds",permalink:"/head-protocol/docs/how-to/incremental-decommit"}},c={},l=[],p={toc:l},d="wrapper";function u(e){let{components:t,...n}=e;return(0,r.kt)(d,(0,a.Z)({},p,n,{components:t,mdxType:"MDXLayout"}),(0,r.kt)("h1",{id:"submit-a-transaction"},"Submit a transaction"),(0,r.kt)("p",null,"This section describes how to submit a transaction to an already open head using the ",(0,r.kt)("inlineCode",{parentName:"p"},"NewTx")," command of the WebSocket API."),(0,r.kt)("p",null,"First, query the UTXO available in the head:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre"},"curl localhost:4001/snapshot/utxo | jq\n")),(0,r.kt)("p",null,"Below is an example response:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-json",metastring:'title="Example response of GET /snapshot/utxo"',title:'"Example',response:!0,of:!0,GET:!0,'/snapshot/utxo"':!0},'{\n  "8690d7618bb88825d6ec7cfbe2676779b8f4633cb137a1c12cd31b4c53f90f32#0": {\n    "address": "addr_test1vrdhewmpp96gv6az4vymy80hlw9082sjz6rylt2srpntsdq6njxxu",\n    "datum": null,\n    "datumhash": null,\n    "inlineDatum": null,\n    "referenceScript": null,\n    "value": {\n      "lovelace": 100000000\n    }\n  }\n}\n')),(0,r.kt)("p",null,"Assuming the single UTXO is owned by ",(0,r.kt)("inlineCode",{parentName:"p"},"some-payment-key.sk")," and you want to send all of it to another address, you can use ",(0,r.kt)("inlineCode",{parentName:"p"},"cardano-cli")," (or your preferred transaction builder) to construct and sign a transaction:"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-shell",metastring:'title="Transaction building"',title:'"Transaction','building"':!0},"cardano-cli transaction build-raw \\\n  --babbage-era \\\n  --tx-in 8690d7618bb88825d6ec7cfbe2676779b8f4633cb137a1c12cd31b4c53f90f32#0 \\\n  --tx-out addr_test1vp5cxztpc6hep9ds7fjgmle3l225tk8ske3rmwr9adu0m6qchmx5z+100000000 \\\n  --fee 0 \\\n  --out-file tx.json\n\ncardano-cli transaction sign \\\n  --tx-body-file tx.json \\\n  --signing-key-file some-payment-key.sk \\\n  --out-file tx-signed.json\n\ncat tx-signed.json | jq -c '{tag: \"NewTx\", transaction: .}'\n")),(0,r.kt)("p",null,"This command generates a message suitable for submission to the ",(0,r.kt)("inlineCode",{parentName:"p"},"hydra-node")," via a WebSocket connection. If ",(0,r.kt)("inlineCode",{parentName:"p"},"hydra-node")," operates on the default port ",(0,r.kt)("inlineCode",{parentName:"p"},"4001"),", the message can be submitted using ",(0,r.kt)("inlineCode",{parentName:"p"},"websocat"),":"),(0,r.kt)("pre",null,(0,r.kt)("code",{parentName:"pre",className:"language-shell"},'cat tx-signed.json | jq -c \'{tag: "NewTx", transaction: .}\' | websocat "ws://127.0.0.1:4001?history=no"\n')),(0,r.kt)("p",null,"The transaction will be validated by all connected ",(0,r.kt)("inlineCode",{parentName:"p"},"hydra-node")," instances. It will result in either a ",(0,r.kt)("inlineCode",{parentName:"p"},"TxInvalid")," message, providing a reason for rejection, or a ",(0,r.kt)("inlineCode",{parentName:"p"},"TxValid")," message followed by a ",(0,r.kt)("inlineCode",{parentName:"p"},"SnapshotConfirmed"),", updating the UTXO available in the head shortly after that."))}u.isMDXComponent=!0}}]);