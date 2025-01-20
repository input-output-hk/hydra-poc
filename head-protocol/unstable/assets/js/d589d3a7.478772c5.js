"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[7924],{56475:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>a,contentTitle:()=>d,default:()=>h,frontMatter:()=>i,metadata:()=>o,toc:()=>c});const o=JSON.parse('{"id":"getting-started","title":"Getting started","description":"To get started quickly, we\'ll walk you through the standard demo setup, which includes:","source":"@site/docs/getting-started.md","sourceDirName":".","slug":"/getting-started","permalink":"/head-protocol/unstable/docs/getting-started","draft":false,"unlisted":false,"editUrl":"https://github.com/cardano-scaling/hydra/tree/master/docs/docs/getting-started.md","tags":[],"version":"current","frontMatter":{},"sidebar":"userDocumentation","previous":{"title":"Known issues and limitations","permalink":"/head-protocol/unstable/docs/known-issues"},"next":{"title":"... without Docker","permalink":"/head-protocol/unstable/docs/getting-started-without-docker"}}');var r=n(74848),s=n(28453);const i={},d="Getting started",a={},c=[{value:"Preparation",id:"preparation",level:2},{value:"Set up the devnet",id:"set-up-the-devnet",level:2},{value:"Start Hydra nodes",id:"start-hydra-nodes",level:2},{value:"Use the head",id:"use-the-head",level:2}];function l(e){const t={a:"a",admonition:"admonition",code:"code",h1:"h1",h2:"h2",header:"header",img:"img",li:"li",p:"p",pre:"pre",strong:"strong",ul:"ul",...(0,s.R)(),...e.components},{Details:o}=t;return o||function(e,t){throw new Error("Expected "+(t?"component":"object")+" `"+e+"` to be defined: you likely forgot to import, pass, or provide it.")}("Details",!0),(0,r.jsxs)(r.Fragment,{children:[(0,r.jsx)(t.header,{children:(0,r.jsx)(t.h1,{id:"getting-started",children:"Getting started"})}),"\n",(0,r.jsx)(t.p,{children:"To get started quickly, we'll walk you through the standard demo setup, which includes:"}),"\n",(0,r.jsxs)(t.ul,{children:["\n",(0,r.jsxs)(t.li,{children:["A cluster of three ",(0,r.jsx)(t.code,{children:"hydra-nodes"}),", each directly connected to the others and configured with one of three Hydra credentials: ",(0,r.jsx)(t.code,{children:"Alice"}),", ",(0,r.jsx)(t.code,{children:"Bob"}),", or ",(0,r.jsx)(t.code,{children:"Carol"})]}),"\n",(0,r.jsxs)(t.li,{children:["A single ",(0,r.jsx)(t.code,{children:"cardano-node"})," producing blocks used as a (very fast) local ",(0,r.jsx)(t.code,{children:"devnet"})]}),"\n",(0,r.jsxs)(t.li,{children:["The ",(0,r.jsx)(t.code,{children:"hydra-tui"})," example for clients to interact with the individual ",(0,r.jsx)(t.code,{children:"hydra-node"}),"."]}),"\n"]}),"\n",(0,r.jsxs)(t.p,{children:["This tutorial uses ",(0,r.jsx)(t.a,{href:"https://www.docker.com/get-started",children:"Docker"})," to install the nodes, so ensure Docker is installed. If you want to explore alternative ways of running the tools, see a ",(0,r.jsx)(t.a,{href:"/head-protocol/unstable/docs/getting-started-without-docker",children:"variant of this tutorial"})," or the ",(0,r.jsx)(t.a,{href:"/head-protocol/unstable/docs/tutorial/",children:"testnet tutorial"}),", which uses pre-built binaries. The documentation pages on ",(0,r.jsx)(t.a,{href:"./installation",children:"installation"})," and ",(0,r.jsx)(t.a,{href:"./configuration",children:"configuration"})," provide more details."]}),"\n",(0,r.jsxs)(t.p,{children:["Additionally, the ",(0,r.jsx)(t.code,{children:"hydra-tui"})," uses the HTTP/WebSocket API provided by the ",(0,r.jsx)(t.code,{children:"hydra-node"})," behind the scenes. The ",(0,r.jsx)(t.a,{href:"/head-protocol/unstable/docs/tutorial/",children:"testnet tutorial"})," will show how to use this API using low-level commands, or you can see the ",(0,r.jsx)(t.a,{href:"https://hydra.family/head-protocol/unstable/api-reference",children:"API reference"})," for more details."]}),"\n",(0,r.jsxs)(o,{children:[(0,r.jsx)("summary",{children:"Video demonstration (a bit dated)"}),(0,r.jsx)("iframe",{style:{width:"100%",height:"480px"},src:"https://www.youtube.com/embed/dJk5_kB3BM4",title:"Hydra Head Demo",frameborder:"0",allow:"accelerometer; autoplay; clipboard-write; encrypted-media; gyroscope; picture-in-picture",allowfullscreen:"true"})]}),"\n",(0,r.jsx)(t.admonition,{title:"OS Compatibility",type:"caution",children:(0,r.jsxs)(t.p,{children:["These instructions have been tested only on Linux environments (Ubuntu, NixOS). If you're on Windows or Mac OS X, you might need to adapt to use ",(0,r.jsx)(t.a,{href:"https://docs.docker.com/storage/volumes/",children:"Volumes"}),"."]})}),"\n",(0,r.jsx)(t.h2,{id:"preparation",children:"Preparation"}),"\n",(0,r.jsxs)(t.p,{children:["All commands below are written as if executed from the ",(0,r.jsx)(t.code,{children:"demo"})," folder in the project repository. Ensure you clone the repository, switch into the ",(0,r.jsx)(t.code,{children:"demo"})," folder, and pull the latest Docker images:"]}),"\n",(0,r.jsx)(t.pre,{children:(0,r.jsx)(t.code,{className:"language-shell",children:"git clone git@github.com:cardano-scaling/hydra.git\ncd hydra/demo\ndocker compose pull\n"})}),"\n",(0,r.jsx)(t.admonition,{title:"Shortcut",type:"info",children:(0,r.jsxs)(t.p,{children:["For convenience, we provide a script ",(0,r.jsx)(t.code,{children:"./run-docker.sh"}),", which combines all the following steps. It also performs a few sanity checks to prevent potential issues."]})}),"\n",(0,r.jsx)(t.h2,{id:"set-up-the-devnet",children:"Set up the devnet"}),"\n",(0,r.jsxs)(t.p,{children:["Next, prepare the devnet configuration to bootstrap a local Cardano blockchain.\n",(0,r.jsx)(t.strong,{children:"Note"})," that we use a simplified variant of Cardano that does not require any stake pools."]}),"\n",(0,r.jsx)(t.pre,{children:(0,r.jsx)(t.code,{className:"language-shell",children:"./prepare-devnet.sh\n"})}),"\n",(0,r.jsx)(t.p,{children:"Bring the Cardano node up with:"}),"\n",(0,r.jsx)(t.pre,{children:(0,r.jsx)(t.code,{className:"language-shell",children:"docker compose up -d cardano-node\n"})}),"\n",(0,r.jsx)(t.p,{children:"Verify that the devnet is up-and-running by checking the logs with:"}),"\n",(0,r.jsx)(t.pre,{children:(0,r.jsx)(t.code,{className:"language-shell",children:"docker compose logs cardano-node -f\n"})}),"\n",(0,r.jsxs)(t.p,{children:["You should see traces containing ",(0,r.jsx)(t.code,{children:"TraceAdoptedBlock"}),", which means that the devnet is producing blocks .. nice!"]}),"\n",(0,r.jsx)(t.admonition,{type:"info",children:(0,r.jsxs)(t.p,{children:["Do not wait too long between these two commands. If you get ",(0,r.jsx)(t.code,{children:"TraceNoLedgerView"})," errors from the Cardano node, the genesis start time is too far in the past, and you need to update them by running ",(0,r.jsx)(t.code,{children:"prepare-devnet.sh"})," again."]})}),"\n",(0,r.jsxs)(t.admonition,{type:"tip",children:[(0,r.jsxs)(t.p,{children:["You can use ",(0,r.jsx)(t.a,{href:"https://jqlang.github.io/jq/",children:"jq"})," to follow the logs and see the node update kinds with the\nfollowing command:"]}),(0,r.jsx)(t.pre,{children:(0,r.jsx)(t.code,{children:"docker compose logs -f --no-log-prefix cardano-node | jq -Rr 'fromjson? | .data.val.kind'\n"})})]}),"\n",(0,r.jsxs)(t.p,{children:["Next, we need to give Alice, Bob, and Carol some UTXOs for committing and ada for paying fees. To do this, use the ",(0,r.jsx)(t.code,{children:"seed-devnet.sh"})," script, which uses the ",(0,r.jsx)(t.code,{children:"cardano-cli"})," within the already running ",(0,r.jsx)(t.code,{children:"cardano-node"})," container:"]}),"\n",(0,r.jsx)(t.pre,{children:(0,r.jsx)(t.code,{className:"language-shell",children:"./seed-devnet.sh\n"})}),"\n",(0,r.jsx)(t.h2,{id:"start-hydra-nodes",children:"Start Hydra nodes"}),"\n",(0,r.jsx)(t.p,{children:"Finally, now that the on-chain preparations are done, we can bring the Hydra network (all three nodes for Alice, Bob, and Carol) up by running:"}),"\n",(0,r.jsx)(t.pre,{children:(0,r.jsx)(t.code,{className:"language-shell",children:"docker compose up -d hydra-node-{1,2,3}\n"})}),"\n",(0,r.jsxs)(t.p,{children:["Using ",(0,r.jsx)(t.code,{children:"compose"}),", you can start the demo Terminal-based User Interface (",(0,r.jsx)(t.code,{children:"hydra-tui"}),") to interact with Hydra nodes. There are three preconfigured TUI services in the ",(0,r.jsx)(t.code,{children:"compose"})," definition: ",(0,r.jsx)(t.code,{children:"hydra-tui-1"}),", ",(0,r.jsx)(t.code,{children:"hydra-tui-2"}),", and ",(0,r.jsx)(t.code,{children:"hydra-tui-3"}),". To connect to the first Hydra node in a terminal, run the following command:"]}),"\n",(0,r.jsx)(t.pre,{children:(0,r.jsx)(t.code,{className:"language-shell",children:"docker compose run hydra-tui-1\n"})}),"\n",(0,r.jsxs)(t.p,{children:["This will start a full-blown terminal interface loaded with signing keys corresponding to the first Hydra node. In other terminals, you can start other nodes in a similar fashion targeting ",(0,r.jsx)(t.code,{children:"hydra-tui-2"})," and ",(0,r.jsx)(t.code,{children:"hydra-tui-3"})," services."]}),"\n",(0,r.jsx)(t.h2,{id:"use-the-head",children:"Use the head"}),"\n",(0,r.jsxs)(t.p,{children:["Using the terminal interface of any node, you can now ",(0,r.jsx)(t.code,{children:"[i]nit"})," the Hydra head and ",(0,r.jsx)(t.code,{children:"[c]ommit"})," pre-distributed funds to it. Note that these steps are near-instant as the devnet is producing blocks much faster than a public testnet or the mainnet. After committing from all nodes, the head will automatically open, and you can also use the ",(0,r.jsx)(t.code,{children:"hydra-tui"})," or the API to create new transactions and submit them to the Hydra head."]}),"\n",(0,r.jsx)(t.p,{children:(0,r.jsx)(t.img,{src:n(70735).A+"",width:"1869",height:"487"})})]})}function h(e={}){const{wrapper:t}={...(0,s.R)(),...e.components};return t?(0,r.jsx)(t,{...e,children:(0,r.jsx)(l,{...e})}):l(e)}},70735:(e,t,n)=>{n.d(t,{A:()=>o});const o=n.p+"assets/images/open-head-112e4af7cb464a179c7514412a8a9e38.png"},28453:(e,t,n)=>{n.d(t,{R:()=>i,x:()=>d});var o=n(96540);const r={},s=o.createContext(r);function i(e){const t=o.useContext(s);return o.useMemo((function(){return"function"==typeof e?e(t):{...t,...e}}),[t,e])}function d(e){let t;return t=e.disableParentContext?"function"==typeof e.components?e.components(r):e.components||r:i(e.components),o.createElement(s.Provider,{value:t},e.children)}}}]);