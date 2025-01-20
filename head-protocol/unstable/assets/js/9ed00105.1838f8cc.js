"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[3873],{8633:(e,n,t)=>{t.r(n),t.d(n,{assets:()=>d,contentTitle:()=>r,default:()=>h,frontMatter:()=>a,metadata:()=>s,toc:()=>c});const s=JSON.parse('{"id":"configuration","title":"Configuration","description":"Running a Hydra head involves operating a Hydra node connected to other Hydra nodes and a Cardano node. The entire configuration of the hydra-node is managed through command-line options. Use the --help option to see a full list of available commands:","source":"@site/docs/configuration.md","sourceDirName":".","slug":"/configuration","permalink":"/head-protocol/unstable/docs/configuration","draft":false,"unlisted":false,"editUrl":"https://github.com/cardano-scaling/hydra/tree/master/docs/docs/configuration.md","tags":[],"version":"current","frontMatter":{},"sidebar":"userDocumentation","previous":{"title":"Installation","permalink":"/head-protocol/unstable/docs/installation"},"next":{"title":"Commit using a blueprint","permalink":"/head-protocol/unstable/docs/how-to/commit-blueprint"}}');var i=t(74848),o=t(28453);const a={},r="Configuration",d={},c=[{value:"Cardano keys",id:"cardano-keys",level:3},{value:"Hydra keys",id:"hydra-keys",level:3},{value:"Contestation period",id:"contestation-period",level:3},{value:"Reference scripts",id:"reference-scripts",level:3},{value:"Ledger parameters",id:"ledger-parameters",level:3},{value:"Fuel v funds",id:"fuel-v-funds",level:3},{value:"Connect to Cardano",id:"connect-to-cardano",level:3},{value:"Offline mode",id:"offline-mode",level:3},{value:"API server",id:"api-server",level:3}];function l(e){const n={a:"a",admonition:"admonition",blockquote:"blockquote",code:"code",em:"em",h1:"h1",h3:"h3",header:"header",li:"li",mdxAdmonitionTitle:"mdxAdmonitionTitle",p:"p",pre:"pre",strong:"strong",ul:"ul",...(0,o.R)(),...e.components};return(0,i.jsxs)(i.Fragment,{children:[(0,i.jsx)(n.header,{children:(0,i.jsx)(n.h1,{id:"configuration",children:"Configuration"})}),"\n",(0,i.jsxs)(n.p,{children:["Running a Hydra head involves operating a Hydra node connected to other Hydra nodes and a Cardano node. The entire configuration of the ",(0,i.jsx)(n.code,{children:"hydra-node"})," is managed through command-line options. Use the ",(0,i.jsx)(n.code,{children:"--help"})," option to see a full list of available commands:"]}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-shell",children:"hydra-node --help\n"})}),"\n",(0,i.jsxs)(n.p,{children:["Below, we document selected aspects of the configuration. For a comprehensive guide, refer to the ",(0,i.jsx)(n.a,{href:"./tutorial",children:"tutorial"})," or specific ",(0,i.jsx)(n.em,{children:"how to"})," articles."]}),"\n",(0,i.jsx)(n.h3,{id:"cardano-keys",children:"Cardano keys"}),"\n",(0,i.jsxs)(n.p,{children:["In a Hydra head, each participant is authenticated using two sets of keys. The first set identifies a participant on the Cardano layer 1 and is used to hold ada for paying fees. Each ",(0,i.jsx)(n.code,{children:"hydra-node"})," requires a ",(0,i.jsx)(n.code,{children:"--cardano-signing-key"}),", and you must provide the ",(0,i.jsx)(n.code,{children:"--cardano-verification-key"})," for each participant."]}),"\n",(0,i.jsxs)(n.p,{children:["Generate a Cardano key pair using the ",(0,i.jsx)(n.code,{children:"cardano-cli"}),":"]}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-shell",children:"cardano-cli address key-gen \\\n  --verification-key-file cardano.vk \\\n  --signing-key-file cardano.sk\n"})}),"\n",(0,i.jsx)(n.p,{children:"These keys authenticate on-chain transactions and ensure that only authorized participants can control the head's lifecycle, preventing unauthorized actors from interfering (eg, aborting an initialized head). While this issue does not put participants' funds at risk, it is still inconvenient and can be avoided."}),"\n",(0,i.jsx)(n.h3,{id:"hydra-keys",children:"Hydra keys"}),"\n",(0,i.jsx)(n.p,{children:"The second set of keys are Hydra keys, used for multi-signing snapshots within a head. Although these keys may eventually support an aggregated multi-signature scheme, they currently use the Ed25519 format."}),"\n",(0,i.jsxs)(n.p,{children:["Generate new Hydra keys using the ",(0,i.jsx)(n.code,{children:"hydra-node"}),":"]}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"hydra-node gen-hydra-key --output-file my-key\n"})}),"\n",(0,i.jsxs)(n.p,{children:["This command creates two files: ",(0,i.jsx)(n.code,{children:"my-key.sk"})," and ",(0,i.jsx)(n.code,{children:"my-key.vk"})," containing Hydra keys suitable for use inside a head."]}),"\n",(0,i.jsxs)(n.p,{children:["For demonstration purposes, we also provide demo key pairs (",(0,i.jsx)(n.code,{children:"alice.{vk,sk}"}),", ",(0,i.jsx)(n.code,{children:"bob.{vk,sk}"}),", and ",(0,i.jsx)(n.code,{children:"carol.{vk,sk}"}),") in our ",(0,i.jsx)(n.a,{href:"https://github.com/cardano-scaling/hydra/tree/master/demo",children:"demo folder"}),". These demo keys should not be used in production."]}),"\n",(0,i.jsx)(n.h3,{id:"contestation-period",children:"Contestation period"}),"\n",(0,i.jsx)(n.p,{children:"The contestation period is a critical protocol parameter, defined in seconds, for example:"}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"hydra-node --contestation-period 120s\n"})}),"\n",(0,i.jsx)(n.p,{children:"The contestation period is used in:"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["Constructing the upper validity bound for ",(0,i.jsx)(n.code,{children:"Close"})," and ",(0,i.jsx)(n.code,{children:"Contest"})," transactions"]}),"\n",(0,i.jsxs)(n.li,{children:["Computing the contestation deadline, which defines the lower validity\nbound for the ",(0,i.jsx)(n.code,{children:"FanOut"})," transaction."]}),"\n"]}),"\n",(0,i.jsxs)(n.p,{children:["The default contestation period is ",(0,i.jsx)(n.em,{children:"60 seconds"}),", but it should be tailored to the network conditions, as different networks have varying slot lengths and block production rates."]}),"\n",(0,i.jsx)(n.admonition,{title:"Consistent contestation period",type:"info",children:(0,i.jsxs)(n.p,{children:["All parties in the Hydra Head protocol must configure the same ",(0,i.jsx)(n.code,{children:"--contestation-period"})," value. Otherwise, the ",(0,i.jsx)(n.code,{children:"Init"})," transaction will be ignored, preventing any participant from stalling or causing a denial-of-service (DoS) attack by setting an unreasonably large contestation period."]})}),"\n",(0,i.jsxs)(n.admonition,{type:"warning",children:[(0,i.jsxs)(n.mdxAdmonitionTitle,{children:["Invalid ",(0,i.jsx)(n.code,{children:"Close"})," and ",(0,i.jsx)(n.code,{children:"Contest"})," transactions"]}),(0,i.jsxs)(n.p,{children:["Depending on the contestation period value and the network conditions,\n",(0,i.jsx)(n.code,{children:"Close"})," and ",(0,i.jsx)(n.code,{children:"Contest"})," transactions could become invalid and be\nsilently rejected by the ",(0,i.jsx)(n.code,{children:"cardano-node"})," to which they have been\nsubmitted. This can happen, for example, if:"]}),(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsx)(n.li,{children:"The network is congested with many transactions waiting to be\nincluded in a block"}),"\n",(0,i.jsx)(n.li,{children:"The node's connectivity to the network drops, and the transaction is\nnot propagated to block producers fast enough."}),"\n"]}),(0,i.jsxs)(n.p,{children:["Currently, the ",(0,i.jsx)(n.code,{children:"hydra-node"})," does not handle this situation. Each client application should implement a retry mechanism based on the expected time for transaction inclusion."]})]}),"\n",(0,i.jsx)(n.h3,{id:"reference-scripts",children:"Reference scripts"}),"\n",(0,i.jsxs)(n.p,{children:["The ",(0,i.jsx)(n.code,{children:"hydra-node"})," uses reference scripts to reduce transaction sizes driving the head's lifecycle. Specify the ",(0,i.jsx)(n.code,{children:"--hydra-scripts-tx-id"})," to reference the correct scripts. The ",(0,i.jsx)(n.code,{children:"hydra-node"})," will verify the availability of these scripts on-chain."]}),"\n",(0,i.jsx)(n.p,{children:"Check the scripts against which a hydra-node was compiled using:"}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-shell",children:"hydra-node --script-info\n"})}),"\n",(0,i.jsxs)(n.p,{children:["For public ",(0,i.jsx)(n.a,{href:"https://book.world.dev.cardano.org/environments.html",children:"(test) networks"}),", we publish Hydra scripts with each new release, listing transaction IDs in the ",(0,i.jsx)(n.a,{href:"https://github.com/cardano-scaling/hydra/releases",children:"release notes"})," and ",(0,i.jsx)(n.a,{href:"https://github.com/cardano-scaling/hydra/blob/master/networks.json",children:(0,i.jsx)(n.code,{children:"networks.json"})}),"."]}),"\n",(0,i.jsxs)(n.p,{children:["To publish scripts yourself, use the ",(0,i.jsx)(n.code,{children:"publish-scripts"})," command:"]}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-shell",children:"hydra-node publish-scripts \\\n  --testnet-magic 42 \\\n  --node-socket /path/to/node.socket \\\n  --cardano-signing-key cardano.sk\n"})}),"\n",(0,i.jsxs)(n.p,{children:["This command outputs a transaction ID upon success. The provided key should hold sufficient funds (> 50 ada) to create multiple ",(0,i.jsx)(n.strong,{children:"UNSPENDABLE"})," UTXO entries on-chain, each carrying a script referenced by the Hydra node."]}),"\n",(0,i.jsx)(n.h3,{id:"ledger-parameters",children:"Ledger parameters"}),"\n",(0,i.jsxs)(n.p,{children:["The ledger is at the core of a Hydra head. Hydra is currently integrated with Cardano and assumes a ledger configuration similar to layer 1. This translates as a command-line option ",(0,i.jsx)(n.code,{children:"--ledger-protocol-parameters"}),". This defines the updatable protocol parameters such as fees or transaction sizes. These parameters follow the same format as the ",(0,i.jsx)(n.code,{children:"cardano-cli query protocol-parameters"})," output."]}),"\n",(0,i.jsxs)(n.p,{children:["We provide existing files in ",(0,i.jsx)(n.a,{href:"https://github.com/cardano-scaling/hydra/blob/master/hydra-cluster/config",children:"hydra-cluster/config"}),", which can be used as the basis. In particular, the protocol parameters nullify costs inside a head. Apart from that, they are the direct copy of the current mainnet parameters. An interesting point about Hydra's ledger is that while it re-uses the same rules and code as layer 1 (isomorphic), some parameters can be altered. For example, fees can be adjusted, but not parameters controlling maximum value sizes or minimum ada values, as altering these could make a head unclosable."]}),"\n",(0,i.jsx)(n.p,{children:"A good rule of thumb is that anything that applies strictly to transactions (fees, execution units, max tx size, etc) is safe to change. But anything that could be reflected in the UTXO is not."}),"\n",(0,i.jsx)(n.admonition,{title:"About protocol parameters",type:"info",children:(0,i.jsx)(n.p,{children:"Many protocol parameters are irrelevant in the Hydra context (eg, there is no treasury or stake pools within a head). Therefore, parameters related to reward incentives or delegation rules are unused."})}),"\n",(0,i.jsx)(n.h3,{id:"fuel-v-funds",children:"Fuel v funds"}),"\n",(0,i.jsxs)(n.p,{children:["Transactions driving the head lifecycle (",(0,i.jsx)(n.code,{children:"Init"}),", ",(0,i.jsx)(n.code,{children:"Abort"}),", ",(0,i.jsx)(n.code,{children:"Close"}),", etc) must be submitted to layer 1 and hence incur costs. Any UTXO owned by the ",(0,i.jsx)(n.code,{children:"--cardano-signing-key"})," provided to the ",(0,i.jsx)(n.code,{children:"--hydra-node"})," can be used to pay fees or serve as collateral for these transactions. We refer to this as ",(0,i.jsx)(n.strong,{children:"fuel"}),"."]}),"\n",(0,i.jsxs)(n.p,{children:["Consequently, sending some ada to the address of this 'internal wallet' is required. To get the address for the Cardano keys as generated above, one can use, for example, the ",(0,i.jsx)(n.code,{children:"cardano-cli"}),":"]}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-shell",children:"cardano-cli address build --verification-key-file cardano.vk --mainnet\n# addr1v92l229athdj05l20ggnqz24p4ltlj55e7n4xplt2mxw8tqsehqnt\n"})}),"\n",(0,i.jsxs)(n.p,{children:["While the ",(0,i.jsx)(n.code,{children:"hydra-node"})," needs to pay fees for protocol transactions, any wallet can be used to commit ",(0,i.jsx)(n.strong,{children:"funds"})," into an ",(0,i.jsx)(n.code,{children:"initializing"})," Hydra head. The ",(0,i.jsx)(n.code,{children:"hydra-node"})," provides an HTTP endpoint at ",(0,i.jsx)(n.code,{children:"/commit"}),", allowing you to specify either:"]}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["A set of ",(0,i.jsx)(n.code,{children:"UTXO"})," outputs to commit (belonging to public keys), or"]}),"\n",(0,i.jsxs)(n.li,{children:["A ",(0,i.jsx)(n.em,{children:"blueprint"})," transaction along with the ",(0,i.jsx)(n.code,{children:"UTXO"})," that resolves it."]}),"\n"]}),"\n",(0,i.jsxs)(n.p,{children:["This endpoint returns a commit transaction, which is balanced, and all fees are paid by the ",(0,i.jsx)(n.code,{children:"hydra-node"}),". The integrated wallet must sign and submit this transaction to the Cardano network. See the ",(0,i.jsx)(n.a,{href:"pathname:///api-reference/#operation-publish-/commit",children:"API documentation"})," for details."]}),"\n",(0,i.jsxs)(n.p,{children:["If using your own UTXO to commit to a head, send the appropriate JSON representation of the said UTXO to the ",(0,i.jsx)(n.code,{children:"/commit"})," API endpoint.\nUsing a ",(0,i.jsx)(n.em,{children:"blueprint"})," transaction with ",(0,i.jsx)(n.code,{children:"/commit"})," offers flexibility, as ",(0,i.jsx)(n.code,{children:"hydra-node"})," adds necessary commit transaction data without removing additional information specified in the blueprint transaction (eg, reference inputs, redeemers, validity ranges)."]}),"\n",(0,i.jsxs)(n.blockquote,{children:["\n",(0,i.jsxs)(n.p,{children:["Note: Outputs of a blueprint transaction are not considered \u2014 only inputs are used to commit funds to the head. The ",(0,i.jsx)(n.code,{children:"hydra-node"})," will also ",(0,i.jsx)(n.strong,{children:"ignore"})," any minting or burning specified in the blueprint transaction."]}),"\n"]}),"\n",(0,i.jsxs)(n.p,{children:["For more details, refer to this ",(0,i.jsx)(n.a,{href:"./how-to/commit-blueprint",children:"how to"})," guide on committing to a head using a blueprint transaction."]}),"\n",(0,i.jsx)(n.h3,{id:"connect-to-cardano",children:"Connect to Cardano"}),"\n",(0,i.jsxs)(n.p,{children:["The ",(0,i.jsx)(n.code,{children:"hydra-node"})," must be connected to the Cardano network, unless running in ",(0,i.jsx)(n.a,{href:"/head-protocol/unstable/docs/configuration#offline-mode",children:"offline mode"}),"."]}),"\n",(0,i.jsxs)(n.p,{children:["A direct connection to a ",(0,i.jsx)(n.a,{href:"https://github.com/input-output-hk/cardano-node/",children:(0,i.jsx)(n.code,{children:"cardano-node"})})," is a prerequisite. Please refer to existing documentation on starting a node, for example on ",(0,i.jsx)(n.a,{href:"https://developers.cardano.org/docs/get-started/running-cardano",children:"developers.cardano.org"}),", or ",(0,i.jsx)(n.a,{href:"https://mithril.network/doc/manual/getting-started/bootstrap-cardano-node",children:"use Mithril"})," to bootstrap the local node."]}),"\n",(0,i.jsxs)(n.p,{children:["To specify how to connect to the local ",(0,i.jsx)(n.code,{children:"cardano-node"}),", use ",(0,i.jsx)(n.code,{children:"--node-socket"})," and ",(0,i.jsx)(n.code,{children:"--testnet-magic"}),":"]}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-shell",children:"hydra-node \\\n  --testnet-magic 42 \\\n  --node-socket devnet/node.socket \\\n"})}),"\n",(0,i.jsx)(n.admonition,{type:"info",children:(0,i.jsxs)(n.p,{children:["The ",(0,i.jsx)(n.code,{children:"hydra-node"})," is compatible with the Cardano ",(0,i.jsx)(n.code,{children:"mainnet"})," network, and can consequently operate using ",(0,i.jsx)(n.strong,{children:"real funds"}),". Please be sure to read the ",(0,i.jsx)(n.a,{href:"/docs/known-issues",children:"known issues"})," to fully understand the limitations and consequences of running Hydra nodes on mainnet. To choose ",(0,i.jsx)(n.code,{children:"mainnet"}),", use ",(0,i.jsx)(n.code,{children:"--mainnet"})," instead of ",(0,i.jsx)(n.code,{children:"--testnet-magic"}),"."]})}),"\n",(0,i.jsxs)(n.p,{children:["Using the direct node connection, the ",(0,i.jsx)(n.code,{children:"hydra-node"})," synchronizes the chain and observes Hydra protocol transactions. On startup, it starts observing from the chain's tip. Once a Hydra head has been observed, the point of the last known state change is used automatically."]}),"\n",(0,i.jsxs)(n.p,{children:["You can manually set the intersection point using ",(0,i.jsx)(n.code,{children:"--start-chain-from <slot>.<hash>"})," which specifies a ",(0,i.jsx)(n.code,{children:"slot"})," and block header ",(0,i.jsx)(n.code,{children:"hash"}),". For example:"]}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-shell",children:"hydra-node \\\n  --testnet-magic 2 \\\n  --node-socket preview/node.socket \\\n  --start-chain-from 49533501.e364500a42220ea47314215679b7e42e9bbb81fa69d1366fe738d8aef900f7ee\n"})}),"\n",(0,i.jsxs)(n.p,{children:["To synchronize from the genesis block, use ",(0,i.jsx)(n.code,{children:"--start-chain-from 0"}),"."]}),"\n",(0,i.jsx)(n.admonition,{type:"info",children:(0,i.jsxs)(n.p,{children:["If the ",(0,i.jsx)(n.code,{children:"hydra-node"})," already tracks a head in its ",(0,i.jsx)(n.code,{children:"state"})," and ",(0,i.jsx)(n.code,{children:"--start-chain-from"})," is given, the ",(0,i.jsx)(n.strong,{children:"newer"})," point is used."]})}),"\n",(0,i.jsx)(n.h3,{id:"offline-mode",children:"Offline mode"}),"\n",(0,i.jsx)(n.p,{children:"Hydra supports an offline mode that allows for disabling the layer 1 interface \u2013 the underlying Cardano blockchain from which Hydra heads acquire funds and to which funds are eventually withdrawn. Disabling layer 1 interactions allows use cases that would otherwise require running and configuring an entire layer 1 private devnet. For example, the offline mode can be used to quickly validate a series of transactions against a UTXO, without having to spin up an entire layer 1 Cardano node."}),"\n",(0,i.jsxs)(n.p,{children:["To initialize the layer 2 ledger's UTXO state, offline mode takes an obligatory ",(0,i.jsx)(n.code,{children:"--initial-utxo"})," parameter, which points to a JSON-encoded UTxO file. See the ",(0,i.jsx)(n.a,{href:"https://hydra.family/head-protocol/api-reference/#schema-UTxO",children:"API reference"})," for the schema."]}),"\n",(0,i.jsx)(n.p,{children:"Using this example UTxO:"}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-json",metastring:"utxo.json",children:'{\n  "0000000000000000000000000000000000000000000000000000000000000000#0": {\n    "address": "addr_test1vqg9ywrpx6e50uam03nlu0ewunh3yrscxmjayurmkp52lfskgkq5k",\n    "value": {\n      "lovelace": 100000000\n    }\n  }\n}\n'})}),"\n",(0,i.jsx)(n.p,{children:"An offline mode hydra-node can be started with:"}),"\n",(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{className:"language-shell",children:"hydra-node offline \\\n  --hydra-signing-key hydra.sk \\\n  --ledger-protocol-parameters protocol-parameters.json \\\n  --initial-utxo utxo.json\n"})}),"\n",(0,i.jsxs)(n.p,{children:["As the node is not connected to a real network, genesis parameters that normally influence things like time-based transaction validation cannot be fetched and are set to defaults. To configure block times, set ",(0,i.jsx)(n.code,{children:"--ledger-genesis"})," to a Shelley genesis file similar to the ",(0,i.jsx)(n.a,{href:"https://book.world.dev.cardano.org/environments/mainnet/shelley-genesis.json",children:"shelley-genesis.json"}),"."]}),"\n",(0,i.jsx)(n.h3,{id:"api-server",children:"API server"}),"\n",(0,i.jsxs)(n.p,{children:["The ",(0,i.jsx)(n.code,{children:"hydra-node"})," exposes an ",(0,i.jsx)(n.a,{href:"/api-reference",children:"API"})," for clients to interact with the hydra node, submit transactions to an open, but also initialize / close Hydra heads!"]}),"\n",(0,i.jsxs)(n.p,{children:["As the API is not authenticated by default, the node is only binding to ",(0,i.jsx)(n.code,{children:"localhost"}),"/",(0,i.jsx)(n.code,{children:"127.0.0.1"})," interfaces and listens on port ",(0,i.jsx)(n.code,{children:"4001"}),". This can be configured using ",(0,i.jsx)(n.code,{children:"--api-host"})," and ",(0,i.jsx)(n.code,{children:"--api-port"}),"."]}),"\n",(0,i.jsx)(n.admonition,{type:"warning",children:(0,i.jsx)(n.p,{children:"The API is not authenticated, and if exposed, an open head can be easily closed through the API!"})}),"\n",(0,i.jsxs)(n.p,{children:["The API server also supports ",(0,i.jsx)(n.code,{children:"TLS"})," connections (",(0,i.jsx)(n.code,{children:"https://"})," and ",(0,i.jsx)(n.code,{children:"wss://"}),") when a certificate and key are configured with ",(0,i.jsx)(n.code,{children:"--tls-cert"})," and ",(0,i.jsx)(n.code,{children:"--tls-key"})," respectively."]})]})}function h(e={}){const{wrapper:n}={...(0,o.R)(),...e.components};return n?(0,i.jsx)(n,{...e,children:(0,i.jsx)(l,{...e})}):l(e)}},28453:(e,n,t)=>{t.d(n,{R:()=>a,x:()=>r});var s=t(96540);const i={},o=s.createContext(i);function a(e){const n=s.useContext(o);return s.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function r(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(i):e.components||i:a(e.components),s.createElement(o.Provider,{value:n},e.children)}}}]);