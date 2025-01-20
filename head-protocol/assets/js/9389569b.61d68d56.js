"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[8667],{3905:(e,t,n)=>{n.d(t,{Zo:()=>c,kt:()=>h});var r=n(67294);function a(e,t,n){return t in e?Object.defineProperty(e,t,{value:n,enumerable:!0,configurable:!0,writable:!0}):e[t]=n,e}function i(e,t){var n=Object.keys(e);if(Object.getOwnPropertySymbols){var r=Object.getOwnPropertySymbols(e);t&&(r=r.filter((function(t){return Object.getOwnPropertyDescriptor(e,t).enumerable}))),n.push.apply(n,r)}return n}function o(e){for(var t=1;t<arguments.length;t++){var n=null!=arguments[t]?arguments[t]:{};t%2?i(Object(n),!0).forEach((function(t){a(e,t,n[t])})):Object.getOwnPropertyDescriptors?Object.defineProperties(e,Object.getOwnPropertyDescriptors(n)):i(Object(n)).forEach((function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}))}return e}function l(e,t){if(null==e)return{};var n,r,a=function(e,t){if(null==e)return{};var n,r,a={},i=Object.keys(e);for(r=0;r<i.length;r++)n=i[r],t.indexOf(n)>=0||(a[n]=e[n]);return a}(e,t);if(Object.getOwnPropertySymbols){var i=Object.getOwnPropertySymbols(e);for(r=0;r<i.length;r++)n=i[r],t.indexOf(n)>=0||Object.prototype.propertyIsEnumerable.call(e,n)&&(a[n]=e[n])}return a}var p=r.createContext({}),s=function(e){var t=r.useContext(p),n=t;return e&&(n="function"==typeof e?e(t):o(o({},t),e)),n},c=function(e){var t=s(e.components);return r.createElement(p.Provider,{value:t},e.children)},u="mdxType",d={inlineCode:"code",wrapper:function(e){var t=e.children;return r.createElement(r.Fragment,{},t)}},m=r.forwardRef((function(e,t){var n=e.components,a=e.mdxType,i=e.originalType,p=e.parentName,c=l(e,["components","mdxType","originalType","parentName"]),u=s(n),m=a,h=u["".concat(p,".").concat(m)]||u[m]||d[m]||i;return n?r.createElement(h,o(o({ref:t},c),{},{components:n})):r.createElement(h,o({ref:t},c))}));function h(e,t){var n=arguments,a=t&&t.mdxType;if("string"==typeof e||a){var i=n.length,o=new Array(i);o[0]=m;var l={};for(var p in t)hasOwnProperty.call(t,p)&&(l[p]=t[p]);l.originalType=e,l[u]="string"==typeof e?e:a,o[1]=l;for(var s=2;s<i;s++)o[s]=n[s];return r.createElement.apply(null,o)}return r.createElement.apply(null,n)}m.displayName="MDXCreateElement"},57257:(e,t,n)=>{n.r(t),n.d(t,{assets:()=>p,contentTitle:()=>o,default:()=>d,frontMatter:()=>i,metadata:()=>l,toc:()=>s});var r=n(87462),a=(n(67294),n(3905));const i={sidebar_position:10},o="Profiling Hydra scripts",l={unversionedId:"profiling",id:"profiling",title:"Profiling Hydra scripts",description:"This tutorial explains how to profile Hydra scripts and is intended for contributors to the hydra-node.",source:"@site/benchmarks/profiling.md",sourceDirName:".",slug:"/profiling",permalink:"/head-protocol/benchmarks/profiling",draft:!1,tags:[],version:"current",sidebarPosition:10,frontMatter:{sidebar_position:10},sidebar:"defaultSidebar",previous:{title:"End-to-end benchmarks",permalink:"/head-protocol/benchmarks/end-to-end-benchmarks"},next:{title:"Ledger micro-benchmarks",permalink:"/head-protocol/benchmarks/ledger"}},p={},s=[{value:"Overview",id:"overview",level:2},{value:"Isolating a transaction to profile",id:"isolating-a-transaction-to-profile",level:2},{value:"Compiling a script for profiling",id:"compiling-a-script-for-profiling",level:2},{value:"Acquiring an executable script",id:"acquiring-an-executable-script",level:2},{value:"Running the script and analyzing the results",id:"running-the-script-and-analyzing-the-results",level:2}],c={toc:s},u="wrapper";function d(e){let{components:t,...i}=e;return(0,a.kt)(u,(0,r.Z)({},c,i,{components:t,mdxType:"MDXLayout"}),(0,a.kt)("h1",{id:"profiling-hydra-scripts"},"Profiling Hydra scripts"),(0,a.kt)("p",null,"This tutorial explains how to profile Hydra scripts and is intended for contributors to the ",(0,a.kt)("inlineCode",{parentName:"p"},"hydra-node"),"."),(0,a.kt)("h2",{id:"overview"},"Overview"),(0,a.kt)("p",null,"For every pull request and the latest ",(0,a.kt)("inlineCode",{parentName:"p"},"master")," branch, we compute typical transaction costs in terms of size, memory, and CPU usage of the Hydra protocol transactions on Cardano. You can view the latest results ",(0,a.kt)("a",{parentName:"p",href:"https://hydra.family/head-protocol/benchmarks/transaction-cost/"},"here"),"."),(0,a.kt)("p",null,"Such benchmarks provide a comprehensive overview of the constraints for a given transaction, including maximum transaction size and percent of maximum memory and CPU budget. For a detailed assessment, we analyze ",(0,a.kt)("em",{parentName:"p"},"all")," scripts that run within a given transaction."),(0,a.kt)("p",null,"To gain detailed insights into ",(0,a.kt)("em",{parentName:"p"},"what exactly")," results in excessive memory or CPU usage, we need to profile the scripts as they validate a transaction."),(0,a.kt)("p",null,"Follow the instructions provided by the ",(0,a.kt)("a",{parentName:"p",href:"https://github.com/input-output-hk/plutus"},(0,a.kt)("inlineCode",{parentName:"a"},"Plutus"))," project ",(0,a.kt)("a",{parentName:"p",href:"https://plutus.readthedocs.io/en/latest/howtos/profiling-scripts.html"},"here"),", adapted for the ",(0,a.kt)("inlineCode",{parentName:"p"},"hydra")," codebase."),(0,a.kt)("h2",{id:"isolating-a-transaction-to-profile"},"Isolating a transaction to profile"),(0,a.kt)("p",null,"First, isolate the specific Cardano transaction you want to profile. For example, let's investigate what the ",(0,a.kt)("inlineCode",{parentName:"p"},"collectCom")," transaction\nfor ",(0,a.kt)("inlineCode",{parentName:"p"},"5")," parties in the ",(0,a.kt)("inlineCode",{parentName:"p"},"tx-cost")," benchmark is spending most time and memory on."),(0,a.kt)("p",null,"The benchmark computes many transactions with the growing number of participants in ",(0,a.kt)("inlineCode",{parentName:"p"},"computeCollectComCost"),":"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-haskell"},"computeCollectComCost =\n  catMaybes <$> mapM compute [1 .. 100]\n where\n  compute numParties = do\n    (st, tx) <- generate $ genCollectComTx numParties\n    let utxo = getKnownUTxO st\n    case checkSizeAndEvaluate tx utxo of\n      -- [...]\n")),(0,a.kt)("p",null,"Here, isolate the transaction for ",(0,a.kt)("inlineCode",{parentName:"p"},"5")," parties by altering the function to ",(0,a.kt)("inlineCode",{parentName:"p"},"maybe [] pure <$> compute 5"),"."),(0,a.kt)("h2",{id:"compiling-a-script-for-profiling"},"Compiling a script for profiling"),(0,a.kt)("p",null,"The ",(0,a.kt)("inlineCode",{parentName:"p"},"collectCom")," transaction utilizes the ",(0,a.kt)("inlineCode",{parentName:"p"},"vCommit")," and ",(0,a.kt)("inlineCode",{parentName:"p"},"vHead")," validator scripts. To enable profiling, add the following directive to the modules ",(0,a.kt)("a",{parentName:"p",href:"/haddock/hydra-plutus/Hydra-Contract-Commit.html"},(0,a.kt)("inlineCode",{parentName:"a"},"Hydra.Contract.Commit"))," and ",(0,a.kt)("a",{parentName:"p",href:"/haddock/hydra-plutus/Hydra-Contract-Head.html"},(0,a.kt)("inlineCode",{parentName:"a"},"Hydra.Contract.Head")),":"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre"},"{-# OPTIONS_GHC -fplugin-opt PlutusTx.Plugin:profile-all #-}\n")),(0,a.kt)("h2",{id:"acquiring-an-executable-script"},"Acquiring an executable script"),(0,a.kt)("p",null,"You can achieve this using\n",(0,a.kt)("a",{parentName:"p",href:"/haddock/hydra-tx/Hydra-Ledger-Cardano-Evaluate.html#v:prepareTxScripts"},(0,a.kt)("inlineCode",{parentName:"a"},"prepareTxScripts")),".\nTo acquire and save the fully applied scripts from the transaction onto disk, run:"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre",className:"language-haskell"},'-- [...]\n(st, tx) <- generate $ genCollectComTx numParties\nlet utxo = getKnownUTxO st\nscripts <- either die pure $ prepareTxScripts tx utxo\nforM_ (zip [1 ..] scripts) $ \\(i, s) -> writeFileBS ("scripts-" <> show i <> ".flat") s\n-- [...]\n')),(0,a.kt)("p",null,"After running the corresponding code (",(0,a.kt)("inlineCode",{parentName:"p"},"tx-cost")," in our example), you will have\n",(0,a.kt)("inlineCode",{parentName:"p"},"scripts-{1,2,3,4,5}.flat")," files in the current directory."),(0,a.kt)("p",null,"Unfortunately, it's quite hard to distinguish them, but script sizes should help in identifying the larger ",(0,a.kt)("inlineCode",{parentName:"p"},"vHead")," script from the smaller ",(0,a.kt)("inlineCode",{parentName:"p"},"vCommit")," script. In the profile, the names of original ",(0,a.kt)("inlineCode",{parentName:"p"},"plutus-tx")," functions will be retained, which should make it clear at the latest."),(0,a.kt)("h2",{id:"running-the-script-and-analyzing-the-results"},"Running the script and analyzing the results"),(0,a.kt)("p",null,"To perform this step, use the following tools available through Nix:"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre"},"nix shell nixpkgs#flamegraph github:input-output-hk/plutus#x86_64-linux.plutus.library.plutus-project-924.hsPkgs.plutus-core.components.exes.traceToStacks github:input-output-hk/plutus#x86_64-linux.plutus.library.plutus-project-924.hsPkgs.plutus-core.components.exes.uplc\n")),(0,a.kt)("p",null,"To produce the profile log as explained above, you need to use a different input format since ",(0,a.kt)("inlineCode",{parentName:"p"},"prepareTxScripts")," retains the original name annotations."),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre"},"uplc evaluate -t -i scripts-1.flat --if flat-namedDeBruijn --trace-mode LogsWithBudgets -o logs\n")),(0,a.kt)("p",null,"Check for a ",(0,a.kt)("inlineCode",{parentName:"p"},"logs")," file output. If not present, ensure the script was compiled with profiling enabled as specified."),(0,a.kt)("p",null,"Finally, you can inspect the logs or generate flame graph SVGs as outlined in the original tutorial:"),(0,a.kt)("pre",null,(0,a.kt)("code",{parentName:"pre"},"cat logs | traceToStacks | flamegraph.pl > cpu.svg\ncat logs | traceToStacks --column 2 | flamegraph.pl > mem.svg\n")),(0,a.kt)("p",null,"Here's an example of a memory profile for a ",(0,a.kt)("inlineCode",{parentName:"p"},"5")," party ",(0,a.kt)("inlineCode",{parentName:"p"},"collectCom"),":"),(0,a.kt)("p",null,(0,a.kt)("img",{src:n(28775).Z,width:"1200",height:"342"})),(0,a.kt)("admonition",{type:"tip"},(0,a.kt)("p",{parentName:"admonition"},"Open the SVG in a browser to interactively search and explore the profile in detail.")))}d.isMDXComponent=!0},28775:(e,t,n)=>{n.d(t,{Z:()=>r});const r=n.p+"assets/images/profile-mem-1962b88f74167af737797653b64ca737.svg"}}]);