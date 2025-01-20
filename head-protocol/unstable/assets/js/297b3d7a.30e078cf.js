"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[5506],{93128:(e,n,s)=>{s.r(n),s.d(n,{assets:()=>r,contentTitle:()=>c,default:()=>h,frontMatter:()=>l,metadata:()=>i,toc:()=>d});var i=s(28112),t=s(74848),o=s(28453);const l={slug:20,title:"20. Handling time\n",authors:[],tags:["Accepted"]},c=void 0,r={authorsImageUrls:[]},d=[{value:"Status",id:"status",level:2},{value:"Context",id:"context",level:2},{value:"Decision",id:"decision",level:2},{value:"Consequences",id:"consequences",level:2}];function a(e){const n={a:"a",code:"code",em:"em",h2:"h2",img:"img",li:"li",p:"p",strong:"strong",ul:"ul",...(0,o.R)(),...e.components};return(0,t.jsxs)(t.Fragment,{children:[(0,t.jsx)(n.h2,{id:"status",children:"Status"}),"\n",(0,t.jsx)(n.p,{children:"Accepted"}),"\n",(0,t.jsx)(n.h2,{id:"context",children:"Context"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:["\n",(0,t.jsx)(n.p,{children:"The Hydra Head protocol is expected to be isomorphic to the ledger it runs on. That means, it should support the same transaction formats and (if desired) use the same ledger rules as the layer 1."}),"\n"]}),"\n",(0,t.jsxs)(n.li,{children:["\n",(0,t.jsxs)(n.p,{children:["Cardano is our layer 1 and its consensus layer separates time into discrete steps, where each step is called a ",(0,t.jsx)(n.code,{children:"Slot"}),". The network is expected to evolve strictly monotonically on this time scale and so slot numbers (",(0,t.jsx)(n.code,{children:"SlotNo"}),") are always increasing."]}),"\n"]}),"\n",(0,t.jsxs)(n.li,{children:["\n",(0,t.jsx)(n.p,{children:"The Cardano mainnet has a block scheduled every 20 seconds, although it may take longer."}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:["This is because ",(0,t.jsx)(n.code,{children:"slotLength = 1.0"}),' and every 20th slot is "active" with ',(0,t.jsx)(n.code,{children:"f = 0.05"}),"."]}),"\n",(0,t.jsxs)(n.li,{children:["The consensus protocol requires ",(0,t.jsx)(n.code,{children:"k"})," blocks to be produced within ",(0,t.jsx)(n.code,{children:"3k/f"})," slots, where ",(0,t.jsx)(n.code,{children:"k = 2160"})," on mainnet."]}),"\n"]}),"\n"]}),"\n",(0,t.jsxs)(n.li,{children:["\n",(0,t.jsxs)(n.p,{children:["Transactions on Cardano may have a validity range with a lower and upper bound given as ",(0,t.jsx)(n.code,{children:"SlotNo"}),"."]}),"\n"]}),"\n",(0,t.jsxs)(n.li,{children:["\n",(0,t.jsxs)(n.p,{children:["Wall-clock time can be converted to slots (and back) using an ",(0,t.jsx)(n.code,{children:"EraHistory"})," or ",(0,t.jsx)(n.code,{children:"EpochInterpreter"})," provided by the consensus layer of the cardano node. This is required as the slot lengths could change over time."]}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:["All past points in time since the ",(0,t.jsx)(n.code,{children:"SystemStart"})," can be converted."]}),"\n",(0,t.jsxs)(n.li,{children:["Future points in time can ",(0,t.jsx)(n.strong,{children:"only"}),' be converted in the "safe zone", practically being at least ',(0,t.jsx)(n.code,{children:"3k/f"})," slots (TODO: cross check). Refer to chapter 17 ",(0,t.jsx)(n.em,{children:"Time"})," on the ",(0,t.jsx)(n.a,{href:"https://hydra.iohk.io/build/16997794/download/1/report.pdf",children:"consensus spec"})," for more details."]}),"\n"]}),"\n"]}),"\n",(0,t.jsxs)(n.li,{children:["\n",(0,t.jsxs)(n.p,{children:["The Hydra Head protocol allows ",(0,t.jsx)(n.code,{children:"close"})," and ",(0,t.jsx)(n.code,{children:"contest"})," transactions only up before a deadline ",(0,t.jsx)(n.code,{children:"T_final"}),", and ",(0,t.jsx)(n.code,{children:"fanout"})," transactions after the deadline."]}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:["In the current implementation the deadline is upper validity of ",(0,t.jsx)(n.code,{children:"closed"})," plus the contestation period."]}),"\n",(0,t.jsxs)(n.li,{children:["We also consider protocol variants which push out the deadline by the contestation period on each ",(0,t.jsx)(n.code,{children:"contest"}),"."]}),"\n",(0,t.jsx)(n.li,{children:"Contestation periods may very well be longer than the stability window of the protocol. For example: 7 days, while the mainnet stability window is more like 36 hours."}),"\n"]}),"\n"]}),"\n",(0,t.jsxs)(n.li,{children:["\n",(0,t.jsx)(n.p,{children:"We have encountered two problems with handling time in the past"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:["Trying to convert wall-clock time to slots of the Head protocol deadline led to ",(0,t.jsx)(n.code,{children:"PastHorizonException"})," (when using very low security parameter ",(0,t.jsx)(n.code,{children:"k"}),")"]}),"\n",(0,t.jsxs)(n.li,{children:["Trying to ",(0,t.jsx)(n.code,{children:"fanout"})," after the deadline, but before another block has been seen by the L1 ledger led to ",(0,t.jsx)(n.code,{children:"OutsideValidityIntervalUTxO"}),"."]}),"\n"]}),"\n"]}),"\n",(0,t.jsxs)(n.li,{children:["\n",(0,t.jsx)(n.p,{children:"The second problem scenario and solution ideas are roughly visible on this whiteboard:"}),"\n"]}),"\n"]}),"\n",(0,t.jsx)(n.p,{children:(0,t.jsx)(n.img,{src:s(84655).A+"",width:"1005",height:"1067"})}),"\n",(0,t.jsx)(n.h2,{id:"decision",children:"Decision"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:["\n",(0,t.jsx)(n.p,{children:"The head logic uses wall-clock time to track time and only convert to/from slots when constructing/observing transactions in the chain layer."}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"This ensures that transactions we post or see on the chain can be converted to/from slots."}),"\n",(0,t.jsxs)(n.li,{children:["The head logic would use ",(0,t.jsx)(n.code,{children:"UTCTime"})," for points in time and ",(0,t.jsx)(n.code,{children:"NominalDiffTime"})," for durations."]}),"\n",(0,t.jsxs)(n.li,{children:["The chain layer converts these using the ",(0,t.jsx)(n.code,{children:"SystemStart"})," and ",(0,t.jsx)(n.code,{children:"EraHistory"})," into ",(0,t.jsx)(n.code,{children:"SlotNo"}),"."]}),"\n"]}),"\n"]}),"\n",(0,t.jsxs)(n.li,{children:["\n",(0,t.jsxs)(n.p,{children:["The chain layer informs the logic layer whenever time passed (on the chain) using a new ",(0,t.jsx)(n.code,{children:"Tick"})," event."]}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsx)(n.li,{children:"For the direct chain implementation, this is whenever we see a block in the chain sync protocol."}),"\n",(0,t.jsxs)(n.li,{children:["Per above decision, the ",(0,t.jsx)(n.code,{children:"Tick"})," shall contain a ",(0,t.jsx)(n.code,{children:"UTCTime"}),' corresponding to the new "now" as seen through the block chain.']}),"\n"]}),"\n"]}),"\n"]}),"\n",(0,t.jsx)(n.h2,{id:"consequences",children:"Consequences"}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:["\n",(0,t.jsxs)(n.p,{children:["Conversion from ",(0,t.jsx)(n.code,{children:"UTCTime -> SlotNo"})," and vice versa stays local to the chain layer."]}),"\n"]}),"\n",(0,t.jsxs)(n.li,{children:["\n",(0,t.jsxs)(n.p,{children:["The ",(0,t.jsx)(n.code,{children:"HeadLogic"})," can track chain time in its state and condition ",(0,t.jsx)(n.code,{children:"ReadyToFanout"})," upon seeing it pass the deadline."]}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:["Ensures clients only see ",(0,t.jsx)(n.code,{children:"ReadyToFanout"})," when a following ",(0,t.jsx)(n.code,{children:"Fanout"})," would be really possible."]}),"\n",(0,t.jsxs)(n.li,{children:["Makes the ",(0,t.jsx)(n.code,{children:"Delay"})," effect redundant and we can remove it (only delay via reenqueue on the ",(0,t.jsx)(n.code,{children:"Wait"})," outcome)"]}),"\n"]}),"\n"]}),"\n",(0,t.jsxs)(n.li,{children:["\n",(0,t.jsxs)(n.p,{children:["By introducing ",(0,t.jsx)(n.code,{children:"Tick"})," events, ",(0,t.jsx)(n.code,{children:"IOSim"})," will not be able to detect non-progress (deadlocks)."]}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:["This means we cannot rely on early exit of simulations anymore and need to determine meaningful simulation endings instead of ",(0,t.jsx)(n.code,{children:"waitUntilTheEndOfTime"}),"."]}),"\n"]}),"\n"]}),"\n",(0,t.jsxs)(n.li,{children:["\n",(0,t.jsx)(n.p,{children:'We get a first, rough notion of time for free in our L2 and can support "timed transactions" with same resolution as the L1.'}),"\n",(0,t.jsxs)(n.ul,{children:["\n",(0,t.jsxs)(n.li,{children:["Tracking time in the state makes it trivial to provide it to the ledger when we ",(0,t.jsx)(n.code,{children:"applyTransaction"}),"."]}),"\n",(0,t.jsxs)(n.li,{children:['Of course we could extend the fidelity of this feature using the system clock for "dead reckoning" between blocks. The conversion of wall clock to slot could even be configurable using an L2 ',(0,t.jsx)(n.code,{children:"slotLength"})," analogous to L1 (although we might not want/need this)."]}),"\n"]}),"\n"]}),"\n"]})]})}function h(e={}){const{wrapper:n}={...(0,o.R)(),...e.components};return n?(0,t.jsx)(n,{...e,children:(0,t.jsx)(a,{...e})}):a(e)}},84655:(e,n,s)=>{s.d(n,{A:()=>i});const i=s.p+"assets/images/020-timing-fanout-ab1b5156cfc3fa34b570aa3eec42d0dd.jpg"},28453:(e,n,s)=>{s.d(n,{R:()=>l,x:()=>c});var i=s(96540);const t={},o=i.createContext(t);function l(e){const n=i.useContext(o);return i.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function c(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(t):e.components||t:l(e.components),i.createElement(o.Provider,{value:n},e.children)}},28112:e=>{e.exports=JSON.parse('{"permalink":"/head-protocol/unstable/adr/20","source":"@site/adr/2022-08-02_020-handling-time.md","title":"20. Handling time\\n","description":"Status","date":"2022-08-02T00:00:00.000Z","tags":[{"inline":true,"label":"Accepted","permalink":"/head-protocol/unstable/adr/tags/accepted"}],"readingTime":3.55,"hasTruncateMarker":false,"authors":[],"frontMatter":{"slug":"20","title":"20. Handling time\\n","authors":[],"tags":["Accepted"]},"unlisted":false,"prevItem":{"title":"19. Use of reference scripts\\n","permalink":"/head-protocol/unstable/adr/19"},"nextItem":{"title":"21. Bounded transaction validity on Hydra protocol transactions\\n","permalink":"/head-protocol/unstable/adr/21"}}')}}]);