"use strict";(self.webpackChunkhydra_head_protocol_docs=self.webpackChunkhydra_head_protocol_docs||[]).push([[8661],{35556:(e,n,s)=>{s.r(n),s.d(n,{assets:()=>a,contentTitle:()=>l,default:()=>h,frontMatter:()=>d,metadata:()=>t,toc:()=>o});const t=JSON.parse('{"id":"tests/hydra-tx/hspec-results","title":"Test results for hydra-tx","description":"Hydra.Tx.IsTx","source":"@site/benchmarks/tests/hydra-tx/hspec-results.md","sourceDirName":"tests/hydra-tx","slug":"/tests/hydra-tx/hspec-results","permalink":"/head-protocol/unstable/benchmarks/tests/hydra-tx/hspec-results","draft":false,"unlisted":false,"tags":[],"version":"current","frontMatter":{},"sidebar":"defaultSidebar","previous":{"title":"Test results for hydra-tui","permalink":"/head-protocol/unstable/benchmarks/tests/hydra-tui/hspec-results"}}');var i=s(74848),r=s(28453);const d={},l="Test results for hydra-tx",a={},o=[{value:"Hydra.Tx.IsTx",id:"hydratxistx",level:2},{value:"Tx",id:"tx",level:3},{value:"JSON encoding of (ReasonablySized (Tx ConwayEra))",id:"json-encoding-of-reasonablysized-tx-conwayera",level:4},{value:"UTxO",id:"utxo",level:3},{value:"JSON encoding of (UTxO&#39; (TxOut CtxUTxO ConwayEra))",id:"json-encoding-of-utxo-txout-ctxutxo-conwayera",level:4},{value:"Hydra.Tx.Contract.Contract",id:"hydratxcontractcontract",level:2},{value:"Fanout",id:"fanout",level:3},{value:"ContestDec",id:"contestdec",level:3},{value:"ContestCurrent",id:"contestcurrent",level:3},{value:"CloseUsedDec",id:"closeuseddec",level:3},{value:"CloseUnusedDec",id:"closeunuseddec",level:3},{value:"CloseInitial",id:"closeinitial",level:3},{value:"Recover",id:"recover",level:3},{value:"Deposit",id:"deposit",level:3},{value:"Decrement",id:"decrement",level:3},{value:"Increment",id:"increment",level:3},{value:"CollectCom",id:"collectcom",level:3},{value:"Commit",id:"commit",level:3},{value:"Abort",id:"abort",level:3},{value:"Init",id:"init",level:3},{value:"Serializing commits",id:"serializing-commits",level:3},{value:"TxOut hashing",id:"txout-hashing",level:3},{value:"Signature validator",id:"signature-validator",level:3}];function c(e){const n={code:"code",h1:"h1",h2:"h2",h3:"h3",h4:"h4",header:"header",li:"li",p:"p",pre:"pre",ul:"ul",...(0,r.R)(),...e.components},{Details:s}=n;return s||function(e,n){throw new Error("Expected "+(n?"component":"object")+" `"+e+"` to be defined: you likely forgot to import, pass, or provide it.")}("Details",!0),(0,i.jsxs)(i.Fragment,{children:[(0,i.jsx)(n.header,{children:(0,i.jsx)(n.h1,{id:"test-results-for-hydra-tx",children:"Test results for hydra-tx"})}),"\n",(0,i.jsx)(n.h2,{id:"hydratxistx",children:"Hydra.Tx.IsTx"}),"\n",(0,i.jsx)(n.h3,{id:"tx",children:"Tx"}),"\n",(0,i.jsx)(n.h4,{id:"json-encoding-of-reasonablysized-tx-conwayera",children:"JSON encoding of (ReasonablySized (Tx ConwayEra))"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"allows to encode values with aeson and read them back"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 100 tests.\n"})})]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"produces the same JSON as is found in golden/ReasonablySized (Tx ConwayEra).json"}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"Same TxId before/after JSON encoding"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 100 tests.\n"})})]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"Same TxId as TxBody after JSON decoding"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 100 tests.\n"})})]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"Roundtrip to and from Ledger"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 100 tests.\n"})})]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"Roundtrip CBOR encoding"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 100 tests.\n"})})]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"JSON decode Babbage era transactions"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 100 tests.\n"})})]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"CBOR decode Babbage era transactions"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 100 tests.\n"})})]}),"\n"]}),"\n"]}),"\n",(0,i.jsx)(n.h3,{id:"utxo",children:"UTxO"}),"\n",(0,i.jsx)(n.h4,{id:"json-encoding-of-utxo-txout-ctxutxo-conwayera",children:"JSON encoding of (UTxO' (TxOut CtxUTxO ConwayEra))"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"allows to encode values with aeson and read them back"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 100 tests.\n"})})]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"produces the same JSON as is found in golden/UTxO' (TxOut CtxUTxO ConwayEra).json"}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"Roundtrip to and from Api"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 100 tests.\n"})})]}),"\n"]}),"\n"]}),"\n",(0,i.jsx)(n.h2,{id:"hydratxcontractcontract",children:"Hydra.Tx.Contract.Contract"}),"\n",(0,i.jsx)(n.h3,{id:"fanout",children:"Fanout"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"is healthy"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 100 tests.\n"})})]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"does not survive random adversarial mutations"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 100 tests.\n\nFanoutMutation (100 in total):\n23% MutateDecommitOutputValue\n22% MutateValidityBeforeDeadline\n21% MutateThreadTokenQuantity\n19% MutateAddUnexpectedOutput\n15% MutateFanoutOutputValue\n"})})]}),"\n"]}),"\n"]}),"\n",(0,i.jsx)(n.h3,{id:"contestdec",children:"ContestDec"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"is healthy"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 100 tests.\n"})})]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"does not survive random adversarial mutations"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 200 tests.\n\nContestDecMutation (200 in total):\n22.0% ContestUsedDecAlterRedeemerDecommitHash\n18.5% ContestUnusedDecMutateSnapshotVersion\n17.5% ContestUnusedDecAlterDatumomegaUTxOHash\n16.0% ContestUnusedDecAlterRedeemerDecommitHash\n15.5% ContestUsedDecMutateSnapshotVersion\n10.5% ContestUsedDecAlterDatumomegaUTxOHash\n"})})]}),"\n"]}),"\n"]}),"\n",(0,i.jsx)(n.h3,{id:"contestcurrent",children:"ContestCurrent"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"is healthy"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 100 tests.\n"})})]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"does not survive random adversarial mutations"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 400 tests.\n\nContestMutation (400 in total):\n 7.2% MutateContestUTxOHash\n 6.5% MutateContesters\n 6.0% MutateHeadIdInOutput\n 5.8% MutateMultipleRequiredSigner\n 5.5% MutateNoRequiredSigner\n 5.5% MutateOutputContestationPeriod\n 5.5% NotContinueContract\n 5.2% MutatePartiesInOutput\n 5.0% NotUpdateDeadlineAlthoughItShould\n 4.8% MutateToNonNewerSnapshot\n 4.5% MutateInputContesters\n 4.5% MutateSignatureButNotSnapshotNumber\n 4.5% MutateSnapshotNumberButNotSignature\n 4.5% MutateTokenMintingOrBurning\n 4.2% MutateValidityPastDeadline\n 3.8% ContestFromDifferentHead\n 3.8% MutateRequiredSigner\n 3.5% PushDeadlineAlthoughItShouldNot\n 3.5% SnapshotNotSignedByAllParties\n 3.2% MutateValueInOutput\n 3.0% MutateSnapshotVersion\n"})})]}),"\n"]}),"\n"]}),"\n",(0,i.jsx)(n.h3,{id:"closeuseddec",children:"CloseUsedDec"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"is healthy"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 100 tests.\n"})})]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"does not survive random adversarial mutations"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 400 tests.\n\nCloseMutation (400 in total):\n 6.0% MutateCloseSignatures\n 5.8% MutateNoRequiredSigner\n 5.8% MutateValidityInterval\n 5.5% NotContinueContract\n 5.2% MutateValueInOutput\n 5.0% MutateCloseUTxOToDecommitHash\n 5.0% MutateRequiredSigner\n 4.5% MutateCloseType\n 4.5% MutateInfiniteLowerBound\n 4.5% MutateInfiniteUpperBound\n 4.5% MutateSnapshotNumberButNotSignature\n 4.5% MutateTokenMintingOrBurning\n 4.5% SnapshotNotSignedByAllParties\n 4.0% MutateContestationPeriod\n 3.8% MutateMultipleRequiredSigner\n 3.8% MutatePartiesInOutput\n 3.8% MutateSignatureButNotSnapshotNumber\n 3.5% MutateCloseUTxOHash\n 3.5% MutateHeadIdInOutput\n 3.2% CloseFromDifferentHead\n 3.2% MutateContesters\n 3.0% MutateContestationDeadline\n 3.0% MutateSnapshotVersion\n"})})]}),"\n"]}),"\n"]}),"\n",(0,i.jsx)(n.h3,{id:"closeunuseddec",children:"CloseUnusedDec"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"is healthy"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 100 tests.\n"})})]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"does not survive random adversarial mutations"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 400 tests.\n\nCloseMutation (400 in total):\n 6.8% MutateValidityInterval\n 6.2% MutateNoRequiredSigner\n 6.0% MutateCloseUTxOToDecommitHash\n 6.0% NotContinueContract\n 5.8% MutateRequiredSigner\n 5.5% MutateSignatureButNotSnapshotNumber\n 5.5% MutateValueInOutput\n 5.0% MutateTokenMintingOrBurning\n 4.8% MutateInfiniteLowerBound\n 4.8% MutateSnapshotNumberButNotSignature\n 4.8% SnapshotNotSignedByAllParties\n 4.5% MutateContestationPeriod\n 4.5% MutateHeadIdInOutput\n 4.5% MutateInfiniteUpperBound\n 4.2% MutateMultipleRequiredSigner\n 3.8% MutateCloseUTxOHash\n 3.8% MutateContesters\n 3.8% MutatePartiesInOutput\n 3.5% CloseFromDifferentHead\n 3.5% MutateContestationDeadline\n 3.0% MutateSnapshotVersion\n"})})]}),"\n"]}),"\n"]}),"\n",(0,i.jsx)(n.h3,{id:"closeinitial",children:"CloseInitial"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"is healthy"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 100 tests.\n"})})]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"does not survive random adversarial mutations"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 400 tests.\n\nCloseInitialMutation (400 in total):\n100.0% MutateCloseContestationDeadline'\n"})})]}),"\n"]}),"\n"]}),"\n",(0,i.jsx)(n.h3,{id:"recover",children:"Recover"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"is healthy"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 100 tests.\n"})})]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"does not survive random adversarial mutations"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 100 tests.\n\nRecoverMutation (100 in total):\n37% MutateRecoverOutput\n32% RemoveTxValidityLowerBound\n31% MutateDepositDeadline\n"})})]}),"\n"]}),"\n"]}),"\n",(0,i.jsx)(n.h3,{id:"deposit",children:"Deposit"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"is healthy"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 100 tests.\n"})})]}),"\n"]}),"\n"]}),"\n",(0,i.jsx)(n.h3,{id:"decrement",children:"Decrement"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"is healthy"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 100 tests.\n"})})]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"does not survive random adversarial mutations"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 200 tests.\n\nDecrementMutation (200 in total):\n16.0% DropDecommitOutput\n14.5% ChangeDecrementedValue\n14.5% UseDifferentSnapshotVersion\n13.0% ChangeHeadValue\n12.5% ChangePartiesInOuput\n12.5% ExtractSomeValue\n 9.0% AlterRequiredSigner\n 8.0% ProduceInvalidSignatures\n"})})]}),"\n"]}),"\n"]}),"\n",(0,i.jsx)(n.h3,{id:"increment",children:"Increment"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"is healthy"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 100 tests.\n"})})]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"does not survive random adversarial mutations"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 200 tests.\n\nIncrementMutation (200 in total):\n16.0% AlterRequiredSigner\n14.5% DepositMutateHeadId\n14.5% ProduceInvalidSignatures\n13.0% ChangeHeadValue\n12.5% DepositMutateDepositDeadline\n12.5% IncrementDifferentClaimRedeemer\n 9.0% IncrementUseDifferentSnapshotVersion\n 8.0% IncrementMutateParties\n"})})]}),"\n"]}),"\n"]}),"\n",(0,i.jsx)(n.h3,{id:"collectcom",children:"CollectCom"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"is healthy"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 100 tests.\n"})})]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"does not survive random adversarial mutations"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 200 tests.\n\nCollectComMutation (200 in total):\n14.5% MutateNumberOfParties\n12.5% MutateTokenMintingOrBurning\n10.5% MutateCommitToInitial\n10.5% MutateHeadId\n10.5% RemoveSTFromOutput\n10.0% ExtractSomeValue\n10.0% MutateRequiredSigner\n 8.0% MutateOpenVersion\n 7.5% NotContinueContract\n 6.0% MutateOpenUTxOHash\n"})})]}),"\n"]}),"\n"]}),"\n",(0,i.jsx)(n.h3,{id:"commit",children:"Commit"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"is healthy"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 100 tests.\n"})})]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"does not survive random adversarial mutations"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 200 tests.\n\nCommitMutation (200 in total):\n16.0% UsePTFromDifferentHead\n14.5% MutateCommitOutputValue\n14.5% RecordAllCommittedUTxO\n13.0% MutateRequiredSigner\n12.5% MutateTokenMintingOrBurning\n12.5% NonContinuousHeadId\n 9.0% MutateCommittedAddress\n 8.0% MutateCommittedValue\n"})})]}),"\n"]}),"\n"]}),"\n",(0,i.jsx)(n.h3,{id:"abort",children:"Abort"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"is healthy"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 100 tests.\n"})})]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"does not survive random adversarial mutations"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 200 tests.\n\nAbortMutation (200 in total):\n12.5% BurnOneTokenMore\n10.0% ReorderCommitOutputs\n 9.0% MutateRequiredSigner\n 8.5% MintOnAbort\n 8.0% DropCollectedInput\n 8.0% UseInputFromOtherHead\n 7.0% DoNotBurnSTInitial\n 7.0% MutateThreadTokenQuantity\n 7.0% MutateUseDifferentHeadToAbort\n 6.5% DoNotBurnST\n 6.0% ExtractValue\n 6.0% MutateParties\n 4.5% DropOneCommitOutput\n"})})]}),"\n"]}),"\n"]}),"\n",(0,i.jsx)(n.h3,{id:"init",children:"Init"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"is healthy"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 100 tests.\n"})})]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"does not survive random adversarial mutations"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 200 tests.\n\nInitMutation (200 in total):\n16.0% MutateHeadIdInInitialDatum\n14.5% MutateAddAnotherPT\n14.5% MutateDropSeedInput\n13.0% MutateHeadIdInDatum\n12.5% MintTooManyTokens\n12.5% MutateSeedInDatum\n 9.0% MutateDropInitialOutput\n 8.0% MutateInitialOutputValue\n"})})]}),"\n"]}),"\n"]}),"\n",(0,i.jsx)(n.h3,{id:"serializing-commits",children:"Serializing commits"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"deserializeCommit . serializeCommit === id"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 100 tests.\n"})})]}),"\n"]}),"\n"]}),"\n",(0,i.jsx)(n.h3,{id:"txout-hashing",children:"TxOut hashing"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"hashUTxO == OnChain.hashTxOuts (on sorted tx outs)"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 20 tests.\n"})})]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"OnChain.hashPreSerializedCommits == OnChain.hashTxOuts (on sorted tx outs)"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 20 tests.\n"})})]}),"\n"]}),"\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"does care about ordering of TxOut"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 20 tests; 24 discarded.\n"})})]}),"\n"]}),"\n"]}),"\n",(0,i.jsx)(n.h3,{id:"signature-validator",children:"Signature validator"}),"\n",(0,i.jsxs)(n.ul,{children:["\n",(0,i.jsxs)(n.li,{children:["\n",(0,i.jsx)(n.p,{children:"verifies snapshot multi-signature"}),"\n",(0,i.jsxs)(s,{children:[(0,i.jsx)("summary",{children:"Details"}),(0,i.jsx)(n.pre,{children:(0,i.jsx)(n.code,{children:"+++ OK, passed 100 tests.\n"})})]}),"\n"]}),"\n"]})]})}function h(e={}){const{wrapper:n}={...(0,r.R)(),...e.components};return n?(0,i.jsx)(n,{...e,children:(0,i.jsx)(c,{...e})}):c(e)}},28453:(e,n,s)=>{s.d(n,{R:()=>d,x:()=>l});var t=s(96540);const i={},r=t.createContext(i);function d(e){const n=t.useContext(r);return t.useMemo((function(){return"function"==typeof e?e(n):{...n,...e}}),[n,e])}function l(e){let n;return n=e.disableParentContext?"function"==typeof e.components?e.components(i):e.components||i:d(e.components),t.createElement(r.Provider,{value:n},e.children)}}}]);