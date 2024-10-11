use aiken/collection/list
use cardano/assets.{PolicyId, Value}
use cardano/transaction.{OutputReference, Transaction}

pub type Redeemer {
  ViaAbort
  ViaCommit(List<OutputReference>)
}

const hydra_head_v1: ByteArray = "HydraHeadV1"

type InitialError {
  STNotBurned
  MissingOrInvalidCommitAuthor
  LockedValueDoesNotMatch
  MismatchCommittedTxOutInDatum
  CouldNotFindTheCorrectCurrencySymbolInTokens
  MultipleHeadTokensOrMoreThan1PTsFound
  MissingCommittedTxOutInOutputDatum
  CommittedTxOutMissingInOutputDatum
  MissingDatum
  UnexpectedNonInlineDatum
  ExpectedCommitDatumTypeGotSomethingElse
  ExpectedSingleCommitOutput
  WrongHeadIdInCommitDatum
  MintingOrBurningIsForbidden
  OutRefNotFound
}

fn toErrorCode(err: InitialError) -> String {
  when err is {
    STNotBurned -> @"I01"
    MissingOrInvalidCommitAuthor -> @"I02"
    LockedValueDoesNotMatch -> @"I03"
    MismatchCommittedTxOutInDatum -> @"I04"
    CouldNotFindTheCorrectCurrencySymbolInTokens -> @"I05"
    MultipleHeadTokensOrMoreThan1PTsFound -> @"I06"
    MissingCommittedTxOutInOutputDatum -> @"I07"
    CommittedTxOutMissingInOutputDatum -> @"I08"
    MissingDatum -> @"I09"
    UnexpectedNonInlineDatum -> @"I10"
    ExpectedCommitDatumTypeGotSomethingElse -> @"I11"
    ExpectedSingleCommitOutput -> @"I12"
    WrongHeadIdInCommitDatum -> @"I13"
    MintingOrBurningIsForbidden -> @"I14"
    OutRefNotFound -> @"I15"
  }
}

// | The validator used to collect & open or abort a Head.
// | The v_commit validator verifies that:
//
//   * spent in a transaction also consuming a v_head output
//
//   * ST is burned if the redeemer is 'ViaAbort'
//
//   * ST is present in the output if the redeemer is 'ViaCollectCom'
validator commit {
  spend(datum: Option<PolicyId>, redeemer: Redeemer, _utxo: OutputReference, self: Transaction) -> Bool {
    expect Some(datum) = datum
    when redeemer is {
      // NOTE: The reimbursement of the committed output 'commit' is
      // delegated to the 'head' script who has more information to do it.
      ViaAbort -> True
      ViaCommit(committedRefs) -> True
    }
  }
}

fn has_st(head_id: PolicyId, value: Value) -> Bool {
  assets.quantity_of(value, head_id, hydra_head_v1) == 1
}

fn must_burn_st(head_id: PolicyId, mint: Value) -> Bool {
  let quantity =
    mint
      |> assets.quantity_of(head_id, hydra_head_v1)

  quantity == -1
}

// Helpers

fn traceIfFalse(traceLog: String, predicate: Bool) -> Bool {
  if predicate {
    True
  } else {
    trace traceLog
    False
  }
}
