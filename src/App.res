@react.component
let make = () => {

  let status = Word.Status.Rafa

  let x = Word.Proper.Reason5

  let proper = Word.Commonality.toString(Word.Commonality.Proper(x))
  let reason = Word.Proper.toString(x)
  let common = Word.Commonality.toString(Word.Commonality.Common)

  <>
    <div> {"bye"->React.string} </div>
    <div> {status->Word.Status.toString->React.string} </div>
    <div> {proper.text->React.string} </div>
    <div> {reason.text->React.string} </div>
    <div> {common.text->React.string} </div>
  </>
}
