module StackTrace where


data StackTrace = StackTrace { getNoLines :: Int, getMsg :: ShowS, getTopLine :: ShowS }

stackTraceLimit :: Int
stackTraceLimit = 50

newST :: ShowS -> StackTrace
newST msg = StackTrace 1 msg msg

appendST :: StackTrace -> ShowS -> StackTrace
appendST (StackTrace noLines msg topLine) newLineMsg =
  if noLines >= stackTraceLimit then
    StackTrace (noLines + 1) msg newLineMsg
  else
    StackTrace (noLines + 1) (msg . showString "\n" . newLineMsg) newLineMsg

stackTraceToString :: StackTrace -> String
stackTraceToString (StackTrace noLines msg topLine) =
  if noLines <= stackTraceLimit then
    msg ""
  else if noLines == stackTraceLimit + 1 then
    (msg . showString "\n" . topLine) ""
  else
    (msg . showString "\n...\n" . topLine) ""
