namespace FSKit

module Version =
  let [<Literal>] Major = 0
  let [<Literal>] Minor = 0
  let [<Literal>] Build = 2
  let [<Literal>] Revision = 0
  let [<Literal>] Tag = "alpha"
  let [<Literal>] String = "0.0.2.0"
  let Tupled = (0, 0, 2, 0, Tag)
  let Tagged = sprintf "%s-%s" String Tag
  let FullName = sprintf "FSKit %s" String
