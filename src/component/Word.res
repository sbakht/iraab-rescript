type translated = {text: string}

module Status = {
  type t = Rafa | Nasb | Jar

  let toString = (status: t) => {
    switch status {
    | Rafa => "Rafa"
    | Nasb => "Nasb"
    | Jar => "Jar"
    }
  }
}

module Dual = {
  type t = Reason1 | Reason2
}

module Plural = {
  type t = Reason1 | Reason2 | Reason3
}

module Number = {
  type t = Singular | Dual(Dual.t) | Plural(Plural.t)
}

module Proper = {
  type t =
    | Reason1
    | Reason2
    | Reason3
    | Reason4
    | Reason5
    | Reason6
    | Reason7

  let toString = (reason: t) => {
    let str = switch reason {
    | Reason1 => "Word has al"
    | Reason2 => "Pronoun"
    | Reason3 => "Pointer"
    | Reason4 => "The one being called"
    | Reason5 => "Mudaf when mdafilayhi is proper"
    | Reason6 => "Specific person"
    | Reason7 => "Name of a person"
    }

    {text: str}
  }
}

module Commonality = {
  type t = Proper(Proper.t) | Common

  let toString = (t: t) => {
    switch t {
    | Common => {text: "Common"}
    | Proper(_) => {text: "Proper"}
    }
  }
}

module Feminine = {
  type t =
    | Reason1
    | Reason2
    | Reason3
    | Reason4
    | Reason5

  let toString = (reason: t) => {
    let str = switch reason {
    | Reason1 => ""
    | Reason2 => ""
    | Reason3 => ""
    | Reason4 => ""
    | Reason5 => ""
    }

    {text: str}
  }
}

module Gender = {
  type t = Masculine | Feminine(Feminine.t)

  let toString = (t: t) => {
    switch t {
    | Masculine => "Masculine"
    | Feminine(_) => "Feminine"
    }
  }
}

type word = {
  text: string,
  status: Status.t,
  number: Number.t,
  gender: Gender.t,
  commonality: Commonality.t,
}
