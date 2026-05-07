import HxlFormal.DataSources

namespace HxlFormal

universe u

inductive Requests (ds : DataSources.{u}) : Type u -> Type (u + 1) where
  | pure {α : Type u} (value : α) : Requests ds α
  | one (source : ds.Source) (key : ds.Key source) :
      Requests ds (Option (ds.Value source))
  | ap {α β : Type u} :
      Requests ds (α -> β) -> Requests ds α -> Requests ds β

namespace Requests

variable {ds : DataSources.{u}}

instance : Applicative (Requests ds) where
  pure := Requests.pure
  seq fs xs := Requests.ap fs (xs ())
  map f request := Requests.ap (Requests.pure f) request

def atoms : {α : Type u} -> Requests ds α -> List (Atom ds)
  | _, Requests.pure _ => []
  | _, Requests.one source key => [⟨source, key⟩]
  | _, Requests.ap fs xs => atoms fs ++ atoms xs

@[simp]
theorem atoms_pure (value : α) :
    atoms (Requests.pure (ds := ds) value) = [] :=
  rfl

@[simp]
theorem atoms_ap
    (fs : Requests ds (α -> β)) (xs : Requests ds α) :
    atoms (Requests.ap fs xs) = atoms fs ++ atoms xs :=
  rfl

@[simp]
theorem atoms_one
    (source : ds.Source) (key : ds.Key source) :
    atoms (Requests.one source key) = [⟨source, key⟩] :=
  rfl

end Requests

end HxlFormal
