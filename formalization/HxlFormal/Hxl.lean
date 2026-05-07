import HxlFormal.Errors
import HxlFormal.Requests

namespace HxlFormal

universe u

inductive Hxl (ds : DataSources.{u}) (errors : Errors.{u}) :
    Type u -> Type (u + 1) where
  | run {α : Type u} (requests : Requests ds α) : Hxl ds errors α
  | bind {α β : Type u} :
      Hxl ds errors α -> (α -> Hxl ds errors β) -> Hxl ds errors β
  | errs {α : Type u} (raised : List (Raised errors)) : Hxl ds errors α

namespace Hxl

variable {ds : DataSources.{u}}
variable {errors : Errors.{u}}

def pure (value : α) : Hxl ds errors α :=
  Hxl.run (Requests.pure value)

def map (f : α -> β) (query : Hxl ds errors α) : Hxl ds errors β :=
  Hxl.bind query (fun value => Hxl.pure (f value))

def raise (tag : errors.Tag) (error : errors.Error tag) : Hxl ds errors α :=
  Hxl.errs [Raised.mk tag error]

instance : Pure (Hxl ds errors) where
  pure := Hxl.pure

instance : Bind (Hxl ds errors) where
  bind query next := Hxl.bind query next

inductive Apply :
    Hxl ds errors (α -> β) -> Hxl ds errors α ->
    Hxl ds errors β -> Prop where
  | run_run (fs : Requests ds (α -> β)) (xs : Requests ds α) :
      Apply (Hxl.run fs) (Hxl.run xs) (Hxl.run (fs <*> xs))
  | bind_bind
      {γ δ : Type u}
      {fs : Hxl ds errors γ} {xs : Hxl ds errors δ}
      {fsK : γ -> Hxl ds errors (α -> β)}
      {xsK : δ -> Hxl ds errors α}
      {head : Hxl ds errors (γ × δ)}
      {tail : γ × δ -> Hxl ds errors β} :
      Apply (Hxl.map Prod.mk fs) xs head ->
      (∀ value, Apply (fsK value.1) (xsK value.2) (tail value)) ->
      Apply (Hxl.bind fs fsK) (Hxl.bind xs xsK) (Hxl.bind head tail)
  | bind_left
      {γ : Type u}
      {fs : Hxl ds errors γ} {xs : Hxl ds errors α}
      {fsK : γ -> Hxl ds errors (α -> β)}
      {head : Hxl ds errors (γ × α)}
      {tail : γ × α -> Hxl ds errors β} :
      Apply (Hxl.map Prod.mk fs) xs head ->
      (∀ value, Apply (fsK value.1) (Hxl.pure value.2) (tail value)) ->
      Apply (Hxl.bind fs fsK) xs (Hxl.bind head tail)
  | bind_right
      {δ : Type u}
      {fs : Hxl ds errors (α -> β)} {xs : Hxl ds errors δ}
      {xsK : δ -> Hxl ds errors α}
      {head : Hxl ds errors ((α -> β) × δ)}
      {tail : ((α -> β) × δ) -> Hxl ds errors β} :
      Apply (Hxl.map Prod.mk fs) xs head ->
      (∀ value, Apply (Hxl.pure value.1) (xsK value.2) (tail value)) ->
      Apply fs (Hxl.bind xs xsK) (Hxl.bind head tail)

end Hxl

end HxlFormal
