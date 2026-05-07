namespace HxlFormal

universe u

structure Errors where
  Tag : Type u
  Error : Tag -> Type u

inductive Raised (errors : Errors.{u}) : Type u where
  | mk (tag : errors.Tag) (error : errors.Error tag) : Raised errors

namespace Raised

variable {errors : Errors.{u}}

def errorsAt [DecidableEq errors.Tag] (tag : errors.Tag) :
    List (Raised errors) -> List (errors.Error tag)
  | [] => []
  | mk tag' error :: raised =>
      if h : tag' = tag then
        (h ▸ error) :: errorsAt tag raised
      else
        errorsAt tag raised

end Raised

end HxlFormal
