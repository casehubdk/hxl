import HxlFormal.Hxl

namespace HxlFormal

universe u

namespace Hxl

variable {ds : DataSources.{u}}
variable {errors : Errors.{u}}

inductive Catch [DecidableEq errors.Tag] (tag : errors.Tag) :
    Hxl ds errors α ->
    Hxl ds errors (Except (List (errors.Error tag)) α) ->
    Prop where
  | pure (value : α) :
      Catch tag (Hxl.pure value) (Hxl.pure (Except.ok value))
  | run (requests : Requests ds α) :
      Catch tag (Hxl.run requests) (Hxl.run (Except.ok <$> requests))
  | errs_found (raised : List (Raised errors)) :
      Raised.errorsAt (errors := errors) tag raised ≠ [] ->
      Catch tag (Hxl.errs raised)
        (Hxl.pure
          (Except.error (Raised.errorsAt (errors := errors) tag raised)))
  | errs_foreign (raised : List (Raised errors)) :
      Raised.errorsAt (errors := errors) tag raised = [] ->
      Catch tag (Hxl.errs raised) (Hxl.errs raised)
  | bind
      {query : Hxl ds errors α}
      {next : α -> Hxl ds errors β}
      {head : Hxl ds errors (Except (List (errors.Error tag)) α)}
      {tail : α -> Hxl ds errors (Except (List (errors.Error tag)) β)} :
      Catch tag query head ->
      (∀ value, Catch tag (next value) (tail value)) ->
      Catch tag (Hxl.bind query next)
        (Hxl.bind head fun result =>
          match result with
          | Except.error caught => Hxl.pure (Except.error caught)
          | Except.ok value => tail value)

theorem catch_raise_self [DecidableEq errors.Tag]
    (tag : errors.Tag) (error : errors.Error tag) :
    Catch (ds := ds) (errors := errors) (α := α)
      tag (Hxl.raise tag error) (Hxl.pure (Except.error [error])) := by
  simpa [Hxl.raise, Raised.errorsAt] using
    (Catch.errs_found (ds := ds) (errors := errors) (tag := tag)
      [Raised.mk tag error]
      (by simp [Raised.errorsAt]) :
      Catch tag
        (Hxl.errs [Raised.mk tag error])
        (Hxl.pure
          (Except.error (Raised.errorsAt (errors := errors) tag
            [Raised.mk tag error]))))

theorem catch_raise_other [DecidableEq errors.Tag]
    {wanted actual : errors.Tag} (h : actual ≠ wanted)
    (error : errors.Error actual) :
    Catch (ds := ds) (errors := errors) (α := α)
      wanted (Hxl.raise actual error)
      (Hxl.errs [Raised.mk actual error]) := by
  apply Catch.errs_foreign
  simp [Raised.errorsAt, h]

theorem catch_inner_wins [DecidableEq errors.Tag]
    {outer inner : errors.Tag} (h : outer ≠ inner)
    (outerError : errors.Error outer) (innerError : errors.Error inner) :
    Catch (ds := ds) (errors := errors) (α := α)
      inner
      (Hxl.errs [
        Raised.mk outer outerError,
        Raised.mk inner innerError
      ])
      (Hxl.pure (Except.error [innerError])) := by
  simpa [Raised.errorsAt, h] using
    (Catch.errs_found (ds := ds) (errors := errors) (tag := inner)
      [
        Raised.mk outer outerError,
        Raised.mk inner innerError
      ]
      (by simp [Raised.errorsAt, h]) :
      Catch inner
        (Hxl.errs [
          Raised.mk outer outerError,
          Raised.mk inner innerError
        ])
        (Hxl.pure
          (Except.error (Raised.errorsAt (errors := errors) inner [
            Raised.mk outer outerError,
            Raised.mk inner innerError
          ]))))

theorem catch_pure_error_is_value [DecidableEq errors.Tag]
    {ε : Type u} (tag : errors.Tag) (value : Except ε α) :
    Catch (ds := ds) (errors := errors) (α := Except ε α)
      tag (Hxl.pure value) (Hxl.pure (Except.ok value)) :=
  Catch.pure value

end Hxl

end HxlFormal
